-------------------------------------------------------------------------------
-- Title      : SSI Stream DMA Controller
-- Project    : General Purpose Core
-------------------------------------------------------------------------------
-- File       : AxiStreamDma.vhd
-- Author     : Ryan Herbst, rherbst@slac.stanford.edu
-- Created    : 2014-04-25
-- Last update: 2015-12-18
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:
-- Generic AXI Stream DMA block for frame at a time transfers.
-------------------------------------------------------------------------------
-- Copyright (c) 2014 by Ryan Herbst. All rights reserved.
-------------------------------------------------------------------------------
-- Modification history:
-- 04/25/2014: created.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

use work.StdRtlPkg.all;
use work.AxiStreamPkg.all;
use work.AxiLitePkg.all;
use work.AxiPkg.all;
use work.AxiDmaPkg.all;

entity AxiStreamHwDma is
   generic (
      TPD_G             : time                 := 1 ns;
      FREE_ADDR_WIDTH_G : integer              := 9;
      AXIL_BASE_ADDR_G  : slv(31 downto 0)     := x"00000000";
      AXI_READY_EN_G    : boolean              := false;
      AXIS_READY_EN_G   : boolean              := false;
      AXIS_CONFIG_G     : AxiStreamConfigType  := AXI_STREAM_CONFIG_INIT_C;
      AXI_CONFIG_G      : AxiConfigType        := AXI_CONFIG_INIT_C;
      AXI_BURST_G       : slv(1 downto 0)      := "01";
      AXI_CACHE_G       : slv(3 downto 0)      := "1111"
   );
   port (

      -- Clock/Reset
      axiClk          : in  sl;   -- copy of sysClk200
      axiRst          : in  sl;

      obReady         : in  sl;
      
      -- Register Access & Interrupt
      axilClk         : in  sl;  -- copy of sysClk125
      axilRst         : in  sl;
      axilReadMaster  : in  AxiLiteReadMasterType;
      axilReadSlave   : out AxiLiteReadSlaveType;
      axilWriteMaster : in  AxiLiteWriteMasterType;
      axilWriteSlave  : out AxiLiteWriteSlaveType;

      -- SSI 
      sAxisMaster     : in  AxiStreamMasterType;
      sAxisSlave      : out AxiStreamSlaveType;
      mAxisMaster     : out AxiStreamMasterType;
      mAxisSlave      : in  AxiStreamSlaveType;
      mAxisCtrl       : in  AxiStreamCtrlType;

      -- AXI Interface
      axiReadMaster   : out AxiReadMasterType;
      axiReadSlave    : in  AxiReadSlaveType;
      axiWriteMaster  : out AxiWriteMasterType;
      axiWriteSlave   : in  AxiWriteSlaveType;
      axiWriteCtrl    : in  AxiCtrlType;
      
      dbgout          : in sl
   );
end AxiStreamHwDma;

architecture structure of AxiStreamHwDma is

   
-- MT Added   
      signal axilReadSlaveProbe:  AxiLiteReadSlaveType;
      signal axilWriteSlaveProbe: AxiLiteWriteSlaveType;
   
      signal ibstateProbe  : slv(1 downto 0);
      attribute mark_debug of ibstateProbe : signal is "true";
      signal obstateProbe  : slv(1 downto 0);
      attribute mark_debug of obstateProbe : signal is "true";
   
      signal sAxisSlaveProbe: AxiStreamSlaveType;
      signal axiWriteMasterProbe  : AxiWriteMasterType;
   
      signal rxEnableProbe      : sl;   
      signal txEnableProbe      : sl;   
   
      component ila_AxiStreamDma
       PORT ( clk         : IN STD_LOGIC;
              trig_in     : IN STD_LOGIC;
              probe0      : IN STD_LOGIC_VECTOR(549 DOWNTO 0) );
      end component;
-- end of MT added
      
   constant countWidth : integer := 10;
   
   type StateType is (S_IDLE_C, S_WAIT_C, S_FIFO_0_C, S_FIFO_1_C);

   type RegType is record
      maxRxSize     : slv(23 downto 0);
      buffAddr      : slv(31 downto 0);
      fifoLoad      : sl;
      rxEnable      : sl;
      txEnable      : sl;
      fifoClear     : sl;
      axiReadSlave  : AxiLiteReadSlaveType;
      axiWriteSlave : AxiLiteWriteSlaveType;
   end record RegType;

   constant REG_INIT_C : RegType := (
      maxRxSize     => x"000400",
      buffAddr      => x"3F000000",
      fifoLoad      => '0',
      rxEnable      => '0',
      txEnable      => '0',
      fifoClear     => '1',
      axiReadSlave  => AXI_LITE_READ_SLAVE_INIT_C,
      axiWriteSlave => AXI_LITE_WRITE_SLAVE_INIT_C
      );

   signal r   : RegType := REG_INIT_C;
   signal rin : RegType;

   type IbType is record
      state         : StateType;
      ibReq         : AxiWriteDmaReqType;
      pendListWrite : sl;
      pendListDin   : slv(31 downto 0);
      freeLoadRead  : sl;
      freeListRead  : sl;
   end record IbType;

   constant IB_INIT_C : IbType := (
      state         => S_IDLE_C,
      ibReq         => AXI_WRITE_DMA_REQ_INIT_C,
      pendListWrite => '0',
      pendListDin   => (others=>'0'),
      freeLoadRead  => '0',
      freeListRead  => '0'
      );

   signal ib   : IbType := IB_INIT_C;
   signal ibin : IbType;

   type ObType is record
      state         : StateType;
      obReq         : AxiReadDmaReqType;
      freeListWrite : sl;
      freeListDin   : slv(31 downto 0);
      freeListCount : slv(countWidth-1 downto 0);
      pendListRead  : sl;
   end record ObType;

   constant OB_INIT_C : ObType := (
      state         => S_IDLE_C,
      obReq         => AXI_READ_DMA_REQ_INIT_C,
      freeListWrite => '0',
      freeListDin   => (others=>'0'),
      freeListCount => (others=>'0'),
      pendListRead  => '0'
      );

   signal ob   : ObType := OB_INIT_C;
   signal obin : ObType;

   signal fifoReset     : sl;
   signal maxSize       : slv(23 downto 0);
   
   signal freeLoadEmpty : sl;
   signal freeLoadRd    : sl;
   signal freeLoadDout  : slv(31 downto 0);

   signal freeListRd    : sl;
   signal freeListDout  : slv(31 downto 0);
   signal freeListFull  : sl;
   signal freeListEmpty : sl;

   signal pendListRd    : sl;
   signal pendListDout  : slv(31 downto 0);
   signal pendListFull  : sl;
   signal pendListEmpty : sl;
   
   signal obAck              : AxiReadDmaAckType;
   signal obReq              : AxiReadDmaReqType;
   signal ibAck              : AxiWriteDmaAckType;
   signal ibReq              : AxiWriteDmaReqType;

begin

  U_FreeLoad : entity work.FifoAsync
    generic map (
      TPD_G            => TPD_G,
      FWFT_EN_G        => true,
      DATA_WIDTH_G     => 32,
      ADDR_WIDTH_G     => 10 )
    port map (
      rst        => fifoReset,
      wr_clk     => axilClk,
      wr_en      => r.fifoLoad,
      din        => r.buffAddr,
      rd_clk     => axiClk,
      rd_en      => freeLoadRd,
      dout       => freeLoadDout,
      empty      => freeLoadEmpty );

  U_FreeList : entity work.FifoSync
    generic map (
      TPD_G            => TPD_G,
      FWFT_EN_G        => true,
      DATA_WIDTH_G     => 32,
      ADDR_WIDTH_G     => 10,
      FULL_THRES_G     =>  3,
      EMPTY_THRES_G    =>  3 )
    port map (
      rst        => fifoReset,
      clk        => axiClk,
      wr_en      => ob.freeListWrite,
      rd_en      => freeListRd,
      din        => ob.freeListDin,
      dout       => freeListDout,
      prog_full  => freeListFull,
      prog_empty => freeListEmpty );

  U_PendList : entity work.FifoSync
    generic map (
      TPD_G            => TPD_G,
      FWFT_EN_G        => true,
      DATA_WIDTH_G     => 32,
      ADDR_WIDTH_G     => 10,
      FULL_THRES_G     =>  2,
      EMPTY_THRES_G    =>  2 )
    port map (
      rst        => fifoReset,
      clk        => axiClk,
      wr_en      => ib.pendListWrite,
      rd_en      => pendListRd,
      din        => ib.pendListDin,
      dout       => pendListDout,
      prog_full  => pendListFull,
      prog_empty => pendListEmpty );
    
   -------------------------------------
   -- Local Register Space
   -------------------------------------

   -- Sync
   process (axilClk) is
   begin
      if (rising_edge(axilClk)) then
         r <= rin after TPD_G;
      end if;
   end process;

   -- Async
   process (r, axiRst, axilReadMaster, axilWriteMaster, ib, freeListEmpty, freeListFull, pendListEmpty, pendListFull  ) is
      variable v         : RegType;
      variable axiStatus : AxiLiteStatusType;
   begin
      v := r;
      v.fifoClear := '0';
      v.fifoLoad  := '0';
      
      axiSlaveWaitTxn(axilWriteMaster, axilReadMaster, v.axiWriteSlave, v.axiReadSlave, axiStatus);

      -- Write
      if (axiStatus.writeEnable = '1') then  -- from AxiLitePkg/axiSlaveWaitWriteTxn
                                             --  writeEnable = 1 if (WM)awvalid=1 && (WM)avalid=1 && (WS)bvalid=0 (which requires (WM)bready=1!)

        case axilWriteMaster.awaddr(4 downto 2) is
            when "000" =>
               v.fifoClear := '1';
               v.maxRxSize := axilWriteMaster.wdata(23 downto 0);
            when "001" =>
               v.buffAddr  := axilWriteMaster.wdata;
               v.fifoLoad  := '1';
            when others =>
               null;
         end case;

         axiSlaveWriteResponse(v.axiWriteSlave);
      end if;

      -- Read
      if (axiStatus.readEnable = '1') then  -- from AxiLitePkg/axiSlaveReadTxn
                                            --  readEnable = 1 if (RM)arvalid=1 && (RS)rvalid=0 (which requires (RM)rready=1!)
                                                   
         v.axiReadSlave.rdata := (others=>'0');

         case axilReadMaster.araddr(4 downto 2) is
            when "000" =>
               v.axiReadSlave.rdata := r.maxRxSize;
            when "010" =>
               v.axiReadSlave.rdata(0) := freeListEmpty;
               v.axiReadSlave.rdata(1) := freeListFull;
               v.axiReadSlave.rdata(2) := pendListEmpty;
               v.axiReadSlave.rdata(3) := pendListFull;
            when others =>
               null;
         end case;

         -- Send Axi Response
         axiSlaveReadResponse(v.axiReadSlave);

      end if;

      -- Reset
      if (axiRst = '1') then
         v := REG_INIT_C;
      end if;

      -- Next register assignment
      rin <= v;

      -- Outputs
--      axilReadSlave     <= r.axiReadSlave;
--      axilWriteSlave    <= r.axiWriteSlave;
      axilReadSlaveProbe     <= r.axiReadSlave;
      axilWriteSlaveProbe    <= r.axiWriteSlave;

--   No change seen with ILA
--      rxEnableProbe         <= r.rxEnable;
--      txEnableProbe         <= r.txEnable;
      rxEnableProbe         <= axiStatus.readEnable;
      txEnableProbe         <= axiStatus.writeEnable;
     
   end process;

  U_SyncClear : entity work.SynchronizerOneShot
     generic map ( TPD_G => TPD_G)
     port map ( clk     => axiClk,
                dataIn  => r.fifoClear,
                dataOut => fifoReset );
    
  U_SyncSize : entity work.SynchronizerVector
     generic map ( TPD_G     => TPD_G,
                   WIDTH_G   => 24 )
     port map ( clk      => axiClk,
                dataIn   => r.maxRxSize,
                dataOut  => maxSize );

   -------------------------------------
   -- Inbound Controller
   -------------------------------------
   U_IbDma : entity work.AxiStreamDmaWrite
      generic map (
         TPD_G            => TPD_G,
         AXI_READY_EN_G   => AXI_READY_EN_G,
         AXIS_CONFIG_G    => AXIS_CONFIG_G,
         AXI_CONFIG_G     => AXI_CONFIG_G,
         AXI_BURST_G      => AXI_BURST_G,
         AXI_CACHE_G      => AXI_CACHE_G
      ) port map (
         axiClk          => axiClk,
         axiRst          => axiRst,
         dmaReq          => ibReq,
         dmaAck          => ibAck,
         axisMaster      => sAxisMaster,
--         axisSlave       => sAxisSlave,
         axisSlave       => sAxisSlaveProbe,
--         axiWriteMaster  => axiWriteMaster,
         axiWriteMaster  => axiWriteMasterProbe,
         axiWriteSlave   => axiWriteSlave,
         axiWriteCtrl    => axiWriteCtrl
      );

   -- Sync
   process (axiClk) is
   begin
      if (rising_edge(axiClk)) then
         ib <= ibin after TPD_G;
      end if;
   end process;

   -- Async
   process (ib, r, axiRst, fifoReset, ibAck, freeListEmpty, freeListDout, pendListFull ) is
      variable v : IbType;
   begin
      v := ib;

      v.freeLoadRead  := '0';
      v.freeListRead  := '0';
      v.pendListWrite := '0';

      case ib.state is

         when S_IDLE_C =>
            v.ibReq.maxSize := x"00" & maxSize;

            if pendListFull = '0' then
              if freeLoadEmpty='0' then
                v.ibReq.request := '1';
                v.ibReq.address := freeLoadDout;
                v.freeLoadRead  := '1';
                v.state         := S_WAIT_C;
              elsif freeListEmpty='0' then
                v.ibReq.request := '1';
                v.ibReq.address := freeListDout;
                v.freeListRead  := '1';
                v.state         := S_WAIT_C;
              end if;
            end if;

         when S_WAIT_C =>
            v.pendListDin := "1" & ib.ibReq.address(30 downto 0);

            if ibAck.done = '1' then
               v.pendListWrite := '1';
               v.state         := S_FIFO_0_C;
            end if;

         when S_FIFO_0_C =>
            v.pendListDin(31 downto 24) := x"E0";
            v.pendListDin(23 downto  0) := ibAck.size(23 downto 0);
            v.pendListWrite             := '1';
            v.state                     := S_FIFO_1_C;

         when S_FIFO_1_C =>
            v.pendListDin(31 downto 26) := x"F" & "00";
            v.pendListDin(25)           := ibAck.overflow;
            v.pendListDin(24)           := ibAck.writeError;
            v.pendListDin(23 downto 16) := ibAck.lastUser;
            v.pendListDin(15 downto  8) := ibAck.firstUser;
            v.pendListDin(7  downto  0) := ibAck.dest;
            v.pendListWrite             := '1';
            v.ibReq.request             := '0';
            v.state                     := S_IDLE_C;

         when others => null;
                        
      end case;

      -- Reset
      if axiRst = '1' or fifoReset = '1' then
         v := IB_INIT_C;
      end if;

      -- Next register assignment
      ibin <= v;

      -- Outputs
      ibReq                   <= ib.ibReq;
      freeListRd              <= v.freeListRead;
      freeLoadRd              <= v.freeLoadRead;

   end process;


   -------------------------------------
   -- Outbound Controller
   -------------------------------------
   U_ObDma : entity work.AxiStreamDmaRead 
      generic map (
         TPD_G            => TPD_G,
         AXIS_READY_EN_G  => AXIS_READY_EN_G,
         AXIS_CONFIG_G    => AXIS_CONFIG_G,
         AXI_CONFIG_G     => AXI_CONFIG_G,
         AXI_BURST_G      => AXI_BURST_G,
         AXI_CACHE_G      => AXI_CACHE_G
      ) port map (
         axiClk          => axiClk,
         axiRst          => axiRst,
         dmaReq          => obReq,
         dmaAck          => obAck,
         axisMaster      => mAxisMaster,
         axisSlave       => mAxisSlave,
         axisCtrl        => mAxisCtrl,
         axiReadMaster   => axiReadMaster,
         axiReadSlave    => axiReadSlave
      );

   -- Sync
   process (axiClk) is
   begin
      if (rising_edge(axiClk)) then
         ob <= obin after TPD_G;
      end if;
   end process;

   -- Async
   process (ob, r, axiRst, fifoReset, obAck, pendListEmpty, pendListDout, obReady ) is
      variable v : ObType;
   begin
      v := ob;

      v.pendListRead  := '0';
      v.freeListWrite := '0';

      case ob.state is

         when S_IDLE_C =>
            v.obReq.address := pendListDout;

            if pendListEmpty = '0' then
               v.pendListRead  := '1';
               v.state         := S_FIFO_0_C;
            end if;

         when S_FIFO_0_C =>
            v.obReq.size := x"00" & pendListDout(23 downto 0);

            if pendListEmpty = '0' then
               v.pendListRead  := '1';
               v.state         := S_FIFO_1_C;
            end if;

         when S_FIFO_1_C =>
            v.obReq.lastUser  := pendListDout(23 downto 16);
            v.obReq.firstUser := pendListDout(15 downto  8);
            v.obReq.dest      := pendListDout(7  downto  0);
            v.obReq.id        := (others=>'0');

            if pendListEmpty = '0' and obReady = '1' then
               v.pendListRead  := '1';
               v.obReq.request := '1';
               v.state         := S_WAIT_C;
            end if;

         when S_WAIT_C =>
            if obAck.done = '1' then
               v.obReq.request := '0';
               v.freeListDin   := "1" & ob.obReq.address(30 downto 0);
               v.freeListWrite := '1';
               v.state         := S_IDLE_C;
            end if;

      end case;

      -- Reset
      if axiRst = '1' then
         v := OB_INIT_C;
      end if;

      -- Next register assignment
      obin <= v;

      -- Outputs
      obReq                 <= ob.obReq;
      pendListRd            <= v.pendListRead;
      
   end process;

--   u_ila : ila_1x256x1024
--     port map ( clk         => axiClk,
--                trig_in     => '0',
--                trig_in_ack => open,
--                probe0(0)   => freeListEmpty,
--                probe0(1)   => ob.freeListWrite,
--                probe0(2)   => freeListRd,
--                probe0(3)   => fifoReset,
--                probe0(4)   => pendListEmpty,
--                probe0(5)   => ib.pendListWrite,
--                probe0(6)   => pendListRd,
--                probe0(7)   => pendListFull,
--                probe0(8)   => obReady,
--                probe0(31  downto   9) => (others=>'0'),
--                probe0(63  downto  32) => ob.freeListDin,
--                probe0(95  downto  64) => ib.pendListDin,
--                probe0(127 downto  96) => freeListDout,
--                probe0(159 downto 128) => pendListDout,
--                probe0(255 downto 160) => (others=>'0')
--                );

-- MT added
   ibstateProbe <=   b"00" when ibin.state = S_IDLE_C else
                     b"01" when ibin.state = S_WAIT_C else
                     b"10" when ibin.state = S_FIFO_0_C else
                     b"11";
   
   obstateProbe <=   b"00" when obin.state = S_IDLE_C else
                     b"01" when obin.state = S_FIFO_0_C else
                     b"10" when obin.state = S_FIFO_1_C else
                     b"11";
   

    u_ila : ila_AxiStreamDma
      port map (clk         => axiClk,
                trig_in     => dbgout,
                probe0(0) => axiRst,
                probe0(32  downto   1) => axilReadMaster.araddr(31 downto 0),
                probe0(33) => axilReadMaster.arvalid,
                probe0(34) => axilReadMaster.rready,
                probe0(66  downto  35) => axilWriteMaster.awaddr(31 downto 0),
                probe0(67) => axilWriteMaster.awvalid,      
                probe0(99  downto  68) => axilWriteMaster.wdata(31 downto 0),  -- ==> buffAddr (U_FreeLOad:Din), initialized to 0x3f000000
                probe0(100) => axilWriteMaster.wvalid,      
                probe0(101) => axilWriteMaster.bready,         
                probe0(133  downto 102) => axilReadSlaveProbe.rdata(31 downto 0),
                probe0(134) => axilReadSlaveProbe.arready,
                probe0(135) => axilWriteSlaveProbe.awready,      
                probe0(136) => axilWriteSlaveProbe.bvalid, 
                probe0(168  downto 137) => ibReq.address(31 downto 0),   -- if(pendListFull = '0') ==> freeLoadDout(31:0) elsif(freeListEmpty='0') ==> freeListDout(31:0)
                probe0(200  downto 169) => ibReq.maxsize(31 downto 0),
                probe0(201) => ibReq.request, 
                probe0(217  downto 202) => sAxisMaster.tData(15 downto 0),
                probe0(218) => sAxisMaster.tValid, 
                probe0(219) => sAxisMaster.tLast, 
                probe0(220) => axiWriteCtrl.pause,
                probe0(221) => axiWriteSlave.awready,
                probe0(222) => axiWriteSlave.bvalid,
                probe0(254  downto 223) => ibAck.size(31 downto 0),
                probe0(255) => ibAck.done, 
                probe0(257  downto 256) => ibstateProbe,
                probe0(258) => sAxisSlaveProbe.tReady, 
                probe0(290  downto 259) => axiWriteMasterProbe.awaddr(31 downto 0),
                probe0(298  downto 291) => axiWriteMasterProbe.awlen(7 downto 0),
                probe0(301  downto 299) => axiWriteMasterProbe.awsize(2 downto 0),
                probe0(303  downto 302) => axiWriteMasterProbe.awburst(1 downto 0),
                probe0(367  downto 304) => axiWriteMasterProbe.wdata(63 downto 0),
                probe0(387  downto 368) => (others=>'0'),
                probe0(388) => axiWriteMasterProbe.awvalid, 
                probe0(389) => axiWriteMasterProbe.wlast, 
                probe0(390) => axiWriteMasterProbe.bready, 
                probe0(391) => fifoReset, 
                probe0(392) => r.fifoLoad, 
                probe0(393) => ib.freeLoadRead, 
                probe0(394) => freeLoadEmpty, 
                probe0(395) => freeLoadRd, 
                probe0(396) => ob.freeListWrite,
                probe0(428  downto 397) => ob.freeListDin(31 downto 0),
                probe0(429) => freeListRd,
                probe0(430) => freeListEmpty,        
                probe0(431) => ib.pendListWrite, 
                probe0(463 downto 432) => ib.pendListDin(31 downto 0),
                probe0(495 downto 464) => pendListDout(31 downto 0),
                probe0(496)  => pendListEmpty,
                probe0(497)  => pendListRd,
                probe0(498)  => pendListFull,
                probe0(499) => txEnableProbe,
                probe0(500) => rxEnableProbe, 
                probe0(501) => obAck.done, 
                probe0(503  downto 502) => obstateProbe,
                probe0(535  downto 504) => obReq.address(31 downto 0),
                probe0(536) => obReq.request, 
                probe0(549  downto 537) => (others=>'0')
       );
    
      axilReadSlave <= axilReadSlaveProbe;
      axilWriteSlave <= axilWriteSlaveProbe;
     
      sAxisSlave <= sAxisSlaveProbe;
      axiWriteMaster  <= axiWriteMasterProbe;
     
end structure;

