-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : KotoDpmAppCore.vhd
-- Author     : Larry Ruckman  <ruckman@slac.stanford.edu>
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-07-13
-- Last update: 2017-07-10  by MT (added 10Gb Eth communication to CI)
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2015 SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

use work.StdRtlPkg.all;
use work.AxiStreamPkg.all;
use work.AxiLitePkg.all;
use work.AxiPkg.all;
use work.RceG3Pkg.all;
use work.Config.all;

library unisim;
use unisim.vcomponents.all;

entity KotoDpmAppCore is
   generic (
      TPD_G : time := 1 ns);
   port (
      -- Debug
      led                : out slv(1 downto 0);
      -- 250 MHz Reference Oscillator 
      locRefClkP         : in  sl;
      locRefClkM         : in  sl;
      -- -- RTM High Speed
--      dpmToRtmHsP        : out slv(0 downto 0);
--      dpmToRtmHsM        : out slv(0 downto 0);
--      rtmToDpmHsP        : in  slv(0 downto 0);
--      rtmToDpmHsM        : in  slv(0 downto 0);
      dpmToRtmHsP        : out slv(NUM_RX_LANES-1 downto 0);
      dpmToRtmHsM        : out slv(NUM_RX_LANES-1 downto 0);
      rtmToDpmHsP        : in  slv(NUM_RX_LANES-1 downto 0);
      rtmToDpmHsM        : in  slv(NUM_RX_LANES-1 downto 0);
      -- DTM Signals
      dtmRefClkP         : in  sl;
      dtmRefClkM         : in  sl;
      dtmClkP            : in  slv(1 downto 0);
      dtmClkM            : in  slv(1 downto 0);
      dtmFbP             : out sl;
      dtmFbM             : out sl;
      -- CPU System Clocks
      sysClk125          : in  sl;
      sysClk125Rst       : in  sl;
      sysClk200          : in  sl;
      sysClk200Rst       : in  sl;
      -- External Axi Bus, 0xA0000000 - 0xAFFFFFFF
      axiClk             : in  sl;
      axiRst             : in  sl;
      extAxilReadMaster  : in  AxiLiteReadMasterType;
      extAxilReadSlave   : out AxiLiteReadSlaveType;
      extAxilWriteMaster : in  AxiLiteWriteMasterType;
      extAxilWriteSlave  : out AxiLiteWriteSlaveType;
      -- DMA Interfaces
      dmaClk             : out slv(2 downto 0);
      dmaRst             : out slv(2 downto 0);
      dmaObMaster        : in  AxiStreamMasterArray(2 downto 0);
      dmaObSlave         : out AxiStreamSlaveArray(2 downto 0);
      dmaIbMaster        : out AxiStreamMasterArray(2 downto 0);
      dmaIbSlave         : in  AxiStreamSlaveArray(2 downto 0);
      -- User memory access (for 10Gb Eth communication to CI)
      userWriteSlave     : in  AxiWriteSlaveType;
      userWriteMaster    : out AxiWriteMasterType;
      userReadSlave      : in  AxiReadSlaveType;
      userReadMaster     : out AxiReadMasterType
      );
end KotoDpmAppCore;

architecture mapping of KotoDpmAppCore is

   component ila_DpmApp
    PORT ( clk         : IN STD_LOGIC;
           trig_in     : IN STD_LOGIC;
--           trig_in_ack : OUT STD_LOGIC;
           probe0      : IN STD_LOGIC_VECTOR(511 DOWNTO 0) );
   end component;
   
   signal extAxilReadSlaveProbe  : AxiLiteReadSlaveType;
   signal extAxilWriteSlaveProbe : AxiLiteWriteSlaveType;  
   attribute mark_debug of extAxilReadSlaveProbe : signal is "true";
   attribute mark_debug of extAxilWriteSlaveProbe : signal is "true";

   signal userWriteMasterProbe : AxiWriteMasterType;

   signal sysClk : sl;
   signal sysRst : sl;

   signal dbgout: sl;
   attribute mark_debug of dbgout : signal is "true";

   signal resetfifos: sl;
   
   -- for 10Gb Eth communication to CI
   signal axiDmaClk : sl;
   signal axiDmaRst : sl;
   
   -- AXISTREAM signals
   -- Rx2000 output to IbFifo
   signal RxAxisMaster   : AxiStreamMasterType;
   -- DMA of incoming ADC data (from IbFifo AxiStreamFifo)
   signal userDmaIbMaster   : AxiStreamMasterType;
   signal userDmaIbSlave    : AxiStreamSlaveType;
   signal userDmaIbCtrl     : AxiStreamCtrlType;
   -- DMA of outgoing ADC data (from ObFifo AxiStreamFifo - unused)
   signal userDmaObMaster   : AxiStreamMasterType;
   signal userDmaObSlave    : AxiStreamSlaveType;
   signal userDmaObCtrl     : AxiStreamCtrlType;
   
   -- AXI signals: 
   -- outputs of AxiStreamDma  inputs of AxiWrite(Read)Fifo's
   signal locReadMaster     : AxiReadMasterType;
   signal locReadSlave      : AxiReadSlaveType;
   signal locWriteMaster    : AxiWriteMasterType;
   signal locWriteSlave     : AxiWriteSlaveType;
   signal locWriteCtrl      : AxiCtrlType;
   -- outputs of AxiStreamDma  inputs of Ib(Ob)AxiStreamFifo's
   signal sAxisMaster       : AxiStreamMasterType;
   signal sAxisSlave        : AxiStreamSlaveType;
   signal mAxisMaster       : AxiStreamMasterType;
   signal mAxisSlave        : AxiStreamSlaveType;
   
   constant KOTO_AXIS_DMA_CONFIG_C : AxiStreamConfigType := (
      TSTRB_EN_C    => false,
      TDATA_BYTES_C => 8,
      TDEST_BITS_C  => 8,
      TID_BITS_C    => 0,
      TKEEP_MODE_C  => TKEEP_COMP_C,
      TUSER_BITS_C  => 4,
      TUSER_MODE_C  => TUSER_FIRST_LAST_C);
      
   -- Axi Lite crossbar configuration
   constant CROSSBAR_CONN_C : slv(15 downto 0)  := x"FFFF";
   constant ADC_INDEX_C     : natural           := 0;
   constant DMA_INDEX_C     : natural           := 1;
   constant AXI_CROSSBAR_MASTERS_CONFIG_C : AxiLiteCrossbarMasterConfigArray(DMA_INDEX_C downto ADC_INDEX_C) := (
      ADC_INDEX_C => (
--        baseAddr        => x"00000000",
        baseAddr        => x"A0000000",
        addrBits        => 10,
        connectivity    => CROSSBAR_CONN_C),
      DMA_INDEX_C => (
--        baseAddr        => x"00000400",
        baseAddr        => x"A0000400",
        addrBits        => 10,
        connectivity    => CROSSBAR_CONN_C));
        
   signal axilReadMasters   : AxiLiteReadMasterArray(AXI_CROSSBAR_MASTERS_CONFIG_C'range);
   signal axilReadSlaves    : AxiLiteReadSlaveArray(AXI_CROSSBAR_MASTERS_CONFIG_C'range);
   signal axilWriteMasters  : AxiLiteWriteMasterArray(AXI_CROSSBAR_MASTERS_CONFIG_C'range);
   signal axilWriteSlaves   : AxiLiteWriteSlaveArray(AXI_CROSSBAR_MASTERS_CONFIG_C'range);

begin

   ---------------------------------
   -- Clock and Reset Configurations
   ---------------------------------

   -- Set the application clock and reset
   sysClk <= sysClk125;
   sysRst <= sysClk125Rst;

   -- Set the DMA Clocks and Resets
   dmaClk <= (others => sysClk);
   dmaRst <= (others => sysRst);

   
   -- For the 10Gb communication to the CI
   axiDmaClk <= sysClk200;
--   axiDmaRst <= sysClk200Rst;
   axiDmaRst <= (sysClk200Rst or resetfifos);
   
   U_AxiCrossbar: entity work.AxiLiteCrossbar
     generic map (
       TPD_G                => TPD_G,
       NUM_SLAVE_SLOTS_G    => 1,
       NUM_MASTER_SLOTS_G   => AXI_CROSSBAR_MASTERS_CONFIG_C'length,
       DEC_ERROR_RESP_G     => AXI_RESP_OK_C,
       MASTERS_CONFIG_G     => AXI_CROSSBAR_MASTERS_CONFIG_C,
       DEBUG_G              => true
     ) port map (
       axiClk               => axiClk,
       axiClkRst            => axiRst,
       sAxiWriteMasters(0)  => extAxilWriteMaster,
--       sAxiWriteSlaves(0)   => extAxilWriteSlave,
       sAxiWriteSlaves(0)   => extAxilWriteSlaveProbe,
       sAxiReadMasters(0)   => extAxilReadMaster,
--       sAxiReadSlaves(0)    => extAxilReadSlave,
       sAxiReadSlaves(0)    => extAxilReadSlaveProbe,
       mAxiWriteMasters     => axilWriteMasters,
       mAxiWriteSlaves      => axilWriteSlaves,
       mAxiReadMasters      => axilReadMasters,
       mAxiReadSlaves       => axilReadSlaves 
     );

   ---------------------
   -- DMA Configurations
   ---------------------
   -- DMA[2] = Loopback Configuration - ACP
   dmaIbMaster(2) <= dmaObMaster(2);
   dmaObSlave(2)  <= dmaIbSlave(2);

   -- DMA[1] = Loopback Configuration - HP1
   dmaIbMaster(1) <= dmaObMaster(1);
   dmaObSlave(1)  <= dmaIbSlave(1);

   -- DMA[0] = Loopback Configuration - HP0
   dmaIbMaster(0) <= dmaObMaster(0);
   dmaObSlave(0)  <= dmaIbSlave(0);

   -- DMA[user] = DMA Inbound only, 2000BaseX data - HP3
   userDmaObSlave   <= AXI_STREAM_SLAVE_INIT_C;
   mAxisSlave       <= AXI_STREAM_SLAVE_INIT_C;

   -- DMA[0] = DMA Inbound only, 2000BaseX data
   Rx2000BaseX_Inst : entity work.Rx2000BaseX
      generic map (
         -- General Configurations
         TPD_G                      => 1 ns,
         AXI_ERROR_RESP_G           => AXI_RESP_OK_C,
         -- FIFO Configurations
         BRAM_EN_G                  => true,
         XIL_DEVICE_G               => "7SERIES",
         USE_BUILT_IN_G             => false,
         GEN_SYNC_FIFO_G            => false,
         ALTERA_SYN_G               => false,
         ALTERA_RAM_G               => "M9K",
         CASCADE_SIZE_G             => 1,
         FIFO_ADDR_WIDTH_G          => 9,
         FIFO_PAUSE_THRESH_G        => 2**8,
         -- AXI Stream Configurations
--         MASTER_AXI_STREAM_CONFIG_G => RCEG3_AXIS_DMA_CONFIG_C,
         -- this new constant is the same as old (see RceG3Pkg.vhd)??!!
         MASTER_AXI_STREAM_CONFIG_G => KOTO_AXIS_DMA_CONFIG_C,
         MASTER_AXI_PIPE_STAGES_G   => 0)
      port map (
        -- RtmHs Signals
         gtRefClkP       => locRefClkP,
         gtRefClkM       => locRefClkM,
--         gtTxP           => dpmToRtmHsP(0),
--         gtTxM           => dpmToRtmHsM(0),
--         gtRxP           => rtmToDpmHsP(0),
--         gtRxM           => rtmToDpmHsM(0),
         gtTxP           => dpmToRtmHsP,
         gtTxM           => dpmToRtmHsM,
         gtRxP           => rtmToDpmHsP,
         gtRxM           => rtmToDpmHsM,
         -- Master Port (mAxisClk)
--         mAxisClk        => sysClk,
--         mAxisRst        => sysRst,
         mAxisClk        => axiDmaClk,
         mAxisRst        => axiDmaRst,
         mAxisMaster     => RxAxisMaster,
         mAxisSlave      => userDmaIbSlave,
         -- Register Interface
         locClk          => axiClk,
         locRst          => axiRst,
         --         axilReadMaster  => extAxilReadMaster,
         --         axilReadSlave   => extAxilReadSlave,
         --         axilWriteMaster => extAxilWriteMaster,
         --         axilWriteSlave  => extAxilWriteSlave,
         axilReadMaster  => axilReadMasters (ADC_INDEX_C),
         axilReadSlave   => axilReadSlaves  (ADC_INDEX_C),
         axilWriteMaster => axilWriteMasters(ADC_INDEX_C),
         axilWriteSlave  => axilWriteSlaves (ADC_INDEX_C),
         -- Extra clock
         clk200          => sysClk200,
         clk200Rst       => sysClk200Rst,
         resetfifos      => resetfifos, 
         dbgout          => dbgout);

    --    Blow off the outbound data
--   dmaObSlave(0) <= AXI_STREAM_SLAVE_FORCE_C;

   -----------------------
   -- Misc. Configurations
   -----------------------  

   led <= (others => '0');

   GEN_IBUFDS : for i in 0 to 1 generate
      IBUFDS_Inst : IBUFDS
         generic map (
            DIFF_TERM => true) 
         port map(
            I  => dtmClkP(i),
            IB => dtmClkM(i),
            O  => open);
   end generate;

   OBUFDS_Inst : OBUFDS
      port map(
         I  => '0',
         O  => dtmFbP,
         OB => dtmFbM);


         
   -- DMA
   
   U_AxiStreamDma : entity work.AxiStreamHwDma
     generic map (TPD_G             => TPD_G,
                  FREE_ADDR_WIDTH_G => 12, --4096 entries
                  AXI_READY_EN_G    => false,
                  AXIS_READY_EN_G   => false,
                  AXIS_CONFIG_G     => KOTO_AXIS_DMA_CONFIG_C,
                  AXI_CONFIG_G      => AXI_HP_INIT_C,
                  AXI_BURST_G       => "01",
                  AXI_CACHE_G       => "0000")
     port map (axiClk           => axiDmaClk,
               axiRst           => axiDmaRst,
               obReady          => '1',
               axilClk          => axiClk,
               axilRst          => axiRst,
               axilReadMaster   => axilReadMasters(DMA_INDEX_C),
               axilReadSlave    => axilReadSlaves(DMA_INDEX_C),
               axilWriteMaster  => axilWriteMasters(DMA_INDEX_C),
               axilWriteSlave   => axilWriteSlaves(DMA_INDEX_C),
--               sAxisMaster      => sAxisMaster,
               sAxisMaster      => userDmaIbMaster,
               sAxisSlave       => sAxisSlave,
               mAxisMaster      => mAxisMaster,
               mAxisSlave       => mAxisSlave,
               mAxisCtrl        => userDmaObCtrl,
               axiReadMaster    => locReadMaster,
               axiReadSlave     => locReadSlave,
               axiWriteMaster   => locWriteMaster,
               axiWriteSlave    => locWriteSlave,
               axiWriteCtrl     => locWriteCtrl,
               dbgout           => dbgout);
               
   -- Inbound AXI Stream FIFO
   U_IbFifo : entity work.AxiStreamFifo
      generic map (
         TPD_G                  => TPD_G,
         PIPE_STAGES_G          => 1,
         SLAVE_READY_EN_G       => true,
         VALID_THOLD_G          => 1,
         BRAM_EN_G              => true,
         XIL_DEVICE_G           => "7SERIES",
         USE_BUILT_IN_G         => false,
         GEN_SYNC_FIFO_G        => false,
         ALTERA_SYN_G           => false,
         ALTERA_RAM_G           => "M9K",
         CASCADE_SIZE_G         => 1,
         FIFO_ADDR_WIDTH_G      => 9,
         FIFO_FIXED_THRESH_G    => true,
         FIFO_PAUSE_THRESH_G    => 500,
         SLAVE_AXI_CONFIG_G     => RCEG3_AXIS_DMA_CONFIG_C,
         Master_AXI_CONFIG_G    => RCEG3_AXIS_DMA_CONFIG_C
      ) port map (
         sAxisClk           => axiDmaClk,
         sAxisRst           => axiDmaRst,
         sAxisMaster        => RxAxisMaster,
         sAxisSlave         => userDmaIbSlave,
         sAxisCtrl          => open,
--         sAxisCtrl         => userDmaIbCtrl,
         fifoPauseThresh    => (others => '1'),
         mAxisClk           => axiDmaClk,
         mAxisRst           => axiDmaRst,
         mAxisMaster        => userDmaIbMaster,
         mAxisSlave         => sAxisSlave
      );
      
   -- Outbound AXI Stream FIFO
   U_ObFifo : entity work.AxiStreamFifo
      generic map (
      TPD_G                  => TPD_G,
      PIPE_STAGES_G          => 1,
      SLAVE_READY_EN_G       => false,
      VALID_THOLD_G          => 1,
      BRAM_EN_G              => true,
      XIL_DEVICE_G           => "7SERIES",
      USE_BUILT_IN_G         => false,
      GEN_SYNC_FIFO_G        => false,
      ALTERA_SYN_G           => false,
      ALTERA_RAM_G           => "M9K",
      CASCADE_SIZE_G         => 1,
      FIFO_ADDR_WIDTH_G      => 9,
      FIFO_FIXED_THRESH_G    => true,
      FIFO_PAUSE_THRESH_G    => 475,
      SLAVE_AXI_CONFIG_G     => RCEG3_AXIS_DMA_CONFIG_C,
      Master_AXI_CONFIG_G    => RCEG3_AXIS_DMA_CONFIG_C
   ) port map (
      sAxisClk           => axiDmaClk,
      sAxisRst           => axiDmaRst,
      sAxisMaster        => mAxisMaster,
      sAxisSlave         => userDmaObSlave,
      sAxisCtrl          => userDmaObCtrl,
      fifoPauseThresh    => (others => '1'),
      mAxisClk           => axiDmaClk,
      mAxisRst           => axiDmaRst,
--      mAxisMaster        => userDmaObMaster,
      mAxisMaster        => open,
      mAxisSlave         => mAxisSlave
   );
   
   
   -- Read Path AXI FIFO
   U_AxiReadPathFifo : entity work.AxiReadPathFifo
      generic map (
         TPD_G                  => TPD_G,
         XIL_DEVICE_G           => "7SERIES",
         USE_BUILT_IN_G         => false,
         GEN_SYNC_FIFO_G        => true,
         ALTERA_SYN_G           => false,
         ALTERA_RAM_G           => "M9K",
         ADDR_LSB_G             => 3,
         ID_FIXED_EN_G          => true,
         SIZE_FIXED_EN_G        => true,
         BURST_FIXED_EN_G       => true,
         LEN_FIXED_EN_G         => false,
         LOCK_FIXED_EN_G        => true,
         PROT_FIXED_EN_G        => true,
         CACHE_FIXED_EN_G       => true,
         ADDR_BRAM_EN_G         => false,
         ADDR_CASCADE_SIZE_G    => 1,
         ADDR_FIFO_ADDR_WIDTH_G => 4,
         DATA_BRAM_EN_G         => false,
         DATA_CASCADE_SIZE_G    => 1,
         DATA_FIFO_ADDR_WIDTH_G => 4,
         AXI_CONFIG_G           => AXI_HP_INIT_C
      ) port map (
         sAxiClk            => axiDmaClk,
         sAxiRst            => axiDmaRst,
         sAxiReadMaster     => locReadMaster,
         sAxiReadSlave      => locReadSlave,
         mAxiClk            => axiDmaClk,
         mAxiRst            => axiDmaRst,
         mAxiReadMaster     => userReadMaster,
         mAxiReadSlave      => userReadSlave
      );
      
      
   -- Write Path AXI FIFO
   U_AxiWritePathFifo : entity work.AxiWritePathFifo
      generic map (
         TPD_G                      => TPD_G,
         XIL_DEVICE_G               => "7SERIES",
         USE_BUILT_IN_G             => false,
         GEN_SYNC_FIFO_G            => true,
         ALTERA_SYN_G               => false,
         ALTERA_RAM_G               => "M9K",
         ADDR_LSB_G                 => 3,
         ID_FIXED_EN_G              => true,
         SIZE_FIXED_EN_G            => true,
         BURST_FIXED_EN_G           => true,
         LEN_FIXED_EN_G             => false,
         LOCK_FIXED_EN_G            => true,
         PROT_FIXED_EN_G            => true,
         CACHE_FIXED_EN_G           => true,
         ADDR_BRAM_EN_G             => true,
         ADDR_CASCADE_SIZE_G        => 1,
         ADDR_FIFO_ADDR_WIDTH_G     => 9,
         DATA_BRAM_EN_G             => true,
         DATA_CASCADE_SIZE_G        => 1,
         DATA_FIFO_ADDR_WIDTH_G     => 9,
         DATA_FIFO_PAUSE_THRESH_G   => 456,
         RESP_BRAM_EN_G              => false,
         RESP_CASCADE_SIZE_G        => 1,
         RESP_FIFO_ADDR_WIDTH_G     => 4,
         AXI_CONFIG_G               => AXI_HP_INIT_C
      ) port map (
         sAxiClk            => axiDmaClk,
         sAxiRst            => axiDmaRst,
         sAxiWriteMaster    => locWriteMaster,
         sAxiWriteSlave     => locWriteSlave,
         sAxiCtrl           => locWriteCtrl,
         mAxiClk            => axiDmaClk,
         mAxiRst            => axiDmaRst,
--         mAxiWriteMaster    => userWriteMaster,
         mAxiWriteMaster    => userWriteMasterProbe,
         mAxiWriteSlave     => userWriteSlave
      );


   u_ila : ila_DpmApp
     port map ( clk         => sysClk200,
                trig_in     => dbgout,
--                trig_in_ack => open,

--    0xA000_0000 AxiLight
       probe0(31  downto   0) => extAxilWriteMaster.awaddr(31 downto 0),
       probe0(63  downto  32) => extAxilWriteMaster.wdata(31 downto 0),
       probe0(64)  => extAxilWriteMaster.awvalid,
       probe0(65)  => extAxilWriteMaster.wvalid,
       probe0(66)  => '0',
       probe0(67)  => extAxilWriteMaster.bready,
       probe0(68) => extAxilWriteSlaveProbe.awready,
       probe0(69) => extAxilWriteSlaveProbe.wready,
       probe0(70) => extAxilWriteSlaveProbe.bvalid,

       probe0(102  downto 71) => extAxilReadMaster.araddr(31 downto 0),
       probe0(103)  => extAxilReadMaster.arvalid,
       probe0(104) => extAxilReadMaster.rready,
       probe0(136  downto  105) => extAxilReadSlaveProbe.rdata(31 downto 0),
       probe0(137) => extAxilReadSlaveProbe.arready,
       probe0(138) => extAxilReadSlaveProbe.rvalid,

--     AxiLite Crossbar outputs
       probe0(154  downto  139) => axilWriteMasters(0).awaddr(15 downto 0),
       probe0(162  downto  155) => axilWriteMasters(0).wdata(7 downto 0),
       probe0(163) => axilWriteMasters(0).awvalid,
       probe0(164) => axilWriteMasters(0).wvalid,
       probe0(165) => axilWriteMasters(0).bready,
       probe0(166) => axilWriteSlaves(0).awready,
       probe0(167) => axilWriteSlaves(0).wready,
       probe0(168) => axilWriteSlaves(0).bvalid,
       probe0(184  downto  169) => axilReadMasters(0).araddr(15 downto 0),
       probe0(185) => axilReadMasters(0).arvalid,
       probe0(186) => axilReadMasters(0).rready,
       probe0(206  downto  187) => axilReadSlaves(0).rdata(19 downto 0),
       probe0(207) => axilReadSlaves(0).arready,
       probe0(208) => axilReadSlaves(0).rvalid,

       probe0(224  downto  209) => axilWriteMasters(1).awaddr(15 downto 0),
       probe0(232  downto  225) => axilWriteMasters(1).wdata(7 downto 0),
       probe0(233) => axilWriteMasters(1).awvalid,
       probe0(234) => axilWriteMasters(1).wvalid,
       probe0(235) => axilWriteMasters(1).bready,
       probe0(236) => axilWriteSlaves(1).awready,
       probe0(237) => axilWriteSlaves(1).wready,
       probe0(238) => axilWriteSlaves(1).bvalid,
       probe0(254  downto  239) => axilReadMasters(1).araddr(15 downto 0),
       probe0(255) => axilReadMasters(1).arvalid,
       probe0(256) => axilReadMasters(1).rready,
       probe0(268  downto  257) => axilReadSlaves(1).rdata(11 downto 0),
       probe0(269) => axilReadSlaves(1).arready,
       probe0(270) => axilReadSlaves(1).rvalid,
       
--     RX2000 output to IbFifo       
       probe0(334  downto  271) => RxAxisMaster.tdata(63 downto 0),
       probe0(335) => RxAxisMaster.tValid,
       probe0(336) => RxAxisMaster.tLast,
       
--     IbFifo output to AxiStreamDma 
       probe0(400  downto  337) => userDmaIbMaster.tdata(63 downto 0),
       probe0(401) => userDmaIbMaster.tValid,
       probe0(402) => userDmaIbMaster.tLast,
       probe0(403) => userDmaIbSlave.tReady,
       
--     AxiStreamDma outputs to IbFifos
       probe0(404) => sAxisSlave.tReady,
       
       -- AxiWritePath inputs
       --  from AxiStreamDma outputs 
       probe0(420  downto  405) => locWriteMaster.awaddr(15 downto 0),
       probe0(428  downto  421) => locWriteMaster.wdata(7 downto 0),
       probe0(429) => locWriteMaster.awvalid,
       probe0(430) => locWriteMaster.wvalid,
       probe0(431) => locWriteMaster.wlast,
       probe0(432) => locWriteMaster.bready,
       --  from DpmCore
       probe0(433) => userWriteSlave.awready,
       probe0(434) => userWriteSlave.wready,
       probe0(435) => userWriteSlave.bvalid,
       
--     AxiWritePath outputs
--     to AxiStreamDma
       probe0(436) => locWriteSlave.awready,
       probe0(437) => locWriteSlave.bvalid,
       probe0(438) => locWriteCtrl.pause,
       probe0(439) => locWriteCtrl.overflow,
--     to DpmCore     
       probe0(455  downto  440) => userWriteMasterProbe.awaddr(15 downto 0),
       probe0(463  downto  456) => userWriteMasterProbe.wdata(7 downto 0),
       probe0(464) => userWriteMasterProbe.awvalid,
       probe0(465) => userWriteMasterProbe.wvalid,
       probe0(466) => userWriteMasterProbe.wlast,
       probe0(467) => userWriteMasterProbe.bready,
--     AxiWritePath input from DpmCore 
       probe0(468) => userWriteSlave.awready,
       probe0(469) => userWriteSlave.wready,
       probe0(470) => userWriteSlave.bvalid,

       probe0(511  downto  471) => (others => '0')
      );
      
      extAxilWriteSlave <= extAxilWriteSlaveProbe;
      extAxilReadSlave  <= extAxilReadSlaveProbe;
      
      userWriteMaster <= userWriteMasterProbe;
           
end mapping;
