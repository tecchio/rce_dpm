-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : Rx2000BaseX_2B.vhd
-- Author     : Larry Ruckman(?)  <ruckman@slac.stanford.edu> 
-- Modified   : Lucas Beaufore <beaufore@umich.edu>
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2014-04-02
-- Last update: 2016-7-19
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:   This module is responsible for receiving data from the RTM
-------------------------------------------------------------------------------
-- Copyright (c) 2014 SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;

use work.StdRtlPkg.all;
use work.AxiLitePkg.all;
use work.AxiStreamPkg.all;
use work.StdRtlPkg.all;
use work.SsiPkg.all;
use work.Config.all;    -- Koto RCE user define

library unisim;
use unisim.vcomponents.all;

entity Rx2000BaseX is
    generic (
        -- General Configurations
        TPD_G                      : time                       := 1 ns;
        AXI_ERROR_RESP_G           : slv(1 downto 0)            := AXI_RESP_SLVERR_C;
        -- FIFO Configurations
        BRAM_EN_G                  : boolean                    := true;
        XIL_DEVICE_G               : string                     := "7SERIES";
        USE_BUILT_IN_G             : boolean                    := false;
        GEN_SYNC_FIFO_G            : boolean                    := false;
        ALTERA_SYN_G               : boolean                    := false;
        ALTERA_RAM_G               : string                     := "M9K";
        CASCADE_SIZE_G             : natural range 1 to (2**24) := 1;
        FIFO_ADDR_WIDTH_G          : natural range 4 to 48      := 9;
        FIFO_PAUSE_THRESH_G        : natural range 1 to (2**24) := 2**8;
        -- AXI Stream Configurations
        MASTER_AXI_STREAM_CONFIG_G : AxiStreamConfigType        := ssiAxiStreamConfig(16, TKEEP_COMP_C);
        MASTER_AXI_PIPE_STAGES_G   : natural range 0 to 16      := 0);      
    port (
        -- RtmHs (High speed) Signals
        gtRefClkP       : in  sl;
        gtRefClkM       : in  sl;
        gtTxP           : out slv(NUM_RX_LANES-1 downto 0); -- Transmission Differential pair to RTM
        gtTxM           : out slv(NUM_RX_LANES-1 downto 0);
        gtRxP           : in  slv(NUM_RX_LANES-1 downto 0); -- Receiving Differential pair from RTM
        gtRxM           : in  slv(NUM_RX_LANES-1 downto 0);
        -- Master Port (mAxisClk) DMA to DPM (Direct Memory Access and Data Processing Module)
        mAxisClk        : in  sl;
        mAxisRst        : in  sl;
        mAxisMaster     : out AxiStreamMasterType; -- Moves data to DPMCore to be written to memory (Direct memory access)
        mAxisSlave      : in  AxiStreamSlaveType;  -- Contains the "ready" signal from the DMA
        -- Register interface
        locClk          : in  sl;
        locRst          : in  sl                     := '0';
        axilReadMaster  : in  AxiLiteReadMasterType  := AXI_LITE_READ_MASTER_INIT_C; -- Used to talk to the AxiBus (memory mapped in C program)
        axilReadSlave   : out AxiLiteReadSlaveType;
        axilWriteMaster : in  AxiLiteWriteMasterType := AXI_LITE_WRITE_MASTER_INIT_C;
        axilWriteSlave  : out AxiLiteWriteSlaveType;
        -- Extra clock
        clk200          : in  sl;
        clk200Rst       : in  sl; 
        -- diagnostic
        resetfifos      : out sl;
        dbgout          : out sl);
end Rx2000BaseX;


architecture rtl of Rx2000BaseX is

-- MT added
    signal ilaclk : sl;
    signal dbgtrg : sl;
    attribute mark_debug of dbgtrg : signal is "true";
    
    signal stateProbe : slv(1 downto 0);
    attribute mark_debug of stateProbe : signal is "true";
    
    -- Integrated Logic Analyzer used for debugging on RX recovered clock
    COMPONENT ila_Rx2000
    
    PORT (
        clk : IN STD_LOGIC;   
        trig_in : IN STD_LOGIC;
        trig_in_ack : OUT STD_LOGIC;
        probe0 : IN STD_LOGIC_VECTOR(99 DOWNTO 0) 
     );
       END COMPONENT  ;
       --end debug
    
-- MT added
    constant AXI_STREAM_SLAVE_FORCE_C : AxiStreamSlaveType := (
      tReady => '1');

    constant SSI_CONFIG_C : AxiStreamConfigType := ssiAxiStreamConfig(2, TKEEP_NORMAL_C); -- Says that it will read out 2 bytes? That is the size of the word.
    
    type RegTypeAxi is record
        txPacket       : sl;
        loopback       : slv(2 downto 0);
        rxReset        : sl;
        testVal        : slv(31 downto 0);
        gtxDebug       : slv( 5 downto 0);
        srcMac         : slv(47 downto 0);  -- Ethernet header (source MAC address) 6 Bytes
        ramWr          : sl;
        ramDin         : slv(47 downto 0);  -- Ethernet header (desination MAC address) 6 Bytes
        ramAddr        : slv( 5 downto 0);
        ethType        : slv(15 downto 0);  -- Ethernet header (Ethernet Type) 2 Bytes
        axilReadSlave  : AxiLiteReadSlaveType;
        axilWriteSlave : AxiLiteWriteSlaveType;
    end record;

    constant REG_AXI_INIT_C : RegTypeAxi := (
        txPacket       => '0',
        loopback       => "000",
        rxReset        => '0',
        testVal        => (others=>'0'),
        gtxDebug       => (others=>'1'),
        srcMac         => x"08005600445d",
        ramWr          => '0',
        ramDin         => x"08005600445e",
        ramAddr        => (others=>'0'),
        --ethType        => x"0909",
        ethType        => x"0800",
        axilReadSlave  => AXI_LITE_READ_SLAVE_INIT_C,
        axilWriteSlave => AXI_LITE_WRITE_SLAVE_INIT_C);
  
    type StateType is (
        IDLE_S,
        RECV_S,
        DROP_S);

    type RegTypeTx is record
        txLength       : slv(15 downto 0);
        txData         : slv(15 downto 0);
        txEn           : sl;
        toggle         : sl;
    end record;

    constant REG_TX_INIT_C : RegTypeTx := (
        txLength       => (others => '0'),
        txData         => (others => '0'),
        txEn           => '0',
        toggle         => '0');
    
    type RegTypeRx is record
        overflow       : sl;
        eventCnt       : slv(31 downto 0);
        dataCnt        : slv(31 downto 0);
        dropCnt        : slv(31 downto 0);
        dstIndex       : slv( 5 downto 0);
        delay          : Slv16Array(6 downto 0);
        valid          : slv( 6 downto 0);
        toggle         : sl;
        state          : StateType;
    end record;
    
    constant REG_RX_INIT_C : RegTypeRx := (
        overflow       => '0',
        eventCnt       => (others => '0'),
        dataCnt        => (others => '0'),
        dropCnt        => (others => '0'),
        dstIndex       => (others => '0'),
        delay          => (others => x"0000"),
        valid          => (others => '0'),
        toggle         => '0',
        state          => IDLE_S);
    
    constant MAX_DST_INDEX_C : slv(5 downto 0) := slv(conv_unsigned(0,6));
    constant REFCLKSEL       : Slv3Array(1 downto 0) := ("101","001");
    
    type rRxArray     is array (natural range <>) of RegTypeRx;
    type StatusCnt    is array (natural range <>) of SlVectorArray(31 downto 0,31 downto 0);
    
    signal rAxi   : RegTypeAxi := REG_AXI_INIT_C;
    signal rAxiin : RegTypeAxi;
    signal rRx    : rRxArray(NUM_RX_LANES-1 downto 0)  := (others=>REG_RX_INIT_C);
    signal rRxin  : rRxArray(NUM_RX_LANES-1 downto 0) ;

    signal txAxisMasterIn   : AxiStreamMasterType   := AXI_STREAM_MASTER_INIT_C;
    signal txAxisMasterTemp : AxiStreamMasterType   := AXI_STREAM_MASTER_INIT_C;

    signal rTx    : RegTypeTx  := REG_TX_INIT_C;
    signal rTxin  : RegTypeTx ;
    
    signal txusrclk : slv(NUM_RX_LANES-1 downto 0);
    signal txoutclk : slv(NUM_RX_LANES-1 downto 0);
    signal txData  : Slv16Array(NUM_RX_LANES-1 downto 0);
    signal txDataK : Slv2Array(NUM_RX_LANES-1 downto 0);
    signal txResetDone : slv(NUM_RX_LANES-1 downto 0);
    signal txUsrRdy : slv(NUM_RX_LANES-1 downto 0);
    
    signal rxusrclk : slv(NUM_RX_LANES-1 downto 0);
    signal rxoutclk : slv(NUM_RX_LANES-1 downto 0);
    signal rxData  : Slv16Array(NUM_RX_LANES-1 downto 0);
    signal rxDataK : Slv2Array(NUM_RX_LANES-1 downto 0);
    signal rxResetDone : slv(NUM_RX_LANES-1 downto 0);
    signal rxUsrRdy : slv(NUM_RX_LANES-1 downto 0);
    signal rxDispErr : Slv2Array(NUM_RX_LANES-1 downto 0);
    signal rxDecErr : Slv2Array(NUM_RX_LANES-1 downto 0);
    
    signal gtRefClk : sl;
    signal cpllLock : slv(NUM_RX_LANES-1 downto 0);
    signal gtxDebugT : Slv32Array(NUM_RX_LANES-1 downto 0);
    signal gtxDebugO : slv(31 downto 0);
    
    signal eventCnt, dataCnt, dropCnt : Slv32Array(NUM_RX_LANES-1 downto 0);
    signal txPacket : sl;
    signal txCtrl : AxiStreamCtrlType;
    
    signal srcMac   : slv(47 downto 0);
    signal ethType  : slv(15 downto 0);
    signal dstMac   : slv(47 downto 0);
    
    signal rxStatus, txStatus : Slv32Array(NUM_RX_LANES-1 downto 0) := (others=>(others=>'0'));
    signal rxStatusCnt : StatusCnt(NUM_RX_LANES-1 downto 0);
    signal txStatusCnt : StatusCnt(NUM_RX_LANES-1 downto 0);
    
    signal rxDV : slv(NUM_RX_LANES-1 downto 0);
    
    signal dbgRdData : Slv24Array(NUM_RX_LANES-1 downto 0);
    signal dbgRdEn : slv(NUM_RX_LANES-1 downto 0);
    signal dbgRst : sl := '0';
    signal dbgRdEmpty : slv(NUM_RX_LANES-1 downto 0);
    signal dbgWrEn : slv(NUM_RX_LANES-1 downto 0);
    signal dbgWrFull : slv(NUM_RX_LANES-1 downto 0);
    
    signal txdbgRdData : slv(5 downto 0);
    signal txdbgRdEn   : sl;
    signal txdbgRst : sl := '0';
    signal txdbgRdEmpty : sl;
    signal txdbgWrEn : sl;
    
    signal rxClken : sl := '0';
    signal rxClr : sl := '0';
    signal rxEnable : sl := '0';
    
--    signal oneShotClear : sl := '0';
--    signal fifoClear : sl := '0';
--    signal notCleared : sl := '1';
    
    signal txAxisMasterOut   : AxiStreamMasterType;
    signal fifoReset : sl := '0';
    signal axilReadEnable : sl := '0';
    signal axilWriteEnable : sl := '0';
 
-- MT added for triggering AxiStreamHwDma ILA
    signal trg_ila : sl := '0';  
begin
    
    -- Make the clock for the ILA Debugging tool
    U_ClockManager : entity work.ClockManager7
    generic map ( TPD_G             => TPD_G,
                INPUT_BUFG_G      => true,
                FB_BUFG_G         => false,
                NUM_CLOCKS_G      => 1,
                CLKIN_PERIOD_G    => 8.0,
                CLKFBOUT_MULT_G   => 8,
                CLKOUT0_DIVIDE_G  => 4 )
    port map (  clkIn             => rxusrclk(0),
                rstIn             => '0',
                clkOut(0)         => ilaclk,
                rstOut(0)         => open );
    
    -- Bank111 RefClk0 U8/U7 (Decode differential pair into gtRefClk)
    ibufds_gt : IBUFDS_GTE2  
    port map ( O               => 	open,
               ODIV2           =>   gtRefClk,
               CEB             => 	'0',
               I               => 	gtRefClkP,
               IB              => 	gtRefClkM );
    
    multi_txoutclk : for i in 0 to NUM_RX_LANES-1 generate
        txusrclk_bufg : BUFG
        port map ( I  => txoutclk(i),
                   O  => txusrclk(i) );
    end generate multi_txoutclk;
    
    -- Create 8 tranceivers to receive data from 8 ADC channels (8 lanes enabled)
    -- Need to change the CPLL_REFCLK_SEL_G for different blocks
    multi_gtx : for i in 0 to NUM_RX_LANES-1 generate
        gtx_channel : entity work.Gtx7Core -- This is the transceiver, it converts from the gtR and gtT pins to data
        generic map ( STABLE_CLOCK_PERIOD_G  => 8.0e-9,
                      CPLL_REFCLK_SEL_G     => ite(i<4,"101","110"),   -- 4 channels in one quad, Ch0 - 3 use SouthRefClk0, Ch4-7 use gtRefClk0 (001) (now change it to 110)
                      RX_BUF_EN_G           => false,
                      RX_OUTCLK_SRC_G       => "OUTCLKPMA",
                      RX_USRCLK_SRC_G       => "RXOUTCLK",
                      RX_DDIEN_G            => '1',
                      RX_DLY_BYPASS_G       => '0',
                      ALIGN_MCOMMA_DET_G    => "TRUE",
                      ALIGN_MCOMMA_EN_G     => '1',
                      ALIGN_PCOMMA_DET_G    => "TRUE",
                      ALIGN_PCOMMA_EN_G     => '1' )
        port map ( stableClkIn   => locClk,
                   cPllRefClkIn  => gtRefClk,
                   cPllLockOut   => cpllLock(i),
                   gtRxRefClkBufg=> rxusrclk(i),    -- each transciever has it's own rxusrclk (reconstructed based on data)
                   gtTxP         => gtTxP(i), -- Transmission
                   gtTxN         => gtTxM(i),
                   gtRxP         => gtRxP(i), -- Receiving 
                   gtRxN         => gtRxM(i),
                   rxOutClkOut   => rxusrclk(i),
                   rxUsrClkIn    => rxusrclk(i),
                   rxUsrClk2In   => rxusrclk(i),
                   rxUserResetIn => rAxi.rxReset,
                   rxUserRdyOut  => rxUsrRdy(i),
                   rxResetDoneOut=> rxResetDone(i),
                   rxDataOut     => rxData(i), -- This is the data received from the ADC
                   rxCharIsKOut  => rxDataK(i),
                   rxDecErrOut   => rxDecErr(i),
                   rxDispErrOut  => rxDispErr(i),
                   txOutClkOut   => txoutclk(i),
                   txUsrClkIn    => txusrclk(i),
                   txUsrClk2In   => txusrclk(i),
                   txUserResetIn => '0',
                   txUserRdyOut  => txUsrRdy(i),
                   txResetDoneOut=> txResetDone(i),
                   txDataIn      => txData(i),
                   txCharIsKIn   => txDataK(i),
                   loopbackIn    => rAxi.loopback,
                   debugOut      => gtxDebugT(i) );
    
        txData(i) <= rTx.txLength when rTx.txEn='1' else
                x"50BC";
        txDataK(i) <= "00" when rTx.txEn='1' else
                 "01";
        
        rxDV(i) <= '1' when (rxDataK(0)="00") else '0'; -- Whether or not received data is valid

    end generate multi_gtx;
    
    -- Debug
    gtxDebugO <= gtxDebugT(0);
  
    -- get data from 8 lanes about the messages, synchronize them in FIFOs
    multi_rx_sync_evt : for i in 0 to NUM_RX_LANES-1 generate
        SynchronizerFifo_1 : entity work.SynchronizerFifo
            generic map ( TPD_G        => TPD_G,
                          DATA_WIDTH_G => 32 )
            port map (  rst    => '0',
                        wr_clk => rxusrclk(0),
                        din    => rRx(i).eventCnt,
                        rd_clk => locClk,
                        dout   => eventCnt(i) );
        SynchronizerFifo_2 : entity work.SynchronizerFifo
            generic map ( TPD_G        => TPD_G,
                          DATA_WIDTH_G => 32 )
            port map (  rst    => '0',
                        wr_clk => rxusrclk(0),
                        din    => rRx(i).dataCnt,   -- data word (2 bytes) counter
                        rd_clk => locClk,
                        dout   => dataCnt(i) );
        SynchronizerFifo_3 : entity work.SynchronizerFifo
            generic map ( TPD_G        => TPD_G,
                          DATA_WIDTH_G => 32 )
            port map (  rst    => '0',
                        wr_clk => rxusrclk(0),
                        din    => rRx(i).dropCnt,
                        rd_clk => locClk,
                        dout   => dropCnt(i) );
    end generate multi_rx_sync_evt;
    
    -- Only do it once: Axi DMA interface to the memory
    SynchronizerVector_SrcMac : entity work.SynchronizerVector
        generic map ( TPD_G        => TPD_G,
                      WIDTH_G => 48 )
        port map (  clk     => rxusrclk(0),  -- system clock (change this)
                    dataIn  => rAxi.srcMac,
                    dataOut => srcMac );   
    SynchronizerVector_EthType : entity work.SynchronizerVector
        generic map ( TPD_G        => TPD_G,
                      WIDTH_G => 16 )   
        port map (  clk     => rxusrclk(0),  -- system clock (change this)
                    dataIn  => rAxi.ethType,
                    dataOut => ethType );              
    -- Transmit data (loop back test - only enabled on Ch0)
    SynchronizerOne_1 : entity work.SynchronizerOneShot
        port map ( clk     => txusrclk(0),
                   dataIn  => rAxiin.txPacket,
                   dataOut => txPacket );
    
    -- generate multiple Rx and Tx status
    multi_RxTx_stat : for i in 0 to NUM_RX_LANES-1 generate
        Sync_RxStatus : entity work.SyncStatusVector
            generic map ( CNT_WIDTH_G => 32,
                          WIDTH_G     => 32 )
            port map (  statusIn(1 downto 0)   => rxDecErr(i),
                        statusIn(3 downto 2)   => rxDispErr(i),
                        statusIn(4)            => rRx(i).toggle,
                        statusIn(5)            => rxResetDone(i),
                        statusIn(6)            => rxUsrRdy(i),
                        statusIn(11 downto  7) => (others=>'0'),
                        statusIn(13 downto 12) => rxDataK(i),
                        statusIn(15 downto 14) => (others=>'0'),
                        statusIn(31 downto 16) => rxData(i),
                        statusOut              => rxStatus(i),
                        cntOut                 => rxStatusCnt(i),
                        cntRstIn               => dbgRst,
                        rollOverEnIn           => (others=>'1'),
                        wrClk                  => rxusrclk(0),
                        rdClk                  => locClk );
                        
        Sync_TxStatus : entity work.SyncStatusVector
            generic map ( CNT_WIDTH_G  => 32,
                          WIDTH_G      => 32 )
            port map (  statusIn(0)            => cpllLock(i),
                        statusIn(1)            => txResetDone(i),
                        statusIn(2)            => rTx.toggle,
                        statusIn(3)            => txUsrRdy(i),
                        statusIn(11 downto  4) => (others=>'0'),
                        statusIn(13 downto 12) => txDataK(i),
                        statusIn(15 downto 14) => (others=>'0'),
                        statusIn(31 downto 16) => txData(i),
                        statusOut              => txStatus(i),
                        cntOut                 => txStatusCnt(i),
                        cntRstIn               => '0',
                        rollOverEnIn           => (others=>'1'),
                        wrClk                  => txusrclk(0),
                        rdClk                  => locClk );
    end generate multi_RxTx_stat;
        
    -------------------------------------------
    -- Responsible for responding to registers 
    -------------------------------------------
    combAxi : process (axilReadMaster, axilWriteMaster, locRst, rAxi,
                       eventCnt, dataCnt, dropCnt, txStatus, txStatusCnt, rxStatus, rxStatusCnt, gtxDebugO) is
        variable v             : RegTypeAxi;
        variable axilStatus    : AxiLiteStatusType;
        variable axilWriteResp : slv(1 downto 0);
        variable axilReadResp  : slv(1 downto 0);
        variable i             : integer;
    begin
        -- Latch the current value
        v := rAxi;
        v.txPacket := '0';
        v.gtxDebug := gtxDebugO(5 downto 0);
        v.ramWr    := '0';
        
        dbgRst    <= '0';
        fifoReset <= '0';
        dbgRdEn   <= (others => '0'); -- Tell the FIFO providing debug data to stop outputting for now
        txdbgRdEn <= '0';
        rxClken  <= '0';
        rxClr  <= '0';
        
        trg_ila  <= '0';

        ---------------------------------------------------------------------
        -- Axi-Lite interface: Adresses correspond to offset from 0xA0000000
        ---------------------------------------------------------------------
        axiSlaveWaitTxn(axilWriteMaster, axilReadMaster, v.axilWriteSlave, v.axilReadSlave, axilStatus);
        
        -- Respond to write request
        if (axilStatus.writeEnable = '1') then
            axilWriteResp := AXI_RESP_OK_C;
            axilWriteEnable <= '1';
            case (axilWriteMaster.awaddr(7 downto 0)) is -- Look at the register address, record the new value into attribute of v
                when X"00"  =>  v.txPacket    := '1';
                when X"0C"  =>  v.loopback    := axilWriteMaster.wdata(2 downto 0);       -- register I enter in the program
                                v.rxReset     := axilWriteMaster.wdata(3);
                when X"30"  =>  dbgRst        <= '1';
                                fifoReset     <= '1';
                                v.gtxDebug    := (others=>'1');
                -- MT added:  controls data into RX_FIFO (in case fiber is not connected)
                when X"34"  =>  rxClken        <= '1';
                when X"38"  =>  rxClr          <= '1';
                when X"40"  =>  v.srcMac(31 downto 0)     := axilWriteMaster.wdata;
                when X"44"  =>  v.srcMac(47 downto 32)    := axilWriteMaster.wdata(15 downto 0);
                                v.ethType                 := axilWriteMaster.wdata(31 downto 16);
                when X"48"  =>  v.ramDin(31 downto 0)     := axilWriteMaster.wdata;
                when X"4C"  =>  v.ramDin(47 downto 32)    := axilWriteMaster.wdata(15 downto 0);
                                v.ramAddr                 := axilWriteMaster.wdata(21 downto 16);
                                v.ramWr                   := '1';
                when X"04"  =>  trg_ila  <= '1';    -- added condition to enable AxiStreamHwDma ILA trigger
                when others =>  axilWriteResp := AXI_ERROR_RESP_G;
            end case;
            axiSlaveWriteResponse(v.axilWriteSlave);
        else
            axilWriteEnable <= '0';
        end if;
        
        -- Respond to read request
        if (axilStatus.readEnable = '1') then
            axilReadResp          := AXI_RESP_OK_C;
            v.axilReadSlave.rdata := (others => '0');
            i                     := conv_integer(axilReadMaster.araddr(8 downto 6));  -- loop over every "40" -> 8th to 6th bits are lane numbers (0 to 7, lane 8 will appear to be lane 0 - [1000] but we only take 3 bits)
            axilReadEnable <= '1';
            case (conv_integer(axilReadMaster.araddr(5 downto 2))) is  -- convert slv to integer (conv_integer)
                -- These are address offsets from the start of the memory block (each hex is how many bytes offest)
                when 0 =>   -- position X"00" (because we only take bit 5 to 2 -> got rid of the last two bits)
                    v.axilReadSlave.rdata := eventCnt(i);
                when 1 =>   -- position X"04" --> 100 turns into 1
                    v.axilReadSlave.rdata := dataCnt(i);
                when 2 =>   -- position X"08"
                    v.axilReadSlave.rdata := dropCnt(i);
                when 3 =>   -- position X"0C"
                    v.axilReadSlave.rdata(3 downto 0) := rAxi.rxReset & rAxi.loopback;
                when 4 =>   -- position X"0C"
                    v.axilReadSlave.rdata := txStatus(i);    -- 32 bit txStatus: 50BCxxxx (idle word)
                when 5 =>   -- position X"10"
                    v.axilReadSlave.rdata := muxSlVectorArray(txStatusCnt(i),2);
                when 6 =>   -- position X"14"
                    v.axilReadSlave.rdata := rxStatus(i);    -- 32 bit txStatus: 50BCxxxx (idle word)
                when 7 =>   -- position X"18"
                    v.axilReadSlave.rdata := muxSlVectorArray(rxStatusCnt(i),4);    -- muxSlVectorArray: return the value of rxStatusCnt at position 4+32*i
                when 8 =>   -- position X"1C"
                    v.axilReadSlave.rdata := muxSlVectorArray(rxStatusCnt(i),0);
                when 9 =>   -- position X"20"
                    v.axilReadSlave.rdata := muxSlVectorArray(rxStatusCnt(i),1);
                when 10 =>   -- position X"24"
                    v.axilReadSlave.rdata := muxSlVectorArray(rxStatusCnt(i),2);
                when 11 =>   -- position X"28"
                    v.axilReadSlave.rdata := muxSlVectorArray(rxStatusCnt(i),3);
                when 12 =>   -- position X"2C"
                    v.axilReadSlave.rdata(31) := dbgRdEmpty(i);  -- high: rx FIFO is empty
                    v.axilReadSlave.rdata(23 downto 0) := dbgRdData(i);  -- rx data content (U_dbgFifo : entity work.FifoAsync)
                    dbgRdEn(i) <= '1';
                when 13 =>   -- position X"30"
                    v.axilReadSlave.rdata(31) := txdbgRdEmpty;
                    v.axilReadSlave.rdata(5 downto 0) := txdbgRdData;
                    txdbgRdEn <= '1';
                when 14 =>   -- position X"34"
                    v.axilReadSlave.rdata := gtxDebugO;
                when 15 =>   -- position X"38"
                        v.axilReadSlave.rdata(0) := rxEnable;
                        v.axilReadSlave.rdata(1) := dbgRst;
                when others => -- invalid position
                    axilReadResp := AXI_ERROR_RESP_G;
            end case;
            axiSlaveReadResponse(v.axilReadSlave);
        else
            axilReadEnable <= '0';
        end if;
        
        rAxiin <= v; -- This is sent to a synchronizer, which then writes it to rAxi
        
        -- Outputs
        axilReadSlave  <= rAxi.axilReadSlave;
        axilWriteSlave <= rAxi.axilWriteSlave;
    end process combAxi;


-- MT added, to hold rxEnable HIGH after write to 0x34 until write to 0x38 
   fdrce : process(rxClr,locClk) is  --process with sensitivity list.
   begin  --"begin" statement for the process. 
     if (rxClr = '1' or dbgRst = '1') then  --Asynchronous clear input
           rxEnable <= '0';
     else  
         if ( rxClken = '1' and  rising_edge(locClk) ) then 
           rxEnable <= '1';
         end if;         
     end if;
   end process fdrce;  --end of process statement.


    combTx : process (rTx) is
        variable v : RegTypeTx;
    begin
        -- Latch the current value
        v := rTx;
        v.toggle := not rTx.toggle;   -- for debugging -> see how many times the clock goes high
        
        if rTx.txEn='1' then
            if allBits(rTx.txLength,'0') then
                v.txEn     := '0';    -- after sending the whole packet (count down all the way to 0, turn transmit signal off) -> only send one packet when you write to reg "00" once
            else
                v.txLength := rTx.txLength-1; -- transmit counters counting down to 0 from x"03FF"
            end if;
        elsif txPacket='1' then   -- if register "00" (80 input) is written to be anything, txPacket will go high
            v.txEn     := '1';
            v.txLength := x"03FF";
        end if;

        rTxIn <= v;
    end process combTx;

    combRx : process (rRx) is
        variable v : RegTypeRx;
        --variable v : rRxArray;
        variable txAxisMaster : AxiStreamMasterType;
    begin 
        v := rRx(0);
        v.toggle := not rRx(0).toggle;

        -- Reset strobing signals
        ssiResetFlags(txAxisMaster);
        txAxisMaster.tData := (others => '0');
        txAxisMaster.tValid := '0';

        -- Check for overflow condition or forced EOFE
        if (txCtrl.overflow = '1') then
            -- Latch the overflow error bit for the data packet
            v.overflow := '1';
        end if;
        
        v.valid := rxDv(0) & rRx(0).valid(rRx(0).valid'left downto 1);

        -- State Machine
        case (rRx(0).state) is
            ----------------------------------------------------------------------
            when IDLE_S =>
                -- Check for a start of frame
--                if (rxDV(0)='1' ) then
                  if (rxDV(0)='1' and rxEnable='1' ) then
                    -- Reset the overflow flag
                    v.overflow           := '0';
                    -- Latch the configuration
                    txAxisMaster.tDest := x"00";
                    txAxisMaster.tId   := x"00";
                    -- increase the event destination MAC address one by one
                    if rRx(0).dstIndex=MAX_DST_INDEX_C then
                        v.dstIndex       := (others=>'0');
                    else
                        v.dstIndex       := rRx(0).dstIndex+1;
                    end if;
                  
                    -- Check if the FIFO is ready
                    if txCtrl.pause = '0' then
                        -- Send the upper packetLength value
                        txAxisMaster.tvalid             := '1';
                        -- Add Ethernet Header: DST (6 bytes), SRC (6 bytes), EthType (2 bytes)
                        txAxisMaster.tData(15 downto 0) := dstMac(15 downto 0);
                        v.delay(0) := dstMac(31 downto 16);
                        v.delay(1) := dstMac(47 downto 32);
                        v.delay(2) := dstMac(15 downto 0);
                        v.delay(3) := dstMac(31 downto 16);
                        v.delay(4) := dstMac(47 downto 32);
                        v.delay(5) := ethType;
                        v.delay(6) := rxData(0);    -- actual data
                        v.valid    := (others=>'1');
                        
                        -- Increment the counter
                        v.eventCnt   := rRx(0).eventCnt + 1;
                        -- Increment the counter
                        v.dataCnt    := rRx(0).dataCnt + 1;
                        -- Set the SOF bit
                        --            ssiSetUserSof(SSI_CONFIG_C, v.txAxisMaster, '1');
                        ssiSetUserSof(SSI_CONFIG_C, txAxisMaster, '1');
                        -- Next State
                        v.state      := RECV_S;
                    else
                        v.state      := DROP_S;
                    end if;
                end if;
            ----------------------------------------------------------------------
            when RECV_S =>
--                if (rRx(0).valid(0)='1') then
                  if (rRx(0).valid(0)='1' and rxEnable='1') then
                    -- Check if the FIFO is ready
                    if txCtrl.pause = '0' then
                        txAxisMaster.tValid             := '1';
                        txAxisMaster.tData(15 downto 0) := rRx(0).delay(0);
                        v.delay(5 downto 0)             := rRx(0).delay(6 downto 1);
                        v.delay(6)                      := rxData(0);
                        -- Increment the counter
                        v.dataCnt    := rRx(0).dataCnt + 1;
                    else
                        v.state      := DROP_S;
                    end if;
                else
                    -- Reset the counter
                    -- v.dataCnt            := (others => '0');
                    -- Set the EOF bit                
                    txAxisMaster.tValid := '1';
                    txAxisMaster.tLast  := '1';
                    -- Set the EOFE bit
                    ssiSetUserEofe(SSI_CONFIG_C, txAxisMaster, rRx(0).overflow);
                    -- Next State
                    v.state := IDLE_S;
                end if;
            ----------------------------------------------------------------------
            when DROP_S =>
--                if (rxDV(0)='0') then
                  if (rxDV(0)='0' and rxEnable='1') then
                    v.dropCnt := rRx(0).dropCnt + 1;
                    v.state   := IDLE_S;
                end if;
        end case;

        -- Register the variable for next clock cycle
        rRxin(0) <= v;
        txAxisMasterTemp <= txAxisMaster;
    end process combRx;

    seqAxi : process (locClk) is
    begin
        if rising_edge(locClk) then
            rAxi <= rAxiin after TPD_G;
        end if;
    end process seqAxi;

    seqTx : process (txusrclk(0)) is
    begin
        if rising_edge(txusrclk(0)) then
            rTx <= rTxin after TPD_G;
        end if;
    end process seqTx;
    
    seqRx : process (rxusrclk(0)) is
    begin
        if rising_edge(rxusrclk(0)) then
            rRx(0) <= rRxin(0) after TPD_G;
            txAxisMasterIn <= txAxisMasterTemp after TPD_G;
        end if;
    end process seqRx;
    
--    clearFifo : process (rxusrclk(0), oneShotClear, txCtrl) is
--    begin
--        if (rising_edge(rxusrclk(0))) then
--            if txCtrl.pause = '1' and (notCleared = '1' or oneShotClear = '1') then
--                oneShotClear <= '1';
--                notCleared <= '0';
--            else
--                oneShotClear <= '0';
--            end if;
--        end if;
--    end process clearFifo;
    
--    fifoClear <= fifoReset or oneShotClear;

    AxiStreamFifo_Inst : entity work.AxiStreamFifoDbg
        generic map(
            -- General Configurations
            TPD_G               => TPD_G,
            PIPE_STAGES_G       => MASTER_AXI_PIPE_STAGES_G,
            SLAVE_READY_EN_G    => false,
            VALID_THOLD_G       => 1,
            -- FIFO configurations
            BRAM_EN_G           => BRAM_EN_G,
            XIL_DEVICE_G        => XIL_DEVICE_G,
            USE_BUILT_IN_G      => USE_BUILT_IN_G,
            GEN_SYNC_FIFO_G     => GEN_SYNC_FIFO_G,
            ALTERA_SYN_G        => ALTERA_SYN_G,
            ALTERA_RAM_G        => ALTERA_RAM_G,
            CASCADE_SIZE_G      => CASCADE_SIZE_G,
            FIFO_ADDR_WIDTH_G   => FIFO_ADDR_WIDTH_G,
            FIFO_FIXED_THRESH_G => true,
            FIFO_PAUSE_THRESH_G => FIFO_PAUSE_THRESH_G,
            CASCADE_PAUSE_SEL_G => (CASCADE_SIZE_G-1),
            -- AXI Stream Port Configurations
            SLAVE_AXI_CONFIG_G  => SSI_CONFIG_C,
            MASTER_AXI_CONFIG_G => MASTER_AXI_STREAM_CONFIG_G)
        port map (
            -- Slave Port
            sAxisClk    => rxusrclk(0),
            sAxisRst    => fifoReset,
            --         sAxisMaster => rRx.txAxisMaster,
            sAxisMaster => txAxisMasterIn,
            sAxisSlave  => open,
            sAxisCtrl   => txCtrl,
            -- Master Port
            mAxisClk    => mAxisClk,
            mAxisRst    => mAxisRst,
            mAxisMaster => txAxisMasterOut, 
            mAxisSlave  => mAxisSlave,
-- added to diagnostic ILA
            ilaclk  => ilaclk,
            dbgtrg  => dbgtrg); 
            
            mAxisMaster <= txAxisMasterOut;  -- this is Rx200Base output to DMA


    U_dstMac : entity work.SimpleDualPortRam
        generic map ( DATA_WIDTH_G  => 48,
                      ADDR_WIDTH_G  =>  6 )
        port map ( clka   => locClk,
                   ena    => '1',
                   wea    => rAxi.ramWr,
                   addra  => rAxi.ramAddr,
                   dina   => rAxi.ramDin,
                   clkb   => rxusrclk(0),
                   enb    => '1',
                   addrb  => rRx(0).dstIndex,
                   doutb  => dstMac );
                   
  
    -- Receive data FIFO (get the data from the traceiver and put it in fifo)
    rx_fifo : for i in 0 to NUM_RX_LANES-1 generate
        U_dbgFifo : entity work.FifoAsync
            generic map ( DATA_WIDTH_G  => 24,  -- width of the fifo
                          ADDR_WIDTH_G  => 11 ) -- depth of the fifo
            port map (  rst    => dbgRst,
                        wr_clk => rxusrclk(0),
                        wr_en  => dbgWrEn(i),
                        full => dbgWrFull(i),
                        din(23 downto 22) => rxDecErr(0),
                        din(21 downto 20) => rxDispErr(0),
                        din(19 downto 18) => "00",
                        din(17 downto 16) => rxDataK(0),
                        din(15 downto  0) => rxData(0),
                        rd_clk => locClk,
                        rd_en  => dbgRdEn(i),    -- enable reading
                        dout   => dbgRdData(i),  -- actual data content (16 bits with some extra DecErr and DispErr bits in the front)
                        empty  => dbgRdEmpty(i) );   -- indicates empty fifo (high)
        
        dbgWrEn(i) <= '0' when (rxData(i)=x"50BC" and rxDataK(0)="01") else   -- only take data into the fifo when the data content is not ("50BC" with K character "01")
--                      '0' when ((rxData(i) and x"8000") /= x"8000") or (dbgWrFull(i) = '1') else         -- stop writing data to fifo when the fifo is almost full or when the first bit is not high (ADC headers and COE words: first two bits are high, energy words: first bit is high)
                      '0' when (dbgWrFull(i) = '1') else         -- stop writing data to fifo when the fifo is almost full
                      '0' when (rxEnable = '0') else             -- MT added: AXI lite controlled enable: default is 0
                      '1';
                      
    end generate rx_fifo;


    -- Currently not used
    U_txdbgFifo : entity work.FifoAsync
    generic map ( DATA_WIDTH_G  => 6,
                  ADDR_WIDTH_G  => 10 )
    port map ( rst    => dbgRst,
               wr_clk => locClk,
               wr_en  => txdbgWrEn,
               din    => gtxDebugO(5 downto 0),
               rd_clk => locClk,
               rd_en  => txdbgRdEn,
               dout   => txdbgRdData(5 downto 0),
               empty  => txdbgRdEmpty );
    
    txdbgWrEn <= '0' when (gtxDebugO(5 downto 0)=rAxi.gtxDebug) else
                 '1';
                 
    dbgout <= dbgtrg or trg_ila; 
    resetfifos <= fifoReset;          

-- MT added
    stateProbe <=   b"00" when rRx(0).state = IDLE_S else
                    b"01" when rRx(0).state = RECV_S else
                    b"10" when rRx(0).state = DROP_S else
                    b"11";

    debug : ila_Rx2000
    PORT MAP (
        clk => ilaclk,
        trig_in => dbgRdEn(0),
        trig_in_ack => open,
        probe0(15 downto 0) => rxData(0)(15 downto 0), 
        probe0(17 downto 16) => rxDataK(0)(1 downto 0), 
        probe0(41 downto 18) => dbgRdData(0)(23 downto 0), 
        probe0(57 downto 42) => rRx(0).delay(0),
        probe0(58) => rxDV(0), 
        probe0(59) => dbgWrFull(0),
        probe0(60) => dbgtrg,
        probe0(62 downto 61) => stateProbe, 
        probe0(63) => fifoReset,
        probe0(64) => axilReadEnable,
        probe0(65) => axilWriteEnable,
        probe0(97 downto 66) => txAxisMasterIn.tData(31 downto 0), 
        probe0(98) => txAxisMasterIn.tValid,
        probe0(99) => txAxisMasterIn.tLast
    );

    dbgtrg <= '0' when rxData(0) = x"50BC" else
              '1';

end rtl;