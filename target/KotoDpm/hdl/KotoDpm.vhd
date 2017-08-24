-------------------------------------------------------------------------------
-- Title      : 
-------------------------------------------------------------------------------
-- File       : KotoDpm.vhd
-- Author     : Larry Ruckman  <ruckman@slac.stanford.edu>
-- Company    : SLAC National Accelerator Laboratory
-- Created    : 2015-07-13
-- Last update: 2017-07-05   M. Tecchio, following Dpm10GAxi.vhd from rherbst rcde/dpm_stable/Dpm10GAxi
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2015 SLAC National Accelerator Laboratory
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

use work.RceG3Pkg.all;
use work.StdRtlPkg.all;
use work.AxiLitePkg.all;
use work.AxiStreamPkg.all;
use work.AxiPkg.all;    -- for the 10G Eth to CI

use work.Config.all;

 -- added as is in Dpm10GAxi.vdh
use work.EthMacPkg.all;

entity KotoDpm is
   port (
      -- Debug
      led        : out   slv(1 downto 0);
      -- I2C
      i2cSda     : inout sl;
      i2cScl     : inout sl;
      -- Ethernet: (3 downto 0) for 10Gb
      ethRxP     : in    slv(3 downto 0);
      ethRxM     : in    slv(3 downto 0);
      ethTxP     : out   slv(3 downto 0);
      ethTxM     : out   slv(3 downto 0);
      ethRefClkP : in    sl;
      ethRefClkM : in    sl;
      -- -- RTM High Speed
--      dpmToRtmHsP : out   slv(0 downto 0);
--      dpmToRtmHsM : out   slv(0 downto 0);
--      rtmToDpmHsP : in    slv(0 downto 0);
--      rtmToDpmHsM : in    slv(0 downto 0);
      dpmToRtmHsP : out   slv(NUM_RX_LANES-1 downto 0);
      dpmToRtmHsM : out   slv(NUM_RX_LANES-1 downto 0);
      rtmToDpmHsP : in    slv(NUM_RX_LANES-1 downto 0);
      rtmToDpmHsM : in    slv(NUM_RX_LANES-1 downto 0);
      -- Reference Clocks
      locRefClkP : in    sl;
      locRefClkM : in    sl;
      -- DTM Signals
      dtmRefClkP : in    sl;
      dtmRefClkM : in    sl;
      dtmClkP    : in    slv(1 downto 0);
      dtmClkM    : in    slv(1 downto 0);
      dtmFbP     : out   sl;
      dtmFbM     : out   sl;
      -- Clock Select
      clkSelA    : out   slv(1 downto 0);
      clkSelB    : out   slv(1 downto 0));
end KotoDpm;

architecture TOP_LEVEL of KotoDpm is

   constant TPD_C : time := 1 ns;

   signal sysClk125    : sl;
   signal sysClk125Rst : sl;

   signal sysClk200    : sl;
   signal sysClk200Rst : sl;

  -- AXI-Lite
   signal axiClk            : sl;
   signal axiClkRst         : sl;
   signal extAxilReadMaster : AxiLiteReadMasterType;
   signal extAxilReadSlave  : AxiLiteReadSlaveType;
   signal extAxilWriteMaster: AxiLiteWriteMasterType;
   signal extAxilWriteSlave : AxiLiteWriteSlaveType;

   -- DMA
   signal dmaClk    : slv(2 downto 0);
   signal dmaClkRst : slv(2 downto 0);
   signal dmaState    : RceDmaStateArray(2 downto 0);     -- added as in Dpm10GAxi
   signal dmaObMaster : AxiStreamMasterArray(2 downto 0);
   signal dmaObSlave  : AxiStreamSlaveArray(2 downto 0);
   signal dmaIbMaster : AxiStreamMasterArray(2 downto 0);
   signal dmaIbSlave  : AxiStreamSlaveArray(2 downto 0);
   -- for 10Gb Eth communication to CI
   signal iethRxP : slv(3 downto 0);
   signal iethRxM : slv(3 downto 0);
   signal iethTxP : slv(3 downto 0);
   signal iethTxM : slv(3 downto 0);
   -- for direct AXI connection to HP[2]
   signal userWriteSlave    : AxiWriteSlaveType;
   signal userWriteMaster   : AxiWriteMasterType    := AXI_WRITE_MASTER_INIT_C;
   signal userReadSlave     : AxiReadSlaveType;
   signal userReadMaster    : AxiReadMasterType     := AXI_READ_MASTER_INIT_C;
   signal userInterrupt     : slv(USER_INT_COUNT_C-1 downto 0)    := (others=>'0');

   attribute KEEP_HIERARCHY              : string;
   attribute KEEP_HIERARCHY of U_DpmCore : label is "TRUE";
   attribute KEEP_HIERARCHY of U_AppCore : label is "TRUE";
   
   -- User loopback
   signal userEthObMaster : AxiStreamMasterType;
   signal userEthObSlave  : AxiStreamSlaveType;
   signal userEthIbMaster : AxiStreamMasterType;
   signal userEthIbSlave  : AxiStreamSlaveType;

begin

   --------------------------------------------------
   -- Core
   --------------------------------------------------
   U_DpmCore : entity work.DpmCore
      generic map (
         TPD_G          => TPD_C,
         RCE_DMA_MODE_G => RCE_DMA_AXIS_C,
--         RCE_DMA_MODE_G => RCE_DMA_PPI_C,
         OLD_BSI_MODE_G => false,
--         ETH_10G_EN_G   => false,
         ETH_10G_EN_G   => true,
         AXI_ST_COUNT_G => 3, 
 -- added as is in Dpm10GAxi.vdh
		 UDP_SERVER_EN_G    => true,
         UDP_SERVER_SIZE_G  => 1,
         UDP_SERVER_PORTS_G => (0 => 8192),
         BYP_EN_G           => false,
         BYP_ETH_TYPE_G     => x"AAAA",
         VLAN_EN_G          => false,
         VLAN_SIZE_G        => 1)         
      port map (
         -- I2C
         i2cSda             => i2cSda,
         i2cScl             => i2cScl,
         -- Ethernet
         ethRxP             => iethRxP,
         ethRxM             => iethRxM,
         ethTxP             => iethTxP,
         ethTxM             => iethTxM,
         ethRefClkP         => ethRefClkP,
         ethRefClkM         => ethRefClkM,
         -- Clock Select
         clkSelA            => clkSelA,
         clkSelB            => clkSelB,
         -- Clocks
         sysClk125          => sysClk125,
         sysClk125Rst       => sysClk125Rst,
         sysClk200          => sysClk200,
         sysClk200Rst       => sysClk200Rst,
         -- External Axi Bus, 0xA0000000 - 0xAFFFFFFF
         axiClk             => axiClk,
         axiClkRst          => axiClkRst,
         extAxilReadMaster  => extAxilReadMaster,
         extAxilReadSlave   => extAxilReadSlave,
         extAxilWriteMaster => extAxilWriteMaster,
         extAxilWriteSlave  => extAxilWriteSlave,
         -- DMA Interfaces
         dmaClk             => dmaClk,
         dmaClkRst          => dmaClkRst,
--         dmaState           => open,
         dmaState           => dmaState,
         dmaObMaster        => dmaObMaster,
         dmaObSlave         => dmaObSlave,
         dmaIbMaster        => dmaIbMaster,
         dmaIbSlave         => dmaIbSlave,
         -- User memory access (for the 10 Gb Eth communication to CI)
         userWriteSlave     => userWriteSlave,
         userWriteMaster    => userWriteMaster,
         userReadSlave      => userReadSlave,
         userReadMaster     => userReadMaster,
         -- User Interrupts
         userInterrupt      => (others => '0')
         );  
         
--   -- 1 GigE Mapping
--   ethTxP(0)           <= iethTxP(0);
--   ethTxM(0)           <= iethTxM(0);
--   iethRxP(0)          <= ethRxP(0);
--   iethRxM(0)          <= ethRxM(0);
--   iethRxP(3 downto 1) <= (others => '0');
--   iethRxM(3 downto 1) <= (others => '0');

-- 10 GigE Mapping (a-la-Dpm10GAxi.vdh)
   ethTxP           <= iethTxP;
   ethTxM           <= iethTxM;
   iethRxP          <= ethRxP;
   iethRxM          <= ethRxM;
   
   --------------------------------------------------
   -- Application Core Module
   --------------------------------------------------
   U_AppCore : entity work.KotoDpmAppCore
      generic map (
         TPD_G => TPD_C)
      port map (
         -- Debug
         led                => led,
         -- 250 MHz Reference Oscillator 
         locRefClkP         => locRefClkP,
         locRefClkM         => locRefClkM,
         -- -- RTM High Speed
         dpmToRtmHsP        => dpmToRtmHsP,
         dpmToRtmHsM        => dpmToRtmHsM,
         rtmToDpmHsP        => rtmToDpmHsP,
         rtmToDpmHsM        => rtmToDpmHsM,
         -- DTM Signals
         dtmRefClkP         => dtmRefClkP,
         dtmRefClkM         => dtmRefClkM,
         dtmClkP            => dtmClkP,
         dtmClkM            => dtmClkM,
         dtmFbP             => dtmFbP,
         dtmFbM             => dtmFbM,
         -- CPU System Clocks
         sysClk125          => sysClk125,
         sysClk125Rst       => sysClk125Rst,
         sysClk200          => sysClk200,
         sysClk200Rst       => sysClk200Rst,
         -- External Axi Bus, 0xA0000000 - 0xAFFFFFFF
         axiClk             => axiClk,
         axiRst             => axiClkRst,
         extAxilReadMaster  => extAxilReadMaster,
         extAxilReadSlave   => extAxilReadSlave,
         extAxilWriteMaster => extAxilWriteMaster,
         extAxilWriteSlave  => extAxilWriteSlave,
         -- DMA Interfaces
         dmaClk             => dmaClk,
         dmaRst             => dmaClkRst,
         dmaObMaster        => dmaObMaster,
         dmaObSlave         => dmaObSlave,
         dmaIbMaster        => dmaIbMaster,
         dmaIbSlave         => dmaIbSlave,
         -- User memory access (for 10Gb Eth communication to CI)
         userWriteSlave     => userWriteSlave,
         userWriteMaster    => userWriteMaster,
         userReadSlave      => userReadSlave,
         userReadMaster     => userReadMaster 
    );  
         
    -------------------------
    -- User Ethernet loopback (as in Dpm10GAxi)
    -------------------------
    userEthIbMaster <= userEthObMaster;
    userEthObSlave  <= userEthIbSlave;
           
end architecture TOP_LEVEL;
