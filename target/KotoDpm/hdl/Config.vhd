-------------------------------------------------------------------------------
-- Title         : Koto Rce Configuration, Package File
-- File          : KotoRceConfig.vhd
-- Author        : Stephanie Su, stephsu@slac.stanford.edu
-- Created       : 12/11/2015
-------------------------------------------------------------------------------
-- Description:
-- Package file for Koto Rce related use
-------------------------------------------------------------------------------
-- Copyright (c) 2015 by Stephanie Su. All rights reserved.
-------------------------------------------------------------------------------
-- Modification history:
-- 12/11/2015: created.
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
--use IEEE.NUMERIC_STD.all;	-- this is for converting between hex, integer, and binary values

use work.StdRtlPkg.all;
use work.AxiPkg.all;
use work.AxiStreamPkg.all;

package Config is

   constant NUM_RX_LANES : integer := 7;
--   constant NUM_RX_LANES_16 : std_logic_vector(15 downto 0) := to_unsigned(NUM_RX_LANES, 16);

end Config;


