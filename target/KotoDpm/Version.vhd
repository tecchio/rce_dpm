
library ieee;
use ieee.std_logic_1164.all;

package Version is

   constant FPGA_VERSION_C : std_logic_vector(31 downto 0) := x"AA000001"; -- MAKE_VERSION

   constant BUILD_STAMP_C : string := "10GbAxi_KotoDpm: Vivado v2014.4 (x86_64) Built Sun Jul 30 13:25:42 PST 2017 by tecchio";

end Version;

-------------------------------------------------------------------------------
-- Revision History:
--
-- 30/07/2017 (0xAA000001): First version with working 10GbAxi
--
-------------------------------------------------------------------------------

