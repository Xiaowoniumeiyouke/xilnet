---------------------------------------------------------------------------
-- Author   : Ali Lown <ali@lown.me.uk>
-- File          : config.vhd
--
-- Abstract :
--
---------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;

package config is
  constant OUR_IP  : std_logic_vector(31 downto 0) := "0A0101F0";
  constant OUR_MAC : std_logic_vector(47 downto 0) := "DEADBEEFCAFE";
end package;
