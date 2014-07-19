---------------------------------------------------------------------------
-- Author   : Ali Lown <ali@lown.me.uk>
-- File          : sim_ip_tx_arbitrator.vhd
--
-- Abstract :
--
---------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;
use work.axi.all;
use work.ipv4_types.all;

---------------------------------------------------------------------------
Entity sim_ip_tx_arbitrator is 
---------------------------------------------------------------------------
end entity;


---------------------------------------------------------------------------
Architecture sim_ip_tx_arbitrator_1 of sim_ip_tx_arbitrator is
---------------------------------------------------------------------------

  component ip_tx_arbitrator
    port (
  -- ICMP TX signals
    icmp_tx_start  : in std_logic;
    icmp_tx        : in ipv4_tx_type;
    icmp_tx_result : out std_logic_vector(1 downto 0);
    icmp_tx_dor    : out std_logic;
  -- UDP TX signals
    udp_tx_start  : in std_logic;
    udp_tx        : in ipv4_tx_type;
    udp_tx_result : out std_logic_vector(1 downto 0);
    udp_tx_dor    : out std_logic;
  -- IP signals
    ip_tx_start  : out std_logic;
    ip_tx        : out ipv4_tx_type;
    ip_tx_result : in std_logic_vector(1 downto 0);
    ip_tx_dor    : in std_logic;
  -- system signals
    clk   : in std_logic;
    reset : in std_logic
  );
  end component ip_tx_arbitrator;

  signal clk, reset : std_logic;
  constant clk_period : time := 10 ns;

  signal icmp_tx_start, udp_tx_start, ip_tx_start : std_logic;
  signal icmp_tx_dor, udp_tx_dor, ip_tx_dor : std_logic;
  signal icmp_tx, udp_tx, ip_tx : ipv4_tx_type;
  signal icmp_tx_hdr, udp_tx_hdr : ipv4_tx_header_type;
  signal icmp_tx_result, udp_tx_result, ip_tx_result : std_logic_vector(1 downto 0);

begin

  uut : ip_tx_arbitrator
  port map (
            clk => clk,
            reset => reset,
            icmp_tx_start => icmp_tx_start,
            icmp_tx => icmp_tx,
            icmp_tx_result => icmp_tx_result,
            icmp_tx_dor => icmp_tx_dor,
            udp_tx_start => udp_tx_start,
            udp_tx => udp_tx,
            udp_tx_result => udp_tx_result,
            udp_tx_dor => udp_tx_dor,
            ip_tx_start => ip_tx_start,
            ip_tx => ip_tx,
            ip_tx_result => ip_tx_result,
            ip_tx_dor => ip_tx_dor
           );

  icmp_tx.hdr <= icmp_tx_hdr;
  udp_tx.hdr <= udp_tx_hdr;

  clk_proc : process
  begin
    clk <= '1';
    wait for clk_period/2;
    clk <= '0';
    wait for clk_period/2;
  end process;

  stim : process
  begin
    reset <= '1';
    icmp_tx_start <= '0';
    udp_tx_start <= '0';
    wait for 100 ns;
    reset <= '0';

    icmp_tx_hdr.dst_ip_addr <= x"0a010101";
    udp_tx_hdr.dst_ip_addr <= x"0a0101fe";

    wait until clk = '1';
    icmp_tx_start <= '1';
    wait until clk = '0';
    wait until clk = '1';
    icmp_tx_start <= '0';
    wait for 50 ns;
    icmp_tx.data.data_out_last <= '1';
    wait until clk = '0';
    wait until clk = '1';
    icmp_tx.data.data_out_last <= '0';
    wait until clk = '0';

    wait for 20 ns;

    wait until clk = '1';
    udp_tx_start <= '1';
    wait until clk = '0';
    wait until clk = '1';
    udp_tx_start <= '0';
    wait for 50 ns;
    udp_tx.data.data_out_last <= '1';
    wait until clk = '0';
    wait until clk = '1';
    udp_tx.data.data_out_last <= '0';
    wait until clk = '0';


  end process;
	
end architecture sim_ip_tx_arbitrator_1;

