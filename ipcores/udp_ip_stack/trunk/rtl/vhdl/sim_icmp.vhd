---------------------------------------------------------------------------
-- Author   : Ali Lown <ali@lown.me.uk>
-- File          : sim_icmp.vhd
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
Entity sim_icmp is
---------------------------------------------------------------------------
end entity;


---------------------------------------------------------------------------
Architecture sim_icmp_1 of sim_icmp is
---------------------------------------------------------------------------

  component ICMP
  port  (
  -- IP layer signals RX
    ip_rx_start : in std_logic;
    ip_rx       : in ipv4_rx_type;
  -- IP layer signals TX
    ip_tx_start          : out std_logic;
    ip_tx                : out ipv4_tx_type;
    ip_tx_result         : in std_logic_vector(1 downto 0);
    ip_tx_data_out_ready : in std_logic;
  -- system signals
    rx_clk : in std_logic;
    tx_clk : in std_logic;
    reset  : in std_logic
  );
  end component ICMP;


  signal clk, reset : std_logic;
  constant clk_period : time := 10ns;

  signal ip_rx_start, ip_tx_start : std_logic;
  signal ip_rx : ipv4_rx_type;
  signal ip_tx : ipV4_tx_type;
  signal ip_tx_result : std_logic_vector(1 downto 0);
  signal ip_tx_dor : std_logic;

begin

  uut : ICMP
  port map (
            rx_clk => clk,
            tx_clk => clk,
            reset  => reset,
            ip_rx_start => ip_rx_start,
            ip_rx       => ip_rx,
            ip_tx_start => ip_tx_start,
            ip_tx       => ip_tx,
            ip_tx_result => ip_tx_result,
            ip_tx_data_out_ready => ip_tx_dor
           );

  clk_proc : process
  begin
    clk <= '1';
    wait for clk_period / 2;
    clk <= '0';
    wait for clk_period / 2;
  end process;

  dor : process(reset)
  begin
    if reset = '1' then
      ip_tx_dor <= '0';
    else
      ip_tx_dor <= '1';
    end if;
  end process;

  stim : process
  begin
    reset <= '1';
    wait for 100 ns;
    reset <= '0';

    -- Send ICMP packet
    ip_rx_start <= '1';
    ip_rx.hdr.is_valid <= '1';
    ip_rx.hdr.protocol <= x"01";
    ip_rx.hdr.data_length <= x"0008";
    ip_rx.hdr.src_ip_addr <= x"aabbccdd";
    ip_rx.hdr.is_broadcast <= '0';
    ip_rx.data.data_in_valid <= '1';
    ip_rx.data.data_in_last <= '0';

    ip_rx.data.data_in <= x"00"; ip_rx_start <= '0'; wait for clk_period;
    ip_rx.data.data_in <= x"08"; wait for clk_period;
    ip_rx.data.data_in <= x"00"; wait for clk_period;
    ip_rx.data.data_in <= x"00"; wait for clk_period;
    ip_rx.data.data_in <= x"DE"; wait for clk_period;
    ip_rx.data.data_in <= x"AD"; wait for clk_period;
    ip_rx.data.data_in <= x"00"; wait for clk_period;
    ip_rx.data.data_in <= x"01"; wait for clk_period;

    ip_rx.data.data_in <= x"00"; wait for clk_period;
    ip_rx.data.data_in <= x"00"; ip_rx.data.data_in_last <= '1'; wait for clk_period;
    ip_rx.data.data_in_last <= '0';
    ip_rx.hdr.is_valid <= '0';
    ip_rx.data.data_in_valid <= '0';

    wait for 1 ms;

 end process;

end architecture sim_icmp_1;

