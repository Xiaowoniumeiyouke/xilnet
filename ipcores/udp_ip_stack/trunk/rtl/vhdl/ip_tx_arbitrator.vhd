---------------------------------------------------------------------------
-- Author   : Ali Lown <ali@lown.me.uk>
-- File          : ip_tx_arbitrator.vhd
--
-- Abstract : Arbitrates between ICMP and UDP protocols, giving preference
--            to ICMP when needed.
--
---------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;
use work.axi.all;
use work.ipv4_types.all;

---------------------------------------------------------------------------
Entity ip_tx_arbitrator is
---------------------------------------------------------------------------
port
(
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
  ip_tx_start   : out std_logic;
  ip_tx         : out ipv4_tx_type;
  ip_tx_result  : in std_logic_vector(1 downto 0);
  ip_tx_dor     : in std_logic;
  -- system signals
  clk           : in std_logic;
  reset         : in std_logic
);
end entity;


---------------------------------------------------------------------------
Architecture Behavioral of ip_tx_arbitrator is
---------------------------------------------------------------------------

  type state_type is (IDLE, ICMP, UDP);

  signal state, next_state : state_type;

begin

  arb_comb : process(state, icmp_tx_start, icmp_tx, ip_tx_result, ip_tx_dor,
    udp_tx_start, udp_tx, ip_tx_result, ip_tx_dor)
  begin
    case state is
      when IDLE =>
        ip_tx_start <= '0';
        ip_tx.data.data_out_valid <= '0';
        ip_tx.data.data_out_last  <= '0';
        ip_tx.data.data_out       <= (others => '0');
        ip_tx.hdr.protocol        <= (others => '0');
        ip_tx.hdr.dst_ip_addr     <= (others => '0');
        ip_tx.hdr.data_length     <= (others => '0');

      when ICMP =>
        ip_tx_start    <= icmp_tx_start;
        ip_tx          <= icmp_tx;
        icmp_tx_result <= ip_tx_result;
        icmp_tx_dor    <= ip_tx_dor;

      when UDP =>
        ip_tx_start   <= udp_tx_start;
        ip_tx         <= udp_tx;
        udp_tx_result <= ip_tx_result;
        udp_tx_dor    <= ip_tx_dor;

    end case;
  end process;

  arb_seq : process(clk, reset, icmp_tx_start, udp_tx_start)
  begin
    if rising_edge(clk) then
      if reset = '1' then
        state <= IDLE;
      else
        state <= next_state;
        case state is
          when IDLE =>
            if icmp_tx_start = '1' then
              next_state <= ICMP;
            elsif udp_tx_start = '1' then
              next_state <= UDP;
            else
              -- do nothing
            end if;

          -- Find end of data to switch back to IDLE
          when ICMP =>
            if icmp_tx.data.data_out_last = '1' then
              next_state <= IDLE;
            end if;

          when UDP =>
            if udp_tx.data.data_out_last = '1' then
              next_state <= IDLE;
            end if;

       end case;
      end if;
    end if;
  end process;

end architecture;
