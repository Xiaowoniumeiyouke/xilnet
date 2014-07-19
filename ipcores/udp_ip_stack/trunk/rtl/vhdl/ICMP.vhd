---------------------------------------------------------------------------
-- Author   : Ali Lown <ali@lown.me.uk>
-- File          : ICMP.vhd
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
Entity ICMP is
---------------------------------------------------------------------------
port
(
  -- IP layer signals RX
  ip_rx_start  : in std_logic;
  ip_rx        : in ipv4_rx_type;
  -- IP layer signals TX
  ip_tx_start  : out std_logic;
  ip_tx        : out ipv4_tx_type;
  ip_tx_result : in std_logic_vector(1 downto 0);
  ip_tx_data_out_ready : in std_logic;
  -- system signals
  rx_clk       : in std_logic;
  tx_clk       : in std_logic;
  reset        : in std_logic
);
end entity;

---------------------------------------------------------------------------
Architecture Behavioral of ICMP is
---------------------------------------------------------------------------

  ------------------------------------------------------------------------------
  -- Component Declaration for ICMP TX
  ------------------------------------------------------------------------------

  component ICMP_TX
    port (
    -- ICMP layer signals
      icmp_tx_start          : in std_logic;
      icmp_txi               : in icmp_tx_type;
      icmp_tx_result         : out std_logic_vector(1 downto 0);
      icmp_tx_data_out_ready : out std_logic;
    -- System signals
      clk   : in std_logic;
      reset : in std_logic;
    -- IP layer signals
      ip_tx_start          : out std_logic;
      ip_tx                : out ipv4_tx_type;
      ip_tx_result         : in std_logic_vector(1 downto 0);
      ip_tx_data_out_ready : in std_logic
  );
  end component;
 
  ------------------------------------------------------------------------------
  -- Component Declaration for ICMP RX
  ------------------------------------------------------------------------------

  component ICMP_RX
    port (
      -- ICMP layer signals
      icmp_rx_start : out std_logic;
      icmp_rxo      : out icmp_rx_type;
      -- System signals
      clk           : in std_logic;
      reset         : in std_logic;
      -- IP layer signals
      ip_rx_start   : in std_logic;
      ip_rx         : in ipv4_rx_type
    );
  end component;

  -- types
  type state_type is (IDLE, RECEIPT, TRANSMIT);
  type settable_cnt_type is (RST, SET, INCR, HOLD);
  type set_clr_type is (SET, CLR, HOLD);

  -- state variables
  signal state : state_type;
  signal count : unsigned(15 downto 0);
  signal tx_start_reg : std_logic;

  -- control signals
  signal next_state   : state_type;
  signal set_count    : settable_cnt_type;
  signal set_tx_start : set_clr_type;
  signal set_last     : std_logic;
  signal set_hdr      : std_logic;

  signal icmp_rx_start : std_logic;
  signal icmp_rx_int  : icmp_rx_type;

  signal icmp_tx_start : std_logic;
  signal icmp_tx_int : icmp_tx_type;
  signal icmp_tx_hdr : icmp_tx_header_type;
  signal icmp_tx_data_out_ready : std_logic;
  signal icmp_tx_result : std_logic_vector(1 downto 0);

begin

  -- Instantiate the ICMP TX block
  icmp_tx_block : ICMP_TX port map (
                                   -- ICMP layer signals
                                   icmp_tx_start  => icmp_tx_start,
                                   icmp_txi       => icmp_tx_int,
                                   icmp_tx_result => icmp_tx_result,
                                   icmp_tx_data_out_ready => icmp_tx_data_out_ready,
                                   -- system signals
                                   clk            => tx_clk,
                                   reset          => reset,
                                   -- IP layer TX signals
                                   ip_tx_start    => ip_tx_start,
                                   ip_tx          => ip_tx,
                                   ip_tx_result   => ip_tx_result,
                                   ip_tx_data_out_ready => ip_tx_data_out_ready
                                   );

  -- Instantiate the ICMP RX block
  icmp_rx_block : ICMP_RX port map (
                                    -- ICMP layer signals
                                    icmp_rxo       => icmp_rx_int,
                                    icmp_rx_start => icmp_rx_start,
                                    -- system signals
                                    clk            => rx_clk,
                                    reset          => reset,
                                    -- IP layer RX signals
                                    ip_rx_start    => ip_rx_start,
                                    ip_rx          => ip_rx
                                   );

  -- Process received ICMP packets, and transmit responses as appropriate

  proc_combinatorial : process (
    -- inputs
    icmp_rx_start,
    -- state
    state, count,
    -- controls
    tx_start_reg, set_last, icmp_tx_hdr, icmp_rx_int, icmp_tx_result, icmp_tx_data_out_ready
    )
  begin
    -- set output followers
    icmp_tx_start <= tx_start_reg;
    icmp_tx_int.data.data_out_last <= set_last;
    icmp_tx_int.hdr <= icmp_tx_hdr;

    -- set defaults
    set_count <= HOLD;
    set_tx_start <= HOLD;
    set_last <= '0';
    set_hdr  <= '0';

    -- FSM
    case state is
      when IDLE =>
        set_count <= RST;
        if icmp_rx_start = '1' then
          case icmp_rx_int.hdr.icmptype is
            when x"08" =>
              -- ECHO
              set_hdr <= '1';
              if icmp_rx_int.data.data_in_last = '1' then
                set_tx_start <= SET;
                next_state <= TRANSMIT;
              else
                next_state <= RECEIPT;
              end if;

            when others =>
              -- do nothing
          end case;
        end if;

      when RECEIPT =>
        -- wait until fully received packet
        if icmp_rx_int.data.data_in_last = '1' then
          next_state <= TRANSMIT;
          set_tx_start <= SET;
        end if;

      when TRANSMIT =>
        set_tx_start <= CLR;
        if icmp_tx_result = ICMPTX_RESULT_ERR then
          -- error from IP TX layer, fail
          set_tx_start <= CLR;
          next_state <= IDLE;
        else
          if icmp_tx_result = ICMPTX_RESULT_SENDING then
            set_tx_start <= CLR;
          end if;
          icmp_tx_int.data.data_out_valid <= icmp_tx_data_out_ready;
          if icmp_tx_data_out_ready = '1' then
            if unsigned(count) = x"05" then
              set_last <= '1';
              set_tx_start <= CLR;
              next_state <= IDLE;
            else
              set_count <= INCR;
          end if;
        end if;
      end if;
    end case;
  end process;

  proc_sequential : process (tx_clk, reset)
  begin
    if rising_edge(tx_clk) then
      if reset = '1' then
        -- reset to defaults
        state <= IDLE;
        count <= (others => '0');
        icmp_tx_hdr.dst_ip   <= (others => '0');
        icmp_tx_hdr.icmptype <= (others => '0');
        icmp_tx_hdr.code     <= (others => '0');
        icmp_tx_hdr.checksum <= (others => '0');
        icmp_tx_hdr.ident    <= (others => '0');
        icmp_tx_hdr.seqnum   <= (others => '0');
        tx_start_reg         <= '0';
      else
        -- transition to next state
        state <= next_state;

        -- set tx header
        if set_hdr = '1' then
          icmp_tx_hdr.icmptype <= x"00"; --echo reply message
          icmp_tx_hdr.code     <= x"00";
          icmp_tx_hdr.ident    <= icmp_rx_int.hdr.ident;
          icmp_tx_hdr.dst_ip   <= icmp_rx_int.hdr.src_ip;
          icmp_tx_hdr.checksum <= x"0000";
        else
          icmp_tx_hdr <= icmp_tx_hdr;
        end if;

        -- set tx start
        case set_tx_start is
          when SET  => tx_start_reg <= '1';
          when CLR  => tx_start_reg <= '0';
          when HOLD => tx_start_reg <= tx_start_reg;
        end case;

        case set_count is
          when RST  => count <= (others => '0');
          when SET  => count <= count;
          when INCR => count <= count + 1;
          when HOLD => count <= count;
        end case;
      end if;
    end if;
  end process;

end architecture;
