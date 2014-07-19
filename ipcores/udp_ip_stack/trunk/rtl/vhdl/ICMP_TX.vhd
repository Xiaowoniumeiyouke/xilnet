---------------------------------------------------------------------------
-- Author   : Ali Lown <ali@lown.me.uk>
-- File          : ICMP_TX.vhd
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
Entity ICMP_TX is
---------------------------------------------------------------------------
port
(
  -- ICMP layer signals
  icmp_tx_start : in std_logic;
  icmp_txi      : in icmp_tx_type;
  icmp_tx_result : out std_logic_vector(1 downto 0);
  icmp_tx_data_out_ready : out std_logic;
  -- System signals
  clk : in std_logic;
  reset : in std_logic;
  -- IP layer signals
  ip_tx_start  : out std_logic;
  ip_tx        : out ipv4_tx_type;
  ip_tx_result : in std_logic_vector(1 downto 0);
  ip_tx_data_out_ready : in std_logic
);
end entity;

---------------------------------------------------------------------------
Architecture Behavioral of ICMP_TX is
---------------------------------------------------------------------------

  type tx_state_type is (IDLE, PAUSE, SEND_ICMP_HDR, SEND_ICMP_DATA);
  type count_mode_type is (RST, INC, HOLD);
  type settable_cnt_type is (RST, SET, INCR, HOLD);
  type set_clr_type is (SET, CLR, HOLD);

  -- state variables
  signal tx_state : tx_state_type;
  signal tx_count : unsigned (15 downto 0);
  signal tx_result_reg : std_logic_vector(1 downto 0);
  signal ip_tx_start_reg : std_logic;
  signal data_out_ready_reg : std_logic;

  -- control signals
  signal next_tx_state : tx_state_type;
  signal next_tx_result : std_logic_vector(1 downto 0);
  signal tx_count_val : unsigned(15 downto 0);
  signal tx_count_mode : settable_cnt_type;
  signal tx_data : std_logic_vector(7 downto 0);
  signal set_ip_tx_start : set_clr_type;
  signal set_last : std_logic;
  signal tx_data_valid : std_logic;

begin

  tx_combinatorial : process (
    -- input signals
    icmp_tx_start, icmp_txi, clk, ip_tx_result, ip_tx_data_out_ready,
    -- state variables
    tx_state, tx_count, tx_result_reg, ip_tx_start_reg, 
    -- control variables
    next_tx_state, next_tx_result, tx_count_mode, tx_count_val, tx_data,
    set_last, tx_data_valid, set_ip_tx_start, set_last
  )
  begin
    -- set output followers
    ip_tx_start <= ip_tx_start_reg;
    ip_tx.hdr.protocol <= x"01";
    ip_tx.hdr.data_length <= x"0008"; -- TODO: fix if making use of the actual data!
    ip_tx.hdr.dst_ip_addr <= icmp_txi.hdr.dst_ip;
    ip_tx.data.data_out_valid <= tx_data_valid;
    if icmp_tx_start = '1' and ip_tx_start_reg = '0' then
      icmp_tx_result <= ICMPTX_RESULT_NONE;
    else
      icmp_tx_result <= tx_result_reg;
    end if;

    case tx_state is
      when SEND_ICMP_DATA =>
        ip_tx.data.data_out <= icmp_txi.data.data_out;
        tx_data_valid <= icmp_txi.data.data_out_valid;
        ip_tx.data.data_out_last <= icmp_txi.data.data_out_last;

      when SEND_ICMP_HDR =>
        ip_tx.data.data_out <= tx_data;
        tx_data_valid <= ip_tx_data_out_ready;
        ip_tx.data.data_out_last <= set_last;

      when others =>
        ip_tx.data.data_out <= (others => '0');
        tx_data_valid <= '0';
        ip_tx.data.data_out_last <= set_last;
    end case;

    -- set signal defaults
    next_tx_state <= IDLE;
    tx_count_mode <= HOLD;
    tx_data <= (others => '0');
    set_last <= '0';
    next_tx_result <= ICMPTX_RESULT_NONE;
    tx_count_val <= (others => '0');
    icmp_tx_data_out_ready <= '0';

    -- TX FSM
    case tx_state is
      when IDLE =>
        tx_count_mode <= RST;
        if icmp_tx_start = '1' then
          -- start to send ICMP header
          next_tx_result <= ICMPTX_RESULT_SENDING;
          set_ip_tx_start <= SET;
          next_tx_state <= PAUSE;
        end if;

      when PAUSE =>
        -- delay one clock for IP layer to respond to ip_tx_start and remove any tx error results
        next_tx_state <= SEND_ICMP_HDR;

      when SEND_ICMP_HDR =>
        next_tx_state <= SEND_ICMP_HDR;
        if ip_tx_result = IPTX_RESULT_ERR then
          set_ip_tx_start <= CLR;
          next_tx_result <= ICMPTX_RESULT_ERR;
        elsif ip_tx_data_out_ready = '1' then
          if tx_count = x"0007" then
            tx_count_mode <= SET;
            tx_count_val <= x"0001";
            next_tx_state <= SEND_ICMP_DATA;
          else
            tx_count_mode <= INCR;
          end if;
          case tx_count is
            when x"0000" => tx_data <= icmp_txi.hdr.code;
            when x"0001" => tx_data <= icmp_txi.hdr.icmptype;
            when x"0002" => tx_data <= icmp_txi.hdr.checksum(15 downto 8);
            when x"0003" => tx_data <= icmp_txi.hdr.checksum(7 downto 0);
            when x"0004" => tx_data <= icmp_txi.hdr.ident(15 downto 8);
            when x"0005" => tx_data <= icmp_txi.hdr.ident(7 downto 0);
            when x"0006" => tx_data <= icmp_txi.hdr.seqnum(15 downto 8);
            when x"0007" => tx_data <= icmp_txi.hdr.seqnum(7 downto 0);
            when others =>  next_tx_result <= ICMPTX_RESULT_ERR;
          end case;
        end if;

      when SEND_ICMP_DATA =>
        next_tx_state <= SEND_ICMP_DATA;
        icmp_tx_data_out_ready <= ip_tx_data_out_ready;
        if ip_tx_data_out_ready = '1' then
          if icmp_txi.data.data_out_valid = '1' or tx_count = x"0000" then
            tx_data <= icmp_txi.data.data_out;
            -- TODO: fixme for packet sizes
            if tx_count = x"0008" then
              -- end of packet as expected
              set_last <= '1';
              next_tx_result <= ICMPTX_RESULT_SENT;
              set_ip_tx_start <= CLR;
              next_tx_state <= IDLE;
            elsif icmp_txi.data.data_out_last = '1' then
              -- less data in packet then expected
              set_last <= '1';
              next_tx_result <= ICMPTX_RESULT_ERR;
              set_ip_tx_start <= CLR;
              next_tx_state <= IDLE;
            else
              -- more data to send...
              tx_count_mode <= INCR;
            end if;
          end if;
        end if;
    end case;
  end process;

  tx_sequential : process (clk, reset, data_out_ready_reg)
  begin
    if rising_edge(clk) then
      if reset = '1' then
        -- reset state variables
        tx_state <= IDLE;
        tx_count <= (others => '0');
        tx_result_reg <= ICMPTX_RESULT_NONE;
        ip_tx_start_reg <= '0';
      else
        -- next state
        tx_state <= next_tx_state;
        tx_result_reg <= next_tx_result;

        case set_ip_tx_start is
          when SET => ip_tx_start_reg <= '1';
          when CLR => ip_tx_start_reg <= '0';
          when HOLD => ip_tx_start_reg <= ip_tx_start_reg;
        end case;

        case tx_count_mode is
          when RST  => tx_count <= (others => '0');
          when SET  => tx_count <= tx_count_val;
          when INCR => tx_count <= tx_count + 1;
          when HOLD => tx_count <= tx_count;
        end case;
      end if;
    end if;
  end process;

end architecture;

