---------------------------------------------------------------------------
-- Author   : Ali Lown <ali@lown.me.uk>
-- File          : ICMP_RX.vhd
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
Entity ICMP_RX is
---------------------------------------------------------------------------
port
(
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
end entity;

---------------------------------------------------------------------------
Architecture Behavioral of ICMP_RX is
---------------------------------------------------------------------------

  type rx_state_type is (IDLE, ICMP_HDR, ICMP_DATA, WAIT_END, ERR);
  type rx_event_type is (NO_EVENT, DATA);
  type settable_count_mode_type is (RST, INCR, SET_VAL, HOLD);
  type set_clr_type is (SET, CLR, HOLD);

  -- state variables
  signal rx_state : rx_state_type;
  signal rx_count : unsigned (15 downto 0);
  signal hdr_valid_reg : std_logic;
  signal checksum : std_logic_vector(15 downto 0);
  signal code     : std_logic_vector(7 downto 0);
  signal icmptype : std_logic_vector(7 downto 0);
  signal seqnum   : std_logic_vector(15 downto 0);
  signal ident    : std_logic_vector(15 downto 0);
  signal icmp_rx_start_reg : std_logic;
  signal src_ip         : std_logic_vector(31 downto 0);

  -- rx control signals
  signal next_rx_state : rx_state_type;
  signal rx_event      : rx_event_type;
  signal rx_count_val  : unsigned (15 downto 0);
  signal rx_count_mode : settable_count_mode_type;
  signal set_checksum_h : std_logic;
  signal set_checksum_l : std_logic;
  signal set_code       : std_logic;
  signal set_type       : std_logic;
  signal set_seqnum_h   : std_logic;
  signal set_seqnum_l   : std_logic;
  signal set_ident_h    : std_logic;
  signal set_ident_l    : std_logic;
  signal set_hdr_valid  : set_clr_type;
  signal dataval        : std_logic_vector(7 downto 0);
  signal set_icmp_rx_start : set_clr_type;
  signal set_src_ip     : std_logic;
  signal set_data_last  : std_logic;

begin

  rx_combinatorial : process (
    -- input signals
    ip_rx, ip_rx_start,
    -- state variables
    rx_state, rx_count, checksum, code, icmptype, seqnum, ident, hdr_valid_reg, src_ip,
    -- control signals
    next_rx_state, rx_event, rx_count_mode, rx_count_val,
    set_checksum_h, set_checksum_l, set_code, set_type,
    set_seqnum_h, set_seqnum_l, set_ident_h, set_ident_l, set_hdr_valid,
    set_icmp_rx_start, set_src_ip, icmp_rx_start_reg, set_data_last
  )
  begin

    --set output followers
    icmp_rx_start <= icmp_rx_start_reg;
    icmp_rxo.hdr.is_valid <= hdr_valid_reg;
    icmp_rxo.hdr.icmptype <= icmptype;
    icmp_rxo.hdr.code     <= code;
    icmp_rxo.hdr.checksum <= checksum;
    icmp_rxo.hdr.ident    <= ident;
    icmp_rxo.hdr.seqnum   <= seqnum;
    icmp_rxo.hdr.src_ip   <= src_ip;

    if rx_state = ICMP_DATA then
      icmp_rxo.data.data_in <= ip_rx.data.data_in;
      icmp_rxo.data.data_in_valid <= ip_rx.data.data_in_valid;
      icmp_rxo.data.data_in_last <= set_data_last;
    else
      icmp_rxo.data.data_in <= (others => '0');
      icmp_rxo.data.data_in_valid <= '0';
      icmp_rxo.data.data_in_last <= '0';
    end if;

    -- set signal defaults
    next_rx_state <= IDLE;
    rx_event <= NO_EVENT;
    rx_count_mode <= HOLD;
    rx_count_val <= (others => '0');
    set_hdr_valid  <= HOLD;
    set_checksum_h <= '0';
    set_checksum_l <= '0';
    set_code <= '0';
    set_type <= '0';
    set_seqnum_h <= '0';
    set_seqnum_l <= '0';
    set_ident_h  <= '0';
    set_ident_l  <= '0';
    dataval <= (others => '0');
    set_icmp_rx_start <= HOLD;
    set_src_ip <= '0';
    set_data_last <= '0';

    -- determine event (if any)
    if ip_rx.data.data_in_valid = '1' then
      rx_event <= DATA;
      dataval  <= ip_rx.data.data_in;
    end if;

    -- RX FSM
    case rx_state is
      when IDLE =>
        case rx_event is
          when NO_EVENT => -- do nothing
          when DATA =>
            if ip_rx.hdr.protocol = x"01" then
              -- ICMP protocol
              next_rx_state <= ICMP_HDR;
              rx_count_mode <= INCR;
              set_src_ip    <= '1';
            else
              -- Ignore packet
              next_rx_state <= WAIT_END;
            end if;
        end case;

      when ICMP_HDR =>
        next_rx_state <= ICMP_HDR;
        case rx_event is
          when NO_EVENT => -- do nothing
          when DATA =>
            if rx_count = x"0007" then
              rx_count_mode <= SET_VAL;
              rx_count_val  <= x"0001";
              next_rx_state <= ICMP_DATA;
            else
              rx_count_mode <= INCR;
            end if;

            if ip_rx.data.data_in_last = '1' then
              next_rx_state <= ERR;
            else
              case rx_count is
                when x"0000" => set_code <= '1';
                when x"0001" => set_type <= '1';
                when x"0002" => set_checksum_h <= '1';
                when x"0003" => set_checksum_l <= '1';
                when x"0004" => set_ident_h <= '1';
                when x"0005" => set_ident_l <= '1';
                when x"0006" => set_seqnum_h <= '1';
                when x"0007" => set_seqnum_l <= '1'; set_hdr_valid <= SET; set_icmp_rx_start <= SET;
                when others =>
              end case;
            end if;
        end case;

      when ICMP_DATA =>
        --TODO: process data here.
        -- for now, just skip the rest of the packet
        set_data_last <= '1';
        next_rx_state <= WAIT_END;

        case rx_event is
          when NO_EVENT => -- do nothing
          when DATA =>
        end case;

      when ERR =>
        if ip_rx.data.data_in_last = '0' then
          next_rx_state <= WAIT_END;
        else
          next_rx_state <= IDLE;
        end if;

      when WAIT_END =>
        next_rx_state <= WAIT_END;
        case rx_event is
          when NO_EVENT => -- do nothing
          when DATA =>
            if ip_rx.data.data_in_last = '1' then
              next_rx_state <= IDLE;
              set_icmp_rx_start <= CLR;
            end if;
        end case;
    end case;
  end process;

  rx_sequential : process (clk, reset)
  begin
    if rising_edge(clk) then
      if reset = '1' then
        -- reset state variables
        rx_state <= IDLE;
        rx_count <= x"0000";
        hdr_valid_reg <= '0';
        checksum <= (others => '0');
        code     <= (others => '0');
        icmptype <= (others => '0');
        seqnum   <= (others => '0');
        ident    <= (others => '0');
        icmp_rx_start_reg <= '0';
      else
        -- next state
        rx_state <= next_rx_state;

        -- rx count processing
        case rx_count_mode is
          when RST     => rx_count <= x"0000";
          when INCR    => rx_count <= rx_count + 1;
          when SET_VAL => rx_count <= rx_count_val;
          when HOLD    => rx_count <= rx_count;
        end case;

        -- data capture
        if set_checksum_h = '1' then checksum(15 downto 8) <= dataval; end if;
        if set_checksum_l = '1' then checksum(7 downto 0) <= dataval; end if;
        if set_code = '1' then code <= dataval; end if;
        if set_type = '1' then icmptype <= dataval; end if;
        if set_seqnum_h = '1' then seqnum(15 downto 8) <= dataval; end if;
        if set_seqnum_l = '1' then seqnum(7 downto 0) <= dataval; end if;
        if set_ident_h = '1' then ident(15 downto 8) <= dataval; end if;
        if set_ident_l = '1' then ident(7 downto 0) <= dataval; end if;

        if set_src_ip = '1' then 
          src_ip <= ip_rx.hdr.src_ip_addr;
        else
          src_ip <= src_ip;
        end if;

        case set_hdr_valid is
          when SET => hdr_valid_reg <= '1';
          when CLR => hdr_valid_reg <= '0';
          when HOLD => hdr_valid_reg <= hdr_valid_reg;
        end case;

        case set_icmp_rx_start is
          when SET => icmp_rx_start_reg <= '1';
          when CLR => icmp_rx_start_reg <= '0';
          when HOLD => icmp_rx_start_reg <= icmp_rx_start_reg;
        end case;
      end if;
    end if;
  end process;

end architecture;
