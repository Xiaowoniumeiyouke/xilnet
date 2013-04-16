--------------------------------------------------------------------------------
-- Project    : low latency UDP
-- File       : xv6mac_straight
-- Version    : 0.0
-------------------------------------------------------------------------------
--
--
library unisim;
use unisim.vcomponents.all;

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity xv6mac_straight is
  port (
         -- System controls
         ------------------
         glbl_rst                      : in  std_logic;               -- asynchronous reset
         mac_reset                    : in  std_logic;              -- reset mac layer
         clk_in_p                   : in  std_logic;              -- 200MHz clock input from board
         clk_in_n                   : in  std_logic;

         -- MAC Transmitter (AXI-S) Interface
         ---------------------------------------------
         mac_tx_clock                 : out  std_logic;             -- data sampled on rising edge
         mac_tx_tdata               : in  std_logic_vector(7 downto 0); -- data byte to tx
         mac_tx_tvalid              : in  std_logic;              -- tdata is valid
         mac_tx_tready              : out std_logic;              -- mac is ready to accept data
         mac_tx_tlast               : in  std_logic;              -- indicates last byte of frame

         -- MAC Receiver (AXI-S) Interface
         ------------------------------------------
         mac_rx_clock                 : out  std_logic;             -- data valid on rising edge
         mac_rx_tdata               : out std_logic_vector(7 downto 0); -- data byte received
         mac_rx_tvalid              : out std_logic;              -- indicates tdata is valid
         mac_rx_tready              : in  std_logic;              -- tells mac that we are ready to take data
         mac_rx_tlast               : out std_logic;              -- indicates last byte of the trame

         -- GMII Interface
         -----------------
         phy_resetn                    : out std_logic;
         gmii_txd                      : out std_logic_vector(7 downto 0);
         gmii_tx_en                    : out std_logic;
         gmii_tx_er                    : out std_logic;
         gmii_tx_clk                   : out std_logic;
         gmii_rxd                      : in  std_logic_vector(7 downto 0);
         gmii_rx_dv                    : in  std_logic;
         gmii_rx_er                    : in  std_logic;
         gmii_rx_clk                   : in  std_logic;
         gmii_col                      : in  std_logic;
         gmii_crs                      : in  std_logic;
         gmii_gtx_clk                  : out std_logic;
         phy_int                       : in std_logic;
         phy_mdc                       : out std_logic;
         phy_mdio                      : inout std_logic;

         --Debugging
         icon_control0 : inout std_logic_vector(35 downto 0);
         icon_control1 : inout std_logic_vector(35 downto 0)
       );
end xv6mac_straight;

architecture wrapper of xv6mac_straight is

  component mac_ila
    PORT (
      CONTROL : INOUT STD_LOGIC_VECTOR(35 DOWNTO 0);
      CLK : IN STD_LOGIC;
      TRIG0 : IN STD_LOGIC_VECTOR(25 DOWNTO 0);
      TRIG1 : IN STD_LOGIC_VECTOR(9 DOWNTO 0));
  end component;

  component mac2_ila
    PORT (
      CONTROL : INOUT STD_LOGIC_VECTOR(35 DOWNTO 0);
      CLK : IN STD_LOGIC;
      TRIG0 : IN STD_LOGIC_VECTOR(38 DOWNTO 0);
      TRIG1 : IN STD_LOGIC_VECTOR(12 DOWNTO 0);
      TRIG2 : IN STD_LOGIC_VECTOR(38 DOWNTO 0));
  end component;

  component MAC_top
    Port(
        --System
        Reset : IN std_logic;
        Clk_125M : IN std_logic;
        Clk_user : IN std_logic;
        Clk_reg : IN std_logic;
        Speed : OUT std_logic_vector(2 downto 0);

        --User interface
        Rx_mac_ra : OUT std_logic;
        Rx_mac_rd : IN std_logic;
        Rx_mac_data : OUT std_logic_vector(31 downto 0);
        Rx_mac_BE : OUT std_logic_vector(1 downto 0);
        Rx_mac_pa : OUT std_logic;
        Rx_mac_sop : OUT std_logic;
        Rx_mac_eop : OUT std_logic;

        Tx_mac_wa : OUT std_logic;
        Tx_mac_wr : IN std_logic;
        Tx_mac_data : IN std_logic_vector(31 downto 0);
        Tx_mac_BE : IN std_logic_vector(1 downto 0);
        Tx_mac_sop : IN std_logic;
        Tx_mac_eop : IN std_logic;

        --Pkt Length FIFO
        Pkg_lgth_fifo_rd : IN std_logic;
        Pkg_lgth_fifo_ra : OUT std_logic;
        Pkg_lgth_fifo_data : OUT std_logic_vector(15 downto 0);

        --GMII
        Rx_clk : IN std_logic;
        Rx_er : IN std_logic;
        Rx_dv : IN std_logic;
        Rxd : IN std_logic_vector(7 downto 0);
        Tx_clk : IN std_logic;
        Tx_er : OUT std_logic;
        Tx_en : OUT std_logic;
        Txd : OUT std_logic_vector(7 downto 0);
        Crs : IN std_logic;
        Col : IN std_logic;
        Gtx_clk : OUT std_logic;

        --Host interface
        CSB : IN std_logic;
        WRB : IN std_logic;
        CD_in : IN std_logic_vector(15 downto 0);
        CD_out : OUT std_logic_vector(15 downto 0);
        CA : IN std_logic_vector(7 downto 0);

        --MDIO interface
        Mdi : IN std_logic;
        Mdo : OUT std_logic;
        MdoEn : OUT std_logic;
        Mdc : OUT std_logic
      );

  end component MAC_top;

  component maindcm
  port
   (-- Clock in ports
    SYSCLK_P         : in     std_logic;
    SYSCLK_N         : in     std_logic;
    -- Clock out ports
    CLK_125M          : out    std_logic;
    CLK_80M          : out    std_logic;
    CLK_66M          : out    std_logic;
    CLK_125M_INV     : out    std_logic;
    -- Status and control signals
    RESET             : in     std_logic;
    LOCKED            : out    std_logic
   );
  end component;

  COMPONENT fifo32to8
    PORT (
      rst : IN STD_LOGIC;
      wr_clk : IN STD_LOGIC;
      rd_clk : IN STD_LOGIC;
      din : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
      wr_en : IN STD_LOGIC;
      rd_en : IN STD_LOGIC;
      dout : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
      full : OUT STD_LOGIC;
      empty : OUT STD_LOGIC
    );
  END COMPONENT;

  COMPONENT fifo8to32
    PORT (
      rst : IN STD_LOGIC;
      wr_clk : IN STD_LOGIC;
      rd_clk : IN STD_LOGIC;
      din : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
      wr_en : IN STD_LOGIC;
      rd_en : IN STD_LOGIC;
      dout : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
      full : OUT STD_LOGIC;
      empty : OUT STD_LOGIC
    );
  END COMPONENT;

  constant BOARD_PHY_ADDR                  : std_logic_vector(7 downto 0)  := "00000111";
  ------------------------------------------------------------------------------
  -- internal signals used in this top level wrapper.
  ------------------------------------------------------------------------------

  -- example design clocks
  signal clk_125, clk_80, clk_66, clk_125_invert : std_logic;
  signal dcm_locked : std_logic;
  signal clk_gtx_prebuf : std_logic;
  signal clk_gtx_invert : std_logic;
  signal gmii_rx_clk_buf : std_logic;
  signal gmii_rx_clk_invert : std_logic;

  -- tx handshaking
  signal mac_tx_tready_int            : std_logic;
  signal tx_full_reg                : std_logic;
  signal tx_full_val                : std_logic;
  signal tx_data_reg                : std_logic_vector(7 downto 0);
  signal tx_last_reg                : std_logic;
  signal set_tx_reg                 : std_logic;

  signal is_txing : std_logic := '0';

  signal phy_resetn_int                    : std_logic;

  -- gmii buffering
  signal gmii_txd_int : std_logic_vector(7 downto 0);
  signal gmii_tx_er_int : std_logic;
  signal gmii_tx_en_int : std_logic;

  -- resets (and reset generation)
  signal local_chk_reset                   : std_logic;
  signal chk_reset_int                     : std_logic;
  signal chk_pre_resetn                    : std_logic := '0';
  signal chk_resetn                        : std_logic := '0';

  signal phy_reset_count                   : unsigned(5 downto 0);
  signal glbl_rst_intn                     : std_logic;

  -- MDIO handling
  signal phy_mdi_int  : std_logic;
  signal phy_mdo_int  : std_logic;
  signal phy_mden_int : std_logic;

  -- interface conversion 'mac <-> AXI-S'
  signal mac_rx_tdata_int  : std_logic_vector(7 downto 0);
  signal mac_rx_tvalid_int : std_logic;
  signal mac_rx_tlast_int  : std_logic;

  signal txfifo_rst   : std_logic;
  signal txfifo_empty : std_logic;
  signal txfifo_empty_prev : std_logic;
  signal txfifo_full  : std_logic;

  signal rxfifo_empty : std_logic;
  signal rxfifo_full : std_logic;

  signal rx_mac_ra   : std_logic; --tvalid?
  signal rx_mac_rd   : std_logic; --tready?
  signal rx_mac_data : std_logic_vector(31 downto 0);
  signal rx_mac_be   : std_logic_vector(1 downto 0);
  signal rx_mac_pa   : std_logic;
  signal rx_mac_sop  : std_logic;
  signal rx_mac_eop  : std_logic; --tlast?

  signal tx_mac_wa   : std_logic; --tready?
  signal tx_mac_wr   : std_logic; --tvalid?
  signal tx_mac_data : std_logic_vector(31 downto 0);
  signal tx_mac_be   : std_logic_vector(1 downto 0);
  signal tx_mac_sop  : std_logic;
  signal tx_mac_eop  : std_logic; --tlast?

  -- debugging
  signal trig0 : std_logic_vector(25 downto 0);
  signal trig1 : std_logic_vector(38 downto 0);
  signal trig2 : std_logic_vector(12 downto 0);
  signal trig3 : std_logic_vector(38 downto 0);
  signal trig4 : std_logic_vector(9 downto 0);

------------------------------------------------------------------------------
-- Begin architecture
------------------------------------------------------------------------------

begin

  --Debugging
  trig0(7 downto 0) <= mac_tx_tdata;
  trig0(8) <= rxfifo_empty;
  trig0(9) <= rxfifo_full;
  trig0(10) <= mac_tx_tlast;
  trig0(18 downto 11) <= mac_rx_tdata_int;
  trig0(19) <= mac_rx_tvalid_int;
  trig0(20) <= mac_rx_tready;
  trig0(21) <= mac_rx_tlast_int;
  trig0(22) <= txfifo_full;
  trig0(23) <= txfifo_empty;
  trig0(24) <= mac_tx_tready_int;
  trig0(25) <= mac_tx_tvalid;

  trig1(0) <= rx_mac_ra;
  trig1(1) <= rx_mac_rd;
  trig1(33 downto 2) <= rx_mac_data;
  trig1(35 downto 34) <= rx_mac_be;
  trig1(36) <= rx_mac_pa;
  trig1(37) <= rx_mac_sop;
  trig1(38) <= rx_mac_eop;

  trig2(7 downto 0) <= gmii_rxd;
  trig2(8) <= gmii_rx_dv;
  trig2(9) <= gmii_rx_er;
  trig2(10) <= gmii_rx_clk_buf;
  trig2(11) <= gmii_col;
  trig2(12) <= gmii_crs;

  trig4(7 downto 0) <= gmii_txd_int;
  trig4(8) <= gmii_tx_en_int;
  trig4(9) <= gmii_tx_er_int;

  trig3(0) <= tx_mac_wa;
  trig3(1) <= tx_mac_wr;
  trig3(33 downto 2) <= tx_mac_data;
  trig3(35 downto 34) <= tx_mac_be;
  trig3(36) <= txfifo_rst;
  trig3(37) <= tx_mac_sop;
  trig3(38) <= tx_mac_eop;

  Inst_mac_ila : mac_ila
  port map (
            CONTROL => icon_control0,
            CLK => clk_125,
            TRIG0 => trig0,
            TRIG1 => trig4
           );

  Inst_mac2_ila : mac2_ila
  port map (
            CONTROL => icon_control1,
            CLK => gmii_rx_clk_buf,
            TRIG0 => trig1,
            TRIG1 => trig2,
            TRIG2 => trig3
           );

  --Clocking
  Inst_maindcm : maindcm
  port map (
              SYSCLK_P => clk_in_p,
              SYSCLK_N => clk_in_n,

              CLK_125M => clk_125,
              CLK_80M  => clk_80,
              CLK_66M  => clk_66,
              CLK_125M_INV => clk_125_invert,

              RESET => glbl_rst,
              LOCKED => dcm_locked
           );

  --MDIO
  phy_mdio <= phy_mdo_int when phy_mden_int = '1' else 'Z';
  phy_mdi_int <= phy_mdio;

  ------------------------------------------------------------------------------
  -- Instantiate the Ethernet wrapper
  ------------------------------------------------------------------------------
  gmii_txd <= gmii_txd_int;
  gmii_tx_er <= gmii_tx_er_int;
  gmii_tx_en <= gmii_tx_en_int;

  mac_block : MAC_top
  port map(
        Reset              => (not phy_resetn_int),
        Clk_125M           => clk_125,
        Clk_user           => clk_66,
        Clk_reg            => clk_80,
        Speed              => open,

        --TODO: define this conversion somehow...
        Rx_mac_ra          => rx_mac_ra,
        Rx_mac_rd          => rx_mac_rd,
        Rx_mac_data        => rx_mac_data,
        Rx_mac_BE          => rx_mac_be,
        Rx_mac_pa          => rx_mac_pa,
        Rx_mac_sop         => rx_mac_sop,
        Rx_mac_eop         => rx_mac_eop,

        Tx_mac_wa          => tx_mac_wa,
        Tx_mac_wr          => tx_mac_wr,
        Tx_mac_data        => tx_mac_data,
        Tx_mac_BE          => tx_mac_be,
        Tx_mac_sop         => tx_mac_sop,
        Tx_mac_eop         => tx_mac_eop,

        Pkg_lgth_fifo_rd   => '0',
        Pkg_lgth_fifo_ra   => open,
        Pkg_lgth_fifo_data => open,

        Rx_clk             => gmii_rx_clk_buf,
        Rx_er              => gmii_rx_er,
        Rx_dv              => gmii_rx_dv,
        Rxd                => gmii_rxd,
        Tx_clk             => '0',
        Tx_er              => gmii_tx_er_int,
        Tx_en              => gmii_tx_en_int,
        Txd                => gmii_txd_int,
        Crs                => gmii_crs,
        Col                => gmii_col,
        Gtx_clk            => clk_gtx_prebuf,

        CSB                => '1',
        WRB                => '1',
        CD_in              => (others => '0'),
        CD_out             => open,
        CA                 => (others => '0'),

        Mdi                => phy_mdi_int,
        Mdo                => phy_mdo_int,
        MdoEn              => phy_mden_int,
        Mdc                => phy_mdc
       );

  glbl_rst_intn <= not glbl_rst;

  -- Clock buffering/forwarding/etc.

  -- TODO: I can only do this because I know too much about which clocks
  --       are used where, and how they get routed back through the IP
  clk_gtx_invert <= clk_125_invert;
  gmii_rx_clk_invert <= clk_125_invert;

  Inst_rxclk_bufg : IBUFG
  port map (
            I => gmii_rx_clk,
            O => gmii_rx_clk_buf
           );

  Inst_txclk_oddr2 : ODDR2
  generic map (
                DDR_ALIGNMENT => "NONE",
                INIT => '0',
                SRTYPE => "SYNC"
              )
  port map (
            Q => gmii_tx_clk,
            C0 => gmii_rx_clk_buf,
            C1 => gmii_rx_clk_invert,
            CE => '1',
            D0 => '1',
            D1 => '0',
            R => '0',
            S => '0'
           );

  Inst_gtxclk_oddr2 : ODDR2
  generic map (
                DDR_ALIGNMENT => "NONE",
                INIT => '0',
                SRTYPE => "SYNC"
              )
  port map (
            Q => gmii_gtx_clk,
            C0 => clk_gtx_prebuf,
            C1 => clk_gtx_invert,
            CE => '1',
            D0 => '1',
            D1 => '0',
            R => '0',
            S => '0'
           );

  -- generate the user side clocks
  mac_tx_clock <= clk_125;
  mac_rx_clock <= clk_125;

  -- Interface speed conversion from 32-bit MAC to 8-bit AXI-S
  -- TODO: these interfaces may have many off-by-1 errors (eg. SOP/EOP, first-word-fallthrough)
  -- TODO: currently assumes BE is always "11"
  rxctrl : process(clk_66, rx_mac_ra, rxfifo_full)
  begin
    if clk_66'event and clk_66 = '1' then
      --Does use of full cause possible fragmentation problems?
      if rx_mac_ra = '1' and (not rxfifo_full = '1') then
        rx_mac_rd <= '1';
      else
        rx_mac_rd <= '0';
      end if;
    end if;
  end process;

  rxctl2 : process(clk_125, mac_rx_tready, rxfifo_empty)
  begin
    if clk_125'event and clk_125 = '1' then
      if mac_rx_tready = '1' and (not rxfifo_empty = '1') then
        mac_rx_tvalid_int <= '1';
      else
        mac_rx_tvalid_int <= '0';
      end if;
    end if;
  end process;

  mac_rx_tdata  <= mac_rx_tdata_int;
  mac_rx_tvalid <= mac_rx_tvalid_int;
  mac_rx_tlast  <= mac_rx_tlast_int;
  mac_rx_tlast_int <= rxfifo_empty and mac_rx_tvalid_int;

  Inst_rxfifo : fifo32to8
  port map (
            rst    => (rx_mac_ra and not rx_mac_rd),
            empty  => rxfifo_empty,
            full   => rxfifo_full,

            wr_clk => clk_66,
            din    => rx_mac_data,
            wr_en  => rx_mac_pa,

            rd_clk => clk_125,
            dout   => mac_rx_tdata_int,
            rd_en  => mac_rx_tvalid_int
           );

  tx_mac_be <= "11";
  mac_tx_tready_int <= not txfifo_full;
  mac_tx_tready <= mac_tx_tready_int;

  txctrl2 : process(clk_125, txfifo_empty, mac_tx_tvalid, mac_tx_tlast)
  begin
    if clk_125'event and clk_125 = '1' then
      if mac_tx_tlast = '0' and txfifo_empty = '1' and (not mac_tx_tvalid = '1') then
        txfifo_rst <= '1';
      else
        txfifo_rst <= '0';
      end if;

      if mac_tx_tlast = '1' then
        is_txing <= '1';
      elsif tx_mac_eop <= '1' then
        is_txing <= '0';
      end if;
    end if;
  end process;

  txctrl : process(clk_66, txfifo_empty, txfifo_empty_prev, tx_mac_wa, is_txing)
  begin
    if clk_66'event and clk_66 = '1' then
      if txfifo_empty = '0' and txfifo_empty_prev = '1' then
        tx_mac_sop <= '1';
      else
        tx_mac_sop <= '0';
      end if;
      if txfifo_empty = '1' and txfifo_empty_prev = '0' and tx_mac_wr = '1' then
        tx_mac_eop <= '1';
      else
        tx_mac_eop <= '0';
      end if;
      txfifo_empty_prev <= txfifo_empty;

      if tx_mac_wa = '1' and (not txfifo_empty = '1') and is_txing = '1' then
        tx_mac_wr <= '1';
      else
        tx_mac_wr <= '0';
      end if;
    end if;
  end process;

  Inst_txfifo : fifo8to32
  port map (
            rst    => txfifo_rst,
            empty  => txfifo_empty,
            full   => txfifo_full,

            wr_clk => clk_125,
            din    => mac_tx_tdata,
            wr_en  => mac_tx_tvalid,

            rd_clk => clk_66,
            dout   => tx_mac_data,
            rd_en  => tx_mac_wr
           );

  ------------------------------------------------------------------------------
  -- Generate resets
  ------------------------------------------------------------------------------
  -- in each case the async reset is first captured and then synchronised


  local_chk_reset <= glbl_rst or mac_reset;

  -----------------
  -- data check reset
  --TODO: is this correct?
  resetgen : process(clk_125, dcm_locked, local_chk_reset)
  begin
    if clk_125'event and clk_125 = '1' then
      if dcm_locked = '1' then
        chk_reset_int <= local_chk_reset;
      else
        chk_reset_int <= '1';
      end if;
    end if;
  end process;

  -- Create fully synchronous reset in the gtx clock domain.
  gen_chk_reset : process (clk_125)
  begin
    if clk_125'event and clk_125 = '1' then
      if chk_reset_int = '1' then
        chk_pre_resetn   <= '0';
        chk_resetn       <= '0';
      else
        chk_pre_resetn   <= '1';
        chk_resetn       <= chk_pre_resetn;
      end if;
    end if;
  end process gen_chk_reset;


  -----------------
  -- PHY reset
  -- the phy reset output (active low) needs to be held for at least 10x25MHZ cycles
  -- this is derived using the 125MHz available and a 6 bit counter
  gen_phy_reset : process (clk_125)
  begin
    if clk_125'event and clk_125 = '1' then
      if glbl_rst_intn = '0' then
        phy_resetn_int       <= '0';
        phy_reset_count      <= (others => '0');
      else
        if phy_reset_count /= "111111" then
          phy_reset_count <= phy_reset_count + "000001";
        else
          phy_resetn_int   <= '1';
        end if;
      end if;
    end if;
  end process gen_phy_reset;

  phy_resetn <= phy_resetn_int;


end wrapper;
