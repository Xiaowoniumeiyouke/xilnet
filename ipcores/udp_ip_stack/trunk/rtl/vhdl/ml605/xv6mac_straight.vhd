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
         phy_mdio                      : inout std_logic
       );
end xv6mac_straight;

architecture wrapper of xv6mac_straight is

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

  constant BOARD_PHY_ADDR                  : std_logic_vector(7 downto 0)  := "00000111";
  ------------------------------------------------------------------------------
  -- internal signals used in this top level wrapper.
  ------------------------------------------------------------------------------

  -- example design clocks
  signal gtx_clk_bufg                      : std_logic;
  signal refclk_bufg                       : std_logic;
  signal rx_mac_aclk                       : std_logic;

  -- tx handshaking
  signal mac_tx_tready_int            : std_logic;
  signal tx_full_reg                : std_logic;
  signal tx_full_val                : std_logic;
  signal tx_data_reg                : std_logic_vector(7 downto 0);
  signal tx_last_reg                : std_logic;
  signal set_tx_reg                 : std_logic;

  signal phy_resetn_int                    : std_logic;

  -- resets (and reset generation)
  signal local_chk_reset                   : std_logic;
  signal chk_reset_int                     : std_logic;
  signal chk_pre_resetn                    : std_logic := '0';
  signal chk_resetn                        : std_logic := '0';
  signal dcm_locked                        : std_logic;

  signal glbl_rst_int                      : std_logic;
  signal phy_reset_count                   : unsigned(5 downto 0);
  signal glbl_rst_intn                     : std_logic;

  -- pipeline register for RX signals
  signal rx_data_val                : std_logic_vector(7 downto 0);
  signal rx_tvalid_val                : std_logic;
  signal rx_tlast_val               : std_logic;
  signal rx_data_reg                : std_logic_vector(7 downto 0);
  signal rx_tvalid_reg                : std_logic;
  signal rx_tlast_reg               : std_logic;

  attribute keep : string;
  attribute keep of gtx_clk_bufg             : signal is "true";
  attribute keep of refclk_bufg              : signal is "true";
  attribute keep of mac_tx_tready_int        : signal is "true";
  attribute keep of tx_full_reg           : signal is "true";

  -- MDIO handling
  signal phy_mdi_int  : std_logic;
  signal phy_mdo_int  : std_logic;
  signal phy_mden_int : std_logic;


------------------------------------------------------------------------------
-- Begin architecture
------------------------------------------------------------------------------

begin

  --MDIO
  phy_mdio <= phy_mdo_int when phy_mdoen_int = '1' else 'Z';
  phy_mdi_int <= phy_mdio;

  -- begin proceses
  combinatorial: process (
    rx_data_reg, rx_tvalid_reg, rx_tlast_reg,
    mac_tx_tvalid, mac_tx_tready_int, tx_full_reg, tx_full_val, set_tx_reg
  )
  begin
    -- output followers
    mac_rx_tdata  <= rx_data_reg;
    mac_rx_tvalid <= rx_tvalid_reg;
    mac_rx_tlast  <= rx_tlast_reg;
    mac_tx_tready <= not (tx_full_reg and not mac_tx_tready_int);   -- if not full, we are ready to accept

    -- control defaults
    tx_full_val <= tx_full_reg;
    set_tx_reg <= '0';

    -- tx handshaking logic
    if mac_tx_tvalid = '1' then
      tx_full_val <= '1';
      set_tx_reg <= '1';
    elsif mac_tx_tready_int = '1' then
      tx_full_val <= '0';
    end if;

  end process;

  sequential: process(gtx_clk_bufg)
  begin
    if rising_edge(gtx_clk_bufg) then
      if chk_resetn = '0' then
        -- reset state variables
        rx_data_reg <= (others => '0');
        rx_tvalid_reg <= '0';
        rx_tlast_reg <= '0';
        tx_full_reg <= '0';
        tx_data_reg <= (others => '0');
        tx_last_reg <= '0';
      else
        -- register rx data
        rx_data_reg <= rx_data_val;
        rx_tvalid_reg <= rx_tvalid_val;
        rx_tlast_reg <= rx_tlast_val;

        -- process tx tvalid and tready
        tx_full_reg <= tx_full_val;
        if set_tx_reg = '1' then
          tx_data_reg <= mac_tx_tdata;
          tx_last_reg <= mac_tx_tlast;
        else
          tx_data_reg <= tx_data_reg;
          tx_last_reg <= tx_last_reg;
        end if;
      end if;
    end if;
  end process;

  ------------------------------------------------------------------------------
  -- Instantiate the Ethernet wrapper
  ------------------------------------------------------------------------------
  mac_block : MAC_top
  port map(
        Reset              => phy_resetn_int,
        Clk_125M           => clk_125,
        Clk_user           => clk_66,
        Clk_reg            => clk_80, --Or clk_66
        Speed              => open,

        --TODO: define this conversion somehow...
        Rx_mac_ra          => mac_rx_tvalid,
        Rx_mac_rd          => mac_rx_tready,
        Rx_mac_data(7 downto 0)  => mac_rx_tdata,
        Rx_mac_data(13 downto 8) => open,
        Rx_mac_BE          => open,
        Rx_mac_pa          => open,
        Rx_mac_sop         => open,
        Rx_mac_eop         => mac_rx_tlast,

        Tx_mac_wa          => mac_tx_tready,
        Tx_mac_wr          => mac_tx_tvalid,
        Tx_mac_data(7 downto 0) => mac_tx_tdata,
        Tx_mac_data(31 downto 8) => (others => '0'),
        Tx_mac_BE          => "01",
        Tx_mac_sop         => '0',
        Tx_mac_eop         => mac_tx_tlast,

        Pkg_lgth_fifo_rd   => '0',
        Pkg_lgth_fifo_ra   => open,
        Pkg_lgth_fifo_data => open,

        Rx_clk             => gmii_rx_clk,
        Rx_er              => gmii_rx_er,
        Rx_dv              => gmii_rx_dv,
        Rxd                => gmii_rxd,
        Tx_clk             => gmii_tx_clk,
        Tx_er              => gmii_tx_er,
        Tx_en              => gmii_tx_en,
        Txd                => gmii_txd,
        Crs                => gmii_crs,
        Col                => gmii_col,
        Gtx_clk            => gmii_gtx_clk,

        CSB                => '0',
        WRB                => '0',
        CD_in              => (others => '0'),
        CD_out             => open,
        CA                 => (others => '0'),

        Mdi                => phy_mdi_int,
        Mdo                => phy_mdo_int,
        MdoEn              => phy_mdoen_int,
        Mdc                => phy_mdc
       );

  --TODO: generate clocks

  glbl_rst_intn <= not glbl_rst_int;

  -- generate the user side clocks
  mac_tx_clock <= gtx_clk_bufg;
  mac_rx_clock <= gtx_clk_bufg;

  ------------------------------------------------------------------------------
  -- Generate resets
  ------------------------------------------------------------------------------
  -- in each case the async reset is first captured and then synchronised


  local_chk_reset <= glbl_rst or mac_reset;

  -----------------
  -- data check reset
  chk_reset_gen : tri_mode_eth_mac_v5_4_reset_sync
  port map (
             clk              => gtx_clk_bufg,
             enable           => dcm_locked,
             reset_in         => local_chk_reset,
             reset_out        => chk_reset_int
           );

  -- Create fully synchronous reset in the gtx clock domain.
  gen_chk_reset : process (gtx_clk_bufg)
  begin
    if gtx_clk_bufg'event and gtx_clk_bufg = '1' then
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
  gen_phy_reset : process (gtx_clk_bufg)
  begin
    if gtx_clk_bufg'event and gtx_clk_bufg = '1' then
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
