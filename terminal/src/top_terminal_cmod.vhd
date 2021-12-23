--
-- VGA 80x40 text on 640x480 raster
-- 
-- Doesn't fit in xa15T.  Changing the text RAM to a BRAM
-- now it fits but doesn't meet timing
-- add registers on BRAM input and output
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
-- use IEEE.std_logic_unsigned.all;

entity top_terminal_cmod is

  port (
    clk      : in  std_logic;
    vgaRed   : out std_logic_vector(3 downto 0);
    vgaBlue  : out std_logic_vector(3 downto 0);
    vgaGreen : out std_logic_vector(3 downto 0);
    Hsync    : out std_logic;
    Vsync    : out std_logic;
    led      : out std_logic_vector(1 downto 0);
    RsRx     : in  std_logic;
    RsTx     : out std_logic;
    kb_data : in std_logic;             -- keyboard 4800 baud in
    uart_rxd : in std_logic;        -- USB uart Rx (names sic.)
    uart_txd : out std_logic         -- USB uart Tx
    );
end entity top_terminal_cmod;

architecture arch of top_terminal_cmod is

  component pico_control is
    port (
      clk      : in  std_logic;
      reset    : in  std_logic;
      RX       : in  std_logic;
      TX       : out std_logic;
      control  : out std_logic_vector(31 downto 0);
      control2 : out std_logic_vector(31 downto 0);
      status   : in  std_logic_vector(7 downto 0);
      action   : out std_logic_vector(7 downto 0));
  end component pico_control;

  component mem_text_bram is
    port (
      clka  : IN  STD_LOGIC;
      ena   : IN  STD_LOGIC;
      wea   : IN  STD_LOGIC_VECTOR(0 DOWNTO 0);
      addra : IN  STD_LOGIC_VECTOR(12 DOWNTO 0);
      dina  : IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
      douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
      clkb  : IN  STD_LOGIC;
      enb   : IN  STD_LOGIC;
      web   : IN  STD_LOGIC_VECTOR(0 DOWNTO 0);
      addrb : IN  STD_LOGIC_VECTOR(12 DOWNTO 0);
      dinb  : IN  STD_LOGIC_VECTOR(7 DOWNTO 0);
      doutb : OUT STD_LOGIC_VECTOR(7 DOWNTO 0));
  end component mem_text_bram;

  component clk_wiz_1 is
    port (
      clk_out1 : out std_logic;
      clk_in1  : in  std_logic);
  end component clk_wiz_1;

--  component clk_vga is
--    port (
--      clk_out1 : out std_logic;
--      locked   : out std_logic;
--      clk_in1  : in  std_logic);
--  end component clk_vga;

  component vga80x40 is
    port (
      reset      : in  std_logic;
      clk25MHz   : in  std_logic;
      TEXT_A_ROW : out integer range 039 downto 0;
      TEXT_A_COL : out integer range 079 downto 0;
      TEXT_D     : in  std_logic_vector(07 downto 0);
      FONT_A     : out std_logic_vector(11 downto 0);
      FONT_D     : in  std_logic_vector(07 downto 0);
      ocrx       : in  std_logic_vector(07 downto 0);
      ocry       : in  std_logic_vector(07 downto 0);
      octl       : in  std_logic_vector(07 downto 0);
      R          : out std_logic;
      G          : out std_logic;
      B          : out std_logic;
      hsync      : out std_logic;
      vsync      : out std_logic);
  end component vga80x40;

  component mem_font is
    port (
      addr : in  std_logic_vector(11 downto 0);
      dout : out std_logic_vector(07 downto 0));
  end component mem_font;

  signal pclk  : std_logic;
  signal reset : std_logic;

  signal s_vsync, s_hsync : std_logic;

  signal R, G, B : std_logic;

  signal TEXT_A_ROW : integer range 039 downto 0;
  signal TEXT_A_COL : integer range 079 downto 0;

  signal FONT_A         : std_logic_vector(11 downto 0);
  signal TEXT_D, FONT_D : std_logic_vector(7 downto 0);

  signal TEXT_WR_A_COL : std_logic_vector(6 downto 0);
  signal TEXT_WR_A_ROW : std_logic_vector(5 downto 0);
  signal TEXT_WR_D     : std_logic_vector(7 downto 0);
  signal TEXT_RD_D     : std_logic_vector(7 downto 0);
  signal TEXT_WR_WE    : std_logic;

  signal locked : std_logic;

  signal color : std_logic_vector(2 downto 0);

  signal vs0 : std_logic;

  signal s_control  : std_logic_vector(31 downto 0);
  signal s_control2 : std_logic_vector(31 downto 0);
  signal s_status   : std_logic_vector(7 downto 0);
  signal s_action   : std_logic_vector(7 downto 0);

  signal s_vga_data : std_logic_vector(7 downto 0);
  signal s_vga_addr : std_logic_vector(12 downto 0);

begin  -- architecture arch

  reset <= '0';                         -- we don't need no steenkin reset!

  s_status <= TEXT_RD_D;

  TEXT_WR_D     <= s_control(7 downto 0);
  TEXT_WR_A_COL <= s_control(14 downto 8);
  TEXT_WR_A_ROW <= s_control(21 downto 16);
  TEXT_WR_WE    <= s_action(0);

  led <= "11";

  -- all full intensity
  vgaRed(0) <= R;
  vgaRed(1) <= R;
  vgaRed(2) <= R;

  vgaGreen(0) <= G;
  vgaGreen(1) <= G;
  vgaGreen(2) <= G;

  vgaBlue(0) <= B;
  vgaBlue(1) <= B;
  vgaBlue(2) <= B;

  Hsync <= s_hsync;
  Vsync <= s_vsync;

   clk_wiz_1_1 : clk_wiz_1
     port map (
       clk_out1 => pclk,
       clk_in1  => clk);

--  clk_vga_1 : clk_vga
--    port map (
--      clk_out1 => pclk,
--      locked   => locked,
--      clk_in1  => clk);

  -- dummy clock for now
  -- pclk <= clk;

  pico_control_1 : entity work.pico_control
    port map (
      clk      => pclk,
      reset    => reset,
      RX       => uart_rxd,              -- USB port for simple test
      TX       => uart_txd,
      control  => s_control,
      control2 => s_control2,
      status   => s_status,
      action   => s_action);

  vga80x40_1 : entity work.vga80x40
    port map (
      reset      => reset,
      clk25MHz   => pclk,
      TEXT_A_ROW => TEXT_A_ROW,
      TEXT_A_COL => TEXT_A_COL,
      TEXT_D     => TEXT_D,
      FONT_A     => FONT_A,
      FONT_D     => FONT_D,
      ocrx       => s_control2(15 downto 8),
      ocry       => s_control2(23 downto 16),
      octl       => s_control2(7 downto 0),
      R          => R,
      G          => G,
      B          => B,
      hsync      => s_hsync,
      vsync      => s_vsync);

  mem_text_bram_1: mem_text_bram
    port map (
      -- port A from VGA
      clka  => pclk,
      ena   => '1',
      wea   => "0",
      addra => s_vga_addr,

      dina  => (others => '0'),
      douta => s_vga_data,
      -- port B from picoblaze
      clkb  => pclk,
      enb   => '1',
      web   => (0 => TEXT_WR_WE),
      addrb => TEXT_WR_A_ROW & TEXT_WR_A_COL,
      dinb  => TEXT_WR_D,
      doutb => TEXT_RD_D);

--  -- video RAM
--  mem_text_1 : entity work.mem_text
--    port map (
--      clk       => pclk,
--      addra_row => TEXT_A_ROW,
--      addra_col => TEXT_A_COL,
--      douta     => TEXT_D,
--      dinb      => TEXT_WR_D,
--      addrb_col => TEXT_WR_A_COL,
--      addrb_row => TEXT_WR_A_ROW,
--      web       => TEXT_WR_WE,
--      doutb     => TEXT_RD_D);

  -- character generator
  mem_font_1 : entity work.mem_font
    port map (
      addr => FONT_A,
      dout => FONT_D);

  process (pclk) is
  begin  -- process
    if pclk'event and pclk = '1' then   -- rising clock edge
      s_vga_addr <= std_logic_vector( to_unsigned( TEXT_A_ROW, 6)) 
               & std_logic_vector( to_unsigned( TEXT_A_COL, 7));
    end if;
  end process;

end architecture arch;
