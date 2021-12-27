--
-- VGA 80x40 text on 640x480 raster
--
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
    led0_b   : out std_logic;
    led0_g   : out std_logic;
    led0_r   : out std_logic;
    RsRx     : in  std_logic;
    RsTx     : out std_logic;
    kb_data  : in  std_logic;           -- keyboard 4800 baud in
    uart_rxd : out std_logic;           -- USB uart Rx (names sic.)
    uart_txd : in  std_logic            -- USB uart Tx
    );
end entity top_terminal_cmod;

architecture arch of top_terminal_cmod is

  constant NUARTS : integer := 3;

  component pico_control_multi_uart is
    generic (
      UARTS : integer);
    port (
      clk      : in  std_logic;
      reset    : in  std_logic;
      RX       : in  std_logic_vector(UARTS-1 downto 0);
      TX       : out std_logic_vector(UARTS-1 downto 0);
      control  : out std_logic_vector(31 downto 0);
      control2 : out std_logic_vector(31 downto 0);
      status   : in  std_logic_vector(7 downto 0);
      action   : out std_logic_vector(7 downto 0));
  end component pico_control_multi_uart;

  component mem_text_bram
    port (
      clka  : in  std_logic;
      wea   : in  std_logic_vector(0 downto 0);
      addra : in  std_logic_vector(12 downto 0);
      dina  : in  std_logic_vector(7 downto 0);
      douta : out std_logic_vector(7 downto 0);
      clkb  : in  std_logic;
      web   : in  std_logic_vector(0 downto 0);
      addrb : in  std_logic_vector(12 downto 0);
      dinb  : in  std_logic_vector(7 downto 0);
      doutb : out std_logic_vector(7 downto 0)
      );
  end component;

  component clk_wiz_1 is
    port (
      clk_in  : in  std_logic;
      clk2500 : out std_logic
      );
  end component clk_wiz_1;

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

--column 0 to 79 so 7 bits
  signal TEXT_WR_A_COL : std_logic_vector(6 downto 0);
--row 0 to 39 so 6 bits
  signal TEXT_WR_A_ROW : std_logic_vector(5 downto 0);

  signal TEXT_WR_D  : std_logic_vector(7 downto 0);
  signal TEXT_RD_D  : std_logic_vector(7 downto 0);
  signal TEXT_WR_WE : std_logic;

  signal locked : std_logic;

  signal color : std_logic_vector(2 downto 0);

  signal vs0 : std_logic;

  signal s_control  : std_logic_vector(31 downto 0);
  signal s_control2 : std_logic_vector(31 downto 0);
  signal s_status   : std_logic_vector(7 downto 0);
  signal s_action   : std_logic_vector(7 downto 0);

  signal s_vga_addr : std_logic_vector(12 downto 0);

  signal s_counter : unsigned(23 downto 0);

  signal s_serial_in  : std_logic_vector(NUARTS-1 downto 0);
  signal s_serial_out : std_logic_vector(NUARTS-1 downto 0);

begin  -- architecture arch

  reset <= '0';                         -- we don't need no steenkin reset!

  s_status <= TEXT_RD_D;

  TEXT_WR_D     <= s_control(7 downto 0);
  TEXT_WR_A_COL <= s_control(14 downto 8);
  TEXT_WR_A_ROW <= s_control(21 downto 16);
  TEXT_WR_WE    <= s_action(0);

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
      clk_in  => clk,
      clk2500 => pclk);

--  clk_vga_1 : clk_vga
--    port map (
--      clk_out1 => pclk,
--      locked   => locked,
--      clk_in1  => clk);

  pico_control_multi_uart_1 : entity work.pico_control_multi_uart
    generic map (
      UARTS => NUARTS)
    port map (
      clk      => pclk,
      reset    => reset,
      RX       => s_serial_in,
      TX       => s_serial_out,
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

-- character RAM
  mem_text_bram_2: mem_text_bram
    port map (
      clka  => pclk,
      wea   => "0",
      addra => s_vga_addr,
      dina  => (others => '0'),
      douta => TEXT_D,

      clkb  => pclk,
      web   => (0 => TEXT_WR_WE),
      addrb => TEXT_WR_A_ROW & TEXT_WR_A_COL,
      dinb  => TEXT_WR_D,
      doutb => TEXT_RD_D);

  -- character generator
  mem_font_1 : entity work.mem_font
    port map (
      addr => FONT_A,
      dout => FONT_D);

  s_serial_in <= kb_data & RsRx & uart_txd;

  RsTx     <= s_serial_out(1);
  uart_rxd <= s_serial_out(0);

  s_vga_addr <= std_logic_vector(to_unsigned(TEXT_A_ROW, 6))
                & std_logic_vector(to_unsigned(TEXT_A_COL, 7));

  led(0) <= s_counter(23);
  led(1) <= s_counter(22);

  led0_b <= '0';
  led0_g <= '0';
  led0_r <= '0';

  process (pclk) is
  begin  -- process
    if pclk'event and pclk = '1' then   -- rising clock edge

      s_counter <= s_counter + 1;

    end if;
  end process;

end architecture arch;
