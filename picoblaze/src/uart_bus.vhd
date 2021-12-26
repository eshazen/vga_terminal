--
-- uart_bus.vhd - bidirectional UART package with baud rate generator
--

entity uart_bus is

  port (
    clk           : in  std_logic;
    reset         : in  std_logic;
    set_baud_rate : in  std_logic_vector(15 downto 0);
    data_in       : in  std_logic_vector(7 downto 0);
    data_out      : out std_logic_vector(7 downto 0);
    rd, wr        : in  std_logic;
    status_out    : out std_logic_vector(7 downto 0));

end entity uart_bus;


architecture arch of uart_bus is

  component uart_tx6 is
    port (
      data_in             : in  std_logic_vector(7 downto 0);
      en_16_x_baud        : in  std_logic;
      serial_out          : out std_logic;
      buffer_write        : in  std_logic;
      buffer_data_present : out std_logic;
      buffer_half_full    : out std_logic;
      buffer_full         : out std_logic;
      buffer_reset        : in  std_logic;
      clk                 : in  std_logic);
  end component uart_tx6;

  component uart_rx6 is
    port (
      serial_in           : in  std_logic;
      en_16_x_baud        : in  std_logic;
      data_out            : out std_logic_vector(7 downto 0);
      buffer_read         : in  std_logic;
      buffer_data_present : out std_logic;
      buffer_half_full    : out std_logic;
      buffer_full         : out std_logic;
      buffer_reset        : in  std_logic;
      clk                 : in  std_logic);
  end component uart_rx6;

--
-- Signals used to connect UART_TX6
--
  signal uart_tx_data_in      : std_logic_vector(7 downto 0);
  signal write_to_uart_tx     : std_logic;
  signal pipe_port_id7        : std_logic                     := '0';
  signal uart_tx_data_present : std_logic;
  signal uart_tx_half_full    : std_logic;
  signal uart_tx_full         : std_logic;
  signal uart_tx_reset        : std_logic;
--
-- Signals used to connect UART_RX6
--
  signal uart_rx_data_out     : std_logic_vector(7 downto 0);
  signal read_from_uart_rx    : std_logic                     := '0';
  signal uart_rx_data_present : std_logic;
  signal uart_rx_half_full    : std_logic;
  signal uart_rx_full         : std_logic;
  signal uart_rx_reset        : std_logic;
--
-- Signals used to define baud rate
--
  signal baud_rate_counter    : std_logic_vector(15 downto 0) := X"0000";
  signal en_16_x_baud         : std_logic                     := '0';


begin  -- architecture arch

  status_out(0)          <= uart_tx_data_present;
  status_out(1)          <= uart_tx_half_full;
  status_out(2)          <= uart_tx_full;
  status_out(3)          <= uart_rx_data_present;
  status_out(4)          <= uart_rx_half_full;
  status_out(5)          <= uart_rx_full;
  status_out(7 downto 6) <= "00";

  uart_tx6_1 : entity work.uart_tx6
    port map (
      data_in             => data_in,
      en_16_x_baud        => en_16_x_baud,
      serial_out          => serial_out,
      buffer_write        => wr,
      buffer_data_present => buffer_data_present,
      buffer_half_full    => buffer_half_full,
      buffer_full         => buffer_full,
      buffer_reset        => buffer_reset,
      clk                 => clk);

  uart_rx6_1 : entity work.uart_rx6
    port map (
      serial_in           => serial_in,
      en_16_x_baud        => en_16_x_baud,
      data_out            => data_out,
      buffer_read         => rd,
      buffer_data_present => buffer_data_present,
      buffer_half_full    => buffer_half_full,
      buffer_full         => buffer_full,
      buffer_reset        => buffer_reset,
      clk                 => clk);

  --
  -----------------------------------------------------------------------------------------
  -- UART baud rate 
  -----------------------------------------------------------------------------------------
  --
  -- The baud rate is defined by the frequency of 'en_16_x_baud' pulses. These should occur  
  -- at 16 times the desired baud rate. KCPSM6 computes and sets an 8-bit value into 
  -- 'set_baud_rate' which is used to divide the clock frequency appropriately.
  -- 
  -- For example, if the clock frequency is 200MHz and the desired serial communication 
  -- baud rate is 115200 then PicoBlaze will set 'set_baud_rate' to 6C hex (108 decimal). 
  -- This circuit will then generate an 'en_16_x_baud' pulse once every 109 clock cycles 
  -- (note that 'baud_rate_counter' will include state zero). This would actually result 
  -- in a baud rate of 114,679 baud but that is only 0.45% low and well within limits.
  --

  baud_rate : process(clk)
  begin
    if clk'event and clk = '1' then
      if baud_rate_counter = set_baud_rate then
        baud_rate_counter <= (others => '0');
        en_16_x_baud      <= '1';       -- single cycle enable pulse
      else
        baud_rate_counter <= std_logic_vector(unsigned(baud_rate_counter) + 1);
        en_16_x_baud      <= '0';
      end if;
    end if;
  end process baud_rate;



end architecture arch;
