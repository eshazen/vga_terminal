-- pico_control_multi_uart.vhd
--  
-- PicoBlaze control interface for VGA terminal
--
-- version with a parameterized set of UARTS (up to 4)
--
-- Address Map:
-- Input:  02      - clock frequency (constant)
--         03..05  - instruction data
--         09..0c  - control port 1
--           09    - VGA text data for write
--           0a    - VGA text column address
--           0b    - VGA test row address
--           0c    - VGA attribute data for write
--         0d      - VGA text data read
--         0e      - VGA attr data read
--         10..13  - control port 2
--           10    - video control port
--           11    - Hardware cursor column
--           12    - Hardware cursor row
--                   bits 0..2 (b,g,r) cursor: 4=mode, 5=blink, 6=ena
--                   bit 7 is VGA enable
--         14..17  - UART status for UARTs 0..3
--                   0=Tx data 1=TX half 2=Tx full
--                   3=Rx data 4=TX half 5=RX full
--         18..1B  - UART data for UARTs 0..3
--
-- Output: 03,04   - Program address 11..0
--         05..07  - Program data 17..0
--         09      - VGA text data for write
--         0a      - VGA text column address
--         0b      - VGA test row address
--         0c      - spare controls
--         0d      - bit 0 = '1' triggers VGA text write
--         10..13  - control port 2
--           10    - video control port
--           11    - Hardware cursor column
--           12    - Hardware cursor row
--                   bits 0..2 (b,g,r) cursor: 4=mode, 5=blink, 6=ena
--                   bit 7 is VGA enable
--         14..17  - baud rate lo for UARTS 0..3
--         18--1B  - baud rate hi for UARTS 0..3
--         80--83  - UART transmit data
--
library IEEE;
use IEEE.STD_LOGIC_1164.all;
use ieee.numeric_std.all;

entity pico_control_multi_uart is

  generic (
    UARTS : integer := 3
    );
  port (
    clk      : in  std_logic;           -- logic clock for all
    reset    : in  std_logic;           -- active high asynchronous reset
    RX       : in  std_logic_vector(UARTS-1 downto 0);  -- asynch serial input
    TX       : out std_logic_vector(UARTS-1 downto 0);  -- asynch serial output
    control  : out std_logic_vector(31 downto 0);  -- control register R/W
    control2 : out std_logic_vector(31 downto 0);  -- 2nd control register R/W
    status   : in  std_logic_vector(15 downto 0);   -- status register R/O
    action   : out std_logic_vector(7 downto 0));  -- action (pulsed) register W/O

end entity pico_control_multi_uart;

architecture arch of pico_control_multi_uart is

  component kcpsm6
    generic(hwbuild                 : std_logic_vector(7 downto 0)  := X"00";
            interrupt_vector        : std_logic_vector(11 downto 0) := X"3FF";
            scratch_pad_memory_size : integer                       := 64);
    port (address        : out std_logic_vector(11 downto 0);
          instruction    : in  std_logic_vector(17 downto 0);
          bram_enable    : out std_logic;
          in_port        : in  std_logic_vector(7 downto 0);
          out_port       : out std_logic_vector(7 downto 0);
          port_id        : out std_logic_vector(7 downto 0);
          write_strobe   : out std_logic;
          k_write_strobe : out std_logic;
          read_strobe    : out std_logic;
          interrupt      : in  std_logic;
          interrupt_ack  : out std_logic;
          sleep          : in  std_logic;
          reset          : in  std_logic;
          clk            : in  std_logic);
  end component;

--
-- Program memory (read/write, inferred)
--

  component monitor is
    port (
      address     : in  std_logic_vector(11 downto 0);
      instruction : out std_logic_vector(17 downto 0);
      addr_2      : in  std_logic_vector(11 downto 0);
      wen         : in  std_logic;
      di          : in  std_logic_vector(17 downto 0);
      do          : out std_logic_vector(17 downto 0);
      clk         : in  std_logic);
  end component monitor;

  component uart_bus is
    port (
      clk           : in  std_logic;
      reset         : in  std_logic;
      set_baud_rate : in  std_logic_vector(15 downto 0);
      data_in       : in  std_logic_vector(7 downto 0);
      data_out      : out std_logic_vector(7 downto 0);
      serial_out    : out std_logic;
      serial_in     : in  std_logic;
      rd, wr        : in  std_logic;
      status_out    : out std_logic_vector(7 downto 0));
  end component uart_bus;

--
--
-------------------------------------------------------------------------------------------
--
-- Signals
--
-------------------------------------------------------------------------------------------
--
--

-- Constant to specify the clock frequency in megahertz.
-- (not currently used but can be read by software)
  constant clock_frequency_in_MHz : integer range 0 to 255 := 25;
--
--
-- Signals used to connect KCPSM6
--
  signal address                  : std_logic_vector(11 downto 0);
  signal instruction              : std_logic_vector(17 downto 0);
  signal bram_enable              : std_logic;
  signal in_port                  : std_logic_vector(7 downto 0);
  signal out_port                 : std_logic_vector(7 downto 0);
  signal port_id                  : std_logic_vector(7 downto 0);
  signal write_strobe             : std_logic;
  signal k_write_strobe           : std_logic;
  signal read_strobe              : std_logic;
  signal interrupt                : std_logic;
  signal interrupt_ack            : std_logic;

--
-- Signals used to connect UART array (up to 4)
--
  type word_array_t is array (3 downto 0) of std_logic_vector(15 downto 0);
  signal uart_rd : std_logic_vector(3 downto 0);

  signal uart_wr : std_logic_vector(3 downto 0);

  signal pipe_port  : std_logic_vector(7 downto 0);
  signal uart_reset : std_logic;

--
-- a few mux for up to 4 UARTS (too lazy to parameterize)
--
  type quad_byte_t is array (3 downto 0) of std_logic_vector(7 downto 0);

  signal baud_rate_lo  : quad_byte_t;
  signal baud_rate_hi  : quad_byte_t;
  signal uart_status   : quad_byte_t;
  signal uart_data_out : quad_byte_t;

--
-- signals for access to program ROM
--
  signal peek_addr : std_logic_vector(11 downto 0);
  signal peek_data : std_logic_vector(17 downto 0);
  signal poke_data : std_logic_vector(17 downto 0);

  signal poke_ena : std_logic;

  signal s_control  : std_logic_vector(31 downto 0);
  signal s_control2 : std_logic_vector(31 downto 0);
  signal s_status   : std_logic_vector(15 downto 0);
  signal s_action   : std_logic_vector(7 downto 0);

--
--
-------------------------------------------------------------------------------------------
--
-- Start of circuit description
--
-------------------------------------------------------------------------------------------
--
begin

  control2 <= s_control2;
  control  <= s_control;
  s_status <= status;
  action   <= s_action;

  --
  -----------------------------------------------------------------------------------------
  -- Instantiate KCPSM6 and connect to program ROM
  -----------------------------------------------------------------------------------------
  --
  -- The generics can be defined as required. In this case the 'hwbuild' value is used to 
  -- define a version using the ASCII code for the desired letter. 
  --

  processor : kcpsm6
    generic map (hwbuild                 => X"41",  -- 41 hex is ASCII Character "A"
                 interrupt_vector        => X"7FF",
                 scratch_pad_memory_size => 256)
    port map(address        => address,
             instruction    => instruction,
             bram_enable    => open,
             port_id        => port_id,
             write_strobe   => write_strobe,
             k_write_strobe => k_write_strobe,
             out_port       => out_port,
             read_strobe    => read_strobe,
             in_port        => in_port,
             interrupt      => interrupt,
             interrupt_ack  => interrupt_ack,
             sleep          => '0',
             reset          => '0',
             clk            => clk);

  --
  -- Development Program Memory 
  -- read/write, inferred (not the Xilinx standard!)
  --
  program_rom : monitor
    port map(address     => address,
             instruction => instruction,
             addr_2      => peek_addr,
             di          => poke_data,
             do          => peek_data,
             wen         => poke_ena,
             clk         => clk);


  uarts1 : for i in 0 to UARTS-1 generate

    uart_bus_1 : entity work.uart_bus
      port map (
        clk           => clk,
        reset         => uart_reset,
        set_baud_rate => baud_rate_hi(i) & baud_rate_lo(i),
        data_in       => out_port,
        data_out      => uart_data_out(i),
        serial_in     => RX(i),
        serial_out    => TX(i),
        rd            => uart_rd(i),
        wr            => uart_wr(i),
        status_out    => uart_status(i));

  end generate uarts1;


  --
  -----------------------------------------------------------------------------------------
  -- General Purpose Input Ports. 
  -----------------------------------------------------------------------------------------
  --
  -- Three input ports are used with the UART macros. 
  -- 
  --   The first is used to monitor the flags on both the transmitter and receiver.
  --   The second is used to read the data from the receiver and generate a 'buffer_read' 
  --     pulse. 
  --   The third is used to read a user defined constant that enabled KCPSM6 to know the 
  --     clock frequency so that it can compute values which will define the BAUD rate 
  --     for UART communications (as well as values used to define software delays).
  --

  input_ports : process(clk)
  begin
    if clk'event and clk = '1' then

      case port_id(4 downto 0) is

        -- Read clock frequency contant at port address 02 hex
        when "0" & X"2" => in_port <= std_logic_vector(to_unsigned(clock_frequency_in_MHz, 8));

        -- read peek data at addresses 03, 04, 05
        when "0" & X"3" => in_port <= peek_data(7 downto 0);
        when "0" & X"4" => in_port <= peek_data(15 downto 8);
        when "0" & X"5" => in_port <= "000000" & peek_data(17 downto 16);

        -- read/write the control port
        when "0" & X"9" => in_port <= s_control(7 downto 0);
        when "0" & X"A" => in_port <= s_control(15 downto 8);
        when "0" & X"B" => in_port <= s_control(23 downto 16);
        when "0" & X"C" => in_port <= s_control(31 downto 24);

        -- read the status port
        when "0" & X"D" => in_port <= s_status(7 downto 0);
        when "0" & X"E" => in_port <= s_status(15 downto 8);

        -- read the control2 port for symmetry at 10..13
        when "1" & x"0" => in_port <= s_control2(7 downto 0);
        when "1" & x"1" => in_port <= s_control2(15 downto 8);
        when "1" & x"2" => in_port <= s_control2(23 downto 16);
        when "1" & x"3" => in_port <= s_control2(31 downto 24);

        -- status at 14..17
        when "1" & X"4" => in_port <= uart_status(0);
        when "1" & X"5" => in_port <= uart_status(1);
        when "1" & X"6" => in_port <= uart_status(2);
        when "1" & X"7" => in_port <= uart_status(3);

        -- data at 18..1b
        when "1" & X"8" => in_port <= uart_data_out(0);
        when "1" & X"9" => in_port <= uart_data_out(1);
        when "1" & X"A" => in_port <= uart_data_out(2);
        when "1" & X"B" => in_port <= uart_data_out(3);

        -- Specify don't care for all other inputs to obtain optimum implementation
        when others => in_port <= "XXXXXXXX";

      end case;

      -- Generate 'buffer_read' pulse following read from port addresses 18--1B

      if (read_strobe = '1') and (port_id(4 downto 0) = "11000" ) then
        uart_rd(0) <= '1';
      else
        uart_rd(0) <= '0';
      end if;

      if (read_strobe = '1') and (port_id(4 downto 0) = "11001" ) then
        uart_rd(1) <= '1';
      else
        uart_rd(1) <= '0';
      end if;

      if (read_strobe = '1') and (port_id(4 downto 0) = "11010" ) then
        uart_rd(2) <= '1';
      else
        uart_rd(2) <= '0';
      end if;

      if (read_strobe = '1') and (port_id(4 downto 0) = "11011" ) then
        uart_rd(3) <= '1';
      else
        uart_rd(3) <= '0';
      end if;


    end if;
  end process input_ports;


  --
  -----------------------------------------------------------------------------------------
  -- General Purpose Output Ports 
  -----------------------------------------------------------------------------------------

  output_ports : process(clk)
  begin
    if clk'event and clk = '1' then

      poke_ena <= '0';
      s_action <= (others => '0');

      -- 'write_strobe' is used to qualify all writes to general output ports
      -- along with bit 7 of the address which selects (only) the UART outputs
      if write_strobe = '1' and port_id(7) = '0' then

        -- Write to UART at port addresses 01 hex
        -- See below this clocked process for the combinatorial decode required.

        case port_id(4 downto 0) is

          -- addresses 0-2 currently unused
--          when "0" & x"1" => set_baud_rate(7 downto 0)  <= out_port;
--          when "0" & x"2" => set_baud_rate(15 downto 8) <= out_port;

          -- program address at 03, 04
          -- program data at 05, 06, 07
          when "0" & x"3" => peek_addr(7 downto 0)   <= out_port;
          when "0" & x"4" => peek_addr(11 downto 8)  <= out_port(3 downto 0);
          when "0" & x"5" => poke_data(7 downto 0)   <= out_port(7 downto 0);
          when "0" & x"6" => poke_data(15 downto 8)  <= out_port(7 downto 0);
          when "0" & x"7" => poke_data(17 downto 16) <= out_port(1 downto 0);
                             poke_ena <= '1';

          -- control at 09..0c
          when "0" & x"9" => s_control(7 downto 0)   <= out_port;
          when "0" & x"A" => s_control(15 downto 8)  <= out_port;
          when "0" & x"B" => s_control(23 downto 16) <= out_port;
          when "0" & x"C" => s_control(31 downto 24) <= out_port;

          -- action at 0d
          when "0" & x"D" => s_action <= out_port;

          -- control2 at 10..13
          when "1" & x"0" => s_control2(7 downto 0)   <= out_port;
          when "1" & x"1" => s_control2(15 downto 8)  <= out_port;
          when "1" & x"2" => s_control2(23 downto 16) <= out_port;
          when "1" & x"3" => s_control2(31 downto 24) <= out_port;

          -- baud rate lo at 14..17
          when "1" & x"4" => baud_rate_lo(0) <= out_port;
          when "1" & x"5" => baud_rate_lo(1) <= out_port;
          when "1" & x"6" => baud_rate_lo(2) <= out_port;
          when "1" & x"7" => baud_rate_lo(3) <= out_port;

          -- baud rate hi at 18..1b
          when "1" & x"8" => baud_rate_hi(0) <= out_port;
          when "1" & x"9" => baud_rate_hi(1) <= out_port;
          when "1" & x"a" => baud_rate_hi(2) <= out_port;
          when "1" & x"b" => baud_rate_hi(3) <= out_port;

          when others => null;
        end case;

      end if;

      --
      -- *** To reliably achieve 200MHz performance when writing to the FIFO buffer
      --     within the UART transmitter, 'port_id' is pipelined to exploit both of  
      --     the clock cycles that it is valid.
      --

      pipe_port <= port_id;

    end if;
  end process output_ports;


  

  -- See *** above for definition of 'pipe_port

  -- generate UART data write strobes at address 0x80, 0x81...
  -- certainly there is a better way to do this...
  uart_wr(0) <= '1' when (write_strobe = '1') and (pipe_port(7) = '1')
                and (pipe_port(1) = '0') and (pipe_port(0) = '0') else '0';

  uart_wr(1) <= '1' when (write_strobe = '1') and (pipe_port(7) = '1')
                and (pipe_port(1) = '0') and (pipe_port(0) = '1') else '0';

  uart_wr(2) <= '1' when (write_strobe = '1') and (pipe_port(7) = '1')
                and (pipe_port(1) = '1') and (pipe_port(0) = '0') else '0';

  uart_wr(3) <= '1' when (write_strobe = '1') and (pipe_port(7) = '1')
                and (pipe_port(1) = '1') and (pipe_port(0) = '1') else '0';

  --
  -----------------------------------------------------------------------------------------
  -- Constant-Optimised Output Ports 
  -----------------------------------------------------------------------------------------
  --
  -- One constant-optimised output port is used to facilitate resetting of the UART macros.
  --

  constant_output_ports : process(clk)
  begin
    if clk'event and clk = '1' then
      if k_write_strobe = '1' then

        if port_id(0) = '1' then
          uart_reset <= out_port(0);
        end if;

      end if;
    end if;
  end process constant_output_ports;

  --
  -----------------------------------------------------------------------------------------
  --

end arch;

