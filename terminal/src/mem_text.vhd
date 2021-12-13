--
-- display memory 80x40
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;


entity mem_text is

  port (
    -- common clock for both ports
    clk       : in  std_logic;
    -- port a is read-only for VGA controller
    addra_row : in  integer range 039 downto 0;  -- chr row   < 40 (6 bits)
    addra_col : in  integer range 079 downto 0;  -- chr col   < 80 (7 bits)
    douta     : out std_logic_vector(07 downto 0);
    -- port b is read-write for CPU
    -- addressed as column 0-79 and row 0-39
    dinb      : in  std_logic_vector(7 downto 0);
    addrb_col : in  std_logic_vector(6 downto 0);
    addrb_row : in  std_logic_vector(5 downto 0);
    web       : in  std_logic;
    doutb     : out std_logic_vector(07 downto 0));

end entity mem_text;


architecture arch of mem_text is

  type ram_type is array (39 downto 0, 79 downto 0) of std_logic_vector(7 downto 0);

  signal RAM : ram_type;

  signal addrb : std_logic_vector(11 downto 0);

begin  -- architecture arch

  -- clocked write on port b
  process(clk, web, addrb_row, addrb_col)
  begin
    if(rising_edge(clk)) then
      if(web = '1') then
        -- create linear address from addrb_col + 80*addrb_row
        -- we'll see if the synthesis tool gags on this :)
        RAM(conv_integer( addrb_row), conv_integer(addrb_col)) <= dinb;
      end if;
    end if;
  end process;

  -- asynchronous read on port b
  process(addrb_row, addrb_col)
  begin
    doutb <=    RAM(conv_integer( addrb_row), conv_integer(addrb_col));
  end process;

  -- asynchronous read on port a
  process(addra_row, addra_col)
  begin
    douta <= RAM(addra_row,addra_col);
  end process;


end architecture arch;
