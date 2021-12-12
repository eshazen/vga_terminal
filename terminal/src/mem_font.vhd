--
-- Font memory 3072 (12*256) x 8
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity mem_font is

  port (
    -- one port, read-only
    addr : in  std_logic_vector(11 downto 0);
    dout : out std_logic_vector(07 downto 0)
    );

end entity mem_font;


architecture arch of mem_font is

  type ram_type is array (0 to 3071) of std_logic_vector(7 downto 0);
  -- font supplied with the core
  signal RAM : ram_type := ( X"7E", X"C3",
  X"99", X"99", X"F3", X"E7", X"E7", X"FF", X"E7", X"E7", X"7E",
  X"00", X"00", X"00", X"00", X"76", X"DC", X"00", X"76", X"DC",
  X"00", X"00", X"00", X"00", X"6E", X"D8", X"D8", X"D8", X"D8",
  X"DE", X"D8", X"D8", X"D8", X"6E", X"00", X"00", X"00", X"00",
  X"00", X"6E", X"DB", X"DB", X"DF", X"D8", X"DB", X"6E", X"00",
  X"00", X"00", X"00", X"10", X"38", X"7C", X"FE", X"7C", X"38",
  X"10", X"00", X"00", X"00", X"88", X"88", X"F8", X"88", X"88",
  X"00", X"3E", X"08", X"08", X"08", X"08", X"00", X"F8", X"80",
  X"E0", X"80", X"80", X"00", X"3E", X"20", X"38", X"20", X"20",
  X"00", X"78", X"80", X"80", X"80", X"78", X"00", X"3C", X"22",
  X"3E", X"24", X"22", X"00", X"80", X"80", X"80", X"80", X"F8",
  X"00", X"3E", X"20", X"38", X"20", X"20", X"00", X"22", X"88",
  X"22", X"88", X"22", X"88", X"22", X"88", X"22", X"88", X"22",
  X"88", X"55", X"AA", X"55", X"AA", X"55", X"AA", X"55", X"AA",
  X"55", X"AA", X"55", X"AA", X"EE", X"BB", X"EE", X"BB", X"EE",
  X"BB", X"EE", X"BB", X"EE", X"BB", X"EE", X"BB", X"FF", X"FF",
  X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF",
  X"FF", X"00", X"00", X"00", X"00", X"00", X"00", X"FF", X"FF",
  X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF",
  X"FF", X"00", X"00", X"00", X"00", X"00", X"00", X"F0", X"F0",
  X"F0", X"F0", X"F0", X"F0", X"F0", X"F0", X"F0", X"F0", X"F0",
  X"F0", X"0F", X"0F", X"0F", X"0F", X"0F", X"0F", X"0F", X"0F",
  X"0F", X"0F", X"0F", X"0F", X"88", X"C8", X"A8", X"98", X"88",
  X"00", X"20", X"20", X"20", X"20", X"3E", X"00", X"88", X"88",
  X"50", X"50", X"20", X"00", X"3E", X"08", X"08", X"08", X"08",
  X"00", X"00", X"00", X"06", X"0C", X"18", X"30", X"7E", X"00",
  X"7E", X"00", X"00", X"00", X"00", X"00", X"60", X"30", X"18",
  X"0C", X"7E", X"00", X"7E", X"00", X"00", X"00", X"00", X"00",
  X"06", X"0C", X"FE", X"38", X"FE", X"60", X"C0", X"00", X"00",
  X"00", X"00", X"02", X"0E", X"3E", X"7E", X"FE", X"7E", X"3E",
  X"0E", X"02", X"00", X"00", X"00", X"80", X"E0", X"F0", X"FC",
  X"FE", X"FC", X"F0", X"E0", X"80", X"00", X"00", X"00", X"18",
  X"3C", X"7E", X"18", X"18", X"18", X"18", X"18", X"18", X"00",
  X"00", X"00", X"18", X"18", X"18", X"18", X"18", X"18", X"7E",
  X"3C", X"18", X"00", X"00", X"00", X"00", X"00", X"18", X"0C",
  X"FE", X"0C", X"18", X"00", X"00", X"00", X"00", X"00", X"00",
  X"00", X"30", X"60", X"FE", X"60", X"30", X"00", X"00", X"00",
  X"00", X"00", X"18", X"3C", X"7E", X"18", X"18", X"18", X"7E",
  X"3C", X"18", X"00", X"00", X"00", X"00", X"00", X"28", X"6C",
  X"FE", X"6C", X"28", X"00", X"00", X"00", X"00", X"00", X"06",
  X"06", X"36", X"66", X"FE", X"60", X"30", X"00", X"00", X"00",
  X"00", X"00", X"00", X"00", X"C0", X"7C", X"6E", X"6C", X"6C",
  X"6C", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00",
  X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"18",
  X"3C", X"3C", X"3C", X"18", X"18", X"00", X"18", X"18", X"00",
  X"00", X"00", X"36", X"36", X"14", X"00", X"00", X"00", X"00",
  X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"6C", X"FE",
  X"6C", X"6C", X"6C", X"FE", X"6C", X"00", X"00", X"00", X"10",
  X"7C", X"D6", X"70", X"38", X"1C", X"D6", X"7C", X"10", X"00",
  X"00", X"00", X"00", X"00", X"62", X"66", X"0C", X"18", X"30",
  X"66", X"C6", X"00", X"00", X"00", X"38", X"6C", X"38", X"38",
  X"72", X"FE", X"CC", X"CC", X"76", X"00", X"00", X"1C", X"1C",
  X"0C", X"18", X"00", X"00", X"00", X"00", X"00", X"00", X"00",
  X"00", X"00", X"0C", X"18", X"30", X"30", X"30", X"30", X"30",
  X"18", X"0C", X"00", X"00", X"00", X"30", X"18", X"0C", X"0C",
  X"0C", X"0C", X"0C", X"18", X"30", X"00", X"00", X"00", X"00",
  X"00", X"6C", X"38", X"FE", X"38", X"6C", X"00", X"00", X"00",
  X"00", X"00", X"00", X"00", X"18", X"18", X"7E", X"18", X"18",
  X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00",
  X"00", X"00", X"0C", X"0C", X"0C", X"18", X"00", X"00", X"00",
  X"00", X"00", X"00", X"FE", X"00", X"00", X"00", X"00", X"00",
  X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00",
  X"18", X"18", X"00", X"00", X"00", X"00", X"00", X"06", X"0C",
  X"18", X"30", X"60", X"C0", X"00", X"00", X"00", X"00", X"7C",
  X"C6", X"C6", X"C6", X"D6", X"C6", X"C6", X"C6", X"7C", X"00",
  X"00", X"00", X"18", X"78", X"18", X"18", X"18", X"18", X"18",
  X"18", X"7E", X"00", X"00", X"00", X"7C", X"C6", X"C6", X"0C",
  X"18", X"30", X"60", X"C6", X"FE", X"00", X"00", X"00", X"7C",
  X"C6", X"06", X"06", X"3C", X"06", X"06", X"C6", X"7C", X"00",
  X"00", X"00", X"0C", X"1C", X"3C", X"6C", X"CC", X"FE", X"0C",
  X"0C", X"0C", X"00", X"00", X"00", X"FE", X"C0", X"C0", X"C0",
  X"FC", X"06", X"06", X"C6", X"7C", X"00", X"00", X"00", X"7C",
  X"C6", X"C0", X"C0", X"FC", X"C6", X"C6", X"C6", X"7C", X"00",
  X"00", X"00", X"FE", X"C6", X"0C", X"18", X"30", X"30", X"30",
  X"30", X"30", X"00", X"00", X"00", X"7C", X"C6", X"C6", X"C6",
  X"7C", X"C6", X"C6", X"C6", X"7C", X"00", X"00", X"00", X"7C",
  X"C6", X"C6", X"C6", X"7E", X"06", X"06", X"C6", X"7C", X"00",
  X"00", X"00", X"00", X"00", X"0C", X"0C", X"00", X"00", X"0C",
  X"0C", X"00", X"00", X"00", X"00", X"00", X"00", X"0C", X"0C",
  X"00", X"00", X"0C", X"0C", X"0C", X"18", X"00", X"00", X"0C",
  X"18", X"30", X"60", X"C0", X"60", X"30", X"18", X"0C", X"00",
  X"00", X"00", X"00", X"00", X"00", X"FE", X"00", X"FE", X"00",
  X"00", X"00", X"00", X"00", X"00", X"60", X"30", X"18", X"0C",
  X"06", X"0C", X"18", X"30", X"60", X"00", X"00", X"00", X"7C",
  X"C6", X"C6", X"0C", X"18", X"18", X"00", X"18", X"18", X"00",
  X"00", X"00", X"7C", X"C6", X"C6", X"DE", X"DE", X"DE", X"DC",
  X"C0", X"7E", X"00", X"00", X"00", X"38", X"6C", X"C6", X"C6",
  X"C6", X"FE", X"C6", X"C6", X"C6", X"00", X"00", X"00", X"FC",
  X"66", X"66", X"66", X"7C", X"66", X"66", X"66", X"FC", X"00",
  X"00", X"00", X"3C", X"66", X"C0", X"C0", X"C0", X"C0", X"C0",
  X"66", X"3C", X"00", X"00", X"00", X"F8", X"6C", X"66", X"66",
  X"66", X"66", X"66", X"6C", X"F8", X"00", X"00", X"00", X"FE",
  X"66", X"60", X"60", X"7C", X"60", X"60", X"66", X"FE", X"00",
  X"00", X"00", X"FE", X"66", X"60", X"60", X"7C", X"60", X"60",
  X"60", X"F0", X"00", X"00", X"00", X"7C", X"C6", X"C6", X"C0",
  X"C0", X"CE", X"C6", X"C6", X"7C", X"00", X"00", X"00", X"C6",
  X"C6", X"C6", X"C6", X"FE", X"C6", X"C6", X"C6", X"C6", X"00",
  X"00", X"00", X"3C", X"18", X"18", X"18", X"18", X"18", X"18",
  X"18", X"3C", X"00", X"00", X"00", X"3C", X"18", X"18", X"18",
  X"18", X"18", X"D8", X"D8", X"70", X"00", X"00", X"00", X"C6",
  X"CC", X"D8", X"F0", X"F0", X"D8", X"CC", X"C6", X"C6", X"00",
  X"00", X"00", X"F0", X"60", X"60", X"60", X"60", X"60", X"62",
  X"66", X"FE", X"00", X"00", X"00", X"C6", X"C6", X"EE", X"FE",
  X"D6", X"D6", X"D6", X"C6", X"C6", X"00", X"00", X"00", X"C6",
  X"C6", X"E6", X"E6", X"F6", X"DE", X"CE", X"CE", X"C6", X"00",
  X"00", X"00", X"7C", X"C6", X"C6", X"C6", X"C6", X"C6", X"C6",
  X"C6", X"7C", X"00", X"00", X"00", X"FC", X"66", X"66", X"66",
  X"7C", X"60", X"60", X"60", X"F0", X"00", X"00", X"00", X"7C",
  X"C6", X"C6", X"C6", X"C6", X"C6", X"C6", X"D6", X"7C", X"06",
  X"00", X"00", X"FC", X"66", X"66", X"66", X"7C", X"78", X"6C",
  X"66", X"E6", X"00", X"00", X"00", X"7C", X"C6", X"C0", X"60",
  X"38", X"0C", X"06", X"C6", X"7C", X"00", X"00", X"00", X"7E",
  X"5A", X"18", X"18", X"18", X"18", X"18", X"18", X"3C", X"00",
  X"00", X"00", X"C6", X"C6", X"C6", X"C6", X"C6", X"C6", X"C6",
  X"C6", X"7C", X"00", X"00", X"00", X"C6", X"C6", X"C6", X"C6",
  X"C6", X"C6", X"6C", X"38", X"10", X"00", X"00", X"00", X"C6",
  X"C6", X"D6", X"D6", X"D6", X"FE", X"EE", X"C6", X"C6", X"00",
  X"00", X"00", X"C6", X"C6", X"6C", X"38", X"38", X"38", X"6C",
  X"C6", X"C6", X"00", X"00", X"00", X"66", X"66", X"66", X"66",
  X"3C", X"18", X"18", X"18", X"3C", X"00", X"00", X"00", X"FE",
  X"C6", X"8C", X"18", X"30", X"60", X"C2", X"C6", X"FE", X"00",
  X"00", X"00", X"7C", X"60", X"60", X"60", X"60", X"60", X"60",
  X"60", X"7C", X"00", X"00", X"00", X"00", X"00", X"C0", X"60",
  X"30", X"18", X"0C", X"06", X"00", X"00", X"00", X"00", X"7C",
  X"0C", X"0C", X"0C", X"0C", X"0C", X"0C", X"0C", X"7C", X"00",
  X"00", X"00", X"18", X"3C", X"66", X"00", X"00", X"00", X"00",
  X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00",
  X"00", X"00", X"00", X"00", X"00", X"00", X"FF", X"1C", X"1C",
  X"18", X"0C", X"00", X"00", X"00", X"00", X"00", X"00", X"00",
  X"00", X"00", X"00", X"00", X"00", X"78", X"0C", X"7C", X"CC",
  X"DC", X"76", X"00", X"00", X"00", X"E0", X"60", X"60", X"7C",
  X"66", X"66", X"66", X"66", X"FC", X"00", X"00", X"00", X"00",
  X"00", X"00", X"7C", X"C6", X"C0", X"C0", X"C6", X"7C", X"00",
  X"00", X"00", X"1C", X"0C", X"0C", X"7C", X"CC", X"CC", X"CC",
  X"CC", X"7E", X"00", X"00", X"00", X"00", X"00", X"00", X"7C",
  X"C6", X"FE", X"C0", X"C6", X"7C", X"00", X"00", X"00", X"1C",
  X"36", X"30", X"30", X"FC", X"30", X"30", X"30", X"78", X"00",
  X"00", X"00", X"00", X"00", X"00", X"76", X"CE", X"C6", X"C6",
  X"7E", X"06", X"C6", X"7C", X"00", X"E0", X"60", X"60", X"6C",
  X"76", X"66", X"66", X"66", X"E6", X"00", X"00", X"00", X"18",
  X"18", X"00", X"38", X"18", X"18", X"18", X"18", X"3C", X"00",
  X"00", X"00", X"00", X"0C", X"0C", X"00", X"1C", X"0C", X"0C",
  X"0C", X"CC", X"CC", X"78", X"00", X"E0", X"60", X"60", X"66",
  X"6C", X"78", X"6C", X"66", X"E6", X"00", X"00", X"00", X"70",
  X"30", X"30", X"30", X"30", X"30", X"30", X"34", X"18", X"00",
  X"00", X"00", X"00", X"00", X"00", X"6C", X"FE", X"D6", X"D6",
  X"C6", X"C6", X"00", X"00", X"00", X"00", X"00", X"00", X"DC",
  X"66", X"66", X"66", X"66", X"66", X"00", X"00", X"00", X"00",
  X"00", X"00", X"7C", X"C6", X"C6", X"C6", X"C6", X"7C", X"00",
  X"00", X"00", X"00", X"00", X"00", X"DC", X"66", X"66", X"66",
  X"7C", X"60", X"60", X"F0", X"00", X"00", X"00", X"00", X"76",
  X"CC", X"CC", X"CC", X"7C", X"0C", X"0C", X"1E", X"00", X"00",
  X"00", X"00", X"DC", X"66", X"60", X"60", X"60", X"F0", X"00",
  X"00", X"00", X"00", X"00", X"00", X"7C", X"C6", X"70", X"1C",
  X"C6", X"7C", X"00", X"00", X"00", X"30", X"30", X"30", X"FC",
  X"30", X"30", X"30", X"36", X"1C", X"00", X"00", X"00", X"00",
  X"00", X"00", X"CC", X"CC", X"CC", X"CC", X"CC", X"76", X"00",
  X"00", X"00", X"00", X"00", X"00", X"C6", X"C6", X"C6", X"6C",
  X"38", X"10", X"00", X"00", X"00", X"00", X"00", X"00", X"C6",
  X"C6", X"D6", X"D6", X"FE", X"6C", X"00", X"00", X"00", X"00",
  X"00", X"00", X"C6", X"6C", X"38", X"38", X"6C", X"C6", X"00",
  X"00", X"00", X"00", X"00", X"00", X"C6", X"C6", X"C6", X"CE",
  X"76", X"06", X"C6", X"7C", X"00", X"00", X"00", X"00", X"FE",
  X"8C", X"18", X"30", X"62", X"FE", X"00", X"00", X"00", X"0E",
  X"18", X"18", X"18", X"70", X"18", X"18", X"18", X"0E", X"00",
  X"00", X"00", X"18", X"18", X"18", X"18", X"18", X"18", X"18",
  X"18", X"18", X"00", X"00", X"00", X"70", X"18", X"18", X"18",
  X"0E", X"18", X"18", X"18", X"70", X"00", X"00", X"00", X"76",
  X"DC", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00",
  X"00", X"66", X"66", X"00", X"66", X"66", X"66", X"3C", X"18",
  X"18", X"3C", X"00", X"00", X"30", X"18", X"00", X"38", X"6C",
  X"C6", X"C6", X"FE", X"C6", X"C6", X"00", X"00", X"18", X"30",
  X"00", X"38", X"6C", X"C6", X"C6", X"FE", X"C6", X"C6", X"00",
  X"00", X"38", X"6C", X"38", X"00", X"7C", X"C6", X"C6", X"FE",
  X"C6", X"C6", X"00", X"00", X"76", X"DC", X"00", X"38", X"6C",
  X"C6", X"C6", X"FE", X"C6", X"C6", X"00", X"00", X"6C", X"6C",
  X"00", X"38", X"6C", X"C6", X"C6", X"FE", X"C6", X"C6", X"00",
  X"00", X"38", X"6C", X"38", X"00", X"7C", X"C6", X"C6", X"FE",
  X"C6", X"C6", X"00", X"00", X"7E", X"D8", X"D8", X"D8", X"D8",
  X"FE", X"D8", X"D8", X"D8", X"DE", X"00", X"00", X"00", X"3C",
  X"66", X"C0", X"C0", X"C0", X"C6", X"66", X"3C", X"18", X"CC",
  X"38", X"18", X"0C", X"00", X"FE", X"66", X"60", X"7C", X"60",
  X"66", X"FE", X"00", X"00", X"18", X"30", X"00", X"FE", X"66",
  X"60", X"7C", X"60", X"66", X"FE", X"00", X"00", X"38", X"6C",
  X"00", X"FE", X"66", X"60", X"7C", X"60", X"66", X"FE", X"00",
  X"00", X"6C", X"6C", X"00", X"FE", X"66", X"60", X"7C", X"60",
  X"66", X"FE", X"00", X"00", X"18", X"0C", X"00", X"3C", X"18",
  X"18", X"18", X"18", X"18", X"3C", X"00", X"00", X"18", X"30",
  X"00", X"3C", X"18", X"18", X"18", X"18", X"18", X"3C", X"00",
  X"00", X"3C", X"66", X"00", X"3C", X"18", X"18", X"18", X"18",
  X"18", X"3C", X"00", X"00", X"66", X"66", X"00", X"3C", X"18",
  X"18", X"18", X"18", X"18", X"3C", X"00", X"00", X"00", X"F8",
  X"6C", X"66", X"66", X"F6", X"66", X"66", X"6C", X"F8", X"00",
  X"00", X"76", X"DC", X"00", X"C6", X"E6", X"F6", X"DE", X"CE",
  X"C6", X"C6", X"00", X"00", X"30", X"18", X"00", X"7C", X"C6",
  X"C6", X"C6", X"C6", X"C6", X"7C", X"00", X"00", X"18", X"30",
  X"00", X"7C", X"C6", X"C6", X"C6", X"C6", X"C6", X"7C", X"00",
  X"00", X"38", X"6C", X"00", X"7C", X"C6", X"C6", X"C6", X"C6",
  X"C6", X"7C", X"00", X"00", X"76", X"DC", X"00", X"7C", X"C6",
  X"C6", X"C6", X"C6", X"C6", X"7C", X"00", X"00", X"6C", X"6C",
  X"00", X"7C", X"C6", X"C6", X"C6", X"C6", X"C6", X"7C", X"00",
  X"00", X"00", X"00", X"00", X"00", X"6C", X"38", X"38", X"6C",
  X"00", X"00", X"00", X"00", X"00", X"7E", X"C6", X"CE", X"DE",
  X"D6", X"F6", X"E6", X"C6", X"FC", X"00", X"00", X"30", X"18",
  X"00", X"C6", X"C6", X"C6", X"C6", X"C6", X"C6", X"7C", X"00",
  X"00", X"18", X"30", X"00", X"C6", X"C6", X"C6", X"C6", X"C6",
  X"C6", X"7C", X"00", X"00", X"38", X"6C", X"00", X"C6", X"C6",
  X"C6", X"C6", X"C6", X"C6", X"7C", X"00", X"00", X"6C", X"6C",
  X"00", X"C6", X"C6", X"C6", X"C6", X"C6", X"C6", X"7C", X"00",
  X"00", X"0C", X"18", X"00", X"66", X"66", X"66", X"3C", X"18",
  X"18", X"3C", X"00", X"00", X"00", X"F0", X"60", X"7C", X"66",
  X"66", X"66", X"7C", X"60", X"F0", X"00", X"00", X"00", X"7C",
  X"C6", X"C6", X"C6", X"CC", X"C6", X"C6", X"D6", X"DC", X"80",
  X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00",
  X"00", X"82", X"FE", X"00", X"00", X"00", X"00", X"18", X"18",
  X"00", X"18", X"18", X"3C", X"3C", X"3C", X"18", X"00", X"00",
  X"10", X"7C", X"D6", X"D0", X"D0", X"D6", X"7C", X"10", X"00",
  X"00", X"00", X"38", X"6C", X"60", X"60", X"F0", X"60", X"66",
  X"F6", X"6C", X"00", X"00", X"00", X"3C", X"62", X"60", X"F8",
  X"60", X"F8", X"60", X"62", X"3C", X"00", X"00", X"00", X"66",
  X"66", X"3C", X"18", X"7E", X"18", X"3C", X"18", X"18", X"00",
  X"00", X"6C", X"38", X"00", X"7C", X"C6", X"C0", X"7C", X"06",
  X"C6", X"7C", X"00", X"00", X"7C", X"C6", X"C6", X"60", X"7C",
  X"C6", X"C6", X"7C", X"0C", X"C6", X"C6", X"7C", X"00", X"6C",
  X"38", X"00", X"7C", X"C6", X"70", X"1C", X"C6", X"7C", X"00",
  X"00", X"7E", X"81", X"99", X"A5", X"A1", X"A1", X"A5", X"99",
  X"81", X"7E", X"00", X"00", X"3C", X"6C", X"6C", X"3E", X"00",
  X"7E", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00",
  X"00", X"36", X"6C", X"D8", X"6C", X"36", X"00", X"00", X"00",
  X"00", X"00", X"00", X"00", X"00", X"00", X"7E", X"06", X"06",
  X"06", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00",
  X"7E", X"00", X"00", X"00", X"00", X"00", X"00", X"7E", X"81",
  X"B9", X"A5", X"A5", X"B9", X"A5", X"A5", X"81", X"7E", X"00",
  X"00", X"FF", X"00", X"00", X"00", X"00", X"00", X"00", X"00",
  X"00", X"00", X"00", X"00", X"00", X"38", X"6C", X"38", X"00",
  X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00",
  X"00", X"18", X"18", X"7E", X"18", X"18", X"00", X"7E", X"00",
  X"00", X"00", X"38", X"6C", X"18", X"30", X"7C", X"00", X"00",
  X"00", X"00", X"00", X"00", X"00", X"38", X"6C", X"18", X"6C",
  X"38", X"00", X"00", X"00", X"00", X"00", X"00", X"6C", X"38",
  X"00", X"FE", X"C6", X"0C", X"38", X"62", X"C6", X"FE", X"00",
  X"00", X"00", X"00", X"00", X"00", X"CC", X"CC", X"CC", X"CC",
  X"CC", X"F6", X"C0", X"C0", X"00", X"7F", X"DB", X"DB", X"DB",
  X"7B", X"1B", X"1B", X"1B", X"1B", X"00", X"00", X"00", X"00",
  X"00", X"00", X"00", X"18", X"18", X"00", X"00", X"00", X"00",
  X"00", X"00", X"6C", X"38", X"00", X"FE", X"8C", X"18", X"30",
  X"62", X"FE", X"00", X"00", X"00", X"30", X"70", X"30", X"30",
  X"78", X"00", X"00", X"00", X"00", X"00", X"00", X"38", X"6C",
  X"6C", X"38", X"00", X"7C", X"00", X"00", X"00", X"00", X"00",
  X"00", X"00", X"00", X"00", X"D8", X"6C", X"36", X"6C", X"D8",
  X"00", X"00", X"00", X"00", X"00", X"6E", X"DB", X"DB", X"DF",
  X"D8", X"D8", X"D9", X"DF", X"6E", X"00", X"00", X"00", X"00",
  X"00", X"00", X"6C", X"DA", X"DE", X"D8", X"DA", X"6C", X"00",
  X"00", X"66", X"66", X"00", X"66", X"66", X"3C", X"18", X"18",
  X"18", X"3C", X"00", X"00", X"00", X"00", X"00", X"30", X"30",
  X"00", X"30", X"30", X"60", X"C6", X"C6", X"7C", X"00", X"00",
  X"FF", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00",
  X"00", X"18", X"18", X"18", X"18", X"18", X"18", X"00", X"00",
  X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00",
  X"1F", X"00", X"00", X"00", X"00", X"00", X"00", X"18", X"18",
  X"18", X"18", X"18", X"1F", X"00", X"00", X"00", X"00", X"00",
  X"00", X"00", X"00", X"00", X"00", X"00", X"18", X"18", X"18",
  X"18", X"18", X"18", X"18", X"18", X"18", X"18", X"18", X"18",
  X"18", X"18", X"18", X"18", X"18", X"18", X"18", X"00", X"00",
  X"00", X"00", X"00", X"1F", X"18", X"18", X"18", X"18", X"18",
  X"18", X"18", X"18", X"18", X"18", X"18", X"1F", X"18", X"18",
  X"18", X"18", X"18", X"18", X"00", X"00", X"00", X"00", X"00",
  X"F8", X"00", X"00", X"00", X"00", X"00", X"00", X"18", X"18",
  X"18", X"18", X"18", X"F8", X"00", X"00", X"00", X"00", X"00",
  X"00", X"00", X"00", X"00", X"00", X"00", X"FF", X"00", X"00",
  X"00", X"00", X"00", X"00", X"18", X"18", X"18", X"18", X"18",
  X"FF", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00",
  X"00", X"00", X"00", X"F8", X"18", X"18", X"18", X"18", X"18",
  X"18", X"18", X"18", X"18", X"18", X"18", X"F8", X"18", X"18",
  X"18", X"18", X"18", X"18", X"00", X"00", X"00", X"00", X"00",
  X"FF", X"18", X"18", X"18", X"18", X"18", X"18", X"18", X"18",
  X"18", X"18", X"18", X"FF", X"18", X"18", X"18", X"18", X"18",
  X"18", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"FF",
  X"00", X"00", X"00", X"00", X"6C", X"6C", X"6C", X"6C", X"6C",
  X"6C", X"7C", X"00", X"00", X"00", X"00", X"00", X"00", X"00",
  X"00", X"00", X"3F", X"30", X"3F", X"00", X"00", X"00", X"00",
  X"00", X"6C", X"6C", X"6C", X"6C", X"6F", X"60", X"7F", X"00",
  X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"7C",
  X"6C", X"6C", X"6C", X"6C", X"6C", X"6C", X"6C", X"6C", X"6C",
  X"6C", X"6C", X"6C", X"6C", X"6C", X"6C", X"6C", X"6C", X"6C",
  X"6C", X"00", X"00", X"00", X"00", X"7F", X"60", X"6F", X"6C",
  X"6C", X"6C", X"6C", X"6C", X"6C", X"6C", X"6C", X"6C", X"6F",
  X"60", X"6F", X"6C", X"6C", X"6C", X"6C", X"6C", X"00", X"00",
  X"00", X"00", X"FC", X"0C", X"FC", X"00", X"00", X"00", X"00",
  X"00", X"6C", X"6C", X"6C", X"6C", X"EC", X"0C", X"FC", X"00",
  X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"FF",
  X"00", X"FF", X"00", X"00", X"00", X"00", X"00", X"6C", X"6C",
  X"6C", X"6C", X"EF", X"00", X"FF", X"00", X"00", X"00", X"00",
  X"00", X"00", X"00", X"00", X"00", X"FC", X"0C", X"EC", X"6C",
  X"6C", X"6C", X"6C", X"6C", X"6C", X"6C", X"6C", X"6C", X"EC",
  X"0C", X"EC", X"6C", X"6C", X"6C", X"6C", X"6C", X"00", X"00",
  X"00", X"00", X"FF", X"00", X"EF", X"6C", X"6C", X"6C", X"6C",
  X"6C", X"6C", X"6C", X"6C", X"6C", X"EF", X"00", X"EF", X"6C",
  X"6C", X"6C", X"6C", X"6C", X"60", X"30", X"18", X"00", X"78",
  X"0C", X"7C", X"CC", X"DC", X"76", X"00", X"00", X"18", X"30",
  X"60", X"00", X"78", X"0C", X"7C", X"CC", X"DC", X"76", X"00",
  X"00", X"30", X"78", X"CC", X"00", X"78", X"0C", X"7C", X"CC",
  X"DC", X"76", X"00", X"00", X"00", X"76", X"DC", X"00", X"78",
  X"0C", X"7C", X"CC", X"DC", X"76", X"00", X"00", X"00", X"6C",
  X"6C", X"00", X"78", X"0C", X"7C", X"CC", X"DC", X"76", X"00",
  X"00", X"38", X"6C", X"38", X"00", X"78", X"0C", X"7C", X"CC",
  X"DC", X"76", X"00", X"00", X"00", X"00", X"00", X"7E", X"DB",
  X"1B", X"7F", X"D8", X"DB", X"7E", X"00", X"00", X"00", X"00",
  X"00", X"7C", X"C6", X"C0", X"C0", X"C6", X"7C", X"18", X"6C",
  X"38", X"30", X"18", X"0C", X"00", X"7C", X"C6", X"FE", X"C0",
  X"C6", X"7C", X"00", X"00", X"0C", X"18", X"30", X"00", X"7C",
  X"C6", X"FE", X"C0", X"C6", X"7C", X"00", X"00", X"10", X"38",
  X"6C", X"00", X"7C", X"C6", X"FE", X"C0", X"C6", X"7C", X"00",
  X"00", X"00", X"6C", X"6C", X"00", X"7C", X"C6", X"FE", X"C0",
  X"C6", X"7C", X"00", X"00", X"60", X"30", X"18", X"00", X"38",
  X"18", X"18", X"18", X"18", X"3C", X"00", X"00", X"0C", X"18",
  X"30", X"00", X"38", X"18", X"18", X"18", X"18", X"3C", X"00",
  X"00", X"18", X"3C", X"66", X"00", X"38", X"18", X"18", X"18",
  X"18", X"3C", X"00", X"00", X"00", X"6C", X"6C", X"00", X"38",
  X"18", X"18", X"18", X"18", X"3C", X"00", X"00", X"78", X"30",
  X"78", X"0C", X"7E", X"C6", X"C6", X"C6", X"C6", X"7C", X"00",
  X"00", X"00", X"76", X"DC", X"00", X"DC", X"66", X"66", X"66",
  X"66", X"66", X"00", X"00", X"60", X"30", X"18", X"00", X"7C",
  X"C6", X"C6", X"C6", X"C6", X"7C", X"00", X"00", X"0C", X"18",
  X"30", X"00", X"7C", X"C6", X"C6", X"C6", X"C6", X"7C", X"00",
  X"00", X"10", X"38", X"6C", X"00", X"7C", X"C6", X"C6", X"C6",
  X"C6", X"7C", X"00", X"00", X"00", X"76", X"DC", X"00", X"7C",
  X"C6", X"C6", X"C6", X"C6", X"7C", X"00", X"00", X"00", X"6C",
  X"6C", X"00", X"7C", X"C6", X"C6", X"C6", X"C6", X"7C", X"00",
  X"00", X"00", X"00", X"18", X"18", X"00", X"7E", X"00", X"18",
  X"18", X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"7E",
  X"CE", X"DE", X"F6", X"E6", X"FC", X"00", X"00", X"C0", X"60",
  X"30", X"00", X"CC", X"CC", X"CC", X"CC", X"CC", X"76", X"00",
  X"00", X"0C", X"18", X"30", X"00", X"CC", X"CC", X"CC", X"CC",
  X"CC", X"76", X"00", X"00", X"30", X"78", X"CC", X"00", X"CC",
  X"CC", X"CC", X"CC", X"CC", X"76", X"00", X"00", X"00", X"CC",
  X"CC", X"00", X"CC", X"CC", X"CC", X"CC", X"CC", X"76", X"00",
  X"00", X"0C", X"18", X"30", X"00", X"C6", X"C6", X"C6", X"CE",
  X"76", X"06", X"C6", X"7C", X"00", X"F0", X"60", X"60", X"78",
  X"6C", X"6C", X"6C", X"78", X"60", X"60", X"F0", X"00", X"C6",
  X"C6", X"00", X"C6", X"C6", X"C6", X"CE", X"76", X"06", X"C6", X"7C"
  );

begin  -- architecture arch

  process(addr)
  begin
    dout <= RAM(conv_integer(addr));
  end process;

end architecture arch;