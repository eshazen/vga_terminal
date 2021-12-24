#
# program flash - works in Vivado 2020.2 at least
# N.B. some of this is guesswork
#

set flashDevice {mx25u3233f-spi-x1_x2_x4}

open_hw_manager
connect_hw_server -url localhost:3121
current_hw_target [get_hw_targets *]
open_hw_target
current_hw_device [lindex [get_hw_devices] 0]
create_hw_cfgmem -hw_device [current_hw_device] $flashDevice

set cfgMem [current_hw_cfgmem]

set_property PROGRAM.FILES top_terminal_cmod.bin $cfgMem
set_property PROGRAM.ADDRESS_RANGE {use_file} $cfgMem
set_property PROGRAM.BLANK_CHECK 1 $cfgMem
set_property PROGRAM.ERASE 1 $cfgMem
set_property PROGRAM.CFG_PROGRAM 1 $cfgMem
set_property PROGRAM.VERIFY 1 $cfgMem

create_hw_bitstream -hw_device [lindex [get_hw_devices] 0] [get_property \
            PROGRAM.HW_CFGMEM_BITFILE [ lindex [get_hw_devices] 0]];
            program_hw_devices [lindex [get_hw_devices] 0];

program_hw_cfgmem $cfgMem
close_hw_manager

