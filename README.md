# vga_terminal
VGA display terminal for RC2014 and other applications

The design goal here is to provide a VGA resolution terminal (640x480) at the standard 25.175MHz dot clock rate.  Input would be a serial port at up to 115200 baud, with ANSI escape sequence processing and perhaps eventually a graphics mode with Tek40xx or DEC REGIS graphics.  The target hardware is the Digilent CMOD-A7 with an Artix FPGA and 512kB of asynchronous SRAM.

The text display will be prototyped on a Basys-3 evaluation board and use only block RAM onboard the FPGA for text display.
