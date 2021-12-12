#!/usr/bin/perl
#
# read hex file from picoblaze assembler and output picoblaze boot loader file
# Each output line is four characters, with a function code in the first column
# followed by a uuencoded 16 or 18 bit binary word.
#
# +zzz    specify load address for subsequent data
# =zzz    specify data to load starting at address
#         (usually repeated)
# $zzz    jump to specified address (marks end of file)
#
# skip over leading and trailing zeroes in file and emit an address
# for the first non-zero location
#

my $na = $#ARGV+1;
if( $na < 2) {
    print "usage: $0 <hex_file> <uue_file>\n";
    exit;
}

open HEX, "< $ARGV[0]" or die "Opening $ARGV[0]";
open UUE, "> $ARGV[1]" or die "Opening $ARGV[1]";

my $fill= "00000";		# fill word

# uuencode a binary value up to 18 bits
# first output char is MSB
sub uue {
    my $bin = shift(@_);
    $uu1 = chr( 32 + (($bin >> 12) & 0x3f));
    $uu2 = chr( 32 + (($bin >> 6) & 0x3f));
    $uu3 = chr( 32 + ($bin & 0x3f));
    return $uu1 . $uu2 . $uu3;
}

my @words;

while( $line = <HEX>) {		# read the hex file
    $line =~ s/[[:cntrl:]]//g;	# clean crud from line
    push @words, hex($line);
}
$nwords = $#words;

$lastw = $nwords;

$lastw-- while( $words[$lastw] == $fill); # find last non-zero word
$lastw += 2;			# add a couple for safety

$firstw = 0;
$firstw++ while( $words[$firstw] == $fill);

printf "Non-zero data from 0x%04x to 0x%04x\n", $firstw, $lastw;

print UUE "+" . uue( $firstw) . "\n";	# default load address = 0

for( $i=$firstw; $i<=$lastw; $i++) {
#    printf "Addr: %03x  Data: %06x  UUE: \"%s\"\n", $i, $words[$i], uue( $words[$i]);
    print UUE "=" . uue( $words[$i]) . "\n";
}

print UUE "\$" . uue( $firstw) . "\n";


