use 5.010;
use strict;
use warnings;
use Data::Dumper;
use English qw( -no_match_vars );
use Scalar::Util qw(looks_like_number);

my %counts = ();
my %totals = ();

LINE: while (my $line = <>) {
   chomp $line;
   next LINE if $line =~ /^SEQUENCE /;
   next LINE if $line =~ /^=== /;
   next LINE if $line =~ /^hoons\//;
   $line =~ s/^FIXED-[0-9]\s*//;
   $line =~ s/ # .*$//;
   my ($lhs, @formats) = split ' ', $line;
   die $line if not $lhs;
   $counts{$lhs}++;
   if ($formats[0] =~ m/^[0-9:]+$/) {
       $totals{$lhs}{'other'}++;
       next LINE;
   }
   for my $format (@formats) {
       $totals{$lhs}{$format}++;
   }
   # say $line;
}

LHS: for my $lhs ( sort keys %totals ) {
    # my $jogStyleCount = $totals{$lhs}{'JOG-STYLE'};
    # my $flatCount = $totals{$lhs}{'FLAT'} // 0;
    # next LHS unless $jogStyleCount;
    # FORMAT: for my $format ( sort keys %{$totals{$lhs}} ) {
       # next FORMAT if $format eq 'JOG-STYLE';
       # next LHS if $totals{$lhs}{$format} >= $jogStyleCount;
    # }
    printf "$lhs %s\n", (join " ", %{$totals{$lhs}});
}
