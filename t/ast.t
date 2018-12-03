# Example for blog post on ambiguous languages

use 5.010;
use strict;
use warnings;

use Data::Dumper;
use English qw( -no_match_vars );

use Test::More tests => 4;

require "yahc.pm";

sub slurp {
   my $fileName;
  local $RS = undef;
  open my $fh, $filename, q{<},
  my $file = <$fh>;
  close $fh;
  return \$file;
}

my @tests = (
    ['ast.d/fizzbuzz.hoon', 'ast.d/fizzbuzz.ast'],
    ['ast.d/sieve_b.hoon', 'ast.d/sieve_b.ast'],
    ['ast.d/sieve_k.hoon', 'ast.d/sieve_k.ast'],
    ['ast.d/toe.hoon', 'ast.d/toe.ast'],
);

for my $testData (@tests) {

    my ( $hoonFileName, $astFileName ) = @_;
    my $hoonFile = slurp($hoonFileName);
    my $astFile  = slurp($astFileName);

    my $astRef = MarpaX::YAHC::parse( \$hoonSource );
    die "Parse failed" if not $astRef;
    my $pruned = MarpaX::YAHC::prune($astRef);
    local $Data::Dumper::Deepcopy = 1;
    local $Data::Dumper::Terse    = 1;
    my $dumped = Data::Dumper::Dumper($pruned);

    if ( $dumped eq $astFile ) {
        pass($hoonFileName);
    }
    else {
        pass($astFileName);
    }


}
