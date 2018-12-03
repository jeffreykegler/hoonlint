# Example for blog post on ambiguous languages

use 5.010;
use strict;
use warnings;

use Data::Dumper;
use English qw( -no_match_vars );

use Test::More tests => 4;
use Test::Differences;

require "yahc.pm";

sub slurp {
    my ($fileName) = @_;
    local $RS = undef;
    my $fh;
    open $fh, q{<}, $fileName or die "Cannot open $fileName";
    my $file = <$fh>;
    close $fh;
    return \$file;
}

my @tests = (
    ['t/ast.d/fizzbuzz.hoon', 't/ast.d/fizzbuzz.ast'],
    ['t/ast.d/sieve_b.hoon', 't/ast.d/sieve_b.ast'],
    ['t/ast.d/sieve_k.hoon', 't/ast.d/sieve_k.ast'],
    ['t/ast.d/toe.hoon', 't/ast.d/toe.ast'],
);

for my $testData (@tests) {

    my ( $hoonFileName, $astFileName ) = @{$testData};
    my $pHoonFile = slurp($hoonFileName);
    my $pAstFile  = slurp($astFileName);

    my $astRef = MarpaX::YAHC::parse( $pHoonFile );
    die "Parse failed" if not $astRef;
    my $pruned = MarpaX::YAHC::prune($astRef);
    local $Data::Dumper::Deepcopy = 1;
    local $Data::Dumper::Terse    = 1;
    my $dumped = Data::Dumper::Dumper($pruned) . "\n";

    if ( $dumped eq ${$pAstFile} ) {
        pass($hoonFileName);
    }
    else {
	eq_or_diff $dumped, ${$pAstFile}, $hoonFileName;
    }


}
