# Test of symcnt utility

use 5.010;
use strict;
use warnings;

use Data::Dumper;
use English qw( -no_match_vars );

require "./yahc.pm";

local $Data::Dumper::Deepcopy    = 1;
local $Data::Dumper::Terse    = 1;

my $recce;
my $globalFileName = '!!! ERROR !!!';
my @count = ();
my %reverseIx = ();

sub doNode {
    my ( undef, @stuff ) = @_;
    my @lexemes =
      grep {
      # say STDERR "after reverseIx: $_";
      defined $_; }
      map {
      # say STDERR "before reverseIx: $_";
      $reverseIx{$_}; } map { @{$_}; } grep { $_; } @stuff;
    no warnings 'once';
    my $rule_id = $Marpa::R2::Context::rule;
    my $slg     = $Marpa::R2::Context::slg;
    use warnings;
    my ($lhs) = $slg->rule_expand($rule_id);
    # say STDERR "LHS: ", $lhs;
    # say STDERR "Lexemes: ", join " ", @lexemes;
    for my $sym ( $lhs, @lexemes ) {
        $count[$sym]++;
    }
    return;
}

my $semantics = <<'EOS';
:default ::= action => main::doNode
lexeme default = action => [name] latm => 1
EOS
my $parser = MarpaX::YAHC::new({semantics => $semantics});
my $grammar = $parser->rawGrammar();
SYMBOL_ID: for my $symbolID ($grammar->symbol_ids()) {
    $count[$symbolID] = 0;
    my $name = $grammar->symbol_name($symbolID);
    next SYMBOL_ID unless $name;
    $reverseIx{$name} = $symbolID;
}

my $pFileNames = do { local $RS = undef; my $fileNames = <STDIN>; \$fileNames };

FILE: for my $fileName (split "\n", ${$pFileNames}) {
    my $origLine = $fileName;
    $fileName =~ s/\s*[#].*$//xmsg; # Eliminate comments
    $fileName =~ s/^\s*//xmsg; # Eliminate leading space
    $fileName =~ s/\s*$//xmsg; # Eliminate trailing space
    next FILE unless $fileName;
    
    $globalFileName = $fileName;

    open my $fh, '<', $fileName or die "Cannot open $fileName";
    my $testName = $fileName;
    $testName =~ s/^hoons\///;
    $testName = "Test of " . $testName;
    my $hoonSource = do { local $RS = undef; <$fh>; };
    $parser->read(\$hoonSource);
    $recce = $parser->rawRecce();
    my $astRef = $recce->value();
}

my @symbolData = ();
SYMBOL:
for my $symbolID ( $grammar->symbol_ids() )
{
    my $symbolName = $grammar->symbol_name($symbolID);
    # say join " ", $symbolID, $symbolName;
    next SYMBOL unless $symbolName =~ /^[a-zA-Z]/;
    push @symbolData, [$count[$symbolID], (lc $symbolName), $symbolName];
}

for my $symbolDatum ( sort { $b->[0] <=> $a->[0] or $a->[1] cmp $b->[1] } @symbolData ) {
    my ($count, undef, $symbolName) = @{$symbolDatum};
    say join q{ }, $symbolName, $count;
}

# vim: expandtab shiftwidth=4:
