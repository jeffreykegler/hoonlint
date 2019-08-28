# Example for blog post on ambiguous languages

use 5.010;
use strict;
use warnings;

use Data::Dumper;
use English qw( -no_match_vars );

require "./yahc.pm";

local $Data::Dumper::Deepcopy    = 1;
local $Data::Dumper::Terse    = 1;

my @count = ();
my $fileName = '!ERROR!';
my $recce;

sub doNode {
    no warnings 'once';
    my $ruleID         = $Marpa::R2::Context::rule;
    use warnings;
    if (not $count[$ruleID]) {
        $count[$ruleID] = 1;
        return [];
    }
    $count[$ruleID]++;
    return [];
}

my $semantics = <<'EOS';
:default ::= action => main::doNode
lexeme default = latm => 1
EOS

my $parser = MarpaX::YAHC::new({semantics => $semantics});
my $grammar = $parser->rawGrammar();
for my $ruleID ($grammar->rule_ids()) {
    $count[$ruleID] = 0;
}

my $pFileNames = do { local $RS = undef; my $fileNames = <STDIN>; \$fileNames };

FILE: for my $fileName (split "\n", ${$pFileNames}) {
    my $origLine = $fileName;
    chomp $fileName;
    $fileName =~ s/\s*[#].*$//xmsg; # Eliminate comments
    $fileName =~ s/^\s*//xmsg; # Eliminate leading space
    $fileName =~ s/\s*$//xmsg; # Eliminate trailing space
    next FILE unless $fileName;

    open my $fh, '<', $fileName or die "Cannot open $fileName";
    my $testName = $fileName;
    $testName =~ s/^hoons\///;
    $testName = "Test of " . $testName;
    my $hoonSource = do { local $RS = undef; <$fh>; };
    $parser->read(\$hoonSource);
    my $recce = $parser->rawRecce();
    my $astRef = $recce->value();
}

my @ruleData = ();
RULE:
for my $ruleID ( $grammar->rule_ids() )
{
    my ( $lhs, @rhs ) =
      map { $grammar->symbol_display_form($_) } $grammar->rule_expand($ruleID);
    next RULE unless $lhs =~ /^[a-zA-Z]/;
    my $ruleName = join " ", $lhs, q{::=}, @rhs;
    push @ruleData, [$count[$ruleID], $ruleName];
}

for my $ruleDatum ( sort { $b->[0] <=> $a->[0] or $a->[1] cmp $b->[1] } @ruleData ) {
    my ($count, $ruleName) = @{$ruleDatum};
    say join q{ }, $count, $ruleName;
}

# vim: expandtab shiftwidth=4:
