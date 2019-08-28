# Example for blog post on ambiguous languages

use 5.010;
use strict;
use warnings;

use Data::Dumper;
use English qw( -no_match_vars );

require "./yahc.pm";

local $Data::Dumper::Deepcopy    = 1;
local $Data::Dumper::Terse    = 1;

my $globalFileName = '!ERROR!';
my $recce;

sub doNode {
    my (undef, @stuff) = @_;
    for my $arrayRef (@stuff) {
        # delete after development
        die Data::Dumper::Dumper(\@_) if ref $arrayRef ne 'ARRAY';
    }
    no warnings 'once';
    my $rule_id         = $Marpa::R2::Context::rule;
    my $slg             = $Marpa::R2::Context::slg;
    use warnings;
    my ( $lhs, @rhs ) =
                    map { $slg->symbol_display_form($_) } $slg->rule_expand($rule_id);
    if ($lhs eq 'LuslusCell') {
        my ($sym4k, $start, $symName) = @{$stuff[0]};
        return [] if $sym4k eq 'BUC';
        my ($line, $column) = $recce->line_column($start);
        die "Unknown first RHS for LuslusCell: $sym4k" if $sym4k ne 'SYM4K';
        say join q{ }, $symName, '++', join q{:}, $globalFileName, $line, $column;
    }
    if ($lhs eq 'LushepCell') {
        my ($sym4k, $start, $symName) = @{$stuff[0]};
        return [] if $sym4k eq 'BUC';
        my ($line, $column) = $recce->line_column($start);
        die "Unknown first RHS for LushepCell: $sym4k" if $sym4k ne 'SYM4K';
        say join q{ }, $symName, '+-', join q{:}, $globalFileName, $line, $column;
    }
    if ($lhs eq 'LustisCell') {
        my ($sym4k, $start, $symName) = @{$stuff[0]};
        return [] if $sym4k eq 'BUC';
        my ($line, $column) = $recce->line_column($start);
        die "Unknown first RHS for LustisCell: $sym4k" if $sym4k ne 'SYM4K';
        say join q{ }, $symName, '+=', join q{:}, $globalFileName, $line, $column;
    }
    return [];
}

my $semantics = <<'EOS';
:default ::= action => main::doNode
lexeme default = action => [name, start, value] latm => 1
EOS

my $parser = MarpaX::YAHC::new({semantics => $semantics});

my $pFileNames = do { local $RS = undef; my $fileNames = <STDIN>; \$fileNames };

FILE: for my $fileName (split "\n", ${$pFileNames}) {
    my $origLine = $fileName;
    chomp $fileName;
    $fileName =~ s/\s*[#].*$//xmsg; # Eliminate comments
    $fileName =~ s/^\s*//xmsg; # Eliminate leading space
    $fileName =~ s/\s*$//xmsg; # Eliminate trailing space
    next FILE unless $fileName;
    $globalFileName = $fileName;

    open my $fh, '<', $fileName or die "Cannot open $fileName";
    my $testName = $fileName;
    my $hoonSource = do { local $RS = undef; <$fh>; };
    $parser->read(\$hoonSource);
    $recce = $parser->rawRecce();
    my $astRef = $recce->value();
}

# vim: expandtab shiftwidth=4:
