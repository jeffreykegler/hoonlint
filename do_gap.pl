use 5.010;
use strict;
use warnings;
use English qw( -no_match_vars );
use GetOpt::Long;

use Marpa::R2 2.040000;

sub usage {

    die <<"END_OF_USAGE_MESSAGE";
    $PROGRAM_NAME
    $PROGRAM_NAME --stdin < file
With --stdin arg, reads expression from standard input.
By default, runs a test.
END_OF_USAGE_MESSAGE
} ## end sub usage

my $stdin_flag = 0;
my $getopt_result = Getopt::Long::GetOptions( 'stdin!' => \$stdin_flag, );
usage() if not $getopt_result;

my $input;
if ($stdin_flag) {
    $input = do { local $INPUT_RECORD_SEPARATOR = undef; <> };
}

my $rules = <<'END_OF_GRAMMAR';
:start ::= gap
gap ::= InterPart PrePart
InterPart ::=
InterPart ::= Staircase InterComponents
InterPart ::= Intercomment OptInterComponents
OptInterComponents ::= Intercomponent*
InterComponent ::= InterComment
InterComponent ::= Staircase
InterComponent ::= OtherStuff

Staircase ::= LooseUpperRisers UpperRiser Tread LowerRiser LooseLowerRisers
LooseUpperRisers ::= LooseUpperRiser*
LooseLowerRisers ::= LooseLowerRiser*

PrePart ::=
PrePart ::= PreComment OptPreComponents
OptPreComponents ::= Precomponent*
PreComponent ::= PreComment
PreComponent ::= Otherstuff

Otherstuff ::= MetaComment
Otherstuff ::= BadComment
Otherstuff ::= BlankLine

unicorn ~ [^\d\D]
BadComment ~ unicorn
Blankline ~ unicorn
InterComment ~ unicorn
LooseLowerRiser ~ unicorn
LooseUpperRiser ~ unicorn
LowerRiser ~ unicorn
MetaComment ~ unicorn
PreComment ~ unicorn
UpperRiser ~ unicorn

END_OF_GRAMMAR

# TO HERE !!!

my $grammar = Marpa::R2::Scanless::G->new(
    {   action_object  => 'My_Actions',
        default_action => 'do_arg0',
        source         => \$rules,
    }
);

sub calculate {
    my ($p_string) = @_;

    my $recce = Marpa::R2::Scanless::R->new( { grammar => $grammar } );

    my $self = bless { grammar => $grammar }, 'My_Actions';
    $self->{recce}        = $recce;
    $self->{symbol_table} = {};
    local $My_Actions::SELF = $self;

    if ( not defined eval { $recce->read($p_string); 1 } ) {

        # Add last expression found, and rethrow
        my $eval_error = $EVAL_ERROR;
        chomp $eval_error;
        die $self->show_last_expression(), "\n", $eval_error, "\n";
    } ## end if ( not defined eval { $recce->read($p_string); 1 })
    my $value_ref = $recce->value();
    if ( not defined $value_ref ) {
        die $self->show_last_expression(), "\n",
            "No parse was found, after reading the entire input\n";
    }
    return ${$value_ref}, $self->{symbol_table};

} ## end sub calculate


package My_Actions;
our $SELF;
sub new { return $SELF }

sub do_is_var {
    my ( $self, $var ) = @_;
    my $value = $self->{symbol_table}->{$var};
    Marpa::R2::Context::bail(qq{Undefined variable "$var"})
        if not defined $value;
    return $value;
} ## end sub do_is_var

sub do_set_var {
    my ( $self, $var, undef, $value ) = @_;
    return $self->{symbol_table}->{$var} = $value;
}

sub do_negate {
    return -$_[2];
}

sub do_arg0 { return $_[1]; }
sub do_arg1 { return $_[2]; }
sub do_arg2 { return $_[3]; }

sub do_array {
    my ( undef, $left, undef, $right ) = @_;
    my @value = ();
    my $ref;
    if ( $ref = ref $left ) {
        Marpa::R2::Context::bail("Bad ref type for array operand: $ref")
            if $ref ne 'ARRAY';
        push @value, @{$left};
    }
    else {
        push @value, $left;
    }
    if ( $ref = ref $right ) {
        Marpa::R2::Context::bail("Bad ref type for array operand: $ref")
            if $ref ne 'ARRAY';
        push @value, @{$right};
    }
    else {
        push @value, $right;
    }
    return \@value;
} ## end sub do_array

our %BINOP_CLOSURE;

BEGIN {
    %BINOP_CLOSURE = (
        '*' => sub { $_[0] * $_[1] },
        '/' => sub {
            Marpa::R2::Context::bail('Division by zero') if not $_[1];
            $_[0] / $_[1];
        },
        '+' => sub { $_[0] + $_[1] },
        '-' => sub { $_[0] - $_[1] },
        '^' => sub { $_[0]**$_[1] },
    );
} ## end BEGIN

sub do_binop {
    my ( $op, $left, $right ) = @_;
    my $closure = $BINOP_CLOSURE{$op};
    Marpa::R2::Context::bail(
        qq{Do not know how to perform binary operation "$op"})
        if not defined $closure;
    return $closure->( $left, $right );
} ## end sub do_binop

sub do_caret {
    my ( undef, $left, undef, $right ) = @_;
    return do_binop( '^', $left, $right );
}

sub do_star {
    my ( undef, $left, undef, $right ) = @_;
    return do_binop( '*', $left, $right );
}

sub do_slash {
    my ( undef, $left, undef, $right ) = @_;
    return do_binop( '/', $left, $right );
}

sub do_plus {
    my ( undef, $left, undef, $right ) = @_;
    return do_binop( '+', $left, $right );
}

sub do_minus {
    my ( undef, $left, undef, $right ) = @_;
    return do_binop( '-', $left, $right );
}

sub do_reduce {
    my ( undef, $op, undef, $args ) = @_;
    my $closure = $BINOP_CLOSURE{$op};
    Marpa::R2::Context::bail(
        qq{Do not know how to perform binary operation "$op"})
        if not defined $closure;
    $args = [$args] if ref $args eq '';
    my @stack = @{$args};
    OP: while (1) {
        return $stack[0] if scalar @stack <= 1;
        my $result = $closure->( $stack[-2], $stack[-1] );
        splice @stack, -2, 2, $result;
    }
    Marpa::R2::Context::bail('Should not get here');
} ## end sub do_reduce

sub show_last_expression {
    my ($self) = @_;
    my $recce = $self->{recce};
    my ( $start, $end ) = $recce->last_completed_range('expression');
    return 'No expression was successfully parsed' if not defined $start;
    my $last_expression = $recce->range_to_string( $start, $end );
    return "Last expression successfully parsed was: $last_expression";
} ## end sub show_last_expression

# vim: expandtab shiftwidth=4:
