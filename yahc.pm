# !!! DO NOT EDIT !!!
# This code automatically written by yahc.PM


use 5.010;
use strict;
use warnings;

use Data::Dumper;
use Marpa::R2 4.000;

package MarpaX::YAHC;

use English qw( -no_match_vars );

my $dsl = do { $RS = undef; <DATA> };

my $grammar = Marpa::R2::Scanless::G->new( { source => \$dsl } );

sub divergence {
    die join '', 'Unrecoverable internal error: ', @_;
}

# Given a target, an input and an offset into that input,
# it reads using that recognizer.  The return values
# are the parse value and a new offset in the input.
# Errors are thrown.

sub getValue {
    my ( $target, $input, $offset ) = @_;
    my $input_length = length ${$input};
    my $resume_pos;
    my $this_pos;

    my $nextNL = index ${$input}, "\n", $offset;
    if ($nextNL < 0) {
      die join '', 'Newline missing after triple quotes: "', ${$input}, '"'
    }
    my $initiator = substr ${$input}, $offset, $nextNL-$offset;
    if ($initiator ne "'''" and $initiator !~ m/^''' *::/) {
      die join '', 'Disallowed characters after initial triple quotes: "', $initiator, '"'
    }

    pos ${$input} = $offset;
    my ($indent) = ${$input} =~ /\G( *)[^ ]/g;
    my $terminator = $indent . "'''";

    my $terminatorPos = index ${$input}, $terminator, $offset;
    my $value = substr ${$input}, $nextNL+1, ($terminatorPos - $nextNL - 1);

    say STDERR "Left main READ loop" if $MarpaX::YAHC::DEBUG;

    # Return ref to value and new offset
    return \$value, $terminatorPos + length $terminator;
}

sub parse {
    my ($input) = @_;
    my $debug = $MarpaX::YAHC::DEBUG;
    my $recce = Marpa::R2::Scanless::R->new(
        {
            grammar         => $grammar,
            ranking_method  => 'high_rule_only',
            trace_lexers    => ( $debug ? 1 : 0 ),
            trace_terminals => ( $debug ? 1 : 0 ),
        }
    );

    my $input_length = length ${$input};
    my $this_pos;
    my $ok = eval { $this_pos = $recce->read( $input ) ; 1; };
    if (not $ok) {
       say STDERR $recce->show_progress(0, -1) if $debug;
       die $EVAL_ERROR;
    }

    # The main read loop.  Read starting at $offset.
    # If interrupted execute the handler logic,
    # and, possibly, resume.
    say STDERR "this_pos=$this_pos ; input_length=$input_length" if $debug;

  READ:
    while ( $this_pos < $input_length ) {

	my $resume_pos;

        # Only one event at a time is expected -- more
        # than one is an error.  No event means parsing
        # is exhausted.

        my $events      = $recce->events();
        my $event_count = scalar @{$events};
        if ( $event_count < 0 ) {
            last READ;
        }
        if ( $event_count != 1 ) {
            divergence("One event expected, instead got $event_count");
        }

        # Find the event name

        my $event = $events->[0];
        my $name  = $event->[0];

        if ( $name eq 'tripleQuote' ) {
            say STDERR "$name event" if $MarpaX::YAHC::DEBUG;
            my $value_ref;
            ( $value_ref, $resume_pos ) = getValue( $name, $input, $this_pos );
            my $result = $recce->lexeme_read(
                'TRIPLE QUOTE STRING',
                $this_pos,
                ( length ${$value_ref} ),
                [ ${$value_ref} ]
            );
            say STDERR "lexeme_read('TRIPLE QUOTE STRING',...) returned ",
              Data::Dumper::Dumper( \$result )
              if $MarpaX::YAHC::DEBUG;
        }

	if (not $resume_pos) {
	  die "read() ended prematurely\n",
	    "  input length = $input_length\n",
	    "  length read = $this_pos\n",
	    "  the cause may be an unexpected event";
	}

	say STDERR "this_pos=$this_pos ; input_length=$input_length" if $debug;

        my $ok = eval { $this_pos = $recce->resume($resume_pos); 1; };
        if ( not $ok ) {
            say STDERR $recce->show_progress( 0, -1 ) if $debug;
            die $EVAL_ERROR;
        }

    }

    if ( 0 ) {
    # if ( $recce->ambiguity_metric() > 1 ) {

        # The calls in this section are experimental as of Marpa::R2 2.090
        my $asf = Marpa::R2::ASF->new( { slr => $recce } );
        say STDERR 'No ASF' if not defined $asf;
        my $ambiguities = Marpa::R2::Internal::ASF::ambiguities($asf);
        my @ambiguities = grep { defined } @{$ambiguities}[ 0 .. 1 ];
        die
          "Parse of BNF/Scanless source is ambiguous\n",
          Marpa::R2::Internal::ASF::ambiguities_show( $asf, \@ambiguities );
    } ## end if ( $recce->ambiguity_metric() > 1 )

    my $valueRef = $recce->value();
    if ( !$valueRef ) {
        die "input read, but there was no parse";
    }

    return $valueRef;
}

# Takes one argument and returns a ref to an array of acceptable
# nodes.  The array may be empty.  All scalars are acceptable
# leaf nodes.  Acceptable interior nodes have length at least 1
# and contain a Haskell Standard symbol name, followed by zero or
# more acceptable nodes.
sub prune {
    my ($v) = @_;

    state $deleteIfEmpty = {
        optKets => 1,
    };

    state $nonSemantic = {
        doubleStringElements => 1,
	teakChoice => 1,
	wideTeakChoice => 1,
        wedeFirst            => 1,
        wideHoon             => 1,
        wideHoonJogging      => 1,
        wideHoonJogs         => 1,
        wideHoonJog          => 1,
        wideHoonSeq          => 1,
        wideMold             => 1,
        wideMoldSeq          => 1,
        hoon                 => 1,
        hoonExpression       => 1,
        hoonFile             => 1,
        hoonPrimary          => 1,
        hoonSeq              => 1,
        hoonUnary            => 1,
	long5dWide => 1,
        mold                 => 1,
        moldSeq              => 1,
        norm5d               => 1,
        norm5dWide          => 1,
        rope5d               => 1,
        scat5d               => 1,
    };

    return [] if not defined $v;
    my $reftype = ref $v;
    return [$v] if not $reftype; # An acceptable leaf node
    return prune($$v) if $reftype eq 'REF';
    divergence("Tree node has reftype $reftype") if $reftype ne 'ARRAY';
    my @source = grep { defined } @{$v};
    my $element_count = scalar @source;
    return [] if $element_count <= 0; # must have at least one element
    my $name = shift @source;
    my $nameReftype = ref $name;
    # divergence("Tree node name has reftype $nameReftype") if $nameReftype;
    if ($nameReftype) {
      my @result = ();
      ELEMENT:for my $element ($name, @source) {
	if (ref $element eq 'ARRAY') {
	  push @result, grep { defined }
		  map { @{$_}; }
		  map { prune($_); }
		  @{$element}
		;
	  next ELEMENT;
	}
	push @result, $_;
      }
      return [@result];
    }
    if (defined $deleteIfEmpty->{$name} and $element_count == 1) {
      return [];
    }
    if (defined $nonSemantic->{$name}) {
      # Not an acceptable branch node, but (hopefully)
      # its children are acceptable
      return [ grep { defined }
	      map { @{$_}; }
	      map { prune($_); }
	      @source
	    ];
    }

    # An acceptable branch node
    my @result = ($name);
    push @result, grep { defined }
	    map { @{$_}; }
	    map { prune($_); }
	    @source;
    return [\@result];
}

1;

__DATA__
# start and length will be needed for production
# :default ::= action => [name,start,length,values]
:default ::= action => [name,values]
lexeme default = latm => 1

# === CHARACTER SET ===

# Unicorn is a non-existence character used for various
# tricks: error rules, TODO rules, inaccessbile symbols,
# etc.
UNICORN ~ unicorn
unicorn ~ [^\d\D]

# === Hoon 4i library ===

DOG4I ~ dog4i
dog4i ~ dot4h gay4i

doh4i ~ hep4h hep4h gay4i

gay4i ~ # empty
gay4i ~ gap4k

LOW4I ~ low4i
low4i ~ [a-z]

NUD4I ~ nud4i
nud4i ~ [0-9]

# vul4i ~ '::' optNonNLs nl

# === Hoon 4j library ===

# Two hex numbers
bix4j ~ six4j six4j

MOT4J ~ mot4j
mot4j ~ [12] sid4j
mot4j ~ sed4j

dum4j ~ sid4j+

DIM4J ~ dim4j # a natural number
dim4j ~ '0'
dim4j ~ dip4j

DIP4J ~ dip4j
dip4j ~ [1-9] dip4jRest
dip4jRest ~ [0-9]*

fed4j ::= huf4j
fed4j ::= huf4j doh4i hyf4jSeq
fed4j ::= hof4j
fed4j ::= haf4j
fed4j ::= TIQ4J

haf4j ::= TEP4J TIP4J

# In hoon.hoon, hef and hif differ in semantics.
hef4j ::= TIP4J TIQ4J

hex4j ~ '0' qex4j
hex4j ~ '0' qex4j dog4i qix4jSeq

# In hoon.hoon, hef and hif differ in semantics.
hif4j ::= TIP4J TIQ4J

hof4j ::= hef4j HEP hif4j
hof4j ::= hef4j HEP hif4j HEP hif4j
hof4j ::= hef4j HEP hif4j HEP hif4j HEP hif4j

huf4j ::= hef4j
huf4j ::= hef4j HEP hif4j
huf4j ::= hef4j HEP hif4j HEP hif4j
huf4j ::= hef4j HEP hif4j HEP hif4j HEP hif4j

hyf4j ::= hif4j HEP hif4j
hyf4jSeq ::= hyf4j+ separator=>DOT proper=>1

sed4j ~ [1-9]

sex4j ~ [1-9a-f]

sid4j ~ [0-9]

# hexadecimal digit
six4j ~ [0-9a-f]

siv4j ~ [0-9a-v]

qex4j ~ sex4j
qex4j ~ sex4j hit4k
qex4j ~ sex4j hit4k hit4k
qex4j ~ sex4j hit4k hit4k hit4k

qix4j ~ six4j six4j six4j six4j
QIX4J_SEQ ~ qix4jSeq
qix4jSeq ~ qix4j+ separator=>dot4h proper=>1

# tep, tip and tiq have different semantics in hoon.hoon
TEP4J ~ low4i low4i low4i
TIP4J ~ low4i low4i low4i
TIQ4J ~ low4i low4i low4i

urs4j ::= ursChoice*
ursChoice ::= NUD4I | LOW4I | HEP | DOT | SIG | CAB

urx4j ::= urxChoice*
urxChoice ::= NUD4I | LOW4I | HEP | CAB | DOT
urxChoice ::= SIG hex4j DOT
urxChoice ::= SIG SIG DOT

VUM4J ~ vum4j
vum4j ~ siv4j+

# === Hoon 4k library ===

SYM4K ~ sym4k
CEN_SYM4K ~ cen4h sym4k
sym4k ~ low4i sym4kRest
hig4k ~ [A-Z]

sym4kRest ~ # empty
sym4kRest ~ sym4kRestChars
sym4kRestChars ~ sym4kRestChar+
sym4kRestChar ~ low4i | nud4i | hep4h

VEN4K ~ ven4k
ven4k ~ carCdr
ven4k ~ carCdrPairs
ven4k ~ carCdrPairs carCdr
carCdrPairs ~ carCdrPair+
carCdrPair ~ [-+][<>]
carCdr ~ [-+]

qit4k ::= <UNESCAPED SINGLE QUOTE CHARS>
qit4k ::= EscapedSingleQuoteChar
<UNESCAPED SINGLE QUOTE CHARS> ~ unescapedSingleQuoteChar+

qut4k ::= <single quote string>
<single quote string> ::= ([']) <single quote cord> (['])
<single quote cord> ::= qit4k* separator=>gon4k proper=>1

# <TRIPLE QUOTE START> triggers an event -- the quoted
# string is actually supplies as <TRIPLE QUOTE STRING>.
qut4k ::= <TRIPLE QUOTE START>
qut4k ::= <TRIPLE QUOTE STRING>
:lexeme ~ <TRIPLE QUOTE START> event=>tripleQuote pause=>before
<TRIPLE QUOTE START> ~ ['] ['] [']
<TRIPLE QUOTE STRING> ~ unicorn # implemented with a combinator

# All the printable (non-control) characters except
# bas and soq
unescapedSingleQuoteChar ~ [\x20-\x26\x28-\x5b\x5d-\x7e\x80-\xff]
EscapedSingleQuoteChar ::= (BAS4H) BAS4H | (BAS4H) SOQ4H
EscapedSingleQuoteChar ::= AsciiHexChar
AsciiHexChar ::= (BAS4H) MES4K

dem4k ::= DIT4K_SEQ+ separator=>gon4k proper=>1

DIT4K_SEQ ~ dit4kSeq
dit4kSeq ~ dit4k+
dit4k ~ [0-9]

MES4K ~ mes4k
mes4k ~ hit4k hit4k

# HIT4K ~ hit4k
hit4k ~ dit4k
hit4k ~ [a-fA-F]

gon4k ~ bas4h gay4i fas4h

# === Hoon 4l library ===

# TODO: crub(4l) is incomplete

crub4l ::= date
crub4l ::= timePeriod
crub4l ::= fed4j
crub4l ::= DOT urs4j
crub4l ::= SIG urx4j
crub4l ::= HEP urx4j

date ::= date_part1
date ::= date_part1 DOT DOT date_part2
date ::= date_part1 DOT DOT date_part2 DOT DOT date_part3
date_part1 ::= DIM4J optHep DOT MOT4J DOT DIP4J
optHep ::= # empty
optHep ::= HEP
date_part2 ::= dum4j DOT dum4j DOT dum4j
date_part3 ::= QIX4J_SEQ

timePeriod ::= timePeriodKernel timePeriodFraction
timePeriod ::= timePeriodKernel
timePeriodKernel ::= timePeriodByUnit+ separator=>DOT proper=>1
timePeriodByUnit ::= timePeriodDays
timePeriodByUnit ::= timePeriodHours
timePeriodByUnit ::= timePeriodMinutes
timePeriodByUnit ::= timePeriodSeconds
timePeriodDays ::= LAPSE_DAYS
LAPSE_DAYS ~ 'd' dim4j
timePeriodHours ::= LAPSE_HOURS
LAPSE_HOURS ~ 'h' dim4j
timePeriodMinutes ::= LAPSE_MINUTES
LAPSE_MINUTES ~ 'm' dim4j
timePeriodSeconds ::= LAPSE_SECONDS
LAPSE_SECONDS ~ 's' dim4j
timePeriodFraction ::= (DOT DOT) QIX4J_SEQ

# nuck(4l) is the coin parser
# TODO: Finish nuck4l
nuck4l ::= SYM4K

# tash(4l) is the signed dime parser
nuck4l ::= TODO_tash4l
TODO_tash4l ::= UNICORN

# perd(4l) parses dimes or tuples without their standard prefixes
nuck4l ::= DOT4H perd4l

# Can be either '$~' or '%~'
# -- both seem to have the same semantics
nuck4l ::= moldNullSig
moldNullSig ::= '~'

# perd(4l) parses sig-prefixed coins after the sig prefix
nuck4l ::= SIG twid4l

# TODO: Finish perd4l
perd4l ::= zust4l

# TODO: Finish zust4l
zust4l ::= 'y'
zust4l ::= 'n'

twid4l ::= '0' VUM4J
twid4l ::= crub4l

nuck4l ::= bisk4l
# bisk(4l) parses unsigned dimes of any base
bisk4l ::= NUMBER

#     :~  :-  ['a' 'z']  (cook |=(a/@ta [%$ %tas a]) sym)
#         :-  ['0' '9']  (stag %$ bisk)
#         :-  '-'        (stag %$ tash)
#         :-  '.'        ;~(pfix dot perd)
#         :-  '~'        ;~(pfix sig ;~(pose twid (easy [%$ %n 0])))

# === Hoon 5d library ===

# 5d library: bonk

bonk5d ::= CEN4H SYM4K COL4H SYM4K DOT4H DOT4H dem4k
bonk5d ::= CEN4H SYM4K COL4H SYM4K DOT4H dem4k
bonk5d ::= CEN4H SYM4K DOT4H dem4k
bonk5d ::= CEN4H SYM4K

# 5d library: bont

bont5d ::= CEN4H SYM4K ([.] GAP) hoon
bont5d ::= wideBont5d
wideBont5d ::= CEN4H SYM4K ([.]) wideHoon
wideBont5d ::= CEN4H SYM4K ([.] ACE) wideHoon

# 5d library: bony

# one or more equal signs
bony5d ::= TIS+

# 5d library: bonz

bonz5d ::= (TIS TIS GAP) optBonzElements (GAP TIS TIS)
bonz5d ::= wideBonz5d
wideBonz5d ::= SIG
wideBonz5d ::= (PEL) optWideBonzElements (PER)
optBonzElements ::= bonzElement* separator=>GAP
bonzElement ::= CEN SYM4K (GAP) hoon
optWideBonzElements ::= wideBonzElement* separator=>ACE
wideBonzElement ::= CEN SYM4K (ACE) hoon

mold ::= wideMold
moldSeq ::= mold+ separator=>GAP proper=>1
wideMoldSeq ::= wideMold+ separator=>ACE proper=>1

# 5d library: boog
# Always tall

# TODO: Needs elaboration from hoon.hoon
# TODO: Need to add apse:docs
# TODO: What is the meaning of these various types of battery element?
boog5d ::= lusLusCell
boog5d ::= lusHepCell
boog5d ::= lusTisCell
lusLusCell ::= ('++' GAP) BUC (GAP) hoon
lusLusCell ::= ('++' GAP) SYM4K (GAP) hoon
lusHepCell ::= ('+-' GAP) SYM4K (GAP) hoon
lusHepCell ::= ('+-' GAP) BUC (GAP) hoon
lusTisCell ::= ('+=' GAP) SYM4K (GAP) mold

# 5d library: gash

gash5d ::= limp5d* separator=>[/]
limp5d ::= (optFasSeq) gasp5d
optFasSeq ::= # empty
optFasSeq ::= FAS_SEQ
FAS_SEQ ~ fas4h+
gasp5d ::= tisSeq
tisSeq ~ tis4h+
optTisSeq ::= # empty
optTisSeq ::= TIS_SEQ
TIS_SEQ ~ tis4h+

# 5d library: gasp

gasp5d ::= (optTisSeq) hasp5d (optTisSeq)

# 5d library: hasp

hasp5d ::= (SEL) wideHoon (SER)
hasp5d ::= (PEL) wideHoonSeq (PER)
hasp5d ::= BUC4H
hasp5d ::= qut4k
hasp5d ::= nuck4l

# 5d library: long

long5dWide ::= infixTis rank=>60
long5dWide ::= infixCol rank=>50
long5dWide ::= infixKet rank=>40
long5dWide ::= infixFas rank=>30
long5dWide ::= circumScatParen rank=>20
long5dWide ::= scat5d rank=>10

infixTis ::= scat5d (TIS) wideHoon
infixCol ::= scat5d (COL) wideHoon
infixKet ::= scat5d (KET) wideHoon
infixFas ::= scat5d (FAS) wideHoon
circumScatParen ::= scat5d (PEL) lobo5d (PER)

lobo5d ::= wideHoonJogs
wideHoonJogs ::= wideHoonJog+ separator=>wideHoonJoggingSeparator proper=>1
wideHoonJog ::= rope5d (ACE) wideHoon
wideHoonJoggingSeparator ::= COM ACE

# 5d library: mota

# Lexemes cannot be empty so empty
# aura name must be special cased.
mota5d ::= # empty
mota5d ::= AURA_NAME
AURA_NAME ~ optLow4kSeq optHig4kSeq
optLow4kSeq ~ low4i*
optHig4kSeq ~ hig4k*

# 5d library: norm

# Mold runes
 
# ['_' (rune cab %bccb expa)]

# [':' (rune col %bccl exqs)]
# ++  exqs  |.((butt hunk))                           ::  closed gapped roots
# Running syntax
mold ::= tallBuccolMold
wideMold ::= wideBuccolMold
tallBuccolMold ::= (BUC COL GAP) moldSeq (GAP TIS TIS)
wideBuccolMold ::= (BUC COL '(') wideMoldSeq (')')

# ['%' (rune cen %bccn exqs)]
mold ::= tallBuccenMold
wideMold ::= wideBuccenMold
tallBuccenMold ::= (BUC CEN GAP) moldSeq (GAP TIS TIS)
wideBuccenMold ::= (BUC CEN '(') wideMoldSeq (')')

# ['-' (rune hep %bchp exqb)]
mold ::= tallBuchepMold
wideMold ::= wideBuchepMold
tallBuchepMold ::= (BUC HEP GAP) mold (GAP)  mold
wideBuchepMold ::= (BUC HEP) [(] wideMold (ACE) wideMold [)]

# ['^' (rune ket %bckt exqb)]
# ++  exqb  |.(;~(gunk loan loan))                    ::  two roots
mold ::= tallBucketMold
wideMold ::= wideBucketMold
tallBucketMold ::= (BUC KET GAP) mold (GAP)  mold
wideBucketMold ::= (BUC KET PEL) wideMold (ACE) wideMold (PER)

# ['@' (rune pat %bcpt exqb)]
# ++  exqb  |.(;~(gunk loan loan))                    ::  two roots
mold ::= tallBucpatMold
wideMold ::= wideBucpatMold
tallBucpatMold ::= (BUC PAT GAP) mold (GAP)  mold
wideBucpatMold ::= (BUC PAT PEL) wideMold (ACE) wideMold (PER)

# [';' (rune sem %bcsm expa)]

# ['=' (rune tis %bcts exqg)]

# ['?' (rune wut %bcwt exqs)]
mold ::= tallBucwutMold
wideMold ::= wideBucwutMold
tallBucwutMold ::= (BUC WUT GAP) moldSeq (GAP TIS TIS)
wideBucwutMold ::= (BUC WUT '(') wideMoldSeq (')')

# TODO: Finish adding molds from norm

# 5d library: norm

# Hoon runes

# ['_' (runo cab %brcb [~ ~] exqr)]
# ++  exqr  |.(;~(gunk loan ;~(plug wasp wisp)))      ::  root/aliases?/tail
# wisp must be tall, therefore wasp and BARCAB must be tall
hoon ::= tallBarcab
tallBarcab ::= (BAR CAB GAP) mold (GAP) wasp5d wisp5d

# ['%' (runo cen %brcn [~ ~] expe)]
# ++  expe  |.(wisp)                                  ::  core tail
hoon ::= tallBarcen
tallBarcen ::= (BAR CEN GAP) wisp5d

# [':' (runo col %brcl [~ ~] expb)]
# ++  expb  |.(;~(gunk loaf loaf))                    ::  two hoons
# FIXED: barcol hoon hoon

# ['.' (runo dot %brdt [~ ~] expa)]
# ++  expa  |.(loaf)                                  ::  one hoon
# FIXED: bardot hoon

# ['-' (runo hep %brhp [~ ~] expa)]
# ++  expa  |.(loaf)                                  ::  one hoon
# FIXED: barhep hoon

# ['^' (runo ket %brkt [~ ~] expx)]
# ++  expx  |.  ;~  gunk  loaf [...] wisp ::  hoon and core tail
hoon ::= tallBarket
tallBarket ::= (BAR KET GAP) hoon (GAP) wisp5d

# ['~' (runo sig %brsg [~ ~] exqc)]
#  ++  exqc  |.(;~(gunk loan loaf))                    ::  root then hoon
# FIXED: barsig mold hoon

# ['*' (runo tar %brtr [~ ~] exqc)]
#  ++  exqc  |.(;~(gunk loan loaf))                    ::  root then hoon
# FIXED: bartar mold hoon

# ['=' (runo tis %brts [~ ~] exqc)]
# ++  exqc  |.(;~(gunk loan loaf))                    ::  root then hoon
hoon ::= tallBartis
norm5dWide ::= wideBartis
tallBartis ::= (BAR4H TIS GAP) mold (GAP) hoon
wideBartis ::= (BAR4H TIS) [(] wideMold (ACE) wideHoon [)]

# ['?' (runo wut %brwt [~ ~] expa)]
# ++  expa  |.(loaf)                                  ::  one hoon
# FIXED: barwut hoon

# ['_' (rune cab %bccb expa)]
# ++  expa  |.(loaf)                                  ::  one hoon
# FIXED: buccab hoon

# [':' (rune col %bccl exqs)]
# ++  exqs  |.((butt hunk))                           ::  closed gapped roots
hoon ::= tallBuccol
tallBuccol ::= (BUC COL GAP) moldSeq (GAP TIS TIS)
norm5dWide ::= wideBuccol
wideBuccol ::= (BUC COL '(') wideMoldSeq (')')

# ['%' (rune cen %bccn exqs)]
# ++  exqs  |.((butt hunk))                           ::  closed gapped roots
# Running syntax
hoon ::= tallBuccen
tallBuccen ::= (BUC CEN GAP) moldSeq (GAP TIS TIS)
norm5dWide ::= wideBuccen
wideBuccen ::= (BUC CEN '(') wideMoldSeq (')')

# ['-' (rune hep %bchp exqb)]
# ++  exqb  |.(;~(gunk loan loan))                    ::  two roots
# FIXED: buchep mold mold

# ['^' (rune ket %bckt exqb)]
# ++  exqb  |.(;~(gunk loan loan))                    ::  two roots
# FIXED: bucket mold mold

# ['@' (rune pat %bcpt exqb)]
# ++  exqb  |.(;~(gunk loan loan))                    ::  two roots
# FIXED: bucpat mold mold

# [';' (rune sem %bcsm exqa)]
# ++  exqa  |.(loan)                                  ::  one hoon
# Typo in hoon.hoon -- actually "loan" is a mold
# FIXED: bucsem mold

# ['=' (rune tis %bcts exqg)]
# ++  exqg  |.(;~(gunk sym loan))                     ::  term and root
# FIXED: buctis SYM4K mold

# ['?' (rune wut %bcwt exqs)]
# ++  exqs  |.((butt hunk))                           ::  closed gapped roots
hoon ::= tallBucwut
tallBucwut ::= (BUC WUT GAP) moldSeq (GAP TIS TIS)
norm5dWide ::= wideBucwut
wideBucwut ::= (BUC WUT '(') wideMoldSeq (')')

# ['_' (rune cab %cncb exph)]
# ++  exph  |.((butt ;~(gunk rope rick)))             ::  wing, [tile hoon]s
hoon ::= tallCencab
tallCencab ::= (CEN CAB GAP) rope5d (GAP) rick5d (GAP TIS TIS)
norm5dWide ::= wideCencab
wideCencab ::= (CEN CAB PEL) rope5d (ACE) wideRick5d (PAR)

# ['.' (rune dot %cndt expb)]
# ++  expb  |.(;~(gunk loaf loaf))                    ::  two hoons
# FIXED: cendot hoon hoon

# ['-' (rune hep %cnhp expk)]
# ++  expk  |.(;~(gunk loaf ;~(plug loaf (easy ~))))  ::  list of two hoons
# FIXED: cenhep hoon hoon

# ['^' (rune ket %cnkt expd)]
# ++  expd  |.(;~(gunk loaf loaf loaf loaf))          ::  four hoons
# FIXED: cenket hoon hoon hoon hoon

# ['+' (rune lus %cnls expc)]
# ++  expc  |.(;~(gunk loaf loaf loaf))               ::  three hoons
# FIXED: cenlus hoon hoon hoon

# ['~' (rune sig %cnsg expn)]
# ++  expn  |.  ;~  gunk  rope  loaf                  ::  wing, hoon,
# 		;~(plug loaf (easy ~))              ::  list of one hoon
# 	      ==
# FIXED: censig rope5d hoon hoon

# ['*' (rune tar %cntr expm)]
#  ++  expm  |.((butt ;~(gunk rope loaf rick)))        ::  several [tile hoon]s
hoon ::= tallCentar
tallCentar ::= (CEN TAR GAP) rope5d (GAP) hoon (GAP) rick5d (GAP TIS TIS)
norm5dWide ::= wideCentar
wideCentar ::= (CEN TAR PEL) rope5d (ACE) wideHoon (ACE) wideRick5d (PAR)

# ['=' (rune tis %cnts exph)]
# ++  exph  |.((butt ;~(gunk rope rick)))             ::  wing, [tile hoon]s
hoon ::= tallCentis
tallCentis ::= (CEN TIS GAP) rope5d (GAP) rick5d (GAP TIS TIS)
norm5dWide ::= wideCentis
wideCentis ::= (CEN TIS PEL) rope5d (ACE) wideRick5d (PAR)

# ['_' (rune cab %clcb expb)]
# ++  expb  |.(;~(gunk loaf loaf))                    ::  two hoons
# FIXED: colcab hoon hoon

# ['-' (rune hep %clhp expb)]
# ++  expb  |.(;~(gunk loaf loaf))                    ::  two hoons
# FIXED: colhep hoon hoon

# ['+' (rune lus %clls expc)]
# ++  expc  |.(;~(gunk loaf loaf loaf))               ::  three hoons
# FIXED: collus hoon hoon hoon

# ['^' (rune ket %clkt expd)]
# ++  expd  |.(;~(gunk loaf loaf loaf loaf))          ::  four hoons
# FIXED: colket hoon hoon hoon hoon

# ['~' (rune sig %clsg exps)]
#  ++  exps  |.((butt hank))                           ::  closed gapped hoons
hoon ::= tallColsig
tallColsig ::= (COL SIG GAP) hoonSeq (GAP TIS TIS)
norm5dWide ::= wideColsig
wideColsig ::= (COL SIG '(') wideHoonSeq (')')

# ['*' (rune tar %cltr exps)]
#  ++  exps  |.((butt hank))                           ::  closed gapped hoons
hoon ::= tallColtar
tallColtar ::= (COL TAR GAP) hoonSeq (GAP TIS TIS)
norm5dWide ::= wideColtar
wideColtar ::= (COL TAR PEL) wideHoonSeq (PER)

# ['^' (rune ket %dtkt exqn)]
# ++  exqn  |.(;~(gunk loan (stag %cltr (butt hank))))::  autoconsed hoons
# I do not understand hoon.hoon comment ("autoconsed hoons"), but
# follow the code
hoon ::= tallDotket
tallDotket ::= (DOT KET GAP) mold (GAP) hoonSeq (GAP TIS TIS)
norm5dWide ::= wideDotket
wideDotket ::= (DOT KET PEL) wideMold (ACE) wideHoonSeq (PER)

# ['+' (rune lus %dtls expa)]
# :~  ['+' (rune lus %dtls expa)]
# ++  expa  |.(loaf)                                  ::  one hoon
# FIXED: dotlus hoon

# ['*' (rune tar %dttr expb)]
# ++  expb  |.(;~(gunk loaf loaf))                    ::  two hoons
# FIXED: dottar hoon hoon

# ['=' (rune tis %dtts expb)]
# ++  expb  |.(;~(gunk loaf loaf))                    ::  two hoons
# FIXED: dottis hoon hoon

# ['?' (rune wut %dtwt expa)]
# ++  expa  |.(loaf)                                  ::  one hoon
# FIXED: dotwut hoon

# FAS group are (usually?) ford runes:
# They are not in hoon.hoon, and so were not part of the
# corrections to match hoon.hoon.

# FIXED: fassem hoon hoon

# TODO: Where to classify this?
FASTIS ~ [\/] [=]
hoon ::= tallFastis
tallFastis ::= (FASTIS GAP) SYM4K (GAP) hoon
norm5dWide ::= wideFastis
wideFastis ::= (FASTIS) SYM4K '=' hoon

# ['|' (rune bar %ktbr expa)]
# ++  expa  |.(loaf)                                  ::  one hoon
# FIXED: ketbar hoon

# ['%' (rune cen %ktcn expa)]
# ++  expa  |.(loaf)                                  ::  one hoon
# FIXED: ketcen hoon

# ['.' (rune dot %ktdt expb)]
# ++  expb  |.(;~(gunk loaf loaf))                    ::  two hoons
# FIXED: ketdot hoon hoon

# ['-' (rune hep %kthp exqc)]
# ++  exqc  |.(;~(gunk loan loaf))                    ::  root then hoon
# FIXED: kethep mold hoon

# ['+' (rune lus %ktls expb)]
# ++  expb  |.(;~(gunk loaf loaf))                    ::  two hoons
# FIXED: ketlus hoon hoon

# ['&' (rune pam %ktpm expa)]
# ++  expa  |.(loaf)                                  ::  one hoon
# FIXED: ketpam hoon

# ['~' (rune sig %ktsg expa)]
# ++  expa  |.(loaf)                                  ::  one hoon
# FIXED: ketsig hoon

# ['=' (rune tis %ktts expg)]
# ++  expg  |.(;~(gunk sym loaf))                     ::  term and hoon
# FIXED: kettis SYM4K hoon

# ['?' (rune wut %ktwt expa)]
# ++  expa  |.(loaf)                                  ::  one hoon
# FIXED: ketwut hoon

# [':' (rune col %smcl expi)]
# ++  expi  |.((butt ;~(gunk loaf hank)))             ::  one or more hoons
hoon ::= tallSemcol
tallSemcol ::= (SEM COL GAP) hoon (GAP) hoonSeq (GAP TIS TIS)
norm5dWide ::= wideSemcol
wideSemcol ::= (SEM COL '(') hoon (ACE) wideHoonSeq (')')

# ['/' (rune fas %smfs expa)]
# ++  expa  |.(loaf)                                  ::  one hoon
# FIXED: semfas hoon

# [';' (rune sem %smsm expb)]
# ++  expb  |.(;~(gunk loaf loaf))                    ::  two hoons
# FIXED: semsem hoon hoon

# ['~' (rune sig %smsg expi)]
# ++  expi  |.((butt ;~(gunk loaf hank)))             ::  one or more hoons
hoon ::= tallSemsig
tallSemsig ::= (SEM SIG GAP) hoon (GAP) hoonSeq (GAP TIS TIS)
norm5dWide ::= wideSemsig
wideSemsig ::= (SEM SIG '(') hoon (ACE) wideHoonSeq (')')

# ['|' (rune bar %sgbr expb)]
# FIXED: sigbar hoon hoon

# ['$' (rune buc %sgbc expf)]
# ++  expf  |.(;~(gunk ;~(pfix cen sym) loaf))        ::  %term and hoon
# FIXED: sigbuc CEN_SYM4K hoon

# ['_' (rune cab %sgcb expb)]
# ++  expb  |.(;~(gunk loaf loaf))                    ::  two hoons
# FIXED: sigcab hoon hoon

# ['%' (rune cen %sgcn hind)]
# ++  hind  |.(;~(gunk bonk loaf bonz loaf))          ::  jet hoon "bon"s hoon
# FIXED: sigcen bonk5d rope5d bonz5d hoon

# ['/' (rune fas %sgfs hine)]
# ++  hine  |.(;~(gunk bonk loaf))                    ::  jet-hint and hoon
# FIXED: sigfas bonk5d hoon

# ['<' (rune gal %sggl hinb)]
#  ++  hinb  |.(;~(gunk bont loaf))                    ::  hint and hoon
# FIXED: siggal bont5d hoon

# ['>' (rune gar %sggr hinb)]
#  ++  hinb  |.(;~(gunk bont loaf))                    ::  hint and hoon
# FIXED: siggar bont5d hoon

# ['+' (rune lus %sgls hinc)]
# ++  hinc  |.                                        ::  optional =en, hoon
#           ;~(pose ;~(gunk bony loaf) (stag ~ loaf)) ::
hoon ::= tallSiglus
tallSiglus ::= (SIG LUS GAP) bony5d (GAP) hoon
tallSiglus ::= (SIG LUS GAP) hoon
norm5dWide ::= wideSiglus
wideSiglus ::= (SIG LUS '(') bony5d (ACE) hoon (')')
wideSiglus ::= (SIG LUS '(') hoon (')')

# ['&' (rune pam %sgpm hinf)]
# ++  hinf  |.                                        ::  0-3 >s, two hoons
#  ;~  pose
#    ;~(gunk (cook lent (stun [1 3] gar)) loaf loaf)
#    (stag 0 ;~(gunk loaf loaf))
#  ==
hoon ::= tallSigpam
tallSigpam ::= (SIG PAM GAP) oneToThreeGars (GAP) hoon (GAP) hoon
tallSigpam ::= (SIG PAM GAP) hoon (GAP) hoon
norm5dWide ::= wideSigpam
wideSigpam ::= (SIG PAM PEL) oneToThreeGars (ACE) hoon (ACE) hoon (PER)
wideSigpam ::= (SIG PAM PEL) hoon (ACE) hoon (PER)
oneToThreeGars ::= GAR | GAR GAR | GAR GAR GAR

# ['=' (rune tis %sgts expb)]
# ++  expb  |.(;~(gunk loaf loaf))                    ::  two hoons
# FIXED: sigtis hoon hoon

# ['?' (rune wut %sgwt hing)]
# ++  hing  |.                                        ::  0-3 >s, three hoons
#  ;~  pose
#    ;~(gunk (cook lent (stun [1 3] gar)) loaf loaf loaf)
#    (stag 0 ;~(gunk loaf loaf loaf))
#  ==
hoon ::= tallSigwut
tallSigwut ::= (SIG WUT GAP) oneToThreeGars (GAP) hoon (GAP) hoon (GAP) hoon
tallSigwut ::= (SIG WUT GAP) hoon (GAP) hoon (GAP) hoon
norm5dWide ::= wideSigwut
wideSigwut ::= (SIG WUT PEL) oneToThreeGars (ACE) hoon (ACE) hoon (ACE) hoon (PER)
wideSigwut ::= (SIG WUT PEL) hoon (ACE) hoon (ACE) hoon (PER)

# ['!' (rune zap %sgzp expb)]
# ++  expb  |.(;~(gunk loaf loaf))                    ::  two hoons
# FIXED: sigzap hoon hoon

# ['|' (rune bar %tsbr exqc)]
# ++  exqc  |.(;~(gunk loan loaf))                    ::  root then hoon
# FIXED: tisbar mold hoon

# [':' (rune col %tscl expp)]
# ++  expp  |.(;~(gunk (butt rick) loaf))             ::  [wing hoon]s, hoon
hoon ::= tallTiscol
tallTiscol ::= (TIS COL GAP) rick5d (GAP TIS TIS GAP) hoon
norm5dWide ::= wideTiscol
wideTiscol ::= (TIS COL PEL) wideRick5d (ACE) wideHoon (PAR)

# [',' (rune com %tscm expb)]
# ++  expb  |.(;~(gunk loaf loaf))                    ::  two hoons
# FIXED: tiscom hoon hoon

# ['.' (rune dot %tsdt expq)]
# ++  expq  |.(;~(gunk rope loaf loaf))               ::  wing and two hoons
# FIXED: tisdot rope5d hoon hoon

# ['-' (rune hep %tshp expb)]
# ++  expb  |.(;~(gunk loaf loaf))                    ::  two hoons
# FIXED: tishep hoon hoon

# ['/' (rune fas %tsfs expo)]
# ++  expo  |.(;~(gunk wise loaf loaf))               ::  =;
# FIXED: tisfas wise5d hoon hoon

# ['<' (rune gal %tsgl expb)]
# ++  expb  |.(;~(gunk loaf loaf))                    ::  two hoons
# FIXED: tisgal hoon hoon

# ['>' (rune gar %tsgr expb)]
# ++  expb  |.(;~(gunk loaf loaf))                    ::  two hoons
# FIXED: tisgar hoon hoon

# ['^' (rune ket %tskt expt)]
#     ++  expt  |.(;~(gunk wise rope loaf loaf))          ::  =^
# FIXED: tisket wise5d rope5d hoon hoon

# ['+' (rune lus %tsls expb)]
# ++  expb  |.(;~(gunk loaf loaf))                    ::  two hoons
# FIXED: tislus hoon hoon

# [';' (rune sem %tssm expo)]
# ++  expo  |.(;~(gunk wise loaf loaf))               ::  =;
# FIXED: tissem wise5d hoon hoon

# ['~' (rune sig %tssg expi)]
# ++  expi  |.((butt ;~(gunk loaf hank)))             ::  one or more hoons
hoon ::= tallTissig
tallTissig ::= (TIS SIG GAP) hoon (GAP) hoonSeq (GAP TIS TIS)
norm5dWide ::= wideTissig
wideTissig ::= (TIS SIG '(') hoon (ACE) wideHoonSeq (')')

# ['*' (rune tar %tstr expl)]
# ++  expl  |.(;~(gunk (stag ~ sym) loaf loaf))       ::  term, two hoons
# FIXED: tistar SYM4K hoon hoon

# ['?' (rune wut %tswt expw)]
#     ++  expw  |.(;~(gunk rope loaf loaf loaf))          ::  wing and three hoons
# FIXED: tiswut rope5d hoon hoon hoon

# ['|' (rune bar %wtbr exps)]
#  ++  exps  |.((butt hank))                           ::  closed gapped hoons
hoon ::= tallWutbar
tallWutbar ::= (WUT BAR GAP) hoonSeq (GAP TIS TIS)
norm5dWide ::= wideWutbar
wideWutbar ::= (WUT BAR '(') wideHoonSeq (')')

# [':' (rune col %wtcl expc)]
# ++  expc  |.(;~(gunk loaf loaf loaf))               ::  three hoons
# FIXED: wutcol hoon hoon hoon

# ['.' (rune dot %wtdt expc)]
# ++  expc  |.(;~(gunk loaf loaf loaf))               ::  three hoons
# FIXED: wutdot hoon hoon hoon

# ['<' (rune gal %wtgl expb)]
# ++  expb  |.(;~(gunk loaf loaf))                    ::  two hoons
# FIXED: wutgal hoon hoon

# ['>' (rune gar %wtgr expb)]
# ++  expb  |.(;~(gunk loaf loaf))                    ::  two hoons
# FIXED: wutgar hoon hoon

# ['-' ;~(pfix hep (toad tkhp))]
# ++  tkhp  |.  %+  cook  |=  {a/tiki b/(list (pair root hoon))}
#			(~(wthp ah a) b)
#	      (butt ;~(gunk teak ruck))
hoon ::= tallWuthep
tallWuthep ::= (WUT HEP GAP) teak5d (GAP) ruck5d (GAP TIS TIS)
norm5dWide ::= wideWuthep
wideWuthep ::= (WUT HEP PER) teak5d (ACE) wideRuck5d (PAR)

# ['^' ;~(pfix ket (toad tkkt))]
# ++  tkkt  |.  %+  cook  |=  {a/tiki b/hoon c/hoon}
#			(~(wtkt ah a) b c)
#	      ;~(gunk teak loaf loaf)
# FIXED: wutket teak5d hoon hoon

# ['+' ;~(pfix lus (toad tkls))]
# ++  tkls  |.  %+  cook  |=  {a/tiki b/hoon c/(list (pair root hoon))}
# 			(~(wtls ah a) b c)
#	      (butt ;~(gunk teak loaf ruck))
hoon ::= tallWutlus
tallWutlus ::= (WUT LUS GAP) teak5d (GAP) hoon (GAP) ruck5d (GAP TIS TIS)
norm5dWide ::= wideWutlus
wideWutlus ::= (WUT LUS PEL) teak5d (ACE) hoon (ACE) wideRuck5d (PAR)

# ['&' (rune pam %wtpm exps)]
#  ++  exps  |.((butt hank))                           ::  closed gapped hoons
hoon ::= tallWutpam
tallWutpam ::= (WUT PAM GAP) hoonSeq (GAP TIS TIS)
norm5dWide ::= wideWutpam
wideWutpam ::= (WUT PAM '(') wideHoonSeq (')')

# ['@' ;~(pfix pat (toad tkpt))]
# ++  tkpt  |.  %+  cook  |=  {a/tiki b/hoon c/hoon}
#			(~(wtpt ah a) b c)
#	      ;~(gunk teak loaf loaf)
# FIXED: wutpat teak5d hoon hoon

# ['~' ;~(pfix sig (toad tksg))]
# ++  tksg  |.  %+  cook  |=  {a/tiki b/hoon c/hoon}
# 			  (~(wtsg ah a) b c)
# 		;~(gunk teak loaf loaf)
# FIXED: wutsig teak5d hoon hoon

# ['=' ;~(pfix tis (toad tkts))]
# ++  tksg  |.  %+  cook  |=  {a/tiki b/hoon c/hoon}
# 			(~(wtsg ah a) b c)
# 	      ;~(gunk teak loaf loaf)
# FIXED: wuttis teak5d hoon hoon

# ['!' (rune zap %wtzp expa)]
# ++  expa  |.(loaf)                                  ::  one hoon
# FIXED: wutzap hoon
 
# [':' ;~(pfix col (toad expz))]
#    ++  expz  |.(loaf(bug &))                           ::  hoon with tracing
# FIXED: zapcol hoon

# ['.' ;~(pfix dot (toad |.(loaf(bug |))))]
# FIXED: zapdot hoon

# [',' (rune com %zpcm expb)]
# ++  expb  |.(;~(gunk loaf loaf))                    ::  two hoons
# FIXED: zapcom hoon hoon

# ['>' (rune gar %zpgr expa)]
# ++  expa  |.(loaf)                                  ::  one hoon
# FIXED: zapgar hoon

# [';' (rune sem %zpsm expb)]
# ++  expb  |.(;~(gunk loaf loaf))                    ::  two hoons
# FIXED zapsem hoon hoon

# ['=' (rune tis %zpts expa)]
# ++  expa  |.(loaf)                                  ::  one hoon
# FIXED: zaptis hoon

# ['?' (rune wut %zpwt hinh)]
# ++  hinh  |.                                        ::  1/2 numbers, hoon
#         ;~  gunk
#           ;~  pose
#             dem
#             (ifix [sel ser] ;~(plug dem ;~(pfix ace dem)))
#           ==
#           loaf
#         ==
hoon ::= tallZapWut
norm5dWide ::= wideZapWut
tallZapWut ::= (ZAP WUT GAP) dem4k (GAP) hoon
tallZapWut ::= (ZAP WUT GAP SEL) dem4k (ACE) dem4k (SER GAP) hoon
wideZapWut ::= (ZAP WUT ACE) dem4k (ACE) hoon
wideZapWut ::= (ZAP WUT ACE SEL) dem4k (ACE) dem4k (SER ACE) hoon

# zapzap (= crash) is implemented in scat5d

# 5d library: poor

poor5d ::= gash5d
poor5d ::= gash5d CEN4H porc5d

# 5d library: porc

porc5d ::= optCen4hSeq FAS gash5d
optCen4hSeq ::= # empty
optCen4hSeq ::= CEN4H_SEQ
CEN4H_SEQ ~ cen4h+

# 5d library: rood
# rood is the path parser

rood5d ::= [/] poor5d

# 5d library: rope

# the wing type is parsed by the rope(5d)
rope5d ::= limb+ separator=>[.] proper=>1
limb ::= ','
limb ::= optKets '$'
limb ::= optKets SYM4K
optKets ::= KET*
limb ::= BAR4H DIM4J
limb ::= LUS DIM4J
limb ::= PAM4H DIM4J
limb ::= VEN4K
limb ::= '.'

# 5d library: rick

rick5d ::= rick5dJog+ separator=>GAP proper=>1
rick5dJog ::= rope5d (GAP) hoon

wideRick5d ::= wideRick5dJog+ separator=>ACE proper=>1
wideRick5dJog ::= rope5d (ACE) hoon

# 5d library: ruck

ruck5d ::= ruck5dJog+ separator=>GAP proper=>1
ruck5dJog ::= mold (GAP) hoon

wideRuck5d ::= wideRuck5dJog+ separator=>commaAce proper=>1
wideRuck5dJog ::= mold (ACE) hoon
commaAce ::= COM ACE

# 5d library: rump

rump5d ::= rope5d
rump5d ::= rope5d wede5d

# 5d library: rupl
# rupl(5d) seems to implement the hoon '[...]', ~[...], and [...]~
# syntaxes.

# TODO: Finish rupl(5d)
rupl5d ::= circumBracket
rupl5d ::= sigCircumBracket
rupl5d ::= circumBracketSig
rupl5d ::= sigCircumBracketSig
# TODO: is the whitespace really what was intended?
circumBracket ::= ('[' ACE) hoonSeq (GAP ']')
circumBracket ::= ('[') wideHoonSeq (']')
sigCircumBracket ::= (SIG '[' ACE) hoonSeq (GAP ']')
sigCircumBracket ::= (SIG '[') wideHoonSeq (']')
circumBracketSig ::= ('[' ACE) hoonSeq (GAP ']' SIG)
circumBracketSig ::= ('[') wideHoonSeq (']' SIG)
sigCircumBracketSig ::= (SIG '[' ACE) hoonSeq (GAP ']' SIG)
sigCircumBracketSig ::= (SIG '[') wideHoonSeq (']' SIG)

# 5d library: scad

# scad(5d) implements the irregular mold syntaxes

# Cases given in hoon.hoon order.  Unfortunately
# not the same as the order in scat(5d).

# '_'
# Same as scat(5d)
wideMold ::= moldPrefixCab
moldPrefixCab ::= ('_') wideHoon

# ','
# Differs from scat(5d)
wideMold ::= moldPrefixCom
moldPrefixCom ::= (',') wideHoon

# '$'
# Differs from scat(5d)
wideMold ::= moldBucbuc
moldBucbuc ::= '$$'

wideMold ::= moldBucpam
moldBucpam ::= '$&'

wideMold ::= moldBucbar
moldBucbar ::= '$|'

wideMold ::= moldBucSingleString
moldBucSingleString ::= '$' qut4k

wideMold ::= moldBucNuck4l
moldBucNuck4l ::= '$' nuck4l

wideMold ::= rump5d

# '%'
# Differs from scat(5d)
wideMold ::= moldCenbuc
moldCenbuc ::= '%$'

wideMold ::= moldCenpam
moldCenpam ::= '%&'

wideMold ::= moldCenbar
moldCenbar ::= '%|'

wideMold ::= moldCenSingleString
moldCenSingleString ::= ('%') qut4k

wideMold ::= moldCenNuck4l
moldCenNuck4l ::= '%' nuck4l

# '('
# Differs from scat(5d)
wideMold ::= moldCircumParen
moldCircumParen ::= ('(') wideHoon (ACE) wideMoldSeq (')')
moldCircumParen ::= ('(') wideHoon (')')

# '{'
# Same as scat(5d)
wideMold ::= moldCircumBrace
moldCircumBrace ::= ('{') wideMoldSeq ('}')

# '['
# Differs from scat(5d)
wideMold ::= moldCircumBracket
moldCircumBracket ::= ('[') wideMoldSeq (']')

# '*'
# Subset of scat(5d)
wideMold ::= moldTar
moldTar ::= '*'

# '@'
# Same as scat(5d)
wideMold ::= moldAura
moldAura ::= '@' mota5d

# '?'
# Same as scat(5d)
wideMold ::= moldPrefixWut
moldPrefixWut ::= ('?(') wideMoldSeq (')')

wideMold ::= moldWut
moldWut ::= '?'

# '~'
# Differs from scat(5d)
wideMold ::= moldSig
moldSig ::= '~'

# '^'
# Differs from scat(5d)
wideMold ::= moldKet
moldKet ~ '^'

# <moldInfixCol> can start with either KET (^) or lowercase char
# This is scab(5d)
# TODO: Delete after development
wideMold ::= moldInfixCol
moldInfixCol ::= rope5d [:] moldInfixCol2
moldInfixCol2 ::= rope5d+ separator=>[:] proper=>1

# '='
# Differs from scat(5d)
wideMold ::= moldPrefixTis
# TODO: Is this right?
moldPrefixTis ::= ('=') wideMold (')')

# ['a' 'z']
# Differs from scat(5d)
# for scab(5d), see the KET subcase
wideMold ::= moldInfixFas rank=>1
wideMold ::= moldInfixTis rank=>1
moldInfixFas ::= SYM4K FAS wideMold
moldInfixTis ::= SYM4K TIS wideMold

# End of scad(5d)

# 5d library: scat
# scat(5d) implements the irregular hoon syntaxes

# For convenience in referring back
# to hoon.hoon, I use scat(5d)'s order, as is.
# Unfortunately this is not in the same
# order as in scad.

# ','
# Differs from scad(5)
# TODO: Finish
wideBuccol ::= (',[') wideMoldSeq (']')

# TODO: <hoonUnary> was a Marpa hack (that it, not in hoon.hoon)
# to duplicate Hoon precedence.  It may no longer be necessary.
# This needs to be tested.

# '!'
# Not in scad(5)
# TODO: Finish
hoonUnary ::= prefixZap
prefixZap ::= (ZAP) wideHoon
scat5d ::= wideZapzap
wideZapzap ~ zap4h zap4h

# '_'
# Same as scad(5)
hoonUnary ::= prefixCab
prefixCab ::= (CAB) wideHoon

# '$'
# For rump, see subcase ['a' 'z']
# Differs from scad(5)
scat5d ::= bucBuc
scat5d ::= bucPam
scat5d ::= bucBar
scat5d ::= dollarTerm
bucBuc ::= BUC4H BUC4H
bucPam ::= BUC4H PAM4H
bucBar ::= BUC4H BAR4H
dollarTerm ::= BUC4H qut4k
dollarTerm ::= BUC4H nuck4l

# '%'
# Differs from scad(5)
scat5d ::= cenPath
scat5d ::= cenBuc
scat5d ::= cenPam
scat5d ::= cenBar
scat5d ::= cenTerm
scat5d ::= cenDirectories
cenPath ::= CEN4H porc5d
cenBuc ::= CEN4H BUC4H
cenPam ::= CEN4H PAM4H
cenBar ::= CEN4H BAR4H
cenTerm ::= CEN4H qut4k
cenTerm ::= CEN4H nuck4l
cenDirectories ::= CEN4H+

# '&'
# Not in scad(5)
# TODO: Finish
# For rope(5d), see subcase ['a' 'z'] and rump(5d)
scat5d ::= prefixPam
scat5d ::= pamPlusPrefix
scat5d ::= soloPam
prefixPam ::= (PAM4H '(') wideHoonSeq (')')
pamPlusPrefix ::= (PAM4H) wede5d
soloPam ::= PAM4H

# '\''
# Not in scad(5)
scat5d ::= singleQuoteString
singleQuoteString ::= qut4k

# '('
# Differs from scad(5)
# See https://raw.githubusercontent.com/urbit/old-urbit.org/master/doc/hoon/lan/irregular.markdown
# and cenhep in https://urbit.org/docs/hoon/irregular/
scat5d ::= circumParen1
scat5d ::= circumParen2
circumParen1 ::= ('(') wideHoon (')')
circumParen2 ::= ('(') wideHoon (ACE) wideHoonSeq (')')

# '{'
# Same as scad(5)
scat5d ::= circumBraces
circumBraces ::= ('{') wideMoldSeq ('}')

# '*'
# Superset of scad(5)
scat5d ::= prefixTar
scat5d ::= soloTar
prefixTar ::= TAR wideMold
soloTar ::= TAR

# '@'
# TODO: NYI
# Same as scad(5)
# '@'
# Same as scat(5d)
scat5d ::= aura
aura ::= '@' mota5d

# '+'
# Not in scad(5)
# TODO: Finish
scat5d ::= irrDotlus
irrDotlus ::= (LUS PEL) wideHoon (PER)

# '-'
# TODO: NYI
# Not in scad(5)

# '.'
# TODO: NYI
# Not in scad(5)

# ['0' '9']
# Not in scad(5)
# This subcase handles infix expressions
# starting with a digit.
scat5d ::= bisk4l
scat5d ::= bisk4l wede5d

# ':'
# Not in scad(5)
scat5d ::= circumColParen
scat5d ::= prefixColFas
circumColParen ::= (COL PEL) wideHoonSeq (PER)
prefixColFas ::= (COL FAS) wideHoon

# '='
# Differs from scad(5)
tallDottis ::= (TIS GAP) hoon
scat5d ::= irrDottis
irrDottis ::= ('=(') wideHoon (ACE) wideHoon (')')
irrDottis ::= ('=(') wideHoon (')')

# '?'
# Same as scad(5)
# TODO: Finish
scat5d ::= circumWutParen
scat5d ::= soloWut
circumWutParen ::= (WUT PEL) wideMoldSeq (PER)
soloWut ::= WUT

# '['
# Differs from scad(5)
scat5d ::= rupl5d

# '^'
# Differs from scad(5)
# For rope(5d) see ['a' 'z'] subcase and rump(5d)
scat5d ::= soloKet
soloKet ::= KET

# '`'
# Not in scad(5)
scat5d ::= prefixTecChoices
prefixTecChoices ::= prefixTecAura rank=>5
prefixTecChoices ::= prefixTecTar rank=>4
prefixTecChoices ::= prefixTecMold rank=>3
prefixTecChoices ::= prefixTecHoon rank=>2
prefixTecChoices ::= prefixSoloTec rank=>1
prefixTecAura ::= (TEC PAT) mota5d (TEC) wideHoon
prefixTecTar ::= (TEC TAR TEC) wideHoon
prefixTecMold ::= (TEC) wideMold (TEC) wideHoon
prefixTecHoon ::= (TEC LUS) wideHoon (TEC) wideHoon
prefixSoloTec ::= (TEC) wideHoon

# '"'
# Not in scad(5)
scat5d ::= infixDot
infixDot ::= soil5d+ separator=>DOG4I proper=>1

# ['a' 'z']
# Differs from scad(5)
scat5d ::= rump5d

# '|'
# Not in scad(5)
# TODO: Finish
scat5d ::= prefixBar rank=>1
scat5d ::= circumBarParen rank=>1
scat5d ::= soloBar
prefixBar ::= (BAR4H) wede5d
circumBarParen ::= (BAR4H PEL) wideHoonSeq (PER)
soloBar ::= BAR4H

# '~'
# Differs from scad(5)
# See also rupl(5d) in the '[' subcase
scat5d ::= circumSigParen
scat5d ::= circumSigBracket
scat5d ::= (SIG) twid4l
scat5d ::= (SIG) wede5d
scat5d ::= soloSig
circumSigBracket ::= (SIG SEL) wideHoonSeq (SER)
circumSigParen ::= (SIG PEL) rope5d (ACE) wideHoon (ACE) wideHoonSeq (PER)
soloSig ::= SIG

# '/'
# Not in scad(5)
scat5d ::= rood5d

# '<'
# Not in scad(5)
scat5d ::= circumGalgar
circumGalgar ::= ('<') wideHoonSeq ('>')

# '>'
# Not in scad(5)
scat5d ::= circumGargal
circumGargal ::= ('>') wideHoonSeq ('<')

# 5d library: soil

# TODO -- Finish soil(5d) -- add triple double strings

soil5d ::= doubleQuoteString

doubleQuoteString ::= (["]) <double quote cord> (["])
<double quote cord> ::= <double quote element>*
<double quote element> ::= <UNESCAPED DOUBLE QUOTE CHARS>
<double quote element> ::= <ESCAPED DOUBLE QUOTE CHAR>
<double quote element> ::= sump5d

# All the printable (non-control) characters except
# bas (x5c) kel (x7b) and doq (x22)
<UNESCAPED DOUBLE QUOTE CHARS> ~ unescapedDoubleQuoteChar+
unescapedDoubleQuoteChar ~ [\x20-\x21\x23-\x5b\x5d-\x7a\x7c-\x7e\x80-\xff]
<ESCAPED DOUBLE QUOTE CHAR> ~ bas4h bas4h | bas4h doq4h | bas4h kel4h | bas4h bix4j

sump5d ::= KEL wideHoonSeq KER

# 5d library: teak

# teak is
#
# 1) a mold, if possible, hoon otherwise,
#
# 2) an assignment to <SYM4K>, which is again, of a mold,
# if possible, of a hoon otherwise

teak5d ::= teakChoice
teakChoice ::= (KET TIS GAP) SYM4K (GAP) rope5d rank=>2
teakChoice ::= (KET TIS GAP) SYM4K (GAP) hoon rank=>1
teakChoice ::= hoon rank=>1
teakChoice ::= wideTeak5d rank=>0
wideTeak5d ::= wideTeakChoice
wideTeakChoice ::= SYM4K (TIS) rope5d rank=>2
wideTeakChoice ::= rope5d rank=>2
wideTeakChoice ::= SYM4K (TIS) wideHoon rank=>1
wideTeakChoice ::= wideHoon rank=>1

# 5d library: wasp
# Always occurs with wisp(5d), which must be tall,
# so wisp is always tall

wasp5d ::= # empty
wasp5d ::= (LUS TAR GAP) waspElements (GAP)
waspElements ::= waspElement+ separator=>GAP proper=>1
waspElement  ::= SYM4K (GAP) hoon

# 5d library: wede

wede5d ::= (FAS) wideHoon
wede5d ::= (LUS) wideHoon

# 5d library: wise

wise5d ::= SYM4K
wise5d ::= (TIS) wideMold
wise5d ::= SYM4K (TIS) wideMold
wise5d ::= SYM4K (FAS) wideMold

# 5d library: wisp

wisp5d ::= ('--')
wisp5d ::= whap5d GAP ('--')

# 5d library: whap
# Always tall

whap5d ::= boog5d+ separator=>GAP proper=>1

# End of 5d library

# === HOON FILE ===
:start ::= hoonFile
# TODO: This is a simplication, which may not
# catch all the subtleties of "ford" files
hoonFile ::= (leader) hoonSeq (trailer)

trailer ::= WS
trailer ::=
leader  ::= WS
leader  ::=

# A hack to allow inaccessible symbols
hoonFile ::= UNICORN inaccessible_ok

# === WHITESPACE ===

WS ~ whitespace
whitespace ~ ace
whitespace ~ gap4k
optHorizontalWhitespace ~ horizontalWhitespaceElement*
horizontalWhitespaceElements ~ horizontalWhitespaceElement+
horizontalWhitespaceElement ~ ace
# horizontalWhitespaceElement ~ horizontalGapElement
# horizontalGapElement ~ '+=' # documentation decoration

GAP ~ gap4k

gap4k ~ ace horizontalWhitespaceElements # a "wide" gap
gap4k ~ tallGapPrefix optGapLines optHorizontalWhitespace
# The prefix must contain an <NL> to ensure that this *is* a tall gap
tallGapPrefix ~ optHorizontalWhitespace nl
tallGapPrefix ~ optHorizontalWhitespace comment
optGapLines ~ gapLine*
gapLine ~ optHorizontalWhitespace comment
gapLine ~ optHorizontalWhitespace nl

ACE ~ ace
ace ~ ' '
comment ~ '::' optNonNLs nl

# TODO: Is this treatment of these fas runes OK?
# No, it is not OK, need to look at ford rune
comment ~ '/?' optNonNLs nl
comment ~ '/+' optNonNLs nl
comment ~ '/-' optNonNLs nl

# Documentation decorations treated as comments
comment ~ ':>' optNonNLs nl
comment ~ ':<' optNonNLs nl
comment ~ '+|' optNonNLs nl

# NL ~ nl
nl ~ [\n]
optNonNLs ~ nonNL*
nonNL ~ [^\n]

wsChars ~ wsChar*
wsChar ~ [ \n]

# @ub   unsigned binary          0b10          (2)
NUMBER ~ binaryNumber
# syn match       hoonNumber        "0b[01]\{1,4\}\%(\.\_s*[01]\{4\}\)*"
binaryNumber ~ '0b' binaryPrefix binaryGroups
binaryPrefix ~ binaryDigit
binaryPrefix ~ binaryDigit binaryDigit
binaryPrefix ~ binaryDigit binaryDigit binaryDigit
binaryPrefix ~ binaryDigit binaryDigit binaryDigit binaryDigit
binaryDigit ~ [01]
binaryGroups ~ binaryGroup*
binaryGroup ~ [.] wsChars binaryDigit binaryDigit binaryDigit binaryDigit

# @uc   bitcoin address          0c1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa
# @ud   unsigned decimal         42            (42)
#                                1.420         (1420)
NUMBER ~ decimalNumber
# syn match       hoonNumber        "\d\{1,3\}\%(\.\_s\?\d\{3\}\)*"
decimalNumber ~ decimalPrefix decimalGroups
decimalPrefix ~ decimalDigit
decimalPrefix ~ decimalDigit decimalDigit
decimalPrefix ~ decimalDigit decimalDigit decimalDigit
decimalDigit ~ [0-9]
decimalGroups ~ decimalGroup*
decimalGroup ~ [.] wsChars decimalDigit decimalDigit decimalDigit

# @uv   unsigned base32          0v3ic5h.6urr6
NUMBER ~ vNumber
# syn match       hoonNumber        "0v[0-9a-v]\{1,5\}\%(\.\_s*[0-9a-v]\{5\}\)*"
vNumber ~ '0v' vNumPrefix vNumGroups
vNumPrefix ~ vNumDigit
vNumPrefix ~ vNumDigit vNumDigit
vNumPrefix ~ vNumDigit vNumDigit vNumDigit
vNumPrefix ~ vNumDigit vNumDigit vNumDigit vNumDigit
vNumPrefix ~ vNumDigit vNumDigit vNumDigit vNumDigit vNumDigit
vNumDigit ~ [0-9a-v]
vNumGroups ~ vNumGroup*
vNumGroup ~ [.] wsChars vNumDigit vNumDigit vNumDigit vNumDigit vNumDigit

# @uw   unsigned base64          0wsC5.yrSZC
NUMBER ~ wNumber
# syn match       hoonNumber        "0w[-~0-9a-zA-Z]\{1,5\}\%(\.\_s*[-~0-9a-zA-Z]\{5\}\)*"
wNumber ~ '0w' wNumPrefix wNumGroups
wNumPrefix ~ wNumDigit
wNumPrefix ~ wNumDigit wNumDigit
wNumPrefix ~ wNumDigit wNumDigit wNumDigit
wNumPrefix ~ wNumDigit wNumDigit wNumDigit wNumDigit
wNumPrefix ~ wNumDigit wNumDigit wNumDigit wNumDigit wNumDigit
wNumDigit ~ [-~0-9a-zA-Z]
wNumGroups ~ wNumGroup*
wNumGroup ~ [.] wsChars wNumDigit wNumDigit wNumDigit wNumDigit wNumDigit

# @ux   unsigned hexadecimal     0xcafe.babe
NUMBER ~ hexNumber
# syn match       hoonNumber        "0x\x\{1,4\}\%(\.\_s*\x\{4\}\)*"
hexNumber ~ '0x' hexPrefix hexGroups
hexPrefix ~ hexDigit
hexPrefix ~ hexDigit hexDigit
hexPrefix ~ hexDigit hexDigit hexDigit
hexPrefix ~ hexDigit hexDigit hexDigit hexDigit
hexDigit ~ [0-9a-fA-F]
hexGroups ~ hexGroup*
hexGroup ~ [.] wsChars hexDigit hexDigit hexDigit hexDigit

# === CELLS BY TYPE ==

hoonSeq ::= hoon+ separator=>GAP proper=>1
hoon ::= wideHoon
wideHoon ::= hoonUnary
hoonUnary ::= hoonPrimary
hoonPrimary ::= norm5dWide rank=>10
hoonPrimary ::= long5dWide rank=>8

wideHoonSeq ::= wideHoon+ separator=>ACE proper=>1


BAR ~ bar4h
BAR4H ~ bar4h
bar4h ~ [|]
inaccessible_ok ::= BAR
inaccessible_ok ::= BAR4H
BAS ~ bas4h
BAS4H ~ bas4h
bas4h ~ [\x5c]
inaccessible_ok ::= BAS
inaccessible_ok ::= BAS4H
BUC ~ buc4h
BUC4H ~ buc4h
buc4h ~ [$]
inaccessible_ok ::= BUC
inaccessible_ok ::= BUC4H
CAB ~ cab4h
CAB4H ~ cab4h
cab4h ~ [_]
inaccessible_ok ::= CAB
inaccessible_ok ::= CAB4H
CEN ~ cen4h
CEN4H ~ cen4h
cen4h ~ [%]
inaccessible_ok ::= CEN
inaccessible_ok ::= CEN4H
COL ~ col4h
COL4H ~ col4h
col4h ~ [:]
inaccessible_ok ::= COL
inaccessible_ok ::= COL4H
COM ~ com4h
COM4H ~ com4h
com4h ~ [,]
inaccessible_ok ::= COM
inaccessible_ok ::= COM4H
DOQ ~ doq4h
DOQ4H ~ doq4h
doq4h ~ ["]
inaccessible_ok ::= DOQ
inaccessible_ok ::= DOQ4H
DOT ~ dot4h
DOT4H ~ dot4h
dot4h ~ [.]
inaccessible_ok ::= DOT
inaccessible_ok ::= DOT4H
FAS ~ fas4h
FAS4H ~ fas4h
fas4h ~ [/]
inaccessible_ok ::= FAS
inaccessible_ok ::= FAS4H
GAL ~ gal4h
GAL4H ~ gal4h
gal4h ~ [<]
inaccessible_ok ::= GAL
inaccessible_ok ::= GAL4H
GAR ~ gar4h
GAR4H ~ gar4h
gar4h ~ [>]
inaccessible_ok ::= GAR
inaccessible_ok ::= GAR4H
HAX ~ hax4h
HAX4H ~ hax4h
hax4h ~ [#]
inaccessible_ok ::= HAX
inaccessible_ok ::= HAX4H
HEP ~ hep4h
HEP4H ~ hep4h
hep4h ~ [-]
inaccessible_ok ::= HEP
inaccessible_ok ::= HEP4H
KEL ~ kel4h
KEL4H ~ kel4h
kel4h ~ [{]
inaccessible_ok ::= KEL
inaccessible_ok ::= KEL4H
KER ~ ker4h
KER4H ~ ker4h
ker4h ~ [}]
inaccessible_ok ::= KER
inaccessible_ok ::= KER4H
KET ~ ket4h
KET4H ~ ket4h
ket4h ~ [\^]
inaccessible_ok ::= KET
inaccessible_ok ::= KET4H
LUS ~ lus4h
LUS4H ~ lus4h
lus4h ~ [+]
inaccessible_ok ::= LUS
inaccessible_ok ::= LUS4H
PAL ~ pal4h
PAL4H ~ pal4h
pal4h ~ [(]
inaccessible_ok ::= PAL
inaccessible_ok ::= PAL4H
PAM ~ pam4h
PAM4H ~ pam4h
pam4h ~ [&]
inaccessible_ok ::= PAM
inaccessible_ok ::= PAM4H
PAR ~ par4h
PAR4H ~ par4h
par4h ~ [)]
inaccessible_ok ::= PAR
inaccessible_ok ::= PAR4H
PAT ~ pat4h
PAT4H ~ pat4h
pat4h ~ [@]
inaccessible_ok ::= PAT
inaccessible_ok ::= PAT4H
PEL ~ pel4h
PEL4H ~ pel4h
pel4h ~ [(]
inaccessible_ok ::= PEL
inaccessible_ok ::= PEL4H
PER ~ per4h
PER4H ~ per4h
per4h ~ [)]
inaccessible_ok ::= PER
inaccessible_ok ::= PER4H
SEL ~ sel4h
SEL4H ~ sel4h
sel4h ~ [\x5b]
inaccessible_ok ::= SEL
inaccessible_ok ::= SEL4H
SEM ~ sem4h
SEM4H ~ sem4h
sem4h ~ [;]
inaccessible_ok ::= SEM
inaccessible_ok ::= SEM4H
SER ~ ser4h
SER4H ~ ser4h
ser4h ~ [\x5d]
inaccessible_ok ::= SER
inaccessible_ok ::= SER4H
SIG ~ sig4h
SIG4H ~ sig4h
sig4h ~ [~]
inaccessible_ok ::= SIG
inaccessible_ok ::= SIG4H
SOQ ~ soq4h
SOQ4H ~ soq4h
soq4h ~ [']
inaccessible_ok ::= SOQ
inaccessible_ok ::= SOQ4H
TAR ~ tar4h
TAR4H ~ tar4h
tar4h ~ [*]
inaccessible_ok ::= TAR
inaccessible_ok ::= TAR4H
TEC ~ tec4h
TEC4H ~ tec4h
tec4h ~ [`]
inaccessible_ok ::= TEC
inaccessible_ok ::= TEC4H
TIS ~ tis4h
TIS4H ~ tis4h
tis4h ~ [=]
inaccessible_ok ::= TIS
inaccessible_ok ::= TIS4H
WUT ~ wut4h
WUT4H ~ wut4h
wut4h ~ [?]
inaccessible_ok ::= WUT
inaccessible_ok ::= WUT4H
ZAP ~ zap4h
ZAP4H ~ zap4h
zap4h ~ [!]
inaccessible_ok ::= ZAP
inaccessible_ok ::= ZAP4H
# BARCOL hoon hoon
hoon ::= tallBarcol
norm5dWide ::= wideBarcol
tallBarcol ::= (BAR4H COL4H GAP)hoon (GAP) hoon
wideBarcol ::= (BAR4H COL4H) [(] wideHoon (ACE) wideHoon [)]

# BARDOT hoon
hoon ::= tallBardot
norm5dWide ::= wideBardot
tallBardot ::= (BAR4H DOT4H GAP)hoon
wideBardot ::= (BAR4H DOT4H) [(] wideHoon [)]

# BARHEP hoon
hoon ::= tallBarhep
norm5dWide ::= wideBarhep
tallBarhep ::= (BAR4H HEP4H GAP)hoon
wideBarhep ::= (BAR4H HEP4H) [(] wideHoon [)]

# BARSIG mold hoon
hoon ::= tallBarsig
norm5dWide ::= wideBarsig
tallBarsig ::= (BAR4H SIG4H GAP)mold (GAP) hoon
wideBarsig ::= (BAR4H SIG4H) [(] wideMold (ACE) wideHoon [)]

# BARTAR mold hoon
hoon ::= tallBartar
norm5dWide ::= wideBartar
tallBartar ::= (BAR4H TAR4H GAP)mold (GAP) hoon
wideBartar ::= (BAR4H TAR4H) [(] wideMold (ACE) wideHoon [)]

# BARWUT hoon
hoon ::= tallBarwut
norm5dWide ::= wideBarwut
tallBarwut ::= (BAR4H WUT4H GAP)hoon
wideBarwut ::= (BAR4H WUT4H) [(] wideHoon [)]

# BUCCAB hoon
hoon ::= tallBuccab
norm5dWide ::= wideBuccab
tallBuccab ::= (BUC4H CAB4H GAP)hoon
wideBuccab ::= (BUC4H CAB4H) [(] wideHoon [)]

# BUCHEP mold mold
hoon ::= tallBuchep
norm5dWide ::= wideBuchep
tallBuchep ::= (BUC4H HEP4H GAP)mold (GAP) mold
wideBuchep ::= (BUC4H HEP4H) [(] wideMold (ACE) wideMold [)]

# BUCKET mold mold
hoon ::= tallBucket
norm5dWide ::= wideBucket
tallBucket ::= (BUC4H KET4H GAP)mold (GAP) mold
wideBucket ::= (BUC4H KET4H) [(] wideMold (ACE) wideMold [)]

# BUCPAT mold mold
hoon ::= tallBucpat
norm5dWide ::= wideBucpat
tallBucpat ::= (BUC4H PAT4H GAP)mold (GAP) mold
wideBucpat ::= (BUC4H PAT4H) [(] wideMold (ACE) wideMold [)]

# BUCSEM mold
hoon ::= tallBucsem
norm5dWide ::= wideBucsem
tallBucsem ::= (BUC4H SEM4H GAP)mold
wideBucsem ::= (BUC4H SEM4H) [(] wideMold [)]

# BUCTIS SYM4K mold
hoon ::= tallBuctis
norm5dWide ::= wideBuctis
tallBuctis ::= (BUC4H TIS4H GAP)SYM4K (GAP) mold
wideBuctis ::= (BUC4H TIS4H) [(] SYM4K (ACE) wideMold [)]

# CENDOT hoon hoon
hoon ::= tallCendot
norm5dWide ::= wideCendot
tallCendot ::= (CEN4H DOT4H GAP)hoon (GAP) hoon
wideCendot ::= (CEN4H DOT4H) [(] wideHoon (ACE) wideHoon [)]

# CENHEP hoon hoon
hoon ::= tallCenhep
norm5dWide ::= wideCenhep
tallCenhep ::= (CEN4H HEP4H GAP)hoon (GAP) hoon
wideCenhep ::= (CEN4H HEP4H) [(] wideHoon (ACE) wideHoon [)]

# CENKET hoon hoon hoon hoon
hoon ::= tallCenket
norm5dWide ::= wideCenket
tallCenket ::= (CEN4H KET4H GAP)hoon (GAP) hoon (GAP) hoon (GAP) hoon
wideCenket ::= (CEN4H KET4H) [(] wideHoon (ACE) wideHoon (ACE) wideHoon (ACE) wideHoon [)]

# CENLUS hoon hoon hoon
hoon ::= tallCenlus
norm5dWide ::= wideCenlus
tallCenlus ::= (CEN4H LUS4H GAP)hoon (GAP) hoon (GAP) hoon
wideCenlus ::= (CEN4H LUS4H) [(] wideHoon (ACE) wideHoon (ACE) wideHoon [)]

# CENSIG rope5d hoon hoon
hoon ::= tallCensig
norm5dWide ::= wideCensig
tallCensig ::= (CEN4H SIG4H GAP)rope5d (GAP) hoon (GAP) hoon
wideCensig ::= (CEN4H SIG4H) [(] rope5d (ACE) wideHoon (ACE) wideHoon [)]

# COLCAB hoon hoon
hoon ::= tallColcab
norm5dWide ::= wideColcab
tallColcab ::= (COL4H CAB4H GAP)hoon (GAP) hoon
wideColcab ::= (COL4H CAB4H) [(] wideHoon (ACE) wideHoon [)]

# COLHEP hoon hoon
hoon ::= tallColhep
norm5dWide ::= wideColhep
tallColhep ::= (COL4H HEP4H GAP)hoon (GAP) hoon
wideColhep ::= (COL4H HEP4H) [(] wideHoon (ACE) wideHoon [)]

# COLLUS hoon hoon hoon
hoon ::= tallCollus
norm5dWide ::= wideCollus
tallCollus ::= (COL4H LUS4H GAP)hoon (GAP) hoon (GAP) hoon
wideCollus ::= (COL4H LUS4H) [(] wideHoon (ACE) wideHoon (ACE) wideHoon [)]

# COLKET hoon hoon hoon hoon
hoon ::= tallColket
norm5dWide ::= wideColket
tallColket ::= (COL4H KET4H GAP)hoon (GAP) hoon (GAP) hoon (GAP) hoon
wideColket ::= (COL4H KET4H) [(] wideHoon (ACE) wideHoon (ACE) wideHoon (ACE) wideHoon [)]

# DOTLUS hoon
hoon ::= tallDotlus
norm5dWide ::= wideDotlus
tallDotlus ::= (DOT4H LUS4H GAP)hoon
wideDotlus ::= (DOT4H LUS4H) [(] wideHoon [)]

# DOTTAR hoon hoon
hoon ::= tallDottar
norm5dWide ::= wideDottar
tallDottar ::= (DOT4H TAR4H GAP)hoon (GAP) hoon
wideDottar ::= (DOT4H TAR4H) [(] wideHoon (ACE) wideHoon [)]

# DOTTIS hoon hoon
hoon ::= tallDottis
norm5dWide ::= wideDottis
tallDottis ::= (DOT4H TIS4H GAP)hoon (GAP) hoon
wideDottis ::= (DOT4H TIS4H) [(] wideHoon (ACE) wideHoon [)]

# DOTWUT hoon
hoon ::= tallDotwut
norm5dWide ::= wideDotwut
tallDotwut ::= (DOT4H WUT4H GAP)hoon
wideDotwut ::= (DOT4H WUT4H) [(] wideHoon [)]

# FASSEM hoon hoon
hoon ::= tallFassem
norm5dWide ::= wideFassem
tallFassem ::= (FAS4H SEM4H GAP)hoon (GAP) hoon
wideFassem ::= (FAS4H SEM4H) [(] wideHoon (ACE) wideHoon [)]

# KETBAR hoon
hoon ::= tallKetbar
norm5dWide ::= wideKetbar
tallKetbar ::= (KET4H BAR4H GAP)hoon
wideKetbar ::= (KET4H BAR4H) [(] wideHoon [)]

# KETCEN hoon
hoon ::= tallKetcen
norm5dWide ::= wideKetcen
tallKetcen ::= (KET4H CEN4H GAP)hoon
wideKetcen ::= (KET4H CEN4H) [(] wideHoon [)]

# KETDOT hoon hoon
hoon ::= tallKetdot
norm5dWide ::= wideKetdot
tallKetdot ::= (KET4H DOT4H GAP)hoon (GAP) hoon
wideKetdot ::= (KET4H DOT4H) [(] wideHoon (ACE) wideHoon [)]

# KETHEP mold hoon
hoon ::= tallKethep
norm5dWide ::= wideKethep
tallKethep ::= (KET4H HEP4H GAP)mold (GAP) hoon
wideKethep ::= (KET4H HEP4H) [(] wideMold (ACE) wideHoon [)]

# KETLUS hoon hoon
hoon ::= tallKetlus
norm5dWide ::= wideKetlus
tallKetlus ::= (KET4H LUS4H GAP)hoon (GAP) hoon
wideKetlus ::= (KET4H LUS4H) [(] wideHoon (ACE) wideHoon [)]

# KETPAM hoon
hoon ::= tallKetpam
norm5dWide ::= wideKetpam
tallKetpam ::= (KET4H PAM4H GAP)hoon
wideKetpam ::= (KET4H PAM4H) [(] wideHoon [)]

# KETSIG hoon
hoon ::= tallKetsig
norm5dWide ::= wideKetsig
tallKetsig ::= (KET4H SIG4H GAP)hoon
wideKetsig ::= (KET4H SIG4H) [(] wideHoon [)]

# KETTIS SYM4K hoon
hoon ::= tallKettis
norm5dWide ::= wideKettis
tallKettis ::= (KET4H TIS4H GAP)SYM4K (GAP) hoon
wideKettis ::= (KET4H TIS4H) [(] SYM4K (ACE) wideHoon [)]

# KETWUT hoon
hoon ::= tallKetwut
norm5dWide ::= wideKetwut
tallKetwut ::= (KET4H WUT4H GAP)hoon
wideKetwut ::= (KET4H WUT4H) [(] wideHoon [)]

# SEMFAS hoon
hoon ::= tallSemfas
norm5dWide ::= wideSemfas
tallSemfas ::= (SEM4H FAS4H GAP)hoon
wideSemfas ::= (SEM4H FAS4H) [(] wideHoon [)]

# SEMSEM hoon hoon
hoon ::= tallSemsem
norm5dWide ::= wideSemsem
tallSemsem ::= (SEM4H SEM4H GAP)hoon (GAP) hoon
wideSemsem ::= (SEM4H SEM4H) [(] wideHoon (ACE) wideHoon [)]

# SIGBAR hoon hoon
hoon ::= tallSigbar
norm5dWide ::= wideSigbar
tallSigbar ::= (SIG4H BAR4H GAP)hoon (GAP) hoon
wideSigbar ::= (SIG4H BAR4H) [(] wideHoon (ACE) wideHoon [)]

# SIGBUC CEN_SYM4K hoon
hoon ::= tallSigbuc
norm5dWide ::= wideSigbuc
tallSigbuc ::= (SIG4H BUC4H GAP)CEN_SYM4K (GAP) hoon
wideSigbuc ::= (SIG4H BUC4H) [(] CEN_SYM4K (ACE) wideHoon [)]

# SIGCAB hoon hoon
hoon ::= tallSigcab
norm5dWide ::= wideSigcab
tallSigcab ::= (SIG4H CAB4H GAP)hoon (GAP) hoon
wideSigcab ::= (SIG4H CAB4H) [(] wideHoon (ACE) wideHoon [)]

# SIGCEN bonk5d rope5d bonz5d hoon
hoon ::= tallSigcen
norm5dWide ::= wideSigcen
tallSigcen ::= (SIG4H CEN4H GAP)bonk5d (GAP) rope5d (GAP) bonz5d (GAP) hoon
wideSigcen ::= (SIG4H CEN4H) [(] bonk5d (ACE) rope5d (ACE) wideBonz5d (ACE) wideHoon [)]

# SIGFAS bonk5d hoon
hoon ::= tallSigfas
norm5dWide ::= wideSigfas
tallSigfas ::= (SIG4H FAS4H GAP)bonk5d (GAP) hoon
wideSigfas ::= (SIG4H FAS4H) [(] bonk5d (ACE) wideHoon [)]

# SIGGAL bont5d hoon
hoon ::= tallSiggal
norm5dWide ::= wideSiggal
tallSiggal ::= (SIG4H GAL4H GAP)bont5d (GAP) hoon
wideSiggal ::= (SIG4H GAL4H) [(] wideBont5d (ACE) wideHoon [)]

# SIGGAR bont5d hoon
hoon ::= tallSiggar
norm5dWide ::= wideSiggar
tallSiggar ::= (SIG4H GAR4H GAP)bont5d (GAP) hoon
wideSiggar ::= (SIG4H GAR4H) [(] wideBont5d (ACE) wideHoon [)]

# SIGTIS hoon hoon
hoon ::= tallSigtis
norm5dWide ::= wideSigtis
tallSigtis ::= (SIG4H TIS4H GAP)hoon (GAP) hoon
wideSigtis ::= (SIG4H TIS4H) [(] wideHoon (ACE) wideHoon [)]

# SIGZAP hoon hoon
hoon ::= tallSigzap
norm5dWide ::= wideSigzap
tallSigzap ::= (SIG4H ZAP4H GAP)hoon (GAP) hoon
wideSigzap ::= (SIG4H ZAP4H) [(] wideHoon (ACE) wideHoon [)]

# TISBAR mold hoon
hoon ::= tallTisbar
norm5dWide ::= wideTisbar
tallTisbar ::= (TIS4H BAR4H GAP)mold (GAP) hoon
wideTisbar ::= (TIS4H BAR4H) [(] wideMold (ACE) wideHoon [)]

# TISCOM hoon hoon
hoon ::= tallTiscom
norm5dWide ::= wideTiscom
tallTiscom ::= (TIS4H COM4H GAP)hoon (GAP) hoon
wideTiscom ::= (TIS4H COM4H) [(] wideHoon (ACE) wideHoon [)]

# TISDOT rope5d hoon hoon
hoon ::= tallTisdot
norm5dWide ::= wideTisdot
tallTisdot ::= (TIS4H DOT4H GAP)rope5d (GAP) hoon (GAP) hoon
wideTisdot ::= (TIS4H DOT4H) [(] rope5d (ACE) wideHoon (ACE) wideHoon [)]

# TISHEP hoon hoon
hoon ::= tallTishep
norm5dWide ::= wideTishep
tallTishep ::= (TIS4H HEP4H GAP)hoon (GAP) hoon
wideTishep ::= (TIS4H HEP4H) [(] wideHoon (ACE) wideHoon [)]

# TISFAS wise5d hoon hoon
hoon ::= tallTisfas
norm5dWide ::= wideTisfas
tallTisfas ::= (TIS4H FAS4H GAP)wise5d (GAP) hoon (GAP) hoon
wideTisfas ::= (TIS4H FAS4H) [(] wise5d (ACE) wideHoon (ACE) wideHoon [)]

# TISGAL hoon hoon
hoon ::= tallTisgal
norm5dWide ::= wideTisgal
tallTisgal ::= (TIS4H GAL4H GAP)hoon (GAP) hoon
wideTisgal ::= (TIS4H GAL4H) [(] wideHoon (ACE) wideHoon [)]

# TISGAR hoon hoon
hoon ::= tallTisgar
norm5dWide ::= wideTisgar
tallTisgar ::= (TIS4H GAR4H GAP)hoon (GAP) hoon
wideTisgar ::= (TIS4H GAR4H) [(] wideHoon (ACE) wideHoon [)]

# TISKET wise5d rope5d hoon hoon
hoon ::= tallTisket
norm5dWide ::= wideTisket
tallTisket ::= (TIS4H KET4H GAP)wise5d (GAP) rope5d (GAP) hoon (GAP) hoon
wideTisket ::= (TIS4H KET4H) [(] wise5d (ACE) rope5d (ACE) wideHoon (ACE) wideHoon [)]

# TISLUS hoon hoon
hoon ::= tallTislus
norm5dWide ::= wideTislus
tallTislus ::= (TIS4H LUS4H GAP)hoon (GAP) hoon
wideTislus ::= (TIS4H LUS4H) [(] wideHoon (ACE) wideHoon [)]

# TISSEM wise5d hoon hoon
hoon ::= tallTissem
norm5dWide ::= wideTissem
tallTissem ::= (TIS4H SEM4H GAP)wise5d (GAP) hoon (GAP) hoon
wideTissem ::= (TIS4H SEM4H) [(] wise5d (ACE) wideHoon (ACE) wideHoon [)]

# TISTAR SYM4K hoon hoon
hoon ::= tallTistar
norm5dWide ::= wideTistar
tallTistar ::= (TIS4H TAR4H GAP)SYM4K (GAP) hoon (GAP) hoon
wideTistar ::= (TIS4H TAR4H) [(] SYM4K (ACE) wideHoon (ACE) wideHoon [)]

# TISWUT rope5d hoon hoon hoon
hoon ::= tallTiswut
norm5dWide ::= wideTiswut
tallTiswut ::= (TIS4H WUT4H GAP)rope5d (GAP) hoon (GAP) hoon (GAP) hoon
wideTiswut ::= (TIS4H WUT4H) [(] rope5d (ACE) wideHoon (ACE) wideHoon (ACE) wideHoon [)]

# WUTCOL hoon hoon hoon
hoon ::= tallWutcol
norm5dWide ::= wideWutcol
tallWutcol ::= (WUT4H COL4H GAP)hoon (GAP) hoon (GAP) hoon
wideWutcol ::= (WUT4H COL4H) [(] wideHoon (ACE) wideHoon (ACE) wideHoon [)]

# WUTDOT hoon hoon hoon
hoon ::= tallWutdot
norm5dWide ::= wideWutdot
tallWutdot ::= (WUT4H DOT4H GAP)hoon (GAP) hoon (GAP) hoon
wideWutdot ::= (WUT4H DOT4H) [(] wideHoon (ACE) wideHoon (ACE) wideHoon [)]

# WUTGAL hoon hoon
hoon ::= tallWutgal
norm5dWide ::= wideWutgal
tallWutgal ::= (WUT4H GAL4H GAP)hoon (GAP) hoon
wideWutgal ::= (WUT4H GAL4H) [(] wideHoon (ACE) wideHoon [)]

# WUTGAR hoon hoon
hoon ::= tallWutgar
norm5dWide ::= wideWutgar
tallWutgar ::= (WUT4H GAR4H GAP)hoon (GAP) hoon
wideWutgar ::= (WUT4H GAR4H) [(] wideHoon (ACE) wideHoon [)]

# WUTKET teak5d hoon hoon
hoon ::= tallWutket
norm5dWide ::= wideWutket
tallWutket ::= (WUT4H KET4H GAP)teak5d (GAP) hoon (GAP) hoon
wideWutket ::= (WUT4H KET4H) [(] wideTeak5d (ACE) wideHoon (ACE) wideHoon [)]

# WUTPAT teak5d hoon hoon
hoon ::= tallWutpat
norm5dWide ::= wideWutpat
tallWutpat ::= (WUT4H PAT4H GAP)teak5d (GAP) hoon (GAP) hoon
wideWutpat ::= (WUT4H PAT4H) [(] wideTeak5d (ACE) wideHoon (ACE) wideHoon [)]

# WUTSIG teak5d hoon hoon
hoon ::= tallWutsig
norm5dWide ::= wideWutsig
tallWutsig ::= (WUT4H SIG4H GAP)teak5d (GAP) hoon (GAP) hoon
wideWutsig ::= (WUT4H SIG4H) [(] wideTeak5d (ACE) wideHoon (ACE) wideHoon [)]

# WUTTIS teak5d hoon hoon
hoon ::= tallWuttis
norm5dWide ::= wideWuttis
tallWuttis ::= (WUT4H TIS4H GAP)teak5d (GAP) hoon (GAP) hoon
wideWuttis ::= (WUT4H TIS4H) [(] wideTeak5d (ACE) wideHoon (ACE) wideHoon [)]

# WUTZAP hoon
hoon ::= tallWutzap
norm5dWide ::= wideWutzap
tallWutzap ::= (WUT4H ZAP4H GAP)hoon
wideWutzap ::= (WUT4H ZAP4H) [(] wideHoon [)]

# ZAPCOL hoon
hoon ::= tallZapcol
norm5dWide ::= wideZapcol
tallZapcol ::= (ZAP4H COL4H GAP)hoon
wideZapcol ::= (ZAP4H COL4H) [(] wideHoon [)]

# ZAPDOT hoon
hoon ::= tallZapdot
norm5dWide ::= wideZapdot
tallZapdot ::= (ZAP4H DOT4H GAP)hoon
wideZapdot ::= (ZAP4H DOT4H) [(] wideHoon [)]

# ZAPCOM hoon hoon
hoon ::= tallZapcom
norm5dWide ::= wideZapcom
tallZapcom ::= (ZAP4H COM4H GAP)hoon (GAP) hoon
wideZapcom ::= (ZAP4H COM4H) [(] wideHoon (ACE) wideHoon [)]

# ZAPGAR hoon
hoon ::= tallZapgar
norm5dWide ::= wideZapgar
tallZapgar ::= (ZAP4H GAR4H GAP)hoon
wideZapgar ::= (ZAP4H GAR4H) [(] wideHoon [)]

# ZAPTIS hoon
hoon ::= tallZaptis
norm5dWide ::= wideZaptis
tallZaptis ::= (ZAP4H TIS4H GAP)hoon
wideZaptis ::= (ZAP4H TIS4H) [(] wideHoon [)]

