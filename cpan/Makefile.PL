use 5.010001;
use strict;
use warnings;
use ExtUtils::Install;
use Fatal qw(open close mkdir chdir);
use File::Spec 0.82;
use File::Find 1.12;
use File::Copy;
use IPC::Cmd;
use English qw( -no_match_vars );
use Time::Piece 1.12;
use Cwd;

use Config;
use ExtUtils::MakeMaker;

use vars qw($VERSION $STRING_VERSION);
$VERSION        = '0.001_000';
$STRING_VERSION = $VERSION;
## no critic (BuiltinFunctions::ProhibitStringyEval)
$VERSION = eval $VERSION;
## use critic

# This comment reminds me of how to use Module::CoreList.
# It's here for convenience in working on this file.
# perl -MModule::CoreList -E 'print $Module::CoreList::version{5.010001}{Carp};'

my @always_required            = qw(
    Carp
    Cwd
    Data::Dumper
    DynaLoader
    English
    Exporter
    ExtUtils::CBuilder
    ExtUtils::MakeMaker
    ExtUtils::Manifest
    ExtUtils::Mkbootstrap
    Fatal
    File::Copy
    File::Spec
    IPC::Cmd
    List::Util
    Scalar::Util
    Test::More
    Time::Piece
    XSLoader
);

my %requires =
    map { ( $_, 0 ) }
    @always_required;

my @files_to_cleanup = (
);

WriteMakefile(
    clean    => { FILES => ( join q{ }, @files_to_cleanup ) },
    NAME     => 'MarpaX::Hoonlint',
    VERSION => $STRING_VERSION,
    AUTHOR   => 'Jeffrey Kegler',
    ABSTRACT => 'Hoonlint',

    # Let the three of these be the same.
    CONFIGURE_REQUIRES => \%requires,
    BUILD_REQUIRES => \%requires,
    PREREQ_PM => \%requires,

    MIN_PERL_VERSION => '5.10.1',
    META_MERGE => {
        resources =>
            { repository => 'git://github.com/jeffreykegler/hoonlint.git', },
    },
    LICENSE  => 'mit',
);

# vim: shiftwidth=4:
