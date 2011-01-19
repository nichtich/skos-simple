#!perl -T

use Test::More tests => 1;

BEGIN {
	use_ok( 'SKOS::Simple' );
}

diag( "Testing SKOS::Simple $SKOS::Simple::VERSION, Perl $], $^X" );
