#!perl -T

use Test::More tests => 5;

use SKOS::Simple qw(:turtle);

my $ttl = turtle_statement( '<>', 'dc:title' => '"foo"' );
is( $ttl, "<> dc:title \"foo\" .\n", "turtle_statement" );

foreach ( undef, [ ], "" ) {
    $ttl = turtle_statement( '<>', 'dc:title' => $_ );
    is( $ttl, "" );
}

is( turtle_literal("x\t\"\n"), '"x\\t\\"\\n"', 'turtle_literal' );

