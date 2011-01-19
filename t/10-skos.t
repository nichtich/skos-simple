#!perl -T

use Test::More tests => 3;

use_ok( 'SKOS::Simple' );

my $skos = SKOS::Simple->new;
isa_ok( $skos, 'SKOS::Simple' );

my $ttl = $skos->as_turtle;
$ttl =~ s/\s+|\n/ /g;
$ttl =~ s/^\s+|\s+$//g;

is( $ttl, '@prefix skos: <http://www.w3.org/2008/05/skos#> . <> a skos:ConceptScheme .' );

