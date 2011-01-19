#!perl -T

use Test::More tests => 10;

use_ok( 'SKOS::Simple' );

my $skos = SKOS::Simple->new;
isa_ok( $skos, 'SKOS::Simple' );

my $ttl = $skos->as_turtle;
$ttl =~ s/\s+|\n/ /g;
$ttl =~ s/^\s+|\s+$//g;

is( $ttl, '@prefix skos: <http://www.w3.org/2008/05/skos#> . <> a skos:ConceptScheme .' );

is( $skos->size, 0, 'size zero' );

my @c = $skos->top_concepts;
is ( scalar @c, 0, 'no top concepts' );

$skos->add_concept( notation => 'x' );
ok( $skos->has_concept('x') ); 
is( $skos->size, 1, 'one concept' );

is_deeply( [ $skos->top_concepts ], ['x'], 'top concept' );

$skos->add_concept( notation => 'x', broader => 'y' );
is( $skos->size, 2, 'second concept' );
is_deeply( [ $skos->top_concepts ], ['y'], 'top concept' );

