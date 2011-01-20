#!perl -T

use Test::More tests => 12;

use_ok( 'SKOS::Simple' );

my $skos = SKOS::Simple->new;
isa_ok( $skos, 'SKOS::Simple' );

my $ttl = $skos->turtle;
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

is( $skos->turtle_scheme, 
    "<> a skos:ConceptScheme ;\n    skos:hasTopConcept <y> .\n", "full scheme" );

is( $skos->turtle_scheme( top => 0 ), 
    "<> a skos:ConceptScheme .\n", "lean scheme" );

$skos = SKOS::Simple->new( language => 'de' );
$skos->add_concept( notation => 'x', scopeNote => 'test' );

# print $skos->turtle_concept('x', top => 0);
# use Data::Dumper;
# print Dumper($skos);

