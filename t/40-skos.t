#!perl -T

use Test::More qw(no_plan);

use_ok( 'SKOS::Simple' );

my $skos = SKOS::Simple->new( notation => 'unique' );

my $ttl = $skos->turtle;
$ttl =~ s/\s+|\n/ /g;
$ttl =~ s/^\s+|\s+$//g;

is( $ttl, '@prefix skos: <http://www.w3.org/2004/02/skos/core#> . <> a skos:ConceptScheme .' );
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

is( $skos->scheme_turtle, 
    "<> a skos:ConceptScheme ;\n    skos:hasTopConcept <y> .\n", "full scheme" );

my %schemeuris = (
    'http://example.com' => '', '' => '',
    'http://example.org' => 'http://example.org'
);
my ($s1,$s2) = (undef,'');
do {
    $skos = SKOS::Simple->new( base => 'http://example.com', scheme => $s1 );
    is( $skos->scheme_turtle( top => 0 ), "<$s2> a skos:ConceptScheme .\n", "lean scheme" );
} while ( ($s1,$s2) = each(%schemeuris) );

$skos = SKOS::Simple->new( base => 'http://example.org/', notation => 'unique' );
$skos->add_concept( notation => '123' );
$ttl = $skos->concepts_turtle( scheme => 1 );
ok( $ttl =~ /skos:topConceptOf/ and not $ttl =~ /skos:inScheme/ );
$ttl = $skos->concepts_turtle( top => 0 );
ok( not $ttl =~ /skos:topConceptOf/ and $ttl =~ /skos:inScheme/ );
$ttl = $skos->concepts_turtle( top => 0, scheme => 0 );
ok( not $ttl =~ /skos:topConceptOf/ and not $ttl =~ /skos:inScheme/ );
$ttl = $skos->concepts_turtle( scheme => 2 );
ok( $ttl =~ /skos:topConceptOf/ and $ttl =~ /skos:inScheme/ );

is( $skos->concepts_turtle( lean => 1 ), $skos->concepts_turtle( top => 0, scheme => 1 ) );

$skos->add_concept( notation => '456', broader => '123' );
$ttl = $skos->concepts_turtle();
ok( $ttl =~ /skos:topConceptOf/ and $ttl =~ /skos:inScheme/ );

$skos = SKOS::Simple->new( label => 'unique', language => 'de' );
is ( $skos->concept_id( label => 'foo' ), 'foo' );
is ( $skos->concept_id( notation => 'x', label => 'foo' ), 'foo' );
is ( $skos->concept_id( notation => 'x' ), undef );


my $id = $skos->add_concept( label => 'hi' ); #{ 'de' => $label }; 
is ( $id, 'hi' );
#print $skos->concepts;


# $skos = SKOS::Simple->new( language => 'de', notation => 'unique' );
# $skos->add_concept( notation => 'x', scopeNote => 'test' );

$skos = SKOS::Simple->new( notation => 'unique', namespaces => { foo => 'my:id:' } );
$id = $skos->add_concept( notation => '0', 'foo:bar' => 'xxx' );
print $skos->concept_turtle( $id );
#print 
use Data::Dumper;
print Dumper($skos)."\n";


# print $skos->concept_turtle('x', top => 0);
# use Data::Dumper;
# print Dumper($skos);

$skos = SKOS::Simple->new( notation => 'unique' );
$skos->add_concept( notation => 'x', pref => { en => 'X1', 'fr' => 'X2' } );
# print $skos->concept_turtle('x');

