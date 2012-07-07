#!perl -T

use Test::More qw(no_plan);

use_ok( 'SKOS::Simple' );

my $skos = SKOS::Simple->new( notation => 'unique' );

my $ttl = $skos->turtle;
$ttl =~ s/\s+|\n/ /g;
$ttl =~ s/^\s+|\s+$//g;

is( $ttl, '@prefix skos: <http://www.w3.org/2004/02/skos/core#> . <> a skos:ConceptScheme .' );
is( $skos->size, 0, 'size zero' );

my @c = $skos->topConcepts;
is ( scalar @c, 0, 'no top concepts' );

$skos->addConcept( notation => 'x' );
ok( $skos->hasConcept('x') );
ok( $skos->hasConcept( notation => 'x') );  
ok( $skos->hasConcept( id => 'x') );  
is( $skos->size, 1, 'one concept' );

is_deeply( [ $skos->topConcepts ], ['x'], 'top concept' );

$skos->addConcept( notation => 'x', broader => 'y' );
is( $skos->size, 2, 'second concept' );
is_deeply( [ $skos->topConcepts ], ['y'], 'top concept' );

is( $skos->turtleScheme, 
    "<> a skos:ConceptScheme ;\n    skos:hasTopConcept <y> .\n", "full scheme" );

my %schemeuris = (
    'http://example.com' => '', '' => '',
    'http://example.org' => 'http://example.org'
);
my ($s1,$s2) = (undef,'');
do {
    $skos = SKOS::Simple->new( base => 'http://example.com', scheme => $s1 );
    is( $skos->turtleScheme( top => 0 ), "<$s2> a skos:ConceptScheme .\n", "lean scheme" );
} while ( ($s1,$s2) = each(%schemeuris) );

$skos = SKOS::Simple->new( base => 'http://example.org/', notation => 'unique' );
$skos->addConcept( notation => '123' );
$ttl = $skos->turtleConcepts( scheme => 1 );
ok( $ttl =~ /skos:topConceptOf/ and not $ttl =~ /skos:inScheme/ );
$ttl = $skos->turtleConcepts( top => 0 );
ok( not $ttl =~ /skos:topConceptOf/ and $ttl =~ /skos:inScheme/ );
$ttl = $skos->turtleConcepts( top => 0, scheme => 0 );
ok( not $ttl =~ /skos:topConceptOf/ and not $ttl =~ /skos:inScheme/ );
$ttl = $skos->turtleConcepts( scheme => 2 );
ok( $ttl =~ /skos:topConceptOf/ and $ttl =~ /skos:inScheme/ );

is( $skos->turtleConcepts( lean => 1 ), $skos->turtleConcepts( top => 0, scheme => 1 ) );

$skos->addConcept( notation => '456', broader => '123' );
$ttl = $skos->turtleConcepts();
ok( $ttl =~ /skos:topConceptOf/ and $ttl =~ /skos:inScheme/ );

$skos = SKOS::Simple->new( identity => 'label', language => 'de' );
is ( $skos->concept( label => 'foo' ), 'foo' );
is ( $skos->concept( notation => 'x', label => 'bar' ), 'bar' );
is ( $skos->concept( notation => 'x' ), '' );


my $id = $skos->addConcept( label => 'hi' ); #{ 'de' => $label }; 
is ( $id, 'hi' );
#print $skos->concepts;

__END__

# $skos = SKOS::Simple->new( language => 'de', notation => 'unique' );
# $skos->addConcept( notation => 'x', scopeNote => 'test' );

$skos = SKOS::Simple->new( notation => 'unique', namespaces => { foo => 'my:id:' } );
$id = $skos->addConcept( notation => '0', 'foo:bar' => 'xxx' );
print $skos->turtleConcept( $id );
#print 
#use Data::Dumper;
#print Dumper($skos)."\n";


# print $skos->turtleConcept('x', top => 0);
# use Data::Dumper;
# print Dumper($skos);

$skos = SKOS::Simple->new( notation => 'unique' );
$skos->addConcept( notation => 'x', pref => { en => 'X1', 'fr' => 'X2' } );
# print $skos->turtleConcept('x');

