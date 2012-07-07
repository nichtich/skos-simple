#!perl -T

use strict;
use Test::More qw(no_plan);

use_ok( 'SKOS::Simple' );

my $skos = SKOS::Simple->new;
isa_ok( $skos, 'SKOS::Simple' );

### check namespace prefixes
check_ns( $skos, skos => 'http://www.w3.org/2004/02/skos/core#' );

my %ns = ( foo => 'http://example.org/' );

$skos = SKOS::Simple->new( namespaces => \%ns );
check_ns( $skos, skos => 'http://www.w3.org/2008/05/skos#', %ns );

$ns{skos} = 'http://www.w3.org/2008/05/skos#'; # override

$skos = SKOS::Simple->new( namespaces => \%ns );
check_ns( $skos, %ns );

$ns{dc} = 'http://purl.org/dc/elements/1.1/';
$skos = SKOS::Simple->new( namespaces => \%ns, title => 'Hello' );
check_ns( $skos, %ns );

### check title
like( $skos->turtle, qr/dc:title\s"Hello"/, 'simple title' );

$skos = SKOS::Simple->new( title => { en => 'X', fr => 'Y' } );
like( $skos->turtle, 
    qr/dc:title\s("X"\@en, "Y"\@fr|"Y"\@fr, "X"\@en)/,
    'one title per language' );

$skos = SKOS::Simple->new( title => 'Hola', language => 'es' );
like( $skos->turtle, qr/dc:title\s"Hola"\@es/, 'title with default language' );

### check properties
my $prop = { 'a' => '<x:y>', 'skos:subject' => 'y', 'f:oo' => 'b:ar' };
$skos = SKOS::Simple->new( properties => $prop );
is( $skos->turtleScheme, "<> a skos:ConceptScheme ;\n    f:oo b:ar .\n", 'properties' );


sub check_ns {
    my ($skos, %ns) = @_;
    my $ttl = $skos->turtle;
    my (%found) = ($ttl =~ /\@prefix\s+([^:]*):\s+<([^>]*)>/g);
    is_deeply( \%found, \%ns, 'namespaces' );
}

