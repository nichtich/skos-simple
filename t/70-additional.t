use strict;
use warnings;
use Test::More;

my $min_version = 0.130;
eval "use RDF::Trine $min_version";
eval "use RDF::Trine::Parser" unless $@;
if ( $@  ) {
    plan skip_all => "RDF::Trine $min_version required for additional tests";
    exit;
} else {
    plan 'no_plan';
}

use SKOS::Simple;

my %files = (
#    't100.nt' => {
#        'properties' => { 
#            base => 'http://www.museumsvokabular.de/museumvok/moebel/',
#            notation => 'unique',
#        }
#    },
#    't/data/systematik-moebel.nt' => {
#        'properties' => { 
#            base => 'http://www.museumsvokabular.de/museumvok/moebel/',
#        }
#    }
);

foreach my $file ( keys %files ) {
    my $model = RDF::Trine::Model->new;

    print "parsing SKOS data ...\n";
    RDF::Trine::Parser->parse_file_into_model( "file://$file", $file, $model );
    my $hash = $model->as_hashref;

    my $skos = SKOS::Simple->new( %{$files{$file}->{properties}} );
    $skos->add_hashref( $hash );

    print $skos->turtle;
}

ok(1);
