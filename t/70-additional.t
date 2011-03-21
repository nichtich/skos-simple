use strict;
use warnings;
use Test::More;

my ($v_trine,$v_test) = (0.130,0.20);
eval "use RDF::Trine $v_trine";
eval "use RDF::Trine::Parser" unless $@;
eval "use Test::RDF $v_test" unless $@;
if ( $@  ) {
    plan skip_all => 
        "RDF::Trine $v_trine and Test::RDF $v_test required";
    exit;
} else {
    plan 'no_plan';
}

use SKOS::Simple;

my @examples = qw(iconclass); 

foreach my $name (@examples) {
    diag("$name example");
    my $skos = do "t/data/$name.pl";
    my $file = "t/data/$name.ttl";
    my $ttl = do { local ( @ARGV, $/ ) = $file; <> };
    is_rdf ($skos->turtle, 'turtle', $ttl, 'turtle', 'as expected: '.$file );
}

__END__
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

