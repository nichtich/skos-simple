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

my @examples = qw(iconclass thesoz); 

foreach my $name (@examples) {
    diag("$name example");
    my $skos = eval { require "t/data/$name.pl"; };
    isa_ok( $skos, 'SKOS::Simple' );
    my $file = "t/data/$name.ttl";
    my $ttl = do { local ( @ARGV, $/ ) = $file; <> };
    ok ( $skos ) &&
    is_rdf ($skos->turtle, 'turtle', $ttl, 'turtle', 'as expected: '.$file );
}
