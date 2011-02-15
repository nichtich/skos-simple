use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name        => 'SKOS::Simple',
    license            => 'perl',
    dist_author        => 'Jakob Voss <voss@gbv.de>',
    all_from           => 'lib/SKOS/Simple.pm',
    test_requires => {
        'Test::More' => 0,
    },
    recommends         => { 'RDF::Trine' => '0.130' },
    add_to_cleanup     => [ 'SKOS::Simple-*' ],
    create_makefile_pl => 'traditional',
);

$builder->create_build_script();
