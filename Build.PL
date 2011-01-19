use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'SKOS::Simple',
    license             => 'perl',
    dist_author         => 'Jakob Voss <voss@gbv.de>',
    dist_version_from   => 'lib/SKOS/Simple.pm',
    build_requires => {
        'Test::More' => 0,
    },
    add_to_cleanup      => [ 'SKOS::Simple-*' ],
    create_makefile_pl => 'traditional',
);

$builder->create_build_script();
