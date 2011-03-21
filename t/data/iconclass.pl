#!/usr/bin/perl

# This example tests idfilter, idpattern and void

$skos = new SKOS::Simple 
    base      => "http://iconclass.org/rkd/",
    identity  => 'notation',
    idpattern => '[0-9]([0-9]([A-IK-Z]([0-9]+)?)?)?(%28%2B[0-9]+%29)?',
        # TODO: bracketed text, keys, doubling letters, structural digits
    idfilter => sub { $_ = $_[0]; s/ //g; s/\(/%28/g; s/\)/%29/g; s/\+/%2B/; $_; },
    void => 1
; 

$skos->add_concept( 
    id => '49 M 9', label => 'public library',
    # + see also: 41A251
);
$skos->add_concept( id => '49M', label => 'production of printed matter, book-production' );

# add hierarchy
$skos->add_concept( notation => '49 M 9', broader => '49M' );
$skos->add_concept( id       => '49 M',   broader => '49', label => 'education, science and learning' );
$skos->add_concept( id       => '49',     broader => '4',  label => 'Society, Civilization, Culture' );

# notation with special characters
$skos->add_concept( 
    id => '49 M 9 (+8)', 
    label => 'public library (+ plagiary ~ science)',
    broader => '49M9' );

# this should not be allowed
eval { $skos->add_concept( notation => '49J' ); };
ok( $@, 'uri pattern check' );

$skos;
