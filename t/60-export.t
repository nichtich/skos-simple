#!perl -T

# Test exporting functions from SKOS::Simple

use Test::More tests => 1;

use SKOS::Simple qw(skos);
isa_ok( skos(), 'SKOS::Simple' );

