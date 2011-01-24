#!perl -T

use Test::More tests => 1;

use SKOS::Simple qw(skos);

isa_ok( skos(), 'SKOS::Simple' );

