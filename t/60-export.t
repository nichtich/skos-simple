#!perl -T

# Test exporting functions from SKOS::Simple

use Test::More tests => 2;

use SKOS::Simple qw(skos);
isa_ok( skos(), 'SKOS::Simple' );

SKOS::Simple->import(':turtle');

is( turtle_literal("x\t\"\n"), '"x\\t\\"\\n"', 'turtle_literal' );

