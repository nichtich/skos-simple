#!perl -T

### Test Turtle serializer functions

use strict;
use Test::More qw(no_plan);

use SKOS::Simple qw(:turtle);

is( turtle_literal("x\t\"\n"), '"x\\t\\"\\n"', 'turtle_literal' );
is( turtle_literal("y",'en-CA'), '"y"@en-CA', 'turtle_literal with language' );
is( turtle_literal("y", lang => 'en-CA'), '"y"@en-CA', 'turtle_literal with language' );
is( turtle_literal("y", 'my:uri'), '"y"^^<my:uri>', 'turtle_literal width datatype' );
is( turtle_literal("y", type => 'my:uri'), '"y"^^<my:uri>', 'turtle_literal width datatype' );
is( turtle_literal( [qw(a " c)] ), '"a", "\"", "c"', 'turtle_literal width list' );
is( turtle_literal( [qw(" x)], 'es' ), '"\""@es, "x"@es', 'turtle_literal width list and language' );
is( turtle_literal( [qw(" x)], 'my:x' ), '"\""^^<my:x>, "x"^^<my:x>', 'turtle_literal width list and datatype' );

is ( turtle_literal_list( "0" ), '"0"', 'turtle_literal_list' );
is ( turtle_literal_list( { } ), '', 'turtle_literal_list' );
is ( turtle_literal_list( undef ), '', 'turtle_literal_list' );
is ( turtle_literal_list( qw(a " c) ), '"a", "\"", "c"', 'turtle_literal_list' );
is ( turtle_literal_list( [qw(a " c)] ), '"a", "\"", "c"', 'turtle_literal_list' );
is ( turtle_literal_list( { fr => 'a' } ), '"a"@fr', 'turtle_literal_list' );
is ( turtle_literal_list( { fr => ['a','"'] } ), '"a"@fr, "\""@fr', 'turtle_literal_list' );

foreach ( undef, [ ], "" ) {
    is( turtle_literal( $_ ), "", 'empty literal' );
    is( turtle_literal_list( $_ ), "", 'empty literal list' );
    is( turtle_statement( '<>', 'dc:title' => $_ ), "", 'empty statement' );
}

my $ttl = turtle_statement( '<>', 'dc:title' => '"foo"' );
is( $ttl, "<> dc:title \"foo\" .\n", "turtle_statement" );


__END__

  print turtle_statement( 
    "<$uri>",
      "a" => "<http://purl.org/ontology/bibo/Document>",
      "dc:creator" => [ 
          '"Terry Winograd"', 
          '"Fernando Flores"' 
      ],
      "dc:date" => turtle_literal( "1987", type => "xs:gYear" ),
      "dc:title" =>
          { en => "Understanding Computers and Cognition" },
      "dc:decription" => undef
  );

