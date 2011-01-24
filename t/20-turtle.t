#!perl -T

use Test::More tests => 7;

use SKOS::Simple qw(:turtle);

is( turtle_literal("x\t\"\n"), '"x\\t\\"\\n"', 'turtle_literal' );
is( turtle_literal("y",'en-CA'), '"y"@en-CA', 'turtle_literal' );
is( turtle_literal("y",'my:uri'), '"y"^^<my:uri>', 'turtle_literal' );

my $ttl = turtle_statement( '<>', 'dc:title' => '"foo"' );
is( $ttl, "<> dc:title \"foo\" .\n", "turtle_statement" );

foreach ( undef, [ ], "" ) {
    $ttl = turtle_statement( '<>', 'dc:title' => $_ );
    is( $ttl, "" );
}

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

