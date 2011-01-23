package SKOS::Simple;

use strict;
use warnings;

=head1 NAME

SKOS::Simple - SKOS with entailment and without package dependencies

=cut

use Scalar::Util qw(blessed reftype);
use Carp;

our $VERSION = '0.0.5';

=head1 DESCRIPTION

This module provides a simple class to create and handle Simple Knowledge
Organization Systems (thesauri, classifications, etc.) in SKOS. In contrast
to most RDF-related modules, SKOS::Simple does not depend on any non-core 
modules, so you can install it by just copying one file. The module 
implements basic entailment rules of the SKOS standard without the burden
of a full RDF reasoning engine (actually this module internally does not 
use RDF, which is overrated anyway). For this reason the set of possible 
SKOS schemes, that can be handled by SKOS::Simple is limited by some basic 
assumptions. 

The current version of this class is optimized form creating and 
serializing valid SKOS schemes, but not for reading and modifying them.
A common use case of SKOS::Simple is to transform a given terminology
from some custom format to SKOS, which is then 
L<serialized|/"SERIALIZATION METHODS"> in Terse RDF Triple language
(Turtle). You can then publish the Turtle data
and/or process them with general RDF and SKOS tools.

=head1 CURRENT STATE

The current version of this module aims B<at classifications only>! 
Support for thesauri will be implemented later (or just write your 
own patch and send me for inclusion in SKOS::Simple).

=head1 ASSUMPTIONS

An instance of SKOS::Simple holds exactely one skos:ConceptScheme with
the following properties:

=over 4

=item 

All concepts share a common URI base. By default this common prefix is
also the URI of the concept scheme as whole.

=item

All concepts must be identifyable by a unique string, that is refered
to as the concept identifier. The URI of a concept is build of the
common URI prefix and the concept's identifier. The identifier must
either be the skos:notation (so every concept must have one), or the
skos:prefLabel in one fixed language for all concepts.

=item

Empty strings as literal values are ignored. In most cases you can use
C<undef> and C<""> interchangeably.

=item

All notations have the same Datatype URI (this may be changed).

=item

...sure there are some more limitations...

=back

=head1 USAGE

  my $skos = SKOS::Simple->new( 
      base => 'http://example.com/mykos/',
      title => 'My little Knowledge Organization System' 
  );

  ....

  print $skos->turtle;

SKOS::Simple only supports a limited set of possible RDF statements.
To add more RDF data, you can use the L<serializing functions|/FUNCTIONS>:

  use SKOS::Simple qw(:turtle);

  print $skos->turtle_namespaces 
      . $skos->turtle_base;       # unless you already used $skos->turtle
  
  .... 

=cut

use base 'Exporter';
our %EXPORT_TAGS = ( 
    turtle => [qw(turtle_literal turtle_statement turtle_uri)] 
);
our @EXPORT_OK = @{$EXPORT_TAGS{turtle}};

our %NAMESPACES = (
   skos => 'http://www.w3.org/2008/05/skos#',
#   xsd  => 'http://www.w3.org/2001/XMLSchema#',
   dc   => 'http://purl.org/dc/elements/1.1/',
#   dct  => 'http://purl.org/dc/terms/',
#   foaf => 'http://xmlns.com/foaf/0.1/',
);

=head1 METHODS

=head2 new( [ %properties ] )

Creates a new concept scheme with some given properties.

=over 4

=item base

The URI prefix that is used for all concepts.
This property is required.

=item scheme

The URI of the whole concept scheme (C<skos:conceptScheme>).
By default the C<base> property is used as concept scheme URI.

=item namespaces

=item title

=item description

=item language

=item hierarchy

=back

=cut

sub new {
    my $class = shift;
    my (%arg) = @_;

    my $self = bless( { 
        concepts    => { },
        related     => { },
        top         => { }, # ids of top concepts
        hierarchy   => ($arg{hierarchy} || ""), # tree|multi|
        base        => ($arg{base} || ""),  # TODO: check URI and croak if missing
        scheme      => ($arg{scheme} || ""),
        title       => ($arg{title} || ""), # TODO: multilang ? 
        namespaces  => ($arg{namespaces} || { }), # TODO: count usage 
        language    => ($arg{language} || ""),    # TODO
        description => ($arg{description} || ""), # TODO
    }, $class );

    if ( $self->{scheme} eq $self->{base} ) {
        $self->{scheme} = "";
    } elsif ( $self->{scheme} ne "" ) {
        # TODO: croak if no valid URI
    }

    my $scheme = $self->{scheme};
    $scheme = "" if $scheme eq $self->{base};
        my $suri = $self->{scheme};
        $suri = "" if $suri eq $self->{base};

    $self->{namespaces}->{skos} = $NAMESPACES{skos}
        unless $self->{namespaces}->{skos};

    $self->{namespaces}->{dc} = $NAMESPACES{dc}
        if ( ($self->{title} || $self->{description}) && not $self->{namespaces}->{dc});

    return $self;
}

sub _values {
    my @values = map { (reftype($_) && reftype($_) eq 'ARRAY') ? @$_ : $_ } @_;
    @values = grep { $_ and $_ ne '' } @values;
    return @values;
}

=head2 add_concept ( %properties )

Adds a concept with given properties. The only mandatory is the identifying
property to be used as concept id (notation or label). If there already is 
a concept with the same id, both are merged.

=cut

sub add_concept {
    my $self = shift;
    my %arg  = @_;

    my $nocheck   = $arg{_nocheck};
    my @notation  = _values( $arg{notation} );
    my @label     = _values( $arg{label} );
    my @scopeNote = _values( $arg{scopeNote} );
    my @related   = _values( $arg{related} );
    my @broader   = _values( $arg{broader} );
    my @narrower  = _values( $arg{narrower} );

    my $id = $notation[0] || return;

    my @reverse = ( _nocheck => 1, 'notation' );

    $self->{concepts}->{$id} = { 
        notation  => [ ],
        label     => [ ],
        scopeNote => [ ],
        broader   => { },
        narrower  => { },
        related   => { }
    } unless $self->{concepts}->{$id};
    my $concept = $self->{concepts}->{$id};

    if (@notation) {
        push @{ $concept->{notation} }, @notation; # TODO: uniq
    }
    if (@label) {
        push @{ $concept->{label} }, @label; # TODO: uniq
    }

    if (@scopeNote) {
        push @{ $concept->{scopeNote} }, @scopeNote;
        # TODO: add [default] language
        # TODO: entails skos:note
    }

    croak "concept <$id> cannot have broader if it is top concept!"
        if ( @broader && $self->{top}->{$id} );

    foreach my $i (@related) { 
        next if $concept->{related}->{$i};
        $self->add_concept( related => $id, @reverse, $i ) unless $nocheck;
        $concept->{related}->{$i} = 1;
    }
    foreach my $i (@broader) {
        next if $concept->{broader}->{$i};
        if ( $self->{hierarchy} eq 'tree' ) {
            croak "tree property violated by <$id> skos:broader <$i>"
                if %{$concept->{broader}};
        } # TODO: if 'multi[tree]': detect loops
        
        $self->add_concept( narrower => $id, @reverse, $i ) unless $nocheck;
        $concept->{broader}->{$i} = 1;
    }
    foreach my $i (@narrower) {
        next if $concept->{narrower}->{$i};
        $self->add_concept( broader => $id, @reverse, $i ) unless $nocheck;
        $concept->{narrower}->{$i} = 1;
    }
}

=head2 top_concepts ( [ @ids ] )

Marks one or more concepts as top concepts. The given concepts must 
already exist and must not have any broader concepts. Without parameters, 
this methods returns a list of all top concept identifiers. Unless you
explicitly specify top concepts, this is the list of all concepts
without broader concepts. As soon as you explicitly set some top
concepts, these will be the I<only> top concepts. You can reset the 
top concepts to all concepts without broader concepts, provide C<undef>
as only argument.

=cut

sub top_concepts {
    my $self = shift;
    unless ( @_ ) {
        return keys %{ $self->{top} } if %{ $self->{top} };
        return grep { 
            not %{$self->{concepts}->{$_}->{broader}}
        } keys %{$self->{concepts}};
    }
    if ( @_ == 1 && not defined $_[0] ) {
        $self->{top} = { };
        return;
    }
    foreach my $id ( @_ ) {
        croak "Unknown concept <$id>" unless $self->{concepts}->{$id};
        next if $self->{top}->{$id};
        croak "Concept <$id> must not have broader to be top concept"
            if keys %{ $self->{concepts}->{broader} };
        $self->{top}->{$id} = 1;
    }
}

=head2 has_concept ( $id )

Returns whether there is a concept of the given id.

=cut

sub has_concept {
    my $self = shift;
    my %arg = (@_ == 1) ? ( id => $_[0] ) : @_;
    return exists $self->{concepts}->{ $arg{id} }
        if defined $arg{id};
    # TODO: ask by other properties (URI, broader, narrower, is-top, etc.)
    return 0;
}

=head2 size

Returns the number of concepts.

=cut

sub size {
    my $self = shift;
    return scalar keys %{ $self->{concepts} };
}

=head2 concepts

Returns a list of all concept's ids.

=cut

sub concepts {
    my $self = shift;
    return keys %{ $self->{concepts} };
}

=head1 SERIALIZATION METHODS

The following methods serialize the concept scheme or parts of it in 
L<Terse RDF Triple language|http://www.w3.org/TeamSubmission/turtle/>
(RDF/Turtle). A valid serialization must start with some namespace
declarations, and a base declaration. Both are only included by the
C<turtle> method, but they can also be requested independently.
All return values end with a newline unless they are the empty string.

A later version may also support 'hashref' format for serializing.

=head2 turtle ( [ %options ] )

Returns a full Turtle serialization of this concept scheme.
The return value is equivalent to:

    $skos->turtle_namespaces .
    $skos->turtle_base .
    $skos->turtle_scheme .
    $skos->turtle_concepts

The parameter C<< lean => 1 >> enables a lean serialization, which 
does not include infereable RDF statements. Other parameters are
passed to C<turtle_scheme> and C<turtle_concepts> as well.

=cut

sub turtle {
    my ($self,%opt) = @_;

    return 
        $self->turtle_namespaces .
        $self->turtle_base .
        $self->turtle_scheme( %opt ) .
        $self->turtle_concepts( %opt );
}

=head2 turtle_namespaces

Returns Turtle syntax namespace declarations for this scheme.

=cut

sub turtle_namespaces {
    my $self = shift;
   
    my @lines;
    foreach my $name (keys %{$self->{namespaces}}) {
        push @lines, "\@prefix $name: <" . $self->{namespaces}->{$name} . "> .";        
    }

    return join("\n", @lines) . "\n";
}

=head2 turtle_base

Returns a Turtle URI base declaration for this scheme.
An empty string is returned if no URI base is set.

=cut

sub turtle_base {
    my $self = shift;

    return "" if $self->{base} eq "";
    return "\@base <" . $self->{base} . "> .\n" 
}


=head2 turtle_scheme ( [ top => 0 ] )

Returns RDF statements about the concept scheme in Turtle syntax.
Details about concepts or namespace/base declarations are not included.
The option C<< top => 0 >> (enabled by default) can be used to supress 
serializing the C<skos:hasTopConcept> property.

=cut

sub turtle_scheme {
    my ($self,%opt) = @_;
    $opt{top} = 1 unless exists $opt{top};

    my %stm = ( 'a' => 'skos:ConceptScheme' );
   
    $stm{'dc:title'} = $self->turtle_literal( $self->{title} )
        unless $self->{title} eq '';

    # note that lean => 1 does not imply top => 0 !
    if ( $opt{top} ) { 
        my @top = $self->top_concepts();
        $stm{'skos:hasTopConcept'} = [ map { "<$_>" } @top ];
    }

    return $self->turtle_statement( "<" . $self->{scheme} . ">", %stm );
}

=head2 turtle_concept ( $id [, %options ] )

Returns a concept in Turtle syntax. With option C<< top => 0 >> you can disable
serializing the C<skos:topConceptOf> property. By default, each concept is
connected to its concept scheme with either C<skos:topConceptOf>, or with
C<skos:inScheme>. With option C<< scheme => 0 >> you can disable serializing
the latter property. With C<< scheme => 2 >> the property C<skos:inScheme>
is also included in the serialization if C<skos:topConceptOf> is given,
although the former can be derived as super-property of the latter.

=cut

sub turtle_concept {
    my ($self,$id,%opt) = @_;
    $opt{top} = 1 unless exists $opt{top};
    $opt{top} = 0 if $opt{lean};
    $opt{scheme} = 1 unless exists $opt{scheme};

    my $c = $self->{concepts}->{$id};

    my %stm = ( 'a' => 'skos:Concept' );

    # TODO: multiple notations
    if ($c->{notation}) {
        $stm{'skos:notation'} = $self->turtle_literal( $c->{notation}->[0] );
    }

    # TODO: prefLabel

    if ($c->{label}) {
        $stm{'dc:title'} = $self->turtle_literal( $c->{label}->[0] );
    }

    foreach my $rel (qw(broader narrower related)) {
        next unless %{$c->{$rel}};
        $stm{"skos:$rel"} = [ map { "<$_>" } keys %{$c->{$rel}} ];
    }

    $stm{'skos:scopeNote'} = [ 
        map { $self->turtle_literal( $_ ) } @{ $c->{scopeNote} } 
    ];

    my $is_top = 0;
    if ( $opt{top} ) { 
        if ( (keys %{ $self->{top} }) ? $self->{top}->{$id} : not %{$c->{broader}} ) {
            $stm{"skos:topConceptOf"} = "<" . $self->{scheme} . ">";
            $is_top = 1;
        }
    }

    if ( $opt{scheme} - $is_top > 0 ) {
        $stm{'skos:inScheme'} = '<' . $self->{scheme} . '>';
    }

    return $self->turtle_statement( "<$id>", %stm );
}

=head2 turtle_concepts ( [ %options ] )

Returns all concepts in Turtle syntax.

=cut

sub turtle_concepts {
    my ($self,%opt) = @_;
    delete $opt{id};

    return join( "\n", 
        map { $self->turtle_concept( $_, %opt ) } 
        keys %{ $self->{concepts} } );
}

=head1 FUNCTIONS

The following methods can also be used as functions for Turtle serialization.

=head2 turtle_statement ( $subject, $predicate => $object [, ... ] )

Returns a (set of) RDF statements in Turtle syntax. Subject and predicate
parameters must be strings. Object parameters must either be strings or
arrays of strings. This function does not check or validate parameter
values - all strings must be valid parts of Turtle syntax!

=cut

sub turtle_statement {
    shift if blessed($_[0]);
    my ($subject, %statements) = @_;

    my @s = grep { defined $_ } map {
        my ($p,$o) = ($_,$statements{$_});
        $o = join(", ", @$o) if ref($o);
        (defined $o and $o ne '') ? "$p $o" : undef;
    } keys %statements;

    return "" unless @s;

    return "$subject " . join(" ;\n" , shift @s, map { "    $_" } @s) . " .\n";
}

=head2 turtle_literal ( $string )

Escapes a string as literal in Turtle syntax.

=cut

sub turtle_literal {
    shift if blessed($_[0]);
    my $value = shift;
    return "" if not defined $value or $value eq '';
    # TODO: datatype or language tag
    my %ESCAPED = ( "\t" => 't', "\n" => 'n', 
        "\r" => 'r', "\"" => '"', "\\" => '\\' );
    $value =~ s/([\t\n\r\"\\])/\\$ESCAPED{$1}/sg;
    return "\"$value\"";
}

=head2 turtle_uri ( $uri )

...

=cut

sub turtle_uri {
    shift if blessed($_[0]);
    my $value = shift;
    return "<$value>"; # TODO: check uri?
}

1;

=head1 SEE ALSO

The SKOS ontology and its semantics is defined in 
L<http://www.w3.org/TR/skos-primer> and 
L<http://www.w3.org/TR/skos-reference>.

=head1 AUTHOR

Jakob Voss C<< <jakob.voss@gbv.de> >>

=head1 LICENSE

Copyright (C) 2010 by Verbundzentrale Goettingen (VZG) and Jakob Voss

This library is free software; you can redistribute it and/or modify it
under the same terms as Perl itself, either Perl version 5.8.8 or, at
your option, any later version of Perl 5 you may have available.

In addition you may fork this library under the terms of the 
GNU Affero General Public License.

=cut
