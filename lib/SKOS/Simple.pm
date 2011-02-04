package SKOS::Simple;

use strict;
use warnings;

=head1 NAME

SKOS::Simple - SKOS with entailment and without package dependencies

=cut

use Scalar::Util qw(blessed reftype);
use Carp;

our $VERSION = '0.0.6';

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

The current version of this class is optimized form creating and serializing
valid SKOS schemes, but not for reading and modifying them. A common use case
of SKOS::Simple is to transform a given terminology from some custom format 
to SKOS, which is then L<serialized|/"SERIALIZATION METHODS"> in Terse RDF 
Triple language (Turtle). You can then publish the Turtle data and/or process
them with general RDF and SKOS tools.

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

=head1 SYNOPSIS

  my $skos = SKOS::Simple->new( 
      base  => 'http://example.com/kos/',
      title => 'My little Knowledge Organization System',
       
  );

  ....
  $skos->add_concept( pref => { en => 'foo', ru => 'baz' } );
  $skos->add_concept( notation => '42.X-23' );
  ...

  print $skos->turtle;

SKOS::Simple only supports a limited set of possible RDF statements.
To add more RDF data, you can use the L<serializing functions|/FUNCTIONS>:

  use SKOS::Simple qw(:turtle);

  print $skos->turtle;

    
  .... 

=cut

use base 'Exporter';
our %EXPORT_TAGS = ( 
    turtle => [qw(turtle_literal turtle_literal_list turtle_statement turtle_uri)], 
    all    => [qw(turtle_literal turtle_literal_list turtle_statement turtle_uri skos)]
);
our @EXPORT_OK = @{$EXPORT_TAGS{all}};

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

The URI prefix that is used for all concepts (not required but recommended).

=item scheme

The URI of the whole concept scheme (C<skos:conceptScheme>).
By default the C<base> property is used as concept scheme URI.

=item title

Title(s) of the concept scheme. Must be either a string or a
hash that maps language tags to strings.

=item namespaces

An optional hash with additional namespaces. You can also override standard
namespaces (e.g. C<< skos => 'http://www.w3.org/2004/02/skos/core#' >>).
This explicitly specified namespaces are always included as C<< @prefix >>
in the Turtle output.

=item description

...

=item language

...

=item hierarchy

Either C<tree> or C<multi> or C<none> (default). 
At the moment only C<tree> is supported.

=back

=cut

sub new {
    my $class = shift;
    my (%arg) = @_;

    my $self = bless( { 
        concepts    => { },

        related     => { },
        top         => { }, # ids of top concepts

        u_notation => { },
        u_label    => { },

        hierarchy   => ($arg{hierarchy} || ""), # tree|multi|
        base        => ($arg{base} || ""),
        scheme      => ($arg{scheme} || ""),
        title       => ($arg{title} || ""),
        namespaces  => ($arg{namespaces} || { }), # TODO: count usage (?) 
        language    => ($arg{language} || "en"),  # TODO: check tag
        description => ($arg{description} || ""), # TODO: add
        identity    => ($arg{identity} || ""),
        label       => ($arg{label} || ""),
        notation    => ($arg{notation} || ""),
    }, $class );

    # TODO: croak "base of scheme missing" unless $self->{base};
    croak "base must be an URI" unless uri_or_empty($self->{base});
    croak "scheme must be an URI" unless uri_or_empty($self->{scheme});
    $self->{scheme} = "" if $self->{scheme} eq $self->{base};

    if ( $self->{notation} eq 'unique' ) {
        $self->{identity} = 'notation' unless $self->{identity}; 
    }

    if ( $self->{label} eq 'unique' ) {
        $self->{identity} = 'label' unless $self->{identity}; 
    }

    $self->{identity} = 'label' if $self->{identity} eq '';
    croak "Concepts must have identity by 'notation' or by 'label'"
        unless $self->{identity} =~ /^(notation|label)$/;

    $self->{ $self->{identity} } = 'unique';


    $self->{namespaces}->{skos} = $NAMESPACES{skos}
        unless $self->{namespaces}->{skos};

    $self->{namespaces}->{dc} = $NAMESPACES{dc}
        if ( ($self->{title} || $self->{description}) && not $self->{namespaces}->{dc});

    # Add default language, if title without language given
    my $lang = $self->{language};
    if ($self->{title} && not ref($self->{title}) && $lang) {
        $self->{title} = { $lang => $self->{title} };
    }

    return $self;
}

sub _values {
    # TODO: also allow hashref (?)
    my @values = map { (reftype($_) && reftype($_) eq 'ARRAY') ? @$_ : $_ } @_;
    @values = grep { $_ and $_ ne '' } @values;
    return @values;
}

=head2 add_concept ( %properties )

Adds a concept with given properties. Only the identifying property to be 
used as concept id (notation or label) is mandatory. If there already is 
a concept with the same id, both are merged.

Returns the id of the added or modfied concept.

=cut

sub add_concept {
    my $self = shift;
    my %arg  = @_;

    # only for internal use
    my $nocheck = delete $arg{_nocheck}; 

    # connections to other concepts
    my @related   = _values( delete $arg{related} );
    my @broader   = _values( delete $arg{broader} );
    my @narrower  = _values( delete $arg{narrower} );

    my $notation  = delete $arg{notation};
    $notation = "" unless defined $notation;

    my @example = _values( delete $arg{example} );

    my $lang = $self->{language};

    #my @labels    = _values( delete $arg{label} );
    my $labels = delete $arg{label};
    if (not defined $labels or $labels eq '') {
        $labels = { };
    } elsif( not ref($labels) ) {
        $labels = { $lang => $labels }; # TODO: array
    }

    my @scopeNote = _values( delete $arg{scopeNote} ); # scopeNote or note?

    my $prefLabel = $labels->{$lang};
    $prefLabel = "" unless defined $prefLabel;

    my $id = $self->concept_id( notation => $notation, label => $labels );

    croak 'Missing ' . $self->{identity} . ' as concept identifier '
        unless defined $id;

    my $concept = $self->{concepts}->{$id};
    unless ($self->{concepts}->{$id}) {
        $concept = $self->{concepts}->{$id} = { 
            notation  => "",
            label     => [ ],
            pref      => { },
            scopeNote => [ ],
            broader   => { },
            narrower  => { },
            related   => { }
        };
    }

    if ( $self->{identity} eq 'notation' ) {
        croak 'Concepts must have a notation' if $notation eq '';
        $concept->{notation} = $notation; # TODO: multiple notations
    } elsif ( $self->{identity} eq 'label' ) {
        # croak 'Concepts must have one label' unless @labels == 1;
    }

=head1
    # label is not id, but unique
    if ( $self->{label} eq 'unique' and $self->{identity} ne 'label') {
        if (
        $concept->{pref}
                push $concept->{pref}->{$lang} = $prefLabel; # TODO; check if already different!
        @labels = ();
    }

#        croak 'Concepts must be unique per notation' 
#            if $self->{u_notation}->{ $notation };
#        $self->{u_notation}->{ $notation } = $id;
#    }

    } elsif ( $self->{label} eq 'unique' ) {
        # TODO: multiple labels
        croak 'Concepts must have a label' unless @labels;
        croak 'Concepts must have only one label' if @labels > 1;

        croak 'Concepts must be unique per notation' 
            if $self->{u_label}->{ $lang }->{ $labels[0] };
        $self->{u_label}->{ $lang }->{ $labels[0] } = $id;
=cut
    
    my @reverse = ( _nocheck => 1, 'notation' ); # TODO: prefLabel

    if (%$labels) {
        push @{ $concept->{label} }, values %{$labels};      # TODO: uniq
    }

    if (@example) {
        push @{ $concept->{example} }, @example;
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

    return $id;
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

=head2 concept_id ( notation => $notation | label => $label )

Returns the identifier of a concept with given notation and/or
label. The concept does not need to exist.

=cut

sub concept_id {
    my ($self,%arg) = @_;

    return unless %arg;

    if ( $self->{identity} eq 'notation' ) {
        return unless defined $arg{notation};
        return if $arg{notation} eq '';
        return $arg{notation};
    } else { # $self->{identity} eq 'label'
        my $label = $arg{label};

        if ( ref($label) and ref($label) eq 'HASH' ) {
            $label = $label->{ $self->{language} };
        }

        $label = undef if (defined $label and $label eq '');

        return $label;
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

    $skos->namespaces_turtle .
    $skos->base_turtle .
    $skos->scheme_turtle .
    $skos->concepts_turtle

The parameter C<< lean => 1 >> enables a lean serialization, which 
does not include infereable RDF statements. Other parameters are
passed to C<scheme_turtle> and C<concepts_turtle> as well.

=cut

sub turtle {
    my ($self,%opt) = @_;

    return 
        $self->namespaces_turtle .
        $self->base_turtle .
        $self->scheme_turtle( %opt ) .
        $self->concepts_turtle( %opt );
}

=head2 namespaces_turtle

Returns Turtle syntax namespace declarations for this scheme.

=cut

sub namespaces_turtle {
    my $self = shift;
   
    my @lines;
    foreach my $name (keys %{$self->{namespaces}}) {
        push @lines, "\@prefix $name: <" . $self->{namespaces}->{$name} . "> .";        
    }

    return join("\n", @lines) . "\n";
}

=head2 base_turtle

Returns a Turtle URI base declaration for this scheme.
An empty string is returned if no URI base is set.

=cut

sub base_turtle {
    my $self = shift;

    return "" if $self->{base} eq "";
    return "\@base <" . $self->{base} . "> .\n" 
}


=head2 scheme_turtle ( [ top => 0 ] )

Returns RDF statements about the concept scheme in Turtle syntax.
Details about concepts or namespace/base declarations are not included.
The option C<< top => 0 >> (enabled by default) can be used to supress 
serializing the C<skos:hasTopConcept> property.

=cut

sub scheme_turtle {
    my ($self,%opt) = @_;
    $opt{top} = 1 unless exists $opt{top};

    my %stm = ( 'a' => 'skos:ConceptScheme' );
   
    $stm{'dc:title'} = $self->turtle_literal_list( $self->{title} )
        unless $self->{title} eq '';

    # note that lean => 1 does not imply top => 0 !
    if ( $opt{top} ) { 
        my @top = $self->top_concepts();
        $stm{'skos:hasTopConcept'} = [ map { "<$_>" } @top ];
    }

    return $self->turtle_statement( "<" . $self->{scheme} . ">", %stm );
}

=head2 concept_turtle ( $id [, %options ] )

Returns a concept in Turtle syntax. With option C<< top => 0 >> you can disable
serializing the C<skos:topConceptOf> property. By default, each concept is
connected to its concept scheme with either C<skos:topConceptOf>, or with
C<skos:inScheme>. With option C<< scheme => 0 >> you can disable serializing
the latter property. With C<< scheme => 2 >> the property C<skos:inScheme>
is also included in the serialization if C<skos:topConceptOf> is given,
although the former can be derived as super-property of the latter.

=cut

sub concept_turtle {
    my ($self,$id,%opt) = @_;
    $opt{top} = 1 unless exists $opt{top};
    $opt{top} = 0 if $opt{lean};
    $opt{scheme} = 1 unless exists $opt{scheme};

    my $c = $self->{concepts}->{$id};

    my %stm = ( 'a' => 'skos:Concept' );

    # TODO: Support multiple notations
    if ( $c->{notation} ne '' ) {
        $stm{'skos:notation'} = $self->turtle_literal( $c->{notation} );
    }

    # TODO: prefLabel subPropertyOf rdfs:label ?
    $stm{'skos:prefLabel'} = $c->{pref};

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

    if ( $c->{example} ) {
        $stm{'skos:example'} = turtle_literal_list( $c->{example} );
    }
    # foreach my $p ( keys %{$c->{custom}} ) {
    #    $stm = 
    #}

    return $self->turtle_statement( "<$id>", %stm );
}

=head2 concepts_turtle ( [ %options ] )

Returns all concepts in Turtle syntax.

=cut

sub concepts_turtle {
    my ($self,%opt) = @_;
    delete $opt{id};

    return join( "\n", 
        map { $self->concept_turtle( $_, %opt ) } 
        keys %{ $self->{concepts} } );
}

=head1 EXPORTED FUNCTIONS

With C<skos> you can export an abbreviation for C<< SKOS::Simple->new >>.

=head2 skos

This is just a shortcut for C<< SKOS::Simple->new >>.

=cut

sub skos { 
    SKOS::Simple->new(@_)
}

=pod

The following methods Turtle serialization can also be exported as functions 
with the C<:turtle> include parameter. Note that they do not implement a full
Turtle serializer because they don't check whether the URIs, QNames, and/or
language tags are valid. The followinge example shows some ways of use:

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

=head2 turtle_statement ( $subject, $predicate => $object [, ... ] )

Returns a (set of) RDF statements in Turtle syntax. Subject and predicate
parameters must be strings. Object parameters must either be strings or
arrays of strings. This function strips undefined values and empty strings,
but it does not further check or validate parameter values.

=cut

sub turtle_statement {
    shift if blessed($_[0]);
    my ($subject, %statements) = @_;

    my @s = grep { defined $_ } map {
        my ($p,$o) = ($_,$statements{$_});
        if ( ref($o) ) {
           if (reftype($o) eq 'HASH') {
               $o = [ map { turtle_literal($o->{$_},$_) } keys %$o ];
           }
           if (reftype($o) eq 'ARRAY') {
               $o = join(", ", @$o) if ref($o);
           } else { 
               $o = undef; 
           }
        }
        (defined $o and $o ne '') ? "$p $o" : undef;
    } keys %statements;

    return "" unless @s;

    return "$subject " . join(" ;\n" , shift @s, map { "    $_" } @s) . " .\n";
}

=head2 turtle_literal ( $string [ [ lang => ] $language | [ type => ] $datatype ] )

Returns a literal string escaped in Turtle syntax. You can optionally provide
either a language or a full datatype URI (but their values are not validated).
Returns the empty string instead of a Turtle value, if $string is undef or ""!

=cut

sub turtle_literal {
    shift if blessed($_[0]);
    my $value = shift;
    my %opt;

    if ( ref( $value ) and ref($value) eq 'ARRAY') {
        return join( ", ", map { turtle_literal( $_, @_ ) } @$value );
    }

    if ( @_ % 2 ) {
        my $v = shift;
        %opt = ($v =~ /^[a-zA-Z0-9-]+$/) ? ( lang => $v ) : ( type => $v ); 
    } else {

        %opt = @_;
        croak "Literal values cannot have both language and datatype"
            if ($opt{lang} and $opt{type});
    }

    return "" if not defined $value or $value eq '';

    my %ESCAPED = ( "\t" => 't', "\n" => 'n', 
        "\r" => 'r', "\"" => '"', "\\" => '\\' );
    $value =~ s/([\t\n\r\"\\])/\\$ESCAPED{$1}/sg;

    $value = qq("$value");

    if ($opt{lang}) {
        return $value.'@'.$opt{lang};
    } elsif ($opt{type}) {
        return $value.'^^<'.$opt{type} .'>';
    }

    return $value;
}

=head2 turtle_uri ( $uri )

Returns an URI in Turtle syntax, that is C<< "<$uri>" >>. Returns the 
empty string, if C<$uri> is C<undef>, but C<< <> >> if C<$uri> is the
empty string. In most cases you better directly write C<< "<$uri>" >>.

=cut

sub turtle_uri {
    shift if blessed($_[0]);
    my $value = shift;
    return "" unless defined $value;
    # my $value = URI->new( encode_utf8( $value ) )->canonical;
    return "<$value>";
}

=head2 turtle_literal_list ( $literal | @array_of_literals | { $language => $literal } )

Returns a list of literal strings in Turtle syntax.

=cut

sub turtle_literal_list {
    shift if blessed($_[0]);
 
    if ( ref($_[0]) and ref($_[0]) eq 'HASH') {
        my $hash = $_[0];
        return join( ", ", 
            map { turtle_literal( $hash->{$_}, lang => $_ ) } 
            keys %$hash
        );
    } elsif ( @_ > 1 ) {
        return turtle_literal( \@_ );
    } else {
        return turtle_literal( $_[0] );
    }
}

=head2 is_uri ( $uri )

Copied from L<Data::Validate::URI>, originally by Richard Sonnen.

=cut

sub is_uri{
    my $self = shift if ref($_[0]); 
    my $value = shift;
    
    return unless defined($value);
    
    # check for illegal characters
    return if $value =~ /[^a-z0-9\:\/\?\#\[\]\@\!\$\&\'\(\)\*\+\,\;\=\.\-\_\~\%]/i;
    
    # check for hex escapes that aren't complete
    return if $value =~ /%[^0-9a-f]/i;
    return if $value =~ /%[0-9a-f](:?[^0-9a-f]|$)/i;
    
    # from RFC 3986
    my($scheme, $authority, $path, $query, $fragment) = 
        ($value =~ m|(?:([^:/?#]+):)?(?://([^/?#]*))?([^?#]*)(?:\?([^#]*))?(?:#(.*))?|);
    
    # scheme and path are required, though the path can be empty
    return unless (defined($scheme) && length($scheme) && defined($path));
    
    # if authority is present, the path must be empty or begin with a /
    if(defined($authority) && length($authority)){
        return unless(length($path) == 0 || $path =~ m!^/!);
    
    } else {
        # if authority is not present, the path must not start with //
        return if $path =~ m!^//!;
    }
    
    # scheme must begin with a letter, then consist of letters, digits, +, ., or -
    return unless lc($scheme) =~ m!^[a-z][a-z0-9\+\-\.]*$!;
    
    # re-assemble the URL per section 5.3 in RFC 3986
    my $out = $scheme . ':';
    if(defined $authority && length($authority)){
        $out .= '//' . $authority;
    }
    $out .= $path;
    if(defined $query && length($query)){
        $out .= '?' . $query;
    }
    if(defined $fragment && length($fragment)){
        $out .= '#' . $fragment;
    }
    
    return $out;
    
}

=head2 uri_or_empty

=cut

sub uri_or_empty {
    return (not defined $_[0] or $_[0] eq '' or is_uri($_[0]));
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
