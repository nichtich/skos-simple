package SKOS::Simple;

use strict;
use warnings;

=head1 NAME

SKOS::Simple - SKOS without package dependencies

=cut

use Scalar::Util qw(blessed reftype);
use Carp;

our $VERSION = '0.0.1';

=head1 DESCRIPTION

This module provides a simple class to create and handle Simple Knowledge
Organization Systems (thesauri, classifications, etc.) in SKOS. In contrast
to most RDF-related modules, SKOS::Simple does not depend on any non-core 
modules. On the other hand the set of possible SKOS schemes is limited by 
some assumptions based on best-practice rules. The current version of this
class is optimized form creating and serializing SKOS schemes, but not for
reading and modifying.

=cut

=head1 ASSUMPTIONS

=over 4

=item 

All concepts share a common URI prefix, which is the 
URI of the concept scheme.

=item

All empty strings are ignored.

=item

All notations have the same Datatype URI.

=item

...

=back

=cut

=head2 new( [ %properties ] )

Creates a new concept scheme with some given properties.

=cut

sub new {
    my $class = shift;
    my (%arg) = @_;

    my $self = bless( { 
        concepts    => { },
        topconcepts => { },
        related     => { },
        hierarchy   => ($arg{hierarchy} || ""), # tree|multi
        base        => ($arg{base} || ""),
        title       => ($arg{title} || ""), # TODO: multilang 
        namespaces  => ($arg{namespaces} || { }), 
    }, $class );

    $self->{namespaces}->{skos} = 'http://www.w3.org/2008/05/skos#'
        unless $self->{namespaces}->{skos};

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
    my @related   = _values( $arg{related} );
    my @broader   = _values( $arg{broader} );
    my @narrower  = _values( $arg{narrower} );

    my $id = $notation[0] || return;

    my @reverse = ( _nocheck => 1, 'notation' );

    $self->{concepts}->{$id} = { 
        notation => [ ],
        label    => [ ],
        broader  => { },
        narrower => { },
        related  => { }
    } unless $self->{concepts}->{$id};
    my $concept = $self->{concepts}->{$id};

    if (@notation) {
        push @{ $concept->{notation} }, @notation; # TODO: uniq
    }
    if (@label) {
        push @{ $concept->{label} }, @label; # TODO: uniq
    }
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

sub concept_as_turtle {
    my ($self,$id) = @_;

    my $c = $self->{concepts}->{$id};

    my @stm = ("a skos:Concept");

    # TODO: multiple
    if ($c->{notation}) {
        push @stm, "skos:notation " . $self->turtle_literal( $c->{notation}->[0] );
    }

    # TODO: prefLabel

    if ($c->{label}) {
        push @stm, "dc:title " . $self->turtle_literal( $c->{label}->[0] );
    }

    foreach my $rel (qw(broader narrower related)) {
        next unless %{$c->{$rel}};
        push @stm, "skos:$rel " 
                   . join(", ", map { "<$_>" } keys %{$c->{$rel}});
    }

    return $self->turtle_statements( "<$id>", @stm );
}

sub scheme_as_turtle {
    my $self = shift;
    my @stm = ("a skos:ConceptScheme");
   
    push @stm, "dc:title " . $self->turtle_literal( $self->{title} )
        unless $self->{title} eq '';

    my @top = grep { 
        not %{$self->{concepts}->{$_}->{broader}}
    } keys %{$self->{concepts}};

    push @stm, "skos:hasTopConcept " 
               . join(", ", map { "<$_>" } @top) if @top;

    return $self->turtle_statements( "<>", @stm );
}

sub as_turtle {
    my $self = shift;
    return $self->concept_as_turtle( @_ ) if @_;

    my @lines;

    foreach my $name (keys %{$self->{namespaces}}) {
        push @lines, "\@prefix $name: <" . $self->{namespaces}->{$name} . "> .";        
    }
    push @lines, "\@base <" . $self->{base} . "> ." if $self->{base};
    push @lines, "" if @lines;

    push @lines, $self->scheme_as_turtle;

    foreach my $key (keys %{$self->{concepts}}) {
        push @lines, $self->concept_as_turtle( $key );
    }

    return join("\n", @lines);
}


sub turtle_statements {
    my ($self, $subject, @statements) = @_;
    return '' unless @statements;
    return "$subject " . join(" ;\n" , map { "    $_" } @statements) . " .\n";
}

=head2 turtle_literal ( $string )

Escapes a string as literal in Turtle format.

=cut

sub turtle_literal {
    shift if blessed($_[0]);
    my $value = shift;
    # TODO: datatype or language tag
    my %ESCAPED = ( "\t" => 't', "\n" => 'n', 
        "\r" => 'r', "\"" => '"', "\\" => '\\' );
    $value =~ s/([\t\n\r\"\\])/\\$ESCAPED{$1}/sg;
    return "\"$value\"";
}

1;

=head1 AUTHOR

Jakob Voss C<< <jakob.voss@gbv.de> >>

=head1 LICENSE

Copyright (C) 2010 by Verbundzentrale Goettingen (VZG) and Jakob Voss

This library is free software; you can redistribute it and/or modify it
under the same terms as Perl itself, either Perl version 5.8.8 or, at
your option, any later version of Perl 5 you may have available.

In addition you may fork this library under the terms of the 
GNU Affero General Public License.

