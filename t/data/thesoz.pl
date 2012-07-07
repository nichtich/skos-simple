#!/usr/bin/perl

use SKOS::Simple;

eval {
    require Test::More;
    Test::More->import("no_plan"); 
};

# Models a simplified part of TheSoz (without its classification)

$skos = new SKOS::Simple(
    base      => 'http://lod.gesis.org/thesoz/concept/',
    scheme    => 'http://lod.gesis.org/thesoz/',
    title     => 'TheSoz Thesaurus for the Social Sciences',
    page      => 'http://www.gesis.org/en/services/' 
                .'tools-standards/social-science-thesaurus/',
    hierarchy => 'thesaurus', # TODO
    license   => 'http://creativecommons.org/licenses/by-nc-nd/3.0/de/',
    language  => 'de',
    identity  => 'identifier'
);

# TheSoz also has a classification that we ignore here:
#   http://lod.gesis.org/thesoz/classification/

$skos->addConcept(
    # http://lod.gesis.org/thesoz/concept/10039266
    id => '10039266', 
    label => { 
        # http://lod.gesis.org/thesoz/term/10039266
        de => 'Dokumentationswesen',
        # http://lod.gesis.org/thesoz/term/10039266-en
        en => 'documentation system'
    },
    alt => { 
        # http://lod.gesis.org/thesoz/term/term/10041230
        de => 'Dokumentationseinrichtung', 
        # http://lod.gesis.org/thesoz/term/10041230-en
        en => 'documentation center' 
    },
    # http://lod.gesis.org/thesoz/concept/10039265
    related => '10039265',
);

$skos->addConcept(
    # http://lod.gesis.org/thesoz/concept/10039265
    id => '10039265',
    label => {
        # http://lod.gesis.org/thesoz/term/10039265
        de => 'Bibliothekswesen',
        # http://lod.gesis.org/thesoz/term/10039265-en
        en => 'librarianship'
    }
);

#print $skos->turtle;
#print $skos->turtle ( top => 0 );

$skos;

# Mapping with ZBW:
# exactMatch thesoz:10039265 zbw:13351-6
# exactMatch thesoz: 10039266 zbw:16782-6
