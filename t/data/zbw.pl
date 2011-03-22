#!/usr/bin/perl

use SKOS::Simple qw(skos);

eval {
    require Test::More;
    Test::More->import("no_plan"); 
};

$skos = new SKOS::Simple(
    base   => 'http://zbw.eu/stw/descriptor/',
    scheme => 'http://zbw.eu/stw',
    title  => { 
        en => "STW Thesaurus for Economics", 
        de => "Standard-Thesaurus Wirtschaft"
    },
    identity => 'label',
    namespaces => { gbv => 'http://purl.org/ontology/gbv/' }
);

# TODO: this should croak or make id => label
$skos->add_concept( id => '13351-6' ); 

$skos->add_concept( 
    identifier => '13351-6',
    label => { "de" => "Bibliothek", "en" => "library" },
    alt => { "de" => ["Bibliothekswesen", "Bücherei"] },
    # properties => { "gbv:gvkppn" => "091351642" }, # TODO gbv:gvkppn "091351642"
#   skos:narrower <stw/descriptor/13350-1>, <stw/descriptor/13358-6>, <stw/descriptor/16769-5> ;
#    skos:prefLabel "Bibliothek"@de, "Library"@en ;
#    skos:related <stw/descriptor/16771-4>, <stw/descriptor/16802-5>, <stw/descriptor/18945-1>, <stw/descriptor/18952-4> ;
);

print $skos->turtle ( top => 0 );




# <4.2.07>

# http://lod.gesis.org/thesoz/term/10039266

# Die ZBW Kiel bietet ihren Thesaurus an
# http://zbw.eu/stw/descriptor/13350-1

# Die DNB gibt die GND als Linked Data heraus:
# http://d-nb.info/gnd/117060275 (Person)
# http://d-nb.info/gnd/4145258-6 (Sachschlagwort)
# http://d-nb.info/gnd/2026461-6 (Körperschaft)

# http://lod.gesis.org/thesoz/term/10039265
# http://lod.gesis.org/thesoz/term/10039266
