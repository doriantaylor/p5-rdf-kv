use inc::Module::Install;

name        'RDF-KV';
all_from    'lib/RDF/KV.pm';
readme_from 'lib/RDF/KV.pm';
author      q{Dorian Taylor <dorian@cpan.org>};
license     'apache';

build_requires 'Test::More';

requires 'Moose'             => '2.0';
requires 'URI::BNode'        => '0.05';
requires 'RDF::Trine'        => '1.007';
requires 'URI::NamespaceMap' => '0.06';

auto_install;

WriteAll;

