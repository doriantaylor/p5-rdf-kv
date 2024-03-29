use inc::Module::Install;

name        'RDF-KV';
all_from    'lib/RDF/KV.pm';
readme_from 'lib/RDF/KV.pm';
author      q{Dorian Taylor <dorian@cpan.org>};
license     'apache';

resources (
   homepage   => 'https://github.com/doriantaylor/p5-rdf-kv',
   license    => 'http://www.apache.org/licenses/LICENSE-2.0',
   repository => 'git://github.com/doriantaylor/p5-rdf-kv.git',
   bugtracker => 'https://github.com/doriantaylor/p5-rdf-kv/issues',
);

build_requires 'Test::More';

requires 'Moose'              => '2.0';
requires 'URI::BNode'         => '0.06';
requires 'RDF::Trine'         => '1.019';
requires 'URI::NamespaceMap'  => '1.10';
requires 'XML::RegExp'        => '0.04';
requires 'Data::UUID::NCName' => '0.07';
requires 'Data::GUID::Any'    => '0.005';

auto_install;

WriteAll;

