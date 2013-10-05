use inc::Module::Install;

name     'RDF-KV';
all_from 'lib/RDF/KV.pm';
author   q{Dorian Taylor <dorian@cpan.org>};
license  'apache';

build_requires 'Test::More';

auto_install;

WriteAll;

