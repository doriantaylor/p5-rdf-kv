package RDF::KV;

use 5.010;
use strict;
use warnings FATAL => 'all';

# might as well use full-blown moose if URI::NamespaceMap uses it
use Moose;
use namespace::autoclean;

use Carp         ();
use Scalar::Util ();
use XML::RegExp  ();

# XXX remind me to rewrite this using Moo.
use URI::NamespaceMap;

=head1 NAME

RDF::KV - Represent RDF data in key-value pairs

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';


# here's ye olde grammar:

my $MODIFIER = qr/(?:[+-]!|![+-]|!)/o;
my $PLACEHOLDER = qr/#$XML::RegExp::NCName/o;
my $TERM = qr/(?:$PLACEHOLDER|
                  (?:$XML::RegExp::NCName|[A-Za-z][0-9A-Za-z.+-]*)
                  :[^[:space:]]*)/xo;
my $RFC5646 = qr/(?:[A-Za-z]+(?:-[0-9A-Za-z]+)*)/o;
my $SIMPLE_DESIGNATOR = qr/[:_'@^]/o;
my $ATOMIC_DESIGNATOR = qr/[:_'#]/o;
#my $DECL_DESIGNATOR = qr/
my $DESIGNATOR   = qr/(?:$ATOMIC_DESIGNATOR|\@$RFC5646|\^$TERM)/o;
my $PARTIAL_STMT = qr/(?:($MODIFIER)\s+)?
                      (?:($TERM)(?:\s+($TERM))?(?:\s+($DESIGNATOR))?|
                          ($TERM)\s+($DESIGNATOR)\s+($TERM)|
                          ($TERM)\s+($TERM)(?:\s+($DESIGNATOR))?\s+($TERM))/xo;
my $LOCAL_DECL  = qr/($SIMPLE_DESIGNATOR)\s+($PLACEHOLDER)/o;
my $GLOBAL_DECL = qr/(\$$XML::RegExp::NCName)/o;
my $DECLARATION = qr/(?:$LOCAL_DECL|$GLOBAL_DECL)/o;

my $RDFKV = qr/^\s*(?:$DECLARATION|$PARTIAL_STMT)\s*$/o;

# connect regex positions to semantics
my @MAP = qw(designator pdecl gdecl modifier term1 term2 designator
             term1 designator graph term1 term2 designator graph);

# XXX I know I said in the spec that the protocol should be parseable
# by a single regex, but regexes make for lame, all-or-nothing
# parsers. As such, this should really be rewritten when there's time
# to create a more helpful (in the error message sense) parser.

=head1 SYNOPSIS

    my $kv = RDF::KV->new(
        subject    => $uri,      # ordinarily the Request-URI
        graph      => $graphuri, # URI for the default graph
        namespaces => $ns,       # default namespace prefix map
    );

    # Processes a hashref-of-parameters, like found in Catalyst or
    # Plack::Request. This call will ignore obviously non-matching
    # keys, but will croak 

    eval { $kv->process($params) };

    # this will croak if 

=head1 SUBROUTINES/METHODS

=head2 new

=cut

has subject => (
    is => 'rw',
);

has graph => (
    is => 'rw',
);

has namespaces => (
    is => 'ro',
);

has _placeholders => (
    is => 'ro',
    default => sub { {} },
);
has _statements => (
    is => 'ro',
    default => sub { [] },
);


=head2 process


=cut

sub _parse_key {
    my ($self, $k) = @_;
    my @x = ($k =~ $RDFKV);

    # XXX currently this "parser" either matches the protocol or
    # doesn't match anything at all. It would be better if it would
    # ignore keys that clearly weren't part of the protocol, while
    # raising exceptions for malformed attempts to adhere to it.
    return unless @x;

    # kinda important, no?
    die 'INTERNAL ERROR: Map no longer matches grammar.' unless @x == @MAP;

    my %x;
    map { $x{$MAP[$_]} = $x[$_] if defined $x[$_]  } (0..$#x);

    return \%x;
}

# variants of parameter getters

sub _1 {
    my ($params, $k) = @_;
    $params->get_all($k);
}

sub _2 {
    my ($params, $k) = @_;
    my $val = $params->{$k};
    ref $val ? @$val : $val;
}

my %GLOBALS = (
    SUBJECT => sub {
        my ($self, $val) = @_;
        $self->subject($val);
    },
    GRAPH => sub {
        my ($self, $val) = @_;
        $self->graph($val);
    },
    PREFIX => sub {
        my ($self, $val) = @_;
        # XXX CHECK THIS MUTHA
        my ($prefix, $uri) = split /\s+/, $val;
        $self->namespaces->add_mapping($prefix, $uri);
    },
);


sub process {
    my ($self, $params) = @_;

    # assume this can also be a Hash::MultiValue
    my $sub = Scalar::Util::blessed($params)
        && $params->can('get_all') ? \&_1 : \&_2;

    my (%globals, %placeholders, @statements);

    # run over the parsed parameters
    for my $k (keys %$params) {
        my $rec = $self->_parse_key($k) or next;

        my @v   = $sub->($params, $k);

        if (defined $rec->{gdecl}) {
            my $y = $globals{$rec->{gdecl}} ||= [];
            push @$y, @v;
        }
        elsif (defined $rec->{pdecl}) {
            my $y = $placeholders{$rec->{pdecl}} ||= {};
            my $z = $y->{$rec->{designator} || "'"} ||= [];
            push @$z, @v;
        }
        elsif (defined $rec->{term1}) {
            if (defined $rec->{modifier}) {
                $rec->{modifier} = {
                    map { $_ => 1 } (split //, $rec->{modifier}) };
            }
            else {
                $rec->{modifier} = {};
            }

            # now deal with designator
            if (defined $rec->{designator}) {
                my ($a, $b) = ($rec->{designator} =~ /^(.)(.*?)$/);
                if (defined $b and $b ne '') {
                    $rec->{designator} = $a;
                    if ($a eq '@') {
                        $rec->{lang} = $b;
                    }
                    elsif ($a eq '^') {
                        $rec->{datatype} = $b;
                    }
                    else {
                        die "INTERNAL ERROR: Unknown designator $a";
                    }
                }
            }

            push @statements, [$rec, \@v];
        }
        else {
            require Data::Dumper;
            die 'INTERNAL ERROR: Unrecognized parameter: ' .
                Data::Dumper::Dumper($rec);
        }
    }

    return [\%globals, \%placeholders, \@statements];

    # run over globals

    # 
}


=head1 AUTHOR

Dorian Taylor, C<< <dorian at cpan.org> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-rdf-kv at
rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=RDF-KV>.  I will be
notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc RDF::KV

You can also look for information at:

=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=RDF-KV>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/RDF-KV>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/RDF-KV>

=item * Search CPAN

L<http://search.cpan.org/dist/RDF-KV/>

=back

=head1 SEE ALSO

=head1 LICENSE AND COPYRIGHT

Copyright 2013 Dorian Taylor.

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this file except in compliance with the License.  You may
obtain a copy of the License at
L<http://www.apache.org/licenses/LICENSE-2.0>.

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied.  See the License for the specific language governing
permissions and limitations under the License.

=cut

__PACKAGE__->meta->make_immutable;

1; # End of RDF::KV
