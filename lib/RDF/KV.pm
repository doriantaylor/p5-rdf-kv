package RDF::KV;

use 5.010;
use strict;
use warnings FATAL => 'all';

# might as well use full-blown moose if URI::NamespaceMap uses it
use Moose;
use namespace::autoclean;
use Try::Tiny;

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

# XXX I know I said in the spec that the protocol should be parseable
# by a single regex, but regexes make for lame, all-or-nothing
# parsers. As such, this should really be rewritten when there's time
# to create a more helpful (in the error message sense) parser.

# ok you know what? no. This is waaaaaaay simpler.
my $MODIFIER     = qr/(?:[+-]!|![+-]|!|=)/o;
my $PREFIX       = qr/(?:$XML::RegExp::NCName|[A-Za-z][0-9A-Za-z.+-]*)/o;
my $TERM         = qr/(?:$PREFIX:\S*)/o;
my $RFC5646      = qr/(?:[A-Za-z]+(?:-[0-9A-Za-z]+)*)/o;
my $DESIGNATOR   = qr/(?:[:_']|\@$RFC5646|\^$TERM)/o;
my $DECLARATION  = qr/^\s*\$\s+($XML::RegExp::NCName)(?:\s+(\$))?\s*$/mo;
my $MACRO        = qr/(?:\$\{($XML::RegExp::NCName)\}|
                          \$($XML::RegExp::NCName))/xo;
my $NOT_MACRO    = qr/(?:(?!\$$XML::RegExp::NCName|
                              \$\{$XML::RegExp::NCName\}).)*/xo;
my $MACROS       = qr/($NOT_MACRO)(?:$MACRO)?($NOT_MACRO)/smo;
my $PARTIAL_STMT = qr/^\s*(?:($MODIFIER)\s+)?
                      (?:($TERM)(?:\s+($TERM))?(?:\s+($DESIGNATOR))?|
                          ($TERM)\s+($DESIGNATOR)\s+($TERM)|
                          ($TERM)\s+($TERM)(?:\s+($DESIGNATOR))?\s+($TERM))
                      (?:\s+(\$))\s*$/xmo;

my @MAP = qw(modifier term1 term2 designator term1 designator graph
             term1 term2 designator graph deref);

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
    is       => 'rw',
    required => 1,
);

has graph => (
    is  => 'rw',
    isa => 'RDF::Trine::Model',
    
);

has namespaces => (
    is      => 'ro',
    isa     => 'URI::NamespaceMap',
    default => sub { URI::NamespaceMap->new },
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

# XXX these should all get syntax checks/CURIE expansion/etc
my %SPECIALS = (
    SUBJECT => sub {
        my ($self, $val) = @_;
        $self->subject($val->[-1]) if @$val;
    },
    GRAPH => sub {
        my ($self, $val) = @_;
        $self->graph($val->[-1]) if @$val;
    },
    PREFIX => sub {
        my ($self, $val) = @_;
        # XXX CHECK THIS MUTHA
        for my $v (@$val) {
            my ($prefix, $uri) = ($v =~ /^\s*(\S+):\s+(.*)$/)
                or Carp::croak("Invalid prefix mapping $val");
            $self->namespaces->add_mapping($prefix, $uri);
        }
    },
);

sub _deref_content {
    my ($val, $macros) = @_;
    my @out;

    # if $val is scalar, this loop will run just once.
    for my $v (ref $val ? @$val : ($val)) {
        # make this versatile
        $v = $v->[0] if ref $v;

        my @chunks;
        while ($v =~ /\G$MACROS/gco) {
            my $pre   = $1;
            my $macro = $2 || $3;
            my $post  = $4;

            unless (defined $macro) {
                @chunks = ($pre . $post);
                #warn @chunks;
                next;
            }

            # do the actual macro dereferencing or noop in
            # lieu of a bound macro
            my @x = $macros->{$macro} ?
                map { "$pre$_$post" } @{$macros->{$macro}}
                    : ("$pre$macro$post");

            # initialize chunks
            unless (@chunks) {
                @chunks = @x;
                next;
            }

            # replace chunks with product of itself and x
            my @y;
            for my $c (@chunks) {
                for my $d (@x) {
                    push @y, "$c$d";
                }
            }
            @chunks = @y;
        }

        push @out, @chunks;
    }

    wantarray ? @out : \@out;
}

sub _massage_macros {
    my $macros = shift;
    # XXX this currently makes destructive changes to $macros insofar
    # as it rewrites the 'deref' flag with the actual variables to
    # dereference, or to 0 if there aren't any. If this becomes a
    # problem, just use Clone.

    # cycle detect, finished product
    my (%seen, %done);

    # shallow-copy the hash
    my %pending = %$macros;

    # Start a queue with a (quasi) random macro.
    my @queue = (keys %pending)[0];

    # If none of them contain a (bound) macro references, that macro
    # is 'done'.

    # If the values *do* contain bound macro references, check to see
    # if those are 'done'. If they aren't, *prepend* the keys to the
    # queue, before the current macro.

    while (@queue) {
        #warn 'Initial ' . join(';', @queue);
        my $k = shift @queue;
        #warn "beginning \$$k";

        $seen{$k}++;

        my @vals = @{$macros->{$k}};

        # 'done' and 'pending' macros
        my (%dm, %pm);

        # Examine each of its values.

        # Note: this test is equivalent to concatenating the values
        # together with spaces and performing the regex on that. But
        # we can't do that because we're storing the macro-matching
        # state of individual values.
        for my $pair (@vals) {
            my ($val, $deref) = @$pair;

            # no expando
            next unless $deref;

            if (ref $deref) {
                # already been scanned
                for my $m (@$deref) {
                    defined $done{$m} ? $dm{$m}++ : $pm{$m}++;
                }
            }
            else {
                my %m;
                for my $m (grep { defined $_ } ($val =~ /$MACRO/og)) {

                    # check first to see if it's bound
                    next unless $macros->{$m};
                    #warn $m;

                    # if it's yourself, explode
                    Carp::croak("Self reference found!") if $m eq $k;

                    # get this to replace deref
                    $m{$m}++;

                    # and get this to figure out if we can deref
                    defined $done{$m} ? $dm{$m}++ : $pm{$m}++;
                }

                # now replace deref
                $pair->[1] = keys %m ? [sort keys %m] : 0;
            }
        }

        # macro values have pending matches
        if (keys %pm) {
            # this is where we would detect a cycle

            # right HERE

            my @q;
            for my $m (keys %pm) {
                Carp::croak("Cycle detected between $k and $m") if $seen{$m};
                push @q, $m;
            }
            #warn join '/', @q;

            # do it again
            unshift @queue, @q, $k;
            #warn join ',', @queue;

            next;
        }
        elsif (keys %dm) {
            # macro values have actionable matches

            #warn "replacing values for \$$k";

            # replace contents and mark done
            $done{$k} = _deref_content(\@vals, \%done);
        }
        else {
            #warn Data::Dumper::Dumper(\@vals);
            # nothing to do, mark done
            $done{$k} = [map { $_->[0] } @vals];
        }

        # remember to remove this guy or we'll loop forever
        delete $pending{$k};

        # replenish the queue with another pending object
        push @queue, (keys %pending)[0] if !@queue and keys %pending;
    }

    \%done;
}


sub process {
    my ($self, $params) = @_;

    # assume this can also be a Hash::MultiValue
    my $sub = Scalar::Util::blessed($params)
        && $params->can('get_all') ? \&_1 : \&_2;
    # XXX do we want to do ->isa instead?

    my (%macros, %maybe, %neither);

    for my $k (keys %$params) {
        # Step 0: get the values into a homogeneous list
        my @v = $sub->($params, $k);

        # Step 1: pull out all the macro declarations
        if (my ($name, $sigil) = ($k =~ $DECLARATION)) {
            # Step 1.0.1: create [content, deref flag] pairs

            # OOH VERY CLEVER
            push @{$macros{$name} ||= []}, (map { [$_, int(!!$sigil)] } @v);
        }
        # Step 1.1: set aside candidate statements
        elsif ($k =~ /^\s*\S+\s+\S+.*?/ || $k =~ /[:$]/) {
            # valid partial statements will contain space or : or $
            push @{$maybe{$k} ||= []}, @v;
        }
        # Step 1.2: put the rest in a discard pile
        else {
            push @{$neither{$k} ||= []}, @v;
        }
    }

    # cycles should cause a 409 Conflict error, but that isn't our job
    # here.

    # XXX although it may be useful to return an object in $@ that was
    # more informative.

    # Step 2: dereference all the macros (that asked to be)
    try {
        my $x = _massage_macros(\%macros);
        %macros = %$x;
    } catch {
        # move this error up in the stack
        Carp::croak($@);
    };

    # Step 2.1: overwrite any reserved/magic macros
    $macros{NEWUUID} = [[sub {  }, 0]];
    # XXX make this extensible?

    # Step 3: apply special control macros to $self
    try {
        for my $k (keys %SPECIALS) {
            next unless $macros{$k};
            $SPECIALS{$k}->($self, $macros{$k});
        }
    } catch {
        # cough any errors up the stack
        Carp::croak($@);
    };

    # add/remove statements
    my (%pos, %neg);
    for my $k (keys %maybe) {
        # Step 4: dereference macros in statements

        # Step 4.1 dereference macros in statement *templates* first
        # so we can figure out which values need to be dereferenced
        # (since the terminating $ indicator can be substituted in via
        # macro).
        my @k = grep { defined $_ } ($k =~ /$MACRO/og) ?
            _deref_content($k, \%macros) : ($k);

        # we want to check the values for empty strings *before* we
        # dereference them so it's still possible to express the empty
        # string through the use of a macro
        my @v = grep { $_ ne '' } map { s/^\s*(.*?)\s*$/$1/sm }
            grep { defined $_ } @{maybe{$k} || []};

        # very well could loop just once here
        for my $template (@k) {

            # nope actually we're parsing the template now
            my @tokens = ($template =~ /^\s*$PARTIAL_STMT\s*$/smo);

            # ignore if there wasn't a match XXX WARN SOMEHOW?
            next unless @tokens;

            # do not ignore, however, if this screws up
            die 'INTERNAL ERROR: regex does not match map'
                unless @tokens == @MAP;

            # now make a nice hash of the contents
            my %contents = map { $MAP[$_] => $tokens[$_] } (0..$#MAP);

            # pull out the statement modifier first
            $contents{modifier} = {
                map { $_ => 1 } (split //, $contents{modifier} || '') };

            # statement reversal behaviour is not entirely symmetric.

            # + is a noop of the default behaviour: assert S P O or O P S.
            # = means remove S P * before asserting S P O. (no reversal)
            # - means either remove S P *, S P O or O P S, but not O P *.

            # thinking { g => { s => { p => [{}, { langordt => {} }] } } }

            # you can tell a blank node from a resource if it starts
            # with _:

            # for negative wildcards: { g => { s => { p => 1 } } }
            # since removing S P * overrides any S P O.

            # an empty @v means there was no value for this key that
            # was more than whitespace/empty string.


            # in this case we probably can't be clever and reuse the
            # values for multiple templates because some may or may
            # not include the indicator.

            # actually we can reuse the values, we just can't parse
            # them until we've parsed the statement templates, because
            # those tell us what to do with the values.

            # which also means we have to parse the statement
            # templates immediately.

            # there is still the issue of the empty string: what does
            # it mean, and in what context?

            # Step 4.2 dereference macros in statement *values* (that
            # asked to be)


            # Step 5: parse statement templates

            # Step 5.1 expand qnames

            # Step 6: generate complete statements
        }
    }
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
