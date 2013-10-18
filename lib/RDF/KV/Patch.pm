package RDF::KV::Patch;

use strict;
use warnings FATAL => 'all';

use Moose;
use namespace::autoclean;

use RDF::Trine qw(iri blank literal);

=head1 NAME

RDF::KV::Patch - Representation of RDF statements to be added or removed

=head1 SYNOPSIS

    my $patch = RDF::KV::Patch->new;

    $patch->add_this($s, $p, $o, $g);
    $patch->remove_this($s, $p, undef, $g); # a wildcard

    $patch->apply($model); # an RDF::Trine::Model instance

=head1 DESCRIPTION

This module is designed to represent a I<diff> for RDF graphs. You add
statements to its I<add> or I<remove> sides, then you L</apply> them
to a L<RDF::Trine::Model> object. This should probably be part of
L<RDF::Trine> if there isn't something like this in there already.

=cut

# positive statements
has _pos => (
    is      => 'ro',
    isa     => 'HashRef',
    default => sub { {} },
);

# negative statements-or-not
has _neg => (
    is      => 'ro',
    isa     => 'HashRef',
    default => sub { {} },
);

=head2 add_this { $S, $P, $O | $statement } [, $graph ]

Add a statement, or set of terms, to the I<add> side of the patch.

=cut

sub _validate {
    my ($s, $p, $o, $g) = @_;

    if (defined $s) {
        if (Scalar::Util::blessed($s)) {
            if ($s->isa('RDF::Trine::Statement')) {
                # move $p to $g
                if (defined $p) {
                    $g = $p;
                    undef $p;
                }

                # unpack statement
                if ($s->isa('RDF::Trine::Statement::Quad')) {
                    ($s, $p, $o, $g) =
                        map { $s->$_ } qw(subject predicate object graph);
                }
                else {
                    ($s, $p, $o) = map { $s->$_ } qw(subject predicate object);
                }
            }
            elsif ($s->isa('URI::BNode')) {
                $s = blank($s->opaque);
            }
            elsif ($s->isa('URI')) {
                $s = iri($s->as_string);
            }
            elsif ($s->isa('RDF::Trine::Node::Variable')) {
                $s = undef;
            }
            else {
                # dunno
            }
        }
        else {
            # dunno
            $s = URI::BNode->new($s);
            $s = $s->scheme eq '_' ? blank($s->opaque) : iri($s->as_string);
        }
    }

    # predicate will always be an iri
    if (defined $p) {
        if (Scalar::Util::blessed($p)) {
            if ($p->isa('URI')) {
                $p = iri($p->as_string);
            }
            elsif ($p->isa('RDF::Trine::Node::Variable')) {
                $p = undef;
            }
            else {
                # dunno
            }
        }
        else {
            $p = iri("$p");
        }
    }

    if (defined $o) {
        if (my $ref = ref $o) {
            if (Scalar::Util::blessed($o)) {
                if ($o->isa('URI::BNode')) {
                    $o = blank($o->opaque);
                }
                elsif ($o->isa('URI')) {
                    $o = iri($o->as_string);
                }
                elsif ($o->isa('RDF::Trine::Node::Variable')) {
                    $o = undef;
                }
                else {
                    # dunno
                }
            }
            elsif ($ref eq 'ARRAY') {
                $o = literal(@$o);
            }
            else {
                # dunno
            }
        }
        else {
            $o = literal($o);
        }
    }

    if (defined $g) {
        if (Scalar::Util::blessed($g)) {
            if ($g->isa('RDF::Trine::Node')) {
                # do nothing
            }
            elsif ($g->isa('URI')) {
                # scheme is not guaranteed to be present
                $g = ($g->scheme || '') eq '_' ?
                    blank($g->opaque) : iri($g->as_string);
            }
            else {
                # dunno
            }
        }
        else {
            # apparently rdf 1.1 graph identifiers can be bnodes
            $g = URI::BNode->new($g);
            # ditto scheme
            $g = ($g->scheme || '') eq '_' ?
                blank($g->opaque) : iri($g->as_string);
        }
    }

    return ($s, $p, $o, $g);
}

sub _add_either {
    my ($set, $s, $p, $o, $g) = @_;
    # clobber graph, subject and predicate to strings; bnode will be _:
    ($g, $s, $p) = map {
        $_->isa('RDF::Trine::Node::Blank') ?
            $_->sse : ref $_ ? $_->uri_value : $_ } ($g, $s, $p);

    $set->{$g}         ||= {}
    $set->{$g}{$s}     ||= {};
    $set->{$g}{$s}{$p} ||= [{}, {}];

    if ($o) {
        if ($o->isa('RDF::Trine::Node::Literal')) {
            my $l  = $o->literal_value_language;
            my $d  = $o->literal_datatype;
            my $ld = $d ? "^$d" : $l ? "@$l" : '';
            my $x  = $set->{$g}{$s}{$p}[1]{$ld} ||= {};
            $x->{$o} = 1;
        }
        else {
            $o = $o->isa('RDF::Trine::Node::Blank') ? $o->sse : $o->uri_value;
            $set->{$g}{$s}{$p}[0]{$o} = 1;
        }
    }
    else {
        $set->{$g}{$s}{$p} = 1;
    }
}

sub add_this {
    my $self = shift;
    my ($s, $p, $o, $g) = _validate(@_);
    Carp::croak('It makes no sense in this context to add a partial statement')
          unless 3 == grep { ref $_ } ($s, $p, $o);

    my $ret = $g ? RDF::Trine::Statement::Quad->new($s, $p, $o, $g) :
        RDF::Trine::Statement->new($s, $p, $o);

    _add_either($self->_pos, $s, $p, $o, $g);

    $ret;
}

=head2 dont_add_this { $S, $P, $O | $statement } [, $graph ]

Remove a statement, or set of terms, from the I<add> side of the patch.

=cut

sub dont_add_this {
    my $self = shift;
    my ($s, $p, $o, $g) = _validate(@_);
}

=head2 remove_this { $S, $P, $O | $statement } [, $graph ]

Add a statement, or set of terms, to the I<remove> side of the patch.

=cut

sub remove_this {
    my $self = shift;
    my ($s, $p, $o, $g) = _validate(@_);

    Carp::croak('If you want to nuke the whole graph, just do that directly')
          unless 1 > grep { ref $_ } ($s, $p, $o);

    my $ret = $g ? RDF::Trine::Statement::Quad->new($s, $p, $o, $g) :
        RDF::Trine::Statement->new($s, $p, $o);

    _add_either($self->_neg, $s, $p, $o, $g);

    $ret;
}

=head2 dont_remove_this { $S, $P, $O | $statement } [, $graph ]

Remove a statement, or set of terms, from the I<remove> side of the
patch.

=cut

sub dont_remove_this {
    my $self = shift;
    my ($s, $p, $o, $g) = _validate(@_);
}

=head2 apply $model

Apply the patch to an L<RDF::Trine::Model> object.

=cut

sub _node {
    my $x = shift;
    return $x eq '' ? undef : $x =~ /^_:(.*)/ ? bnode($1) : iri($x);
}

sub _traverse {
    my ($structure, $callback) = @_;

    for my $gg (keys %{$structure}) {
        my $g = _node($gg);
        for my $ss (keys %{$structure->{$gg}}) {
            my $s = _node($ss);
            for my $pp (keys %{$structure->{$gg}{$ss}}) {
                my $gsp = $structure->{$gg}{$ss}{$pp};
                my $p = _node($pp);
                if (!ref $gsp or $gsp->[0]{''}) {
                    $callback->($s, $p, undef, $g);
                }
                else {
                    for my $oo (keys %{$gsp->[0]}) {
                        my $o = _node($oo);
                        $callback->($s, $p, $o, $g);
                    }
                    for my $ld (keys %{$gsp->[1]}) {
                        my ($t, $v) = ($ld =~ /^(.)(.*)$/);
                        my @args = $t ? $t eq '@' ?
                            ($v, undef) : (undef, $v) : ();
                        for my $ll (keys %{$gsp->[1]{ld}}) {
                            my $o = literal($ll, @args);
                            $callback->($s, $p, $o, $g);
                        }
                    }
                }
            }
        }
    }
}

sub apply {
    my ($self, $model) = @_;

    $model->begin_bulk_ops;

    _traverse($self->_neg, sub { $model->remove_statements(@_) });
    _traverse($self->_pos,
              sub {
                  my $stmt = @_ > 3 ? RDF::Trine::Statement::Quad->new(@_)
                      : RDF::Trine::Statement->new(@_);
                  $model->add_statement($stmt);
              });

    $model_>end_bulk_ops;

    1;
}

__PACKAGE__->meta->make_immutable;

1;
