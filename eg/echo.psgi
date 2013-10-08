#!/usr/bin/perl

use strict;
use warnings FATAL => 'all';

use Plack::Request;

use RDF::KV;

my $app = sub {
    my $env = shift;
    my $req = Plack::Request->new($env);

    my $res = $req->new_response(200);
    $res->content_type('text/plain');

    my $method = $req->method;
    if ($method eq 'POST') {
        my $kv = RDF::KV->new(subject => $req->uri);
        my $x = $kv->process($req->body_parameters);
        require Data::Dumper;
        $res->body(Data::Dumper::Dumper($x));
    }
    elsif ($method eq 'GET' or $method eq 'HEAD') {
        # i dunno
        #$res->body($req->uri);
        my $data = do {
            local $/; open my $fh, '<:raw', $ARGV[1] or die $!; <$fh> };
        $res->content_type('text/html');
        $res->body($data);
    }
    else {
        # 405 method not allowed
        $req->status(405);
    }


    # here is where we generate

    $res->finalize;
};
