package Moments;

use v5.16;
use strict;
use warnings;

use Encode;
use LWP::UserAgent;
use JSON;
use Data::Dumper;
use HTML::TreeBuilder;
use Mojo::UserAgent;

use Vault;

my $PAGE_URL = "https://www.notion.so/zhuoql/What-is-happening-3fa589c95cb8497fb9a70fec96675db1";

sub fetch_data_mojo {
    my $ua = Mojo::UserAgent->new;
    my $data = $ua->get($PAGE_URL)->res->body;
    return $data;
}

sub fetch_data_lwp {
    my $user_agent = LWP::UserAgent->new("Mozilla Firefox");
    # $user_agent->default_header(
    #     "Authorization" => "..."
    # );
    my $response = $user_agent->get($PAGE_URL);
    my $data = $response->decoded_content;
    return $data;
}

sub fetch_data {
    if (defined($ENV{GATEWAY_INTERFACE}) && $ENV{GATEWAY_INTERFACE} =~ /^CGI\/.*/) {
        return fetch_data_lwp();
    }
    return fetch_data_mojo();
}

sub get_html {
    my $html = fetch_data();
    print($html);
    return $html;
}

sub strip_tags {
    my $tree = HTML::TreeBuilder->new;
    $tree->parse_content($_[0]);
    my $non_html = $tree->as_text();
    return $non_html;
}

sub html2json {
    my $text = shift;
    my @moments = split(qr#<h\d.+?</h\d>|<hr/?>|<p.*?>.{0,1}</p>#, $text);
    @moments = grep {!/^\s*$/} @moments;
    @moments = map { s/^\s+|\s+$//g; $_ } @moments;
    @moments = grep {/^<p.+?>\d{8}[:\x{ff1a}]/} @moments;
    # @moments = map { strip_tags($_) } @moments;
    return @moments;
}


sub get_moments {
    return [get_html()];
}

sub moments {
    my $c = shift;
    my $format = $c->param("format") // "json";
    my $cb = $c->param("callback");
    my $moments = get_moments;

    return $c->render(text => $moments->[0]) if $format eq "html";
    if (!$cb) {
        $c->render(json => $moments);
    } else {
        my $text = $cb . "(" . decode('UTF-8', encode_json($moments)) . ")";
        $c->render(text => $text);
    }
};


1;
