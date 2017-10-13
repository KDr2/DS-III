package MomentsQuip;

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

sub fetch_data_mojo {
    state $host = 'https://platform.quip.com/1/threads/';
    my ($thread_id) = @_;
    my $ua = Mojo::UserAgent->new;
    my $data = $ua->get($host . $thread_id => {"Authorization" => $quip_auth_key})->res->body;
    return $data;
}

sub fetch_data_lwp {
    state $host = 'https://platform.quip.com/1/threads/';
    my ($thread_id) = @_;
    my $user_agent = LWP::UserAgent->new("Mozilla Firefox");
    $user_agent->default_header(
        "Authorization" => $quip_auth_key
    );
    my $response = $user_agent->get($host . $thread_id);
    my $data = $response->decoded_content;
    return $data;
}

sub fetch_data {
    my ($thread_id) = @_;
    if (defined($ENV{GATEWAY_INTERFACE}) && $ENV{GATEWAY_INTERFACE} =~ /^CGI\/.*/) {
        return fetch_data_lwp($thread_id);
    }
    return fetch_data_mojo($thread_id);
}

sub get_html {
    my ($thread_id) = @_;
    my $json_str = fetch_data($thread_id);
    my $json_obj = from_json($json_str, { utf8  => 1 } );
    return $json_obj->{html};
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
    return [html2json(get_html($moments_doc_id))];
}

1;
