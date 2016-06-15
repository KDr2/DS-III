package Tweet;

use v5.16;
use strict;
use warnings;

use Encode;
use LWP::UserAgent;
use JSON;
use DateTime;
use Data::Dumper;
use HTML::TreeBuilder;
use WebConfig;

sub fetch_data {
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
    my @tweets = split(qr|<hr/?>|, $text);
    @tweets = map { strip_tags($_) } @tweets;
    shift @tweets;
    return @tweets;
}


sub get_tweets {
    return [html2json(get_html($tweet_doc_id))];
}

1;
