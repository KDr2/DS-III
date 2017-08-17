package TextSharing;

use v5.22;
use strict;
use warnings;

use Encode;
use LWP::UserAgent;
use JSON;
use Data::Dumper;
use Mojo::UserAgent;
use Data::ICal;

my $ical = "https://calendar.zoho.com/ical/" .
    "6d016b5000420299bb074a12ade0806db28849a820781988902c70c49eff786e48be18500888ced649addd66ff58b76c";

sub fetch_data_mojo {
    my $ua = Mojo::UserAgent->new;
    my $data = $ua->get($ical => {})->res->body;
    return $data;
}

sub fetch_data_lwp {
    my $user_agent = LWP::UserAgent->new("Mozilla Firefox");
    my $response = $user_agent->get($ical);
    my $data = $response->decoded_content;
    return $data;
}

sub fetch_data {
    if (defined($ENV{GATEWAY_INTERFACE}) && $ENV{GATEWAY_INTERFACE} =~ /^CGI\/.*/) {
        return fetch_data_lwp();
    }
    return fetch_data_mojo();
}

sub entry_to_text {
    my $entry = shift;
    my $summary = $_->property("summary")->[0]->value();
    my $desc = $_->property("description")->[0]->value();
    my $url = $_->property("url") ? $_->property("url")->[0]->value() : undef;
    my $date = substr($_->property("dtstart")->[0]->value(), 0, 8);
    $summary = $summary ? ", $summary" : "";
    $desc = $desc? ": $desc" : "";
    $url = $url ? " [$url]" : "";

    return $date . $summary . $desc . $url;
}


sub get_moments {
    my $calendar = Data::ICal->new(data => fetch_data(), rfc_strict => 1, vcal10 => 0);
    my $entries = $calendar->entries();
    my @texts = map { entry_to_text($_) } @{$entries};
    return [@texts];
}

1;
