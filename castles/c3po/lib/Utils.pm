package Utils;

use v5.16;
use strict;
use warnings;

use Encode;
use LWP::UserAgent;
use DateTime;
use Dir::Self;

sub path {
    shift;
    my $relative_path = shift;
    my $root = __DIR__ . "/..";
    return $relative_path ? "$root/$relative_path" : $root;
}

sub paste {
    state $host = 'https://paste.debian.net/';
    my ($class, $data, $lang) = @_;
    $lang //= '-1';
    my $userAgent = LWP::UserAgent->new("Mozilla Firefox");
    my $response = $userAgent->post(
        $host,
        [
            poster => 'C3PO',
            lang => $lang,
            expire => 1800,
            code => encode('utf-8', $data),
            paste => "Send",
        ],
        'Content-Type' => 'multipart/form-data'
    );
    my $location = $response->headers->{location};
    return $host . $location;
}

sub date_ymd {
    my ($class) = @_;
    my $date = DateTime->now(time_zone=>"Asia/Shanghai");
    return $date->ymd("-");
}

1;
