package Master;

use warnings;
use strict;

use Digest::MD5 qw/md5_hex/;

use constant SECRET_KEY => "c3p0_secret_key.";

my $master = {};

sub is {
    my ($class, $raw_nick) = @_;
    return $master->{$raw_nick} // 0;
}

sub mis {
    my ($class, $message) = @_;
    return $class->is($message->{raw_nick});
}

sub auth {
    my ($class, $message) = @_;
    if ($message->{body} =~ m/^,auth\s+(.*?)\s*$/) {
        if ($1 eq md5_hex('xxxxx')) { # TODO, dynamic auth token
            $master->{$message->{raw_nick}} = 1;
            return "auth ok";
        }
    }
    return "auth error";
}

sub revoke {
    my ($class, $raw_nick) = @_;
    delete $master->{$raw_nick};
}

sub mrevoke {
    my ($class, $message) = @_;
    return $class->revoke($message->{raw_nick});
}

1;
