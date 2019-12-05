package Router;

use warnings;
use strict;

use Master;
use Utils;

use Command::Private;

# message types
use constant {
    PUB_MSG  => 1,
    PUB_CMD  => 2,
    CONV_MSG => 3,
    CONV_CMD => 4,
    PRIV_MSG => 5,
    PRIV_CMD => 6,
};

# message subs
my $MSG_METHODS = {
    PUB_MSG()  => "do_pub_msg",
    PUB_CMD()  => "do_pub_cmd",
    CONV_MSG() => "do_conv_msg",
    CONV_CMD() => "do_conv_cmd",
    PRIV_MSG() => "do_priv_msg",
    PRIV_CMD() => "do_priv_cmd",
};

# commands and aliases
my (%PRIV_CMD, %CONV_CMD, %PUB_CMD);
@PRIV_CMD{qw/auth run srun note tech_topic/} = ();
@CONV_CMD{qw/conv/} = ();
@PUB_CMD{qw/pub/} = ();

my %CMD_ALIAS = (
    n  => "note",
    tt => "tech_topic"
);

sub dispatch {
    my ($class, $droid, $message) = @_;
    my $channel = $message->{channel};
    my $prefix_chr = substr($message->{body}, 0, 1);

    my $msg_type = PUB_MSG;
    if (!defined $message->{address}) {
        $msg_type = $prefix_chr eq ',' ? PUB_CMD : PUB_MSG;
    } else {
        if ($channel ne "msg") {
            $msg_type = $prefix_chr eq ',' ? CONV_CMD : CONV_MSG;
        } else {
            $msg_type = $prefix_chr eq ',' ? PRIV_CMD : PRIV_MSG;
        }
    }
    my $method = $MSG_METHODS->{$msg_type};
    return $class->$method($droid, $message);
}

sub _get_cmd {
    my $message = shift;
    my $cmd = substr((split /\s+/, $message->{body}, 2)[0], 1);
    $cmd =~ s#-#_#g;
    if (exists $CMD_ALIAS{$cmd}) {
        return $CMD_ALIAS{$cmd};
    }
    return $cmd;
}

sub do_pub_msg {
    my ($class, $droid, $message) = @_;
    return undef;
}

sub do_pub_cmd {
    my ($class, $droid, $message) = @_;
    my $channel = $message->{channel};
    my $cmd = _get_cmd($message);

    return "PUB CMD";
}

sub do_conv_msg {
    my ($class, $droid, $message) = @_;
    return "CONV MSG";
}

sub do_conv_cmd {
    my ($class, $droid, $message) = @_;
    my $channel = $message->{channel};
    my $cmd = _get_cmd($message);

    if (! exists $CONV_CMD{$cmd}) {
        if (exists $PUB_CMD{$cmd}) {
            return __PACKAGE__->do_pub_cmd($droid, $message);
        }
        return "ERROR: unknow command";
    }
    return "CONV CMD";
}

sub do_priv_msg {
    my ($class, $droid, $message) = @_;
    return "PRIV MSG";
}


sub do_priv_cmd {
    my ($class, $droid, $message) = @_;
    my $channel = $message->{channel};
    my $cmd = _get_cmd($message);

    if (! exists $PRIV_CMD{$cmd}) {
        if (exists $CONV_CMD{$cmd}) {
            return __PACKAGE__->do_conv_cmd($droid, $message);
        } elsif (exists $PUB_CMD{$cmd}) {
            return __PACKAGE__->do_pub_cmd($droid, $message);
        }
        return "ERROR: unknow command";
    }

    if ($cmd =~ /^auth$/i) {
        return Master->auth($message);
    }

    return "Please identify yourself first." if (!Master->mis($message));
    return Command::Private->do_cmd($cmd, $droid, $message);
}

1;
