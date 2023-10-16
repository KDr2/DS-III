#!/usr/bin/perl

# install with crontab
# */5 * * * * /home/kdr2/Work/mine/DS-III/tools/awesome-bat.pl

use strict;
use warnings;

my $threshold = 23;

# setup ENV
my $wm_pid = `pgrep x-window-manage` =~ s/\s*//gr;
my $dbus_session = `grep -z DBUS_SESSION_BUS_ADDRESS /proc/${wm_pid}/environ|cut -d= -f2-` =~ s/\s*//gr;
$ENV{DBUS_SESSION_BUS_ADDRESS} = $dbus_session;

# check battery status
open(ACPITOOL, "-|", "acpitool");
while (<ACPITOOL>) {
    # Battery #1     : Charging, 97.70%, 00:17:04
    # Battery #1     : Discharging, 97.88%, 02:56:21
    # Battery #1     : Discharging, 89.72%
    if (m/^\s*Battery[\d\D]+,\s*(\d+\.\d+)%.*$/i) {
        my $current_percentage = $1;
        if ($current_percentage < $threshold) {
            open(AWE_CLIENT, "|awesome-client") or die "Can't start awesome-client: $!";
            while (<DATA>) {
                s/__CURRENT_PERCENTAGE__/$current_percentage/;
                print AWE_CLIENT;
            }
            close AWE_CLIENT;
        }
        last;
    }
}

__END__
-- lua --
nty = require('naughty');
nty.notify({
    title = "Battery Warning",
    text="Percent Left: __CURRENT_PERCENTAGE__%",
    position = "top_right",
    timeout = 5,
    icon="/path/to/image",
    fg="#327000",
    bg="#FFF918",
    screen = 1,
    ontop = true
});
