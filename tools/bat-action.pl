#!/usr/bin/perl

# install with crontab
# */5 * * * * /home/kdr2/Work/mine/DS-III/tools/bat-action.pl

use 5.16.0;
use strict;
use warnings;

my $threshold = 23;

# check battery status
open(ACPITOOL, "-|", "acpitool");
while (<ACPITOOL>) {
    # Battery #1     : Charging, 97.70%, 00:17:04
    # Battery #1     : Discharging, 97.88%, 02:56:21
    # Battery #1     : Discharging, 89.72%
    if (m/^\s*Battery\s+#\d+\s+:\s+(\w+),\s*(\d+\.\d+)%.*$/i) {
        my ($state, $percentage) = ($1, $2);
        if ($percentage < $threshold && $state =~ /^[dD]ischarging$/) {
            say "Battery emergency, the system is shutting down.";
            say `/bin/sync`;
            say `/sbin/halt -p`;
        }
        last;
    }
}
