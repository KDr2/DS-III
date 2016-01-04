#!/usr/bin/env perl

use strict;
use warnings;

my $prefix = $ARGV[0];
exit 1 unless $prefix =~ m/^(.+?)\/([^\/]*)$/i;
my @versions = `find $1 -mindepth 1 -maxdepth 1 -name "$2*"`;
exit 1 unless @versions;

@versions = map { s/\Q$prefix\E//r } @versions;
@versions = sort {
    my @a = split /\D+/, $a;
    my @b = split /\D+/, $b;
    for (0..$#a) {
        my $cmp = int($a[$_]) <=> int($b[$_] // 0);
        return $cmp if $cmp;
    }
    return $#a <=> $#b;
} @versions;

@versions = map { $prefix . $_ } @versions;

if (($ARGV[1] // "") eq "-max") {
    print(pop @versions)
} elsif (($ARGV[1] // "") eq "-min") {
    print(shift @versions)
} else {
    print(@versions)
}
