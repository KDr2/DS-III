#!/usr/bin/env perl

use Dir::Self;
use lib __DIR__ . "/lib";

use Droid;


my $hostname = `hostname`;
$hostname =~ s/\W//g;

my $bot = Droid->new(
    server => 'irc.rizon.net',
    #server => 'irc.freenode.net',
    #server => 'irc.mozilla.org',
    #server => '127.0.0.1',
    channels => [ '#KDr2'],
    port   => "6667",
    nick => "c3po-$hostname",
    alt_nicks => [map { "$_-$hostname" } qw(c3p0 C3PO C3P0)],
    username  => "Droid C3PO",
    name      => "Droid C3PO",
);

$bot->run();
