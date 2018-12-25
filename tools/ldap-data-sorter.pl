#!/usr/bin/env perl

use v5.12;
use MIME::Base64;

my $all_nodes = {};
my $all_dn =[];
my $current_node = "";
my $current_dn = "";
my $dn_just_assigned = 0;

while(<>) {
    $current_node .= $_;
    if(m/^\s+$/) {
        $all_nodes->{$current_dn} = $current_node;
        push @$all_dn, $current_dn;
        $current_node = "";
        $current_dn = "";
        next;
    }
    if(m/^dn::?\s(.*)$/) {
        $current_dn = $1;
        $dn_just_assigned = 1;
        next;
    }
    if($dn_just_assigned) {
        $current_dn .= $1 if(m/^\s+(.+)/);
        $current_dn = decode_base64($current_dn) if index($current_dn, ",") < 0;
        $dn_just_assigned = 0;
    }
}

$all_dn = [sort {length $a <=> length $b} @$all_dn];
print $all_nodes->{$_} foreach(@$all_dn);
