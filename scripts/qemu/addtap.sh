#!/bin/bash
/usr/sbin/openvpn --mktun --dev $1 --user kdr2 #`id -un`
/sbin/ifconfig $1 0.0.0.0 promisc up
#/sbin/brctl addif br0 $1
