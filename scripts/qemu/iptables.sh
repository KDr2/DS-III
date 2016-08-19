#!/bin/sh
echo "1" > /proc/sys/net/ipv4/ip_forward

/sbin/iptables -P INPUT ACCEPT
/sbin/iptables -P FORWARD ACCEPT
/sbin/iptables -P OUTPUT ACCEPT

#/sbin/iptables -t nat -A POSTROUTING -s 192.168.7.0/24 -d 192.168.1.0/24 -j MASQUERADE
#/sbin/iptables -A FORWARD -s 192.168.7.0/24 -d 192.168.1.0/24 -j ACCEPT
#/sbin/iptables -t nat -A POSTROUTING -s 192.168.1.0/24 -d 192.168.7.0/24 -j MASQUERADE
#/sbin/iptables -A FORWARD -s 192.168.1.0/24 -d 192.168.7.0/24 -j ACCEPT


/sbin/iptables -t nat -A POSTROUTING -s 192.168.7.0/24 -j MASQUERADE
/sbin/iptables -A FORWARD -s 192.168.7.0/24 -j ACCEPT
/sbin/iptables -t nat -A POSTROUTING -d 192.168.7.0/24 -j MASQUERADE
/sbin/iptables -A FORWARD -d 192.168.7.0/24 -j ACCEPT
