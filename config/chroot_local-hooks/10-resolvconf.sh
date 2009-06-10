#!/bin/sh
set -e

# if installed resolvconf package
[ -d /etc/resolvconf/ ] || exit 0

# aptitude -y install resolvconf
rm /etc/resolv.conf /etc/resolv.conf.orig
ln -s /etc/resolvconf/run/resolv.conf /etc/resolv.conf
ln -s /etc/resolvconf/run/resolv.conf /etc/resolv.conf.orig
