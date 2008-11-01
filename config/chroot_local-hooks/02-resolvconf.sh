#!/bin/sh

set -e

# aptitude -y install resolvconf
cd /etc
rm resolv.conf resolv.conf.orig
ln -s /etc/resolvconf/run/resolv.conf resolv.conf
ln -s /etc/resolvconf/run/resolv.conf resolv.conf.orig
