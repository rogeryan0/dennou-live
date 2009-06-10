#!/bin/bash
set -e

# disable console
sed -i 's/^[456]/$&/' /etc/inittab

# parallel init script
sed -i 's/^CONCURRENCY=none/CONCURRENCY=shell/' /etc/init.d/rc
apt-get -y install insserv
update-bootsystem-insserv

