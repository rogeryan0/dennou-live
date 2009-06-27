#!/bin/bash -x
set -e

# disable console tty2,tty3,tty4,tty5,tty6
sed -i 's/^[23456]/#\ &/' /etc/inittab

# enable parallel init script
sed -i 's/^CONCURRENCY=none/CONCURRENCY=startpar/' /etc/init.d/rc

# remove daemon
DAEMON="acct acpi-support avahi-daemon clamav-freshclam cron hdparm partimaged portmap pppd-dns rc.local rsync rsyslog saned ssh system-tools-backends timidity vbesave"
for i in ${DAEMON}; do update-rc.d -f ${i} remove; done

# set insserv
update-bootsystem-insserv

# set usplash
#update-alternatives --set usplash-artwork.so /usr/lib/usplash/CrunchyBranch.so

# remove packages
apt-get -y clean 
apt-get -y autoremove 

# purge package
PURGEPKG=$(dpkg -l | grep ^rc | cut -d' ' -f3)
[ -z "${PURGEPKG}" ] || dpkg -P ${PURGEPKG}

# Removing unused files
find . -name *~ | xargs rm -f

rm -f /var/lib/apt/lists/*Release*
rm -f /var/lib/apt/lists/*Sources*
rm -f /var/lib/apt/lists/*Packages*

# Truncating logs
for FILE in $(find /var/log/ -type f)
do
        : > ${FILE}
done

# Cleaning /lib/init/rw/*
rm -rf /lib/init/rw/*

# prelink
[ -f /usr/sbin/prelink ] && prelink -afmR
