#!/bin/sh
DAEMON="openbsd-inetd stop-bootlogd cron syslogd saned clamav-freshclam rsync"
for i in ${DAEMON}; do /usr/sbin/update-rc.d -f ${i} remove; done
