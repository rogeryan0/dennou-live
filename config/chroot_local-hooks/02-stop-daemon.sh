#!/bin/sh
#update-rc.d -f openbsd-inetd remove
#update-rc.d -f stop-bootlogd remove
#update-rc.d -f cron remove
#update-rc.d -f syslogd remove
#update-rc.d -f saned remove
DAEMON="openbsd-inetd stop-bootlogd cron syslogd saned clamav-freshclam bluetooth rsync"
for i in $DAEMON; do update-rc.d -f $i; done
