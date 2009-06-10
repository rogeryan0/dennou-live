#!/bin/bash -x
set -e

ICEWEASEL_PREFS=/etc/iceweasel/profile/prefs.js

cat << _EOL_ >>${ICEWEASEL_PREFS}

/* Debian Live tune */
user_pref("browser.cache.disk.parent_directory","/tmp");
user_pref("browser.cache.disk.capacity", 5000);
user_pref("browser.startup.homepage", "http://www.debian.or.jp/");
_EOL_
