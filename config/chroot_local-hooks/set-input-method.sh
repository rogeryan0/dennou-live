#!/bin/sh
set -e

# set uim-systray
/usr/sbin/update-alternatives --set xinput-ja_JP /etc/X11/xinit/xinput.d/uim-systray
