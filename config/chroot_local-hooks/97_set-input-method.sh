#!/bin/sh
set -e
# use ibus, see /etc/skel/.gconf/ibus
apt-get -y install ibus-anthy ibus-gtk ibus-qt4
/usr/sbin/update-alternatives --set xinput-ja_JP /etc/X11/xinit/xinput.d/ibus
