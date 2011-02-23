#!/bin/sh
set -e

apt-get -y install ibus-anthy ibus-gtk ibus-qt4
# set uim-toolbar
#/usr/sbin/update-alternatives --set xinput-ja_JP /etc/X11/xinit/xinput.d/uim-toolbar
# set ibus-anthy
/usr/sbin/update-alternatives --set xinput-ja_JP /etc/X11/xinit/xinput.d/ibus
