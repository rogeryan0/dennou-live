#!/bin/sh
set -e
# use ibus, see /etc/skel/.gconf/ibus
aptitude purge uim uim-anthy uim-common uim-fep uim-gtk2.0 uim-gtk3 uim-qt uim-utils uim-xim
apt-get -y install ibus-anthy ibus-gtk ibus-gtk3 ibus-qt4
# /usr/sbin/update-alternatives --set xinput-ja_JP /etc/X11/xinit/xinput.d/ibus
im-config -n ibus
