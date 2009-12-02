#!/bin/sh
set -e

# set uim-systray
/usr/sbin/update-alternatives --set xinput-ja_JP /etc/X11/xinit/xinput.d/uim-toolbar

# set scim-bridge
#/usr/sbin/update-alternatives --install /etc/X11/xinit/xinput.d/ja_JP xinput-ja_JP /etc/X11/xinit/xinput.d/scim-bridge 0
#/usr/sbin/update-alternatives --set xinput-ja_JP /etc/X11/xinit/xinput.d/scim-bridge
