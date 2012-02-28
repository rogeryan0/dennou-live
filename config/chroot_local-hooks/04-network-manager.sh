#!/bin/bash -x
set -e
aptitude -y install \
  network-manager-gnome \
  network-manager-pptp-gnome \
  network-manager-openvpn-gnome \
  network-manager-vpnc-gnome
aptitude -y remove wicd-gtk wicd-daemon wicd wicd-cli wicd-curses 
