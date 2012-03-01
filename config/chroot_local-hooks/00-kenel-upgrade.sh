#!/bin/bash -x
set -e

debconf-set-selections <<EOF
firmware-ipw2x00	firmware-ipw2x00/license/accepted	boolean	true
EOF
firmware_PACKAGES="atmel-firmware firmware-bnx2 firmware-bnx2x firmware-iwlwifi firmware-linux-free firmware-linux-nonfree firmware-linux firmware-qlogic firmware-ralink libertas-firmware zd1211-firmware"
apt-get -y install ${firmware_PACKAGES} -t squeeze-backports

aptitude -y install console-tools
aptitude -y remove kbd
aptitude -y remove wicd
aptitude -y install -R 
