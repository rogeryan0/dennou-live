#!/bin/bash -x
set -e

# install backports package

#openoffice.org openoffice.org-gtk ttf-opensymbol \
#openoffice.org-help-ja openoffice.org-l10n-ja \
#openoffice.org-pdfimport openoffice.org-ogltrans \
#dictionaries-common"
#apt-get -y -t lenny-backports install ${PACKAGES}
#apt-get -y purge lxnm

PACKAGES="wicd acpid rubygems rubygems1.8 ruby1.8 ruby rdoc1.8 rdoc ruby1.8-dev libopenssl-ruby1.8 xfonts-efont-unicode xfonts-efont-unicode-ib openssh-client openssh-blacklist openssh-blacklist-extra xdg-user-dirs-gtk"
apt-get -y install ${PACKAGES}

PACKAGES="less"
apt-get -y install ${PACKAGES}


debconf-set-selections <<EOF
firmware-ipw2x00	firmware-ipw2x00/license/accepted	boolean	true
EOF
PACKAGES="atmel-firmware firmware-bnx2 firmware-bnx2x firmware-iwlwifi firmware-linux-free firmware-linux-nonfree firmware-linux firmware-qlogic firmware-ralink libertas-firmware zd1211-firmware"
apt-get -y install ${PACKAGES}

apt-get -y install librd-html-ext-ruby1.8

gem install rails -v=2.3.5
gem install mongrel
#apt-get -y install rails
gem install rdoc-f95



