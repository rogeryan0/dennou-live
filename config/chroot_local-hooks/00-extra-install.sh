#!/bin/bash -x
set -e

# install backports package 

PACKAGES="wicd pidgin acpid " #\
#openoffice.org openoffice.org-gtk ttf-opensymbol \
#openoffice.org-help-ja openoffice.org-l10n-ja \
#openoffice.org-pdfimport openoffice.org-ogltrans \
#dictionaries-common"
apt-get -y -t lenny-backports install ${PACKAGES}
apt-get -y purge lxnm

# RubyGems
Default_PACKAGES="ruby1.8 ruby rdoc1.8 rdoc ruby1.8-dev libopenssl-ruby1.8 "
PACKAGES="rubygems rubygem1.8"
apt-get -y install ${Default_PACKAGES}
apt-get -y -t lenny-backports install ${PACKAGES}

gem install rails
gem install rdoc-f95

