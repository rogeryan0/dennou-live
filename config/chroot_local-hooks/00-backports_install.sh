#!/bin/bash -x
set -e

# install backports package 

# lxde
if [ -f /usr/bin/startlxde ]; then
PACKAGES="wicd jd \
openoffice.org openoffice.org-gtk ttf-opensymbol \
openoffice.org-help-ja openoffice.org-l10n-ja \
openoffice.org-pdfimport openoffice.org-ogltrans"
  apt-get -y -t lenny-backports install ${PACKAGES}
  apt-get -y purge lxnm
fi

# gnome
if [ -f /usr/bin/gnome-session ]; then
PACKAGES="jd \
openoffice.org openoffice.org-gnome ttf-opensymbol \
openoffice.org-help-ja openoffice.org-l10n-ja \
openoffice.org-pdfimport openoffice.org-ogltrans \
openoffice.org-presentation-minimizer openoffice.org-presenter-console \
openoffice.org-filter-binfilter openoffice.org-report-builder \
openoffice.org-filter-mobiledev"
  apt-get -y -t lenny-backports install ${PACKAGES}
fi

