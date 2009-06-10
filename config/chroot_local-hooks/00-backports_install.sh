#!/bin/bash -x
set -e

# install backported package 
PACKAGES="openoffice.org-pdfimport openoffice.org-presentation-minimizer openoffice.org-presenter-console openoffice.org-filter-binfilter openoffice.org-report-builder openoffice.org-filter-mobiledev openoffice.org-help-ja ttf-opensymbol jd"
apt-get -y -t lenny-backports install ${PACKAGES}
#apt-get -y -t lenny-backports upgrade

# lxde
if [ -e /usr/bin/startlxde ]; then
  apt-get -y -t lenny-backports install wicd
  apt-get -y purge lxnm
fi
