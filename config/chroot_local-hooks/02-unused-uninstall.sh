#!/bin/bash -x
set -e
#apt-get -y remove libnusdas-ruby1.8 libnusdas-ruby
apt-get -y remove uim-anthy uim-gtk2.0 uim-utils uim-xim 
apt-get -y autoremove
