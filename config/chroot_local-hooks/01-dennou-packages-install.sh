#!/bin/bash -x
set -e

gem install rails -v=2.3.14 --no-ri --no-rdoc
gem install mongrel --no-ri --no-rdoc 
gem install rdoc-f95 --no-ri --no-rdoc 
gem install json --no-ri --no-rdoc 

apt-get install -y spml-g95 netcdf-bin
aptitude install -y gphys=1.2.0-1~bpo60+1 
apt-get install -y pry
