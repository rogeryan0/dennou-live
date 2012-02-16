#!/bin/bash -x
set -e
gem install rails -v=2.3.5 --include-dependencies --no-ri --no-rdoc
gem install mongrel --no-ri --no-rdoc --include-dependencies
gem install rdoc-f95 --no-ri --no-rdoc --include-dependencies
