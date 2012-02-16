#!/bin/bash -x
set -e
apt-get -y --no-install-recommends install ptex-bin texlive-latex-extra okumura-clsfiles ghostscript-x gs-cjk-resource xdvik-ja poppler-data cmap-adobe-japan1 cmap-adobe-japan2 dvipsk-ja dvipdfmx
jisftconfig add
update-vfontmap
apt-get -y install dennou-sty-6
apt-get -y install evince-gtk
