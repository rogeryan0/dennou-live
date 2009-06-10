#!/bin/bash 

# read user directory
# XDG_DESKTOP_DIR = desktop directory
. ${XDG_CONFIG_HOME}/user-dirs.dirs

# icons directory
ICON_DIR=/usr/share/applications

# copy icons
ICONS="skype.desktop icedove.desktop sylpheed.desktop AdobeReader.desktop evince.desktop epdfview.desktop f-spot.desktop filezilla.desktop gimp.desktop inkscape.desktop lxde-x-www-browser.desktop ooo-startcenter.desktop pidgin.desktop rhythmbox.desktop system-config-printer.desktop tsclient.desktop vlc.desktop"
for i in ${ICONS}; do
  if [ -e ${ICON_DIR}/${i} ]; then
    [ -e ${XDG_DESKTOP_DIR}/${i} ] || cp ${ICON_DIR}/${i} ${XDG_DESKTOP_DIR}/
  fi
done

# create debian.or.jp
DEBIAN_ICON=${XDG_DESKTOP_DIR}/debian.desktop

if [ ! -e ${DEBIAN_ICON} ]; then
  cat << _EOL_ >>${DEBIAN_ICON}
[Desktop Entry]
Encoding=UTF-8
Name=Debian JP Project
Exec=/usr/bin/x-www-browser http://www.debian.or.jp/
Type=Application
Icon=/usr/share/pixmaps/debian-logo.png
_EOL_
fi
