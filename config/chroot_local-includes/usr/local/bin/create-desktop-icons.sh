#!/bin/bash

# read user directory
# XDG_DESKTOP_DIR = desktop directory
. ${XDG_CONFIG_HOME:-~/.config}/user-dirs.dirs

# icons directory
ICON_DIR=/usr/share/applications

# PDF Viewer
PDF=AdobeReader.desktop
if ! [ -f ${ICON_DIR}/${PDF} ]; then
  PDF=evince.desktop
elif ! [ -f ${ICON_DIR}/${PDF} ]; then
  PDF=epdfview.desktop
fi

# web browser
BROWSER=lxde-x-www-browser.desktop
[ -f ${ICON_DIR}/${BROWSER} ] || BROWSER=iceweasel.desktop

# MUA
MUA=sylpheed.desktop
[ -f ${ICON_DIR}/${MUA} ] || MUA=icedove.desktop

# Media Player
MPLAYER=vlc.desktop
[ -f ${ICON_DIR}/${MPLAYER} ] || MPLAYER=totem.desktop

# Sound Player
SPLAYER=exaile.desktop
[ -f ${ICON_DIR}/${SPLAYER} ] || SPLAYER=rhythmbox.desktop

# skype
SKYPE=skype.desktop
[ -f ${ICON_DIR}/${SKYPE} ] || SKYPE=""

# copy icons
ICONS="${SKYPE} ${MUA} ${BROWSER} ${MPLAYER} ${SPLAYER} ${PDF} filezilla.desktop gimp.desktop inkscape.desktop ooo-startcenter.desktop pidgin.desktop tsclient.desktop"
for i in ${ICONS}; do
  [ -e ${XDG_DESKTOP_DIR}/${i} ] || cp ${ICON_DIR}/${i} ${XDG_DESKTOP_DIR}/
done

# create debian.or.jp
DEBIAN_ICON=${XDG_DESKTOP_DIR}/debian.desktop

[ -f ${DEBIAN_ICON} ] && exit 0
 
cat << _EOL_ >>${DEBIAN_ICON}
[Desktop Entry]
Encoding=UTF-8
Name=Debian JP Project
Exec=/usr/bin/x-www-browser http://www.debian.or.jp/
Type=Application
Icon=/usr/share/pixmaps/debian-logo.png
_EOL_
