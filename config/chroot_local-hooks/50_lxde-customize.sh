#!/bin/bash -x
set -e

# SMB automatic mount in LXDE by fusesmb.

[ -f /usr/bin/startlxde -a -f /usr/bin/fusesmb ] || exit 0

SMBMOUNTSCRIPT=/usr/local/bin/mount-fusesmb.sh
SMBMOUNTAUTOSTART=/etc/skel/.config/autostart/mountsmb.desktop

# create mount-fusesmb.sh
cat << _EOL_ >>${SMBMOUNTSCRIPT}
#!/bin/bash

# read user directory
# XDG_DESKTOP_DIR = desktop directory
. \${XDG_CONFIG_HOME:-~/.config}/user-dirs.dirs

# set permission
FUSESMBCONF=\${HOME}/.smb/fusesmb.conf
if [ ! -f \${FUSESMBCONF} ]; then
  mkdir -p \${HOME}/.smb/
  cp -f /usr/share/doc/fusesmb/examples/fusesmb.conf.ex \${FUSESMBCONF}
fi
/bin/chmod 600 \${FUSESMBCONF}

# check mount directory
SMBMOUNTDIR=\${XDG_DESKTOP_DIR}/WindowsNetwork
[ -d \${SMBMOUNTDIR} ] || mkdir -p "\${SMBMOUNTDIR}"

# mount fusesmb
[ -n "\$(mount | grep fusesmb)" ] || /usr/bin/fusesmb "\${SMBMOUNTDIR}"
_EOL_

chown 1000:1000 ${SMBMOUNTSCRIPT}
chmod 755 ${SMBMOUNTSCRIPT}

# create autostart shortcut
cat << _EOL_ >>${SMBMOUNTAUTOSTART}
[Desktop Entry]
Encoding=UTF-8
Type=Application
Exec=/usr/local/bin/mount-fusesmb.sh
Name=Mount WindowsNetwork
_EOL_

