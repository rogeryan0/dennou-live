#!/bin/sh

# This is a hook for live-helper(7) to enable virtualbox-ose module.
# To enable it, copy or symlink this hook into your config/chroot_local-hooks
# directory.

#apt-get install --yes build-essential

# Building kernel module
#which module-assistant || apt-get install --yes module-assistant
#module-assistant update

#for KERNEL in /boot/vmlinuz-*
#do
#   VERSION="$(basename ${KERNEL} | sed -e 's|vmlinuz-||')"
#   module-assistant --non-inter --quiet auto-install virtualbox-ose -l ${VERSION}
#done

#module-assistant clean virtualbox-ose

# Enabling loading of vboxdrv
#sed -i -e 's|^LOAD_VBOXDRV_MODULE=.*$|LOAD_VBOXDRV_MODULE=1|' /etc/default/virtualbox-ose
