LINUX_PACKAGES   := linux-image-2.6 aufs-modules-2.6 squashfs-modules-2.6
BOOT_OPTION_LIVE := locale=ja_JP.UTF-8 keyb=jp kmodel=jp106 vga=788 splash
BOOT_OPTION_INSTALLER = -- video=vesa:ywrap,mtrr vga=788
MIRROR_DEBIAN="http://ftp.jp.debian.org/debian/"
MIRROR_SECURITY="http://security.debian.org/"

all:
	@echo "usage: make build|clean"
clean:
	sudo lh clean --binary
distclean:
	sudo lh clean --all
	sudo rm -f *.iso *.img *.list *.packages *.buildlog *.md5sum

build: build-iso
build-iso: config-lenny config-iso config-lxde
	sudo lh build
build-usb: config-lenny config-usb config-lxde
	sudo lh build

config-lenny:
	lh config \
	 --language ja \
	 --distribution lenny \
	 --archive-areas "main" \
	 --bootappend-live "${BOOT_OPTION_LIVE}" \
	 --linux-packages "${LINUX_PACKAGES}" \
	 --mirror-bootstrap "${MIRROR_DEBIAN}" \
	 --mirror-chroot "${MIRROR_DEBIAN}" \
	 --mirror-chroot-security "${MIRROR_SECURITY}" \
	 --mirror-binary "${MIRROR_DEBIAN}" \
	 --mirror-binary-security "${MIRROR_SECURITY}" \
	 --bootloader syslinux \
	 --syslinux-menu vesamenu \
	 --syslinux-timeout 30

config-usb:
	lh config --binary-images usb-hdd
config-iso:
	lh config --binary-images iso

config-lxde:
	lh config \
	--bootappend-install "$(BOOT_OPTION_INSTALLER)" \
	--packages-lists lxde
#	--linux-flavours 486 


# config-sid:
# lh_config \
# 	--distribution sid \
# 	--bootappend-live "$(BOOTOPTION_LIVE) klayout=jp" \
# 	--linux-packages "linux-image-2.6 aufs-modules-2.6"

# config-gnome:
# 	lh_config \
# 	--bootappend-install "$(BOOTOPTION_INSTALLER) desktop=gnome" \
# 	--linux-flavours 686 \
# 	--packages-lists "gnome-full 01-system 10-gnome-application 20-japanese"
