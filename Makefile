BOOT_OPTION_LIVE := quiet locale=ja_JP.UTF-8 keyb=jp kmodel=acpi utc=no tz=Asia/Tokyo persistent
BOOT_OPTION_INSTALLER = -- locale=ja_JP.UTF-8
LINUX_PACKAGES   := linux-image-2.6 aufs-modules-2.6 squashfs-modules-2.6 loop-aes-modules-2.6

all:
	@echo "make build-iso|build-usb|clean|distclean"

clean:
	sudo lh clean
	sudo rm -f *.list *.packages *.buildlog *.md5sum

distclean: clean
	sudo lh clean --purge
	sudo rm -f *.iso *.img 

build: build-iso
build-iso: clean config-lenny config-iso config-lxde
	sudo lh build
build-usb: clean config-lenny config-usb config-lxde
	sudo lh build

config-lenny:
	lh config \
	--distribution lenny \
	--bootappend-live "${BOOT_OPTION_LIVE}" \
	--linux-packages "${LINUX_PACKAGES}" \
	--iso-publisher "GFD Dennou Club; http://www.gfd-dennou.org/;"  \
	--debian-installer-gui true

config-usb:
	lh config \
	--binary-images usb-hdd \
	--binary-filesystem fat32

config-iso:
	lh config --binary-images iso

config-lxde:
	lh config \
	--bootappend-install "$(BOOT_OPTION_INSTALLER)" \
	--packages-lists "lxde"
