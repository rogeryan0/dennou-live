BOOTOPTION_LIVE = quiet locale=ja_JP.UTF-8 keyb=jp kmodel=jp106 vga=788 splash
BOOTOPTION_INSTALLER = -- video=vesa:ywrap,mtrr vga=788

build: 
	sudo lh_build 

clean:
	sudo lh_clean

distclean: clean
	sudo lh_clean --purge
	sudo rm -f *.iso *.img *.list *.packages *.buildlog *.md5sum

config-usb:
	lh_config \
		--bootloader syslinux \
		--binary-images usb-hdd 

config-iso: 
	lh_config \
		--bootloader grub \
		--binary-images iso 

lxde-config: clean
	lh_config \
		--bootappend-live "$(BOOTOPTION_LIVE)" \
		--bootappend-install "$(BOOTOPTION_INSTALLER) desktop=lxde" \
                --linux-packages "linux-image-2.6 aufs-modules-2.6 squashfs-modules-2.6" \
		--linux-flavours 486 \
		--packages-lists "lxde 01-system 10-lxde-application 20-japanese" 

gnome-config: clean
	lh_config \
		--bootappend-live "$(BOOTOPTION_LIVE)" \
		--bootappend-install "$(BOOTOPTION_INSTALLER) desktop=gnome" \
		--linux-packages "linux-image-2.6 aufs-modules-2.6 squashfs-modules-2.6" \
		--linux-flavours 686 \
		--packages-lists "gnome-full 01-system 10-gnome-application 20-japanese 30-rescue 50-debian_meeting" 

lxde-iso: lxde-config config-iso build

lxde-usb: lxde-config config-usb build

gnome-iso: gnome-config config-iso build

gnome-usb: gnome-config config-usb build

all: gnome-usb

