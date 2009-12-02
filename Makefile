BOOTOPTION_LIVE = locale=ja_JP.UTF-8 kmodel=jp106 vga=788 
BOOTOPTION_INSTALLER = -- video=vesa:ywrap,mtrr vga=788

build: clean config-lenny config-iso config-lxde
	sudo lh_build

build-usb: clean config-lenny config-usb config-lxde
	sudo lh_build

clean:
	sudo lh_clean

distclean: clean
	sudo rm -f *.list *.packages *.buildlog *.md5sum

distclean.all: distclean
	sudo lh_clean --purge
	sudo rm -f *.iso *.img

config-lenny:
	lh_config \
		--distribution lenny \
		--bootappend-live "$(BOOTOPTION_LIVE) keyb=jp106" \
		--linux-packages "linux-image-2.6 aufs-modules-2.6 squashfs-modules-2.6" 

config-sid:
	lh_config \
		--distribution sid \
		--bootappend-live "$(BOOTOPTION_LIVE) klayout=jp" \
		--linux-packages "linux-image-2.6 aufs-modules-2.6" 

config-usb:
	lh_config --binary-images usb-hdd 

config-iso: 
	lh_config --binary-images iso 

config-lxde: 
	lh_config \
		--bootappend-install "$(BOOTOPTION_INSTALLER) desktop=lxde" \
		--linux-flavours 686 \
		--packages-lists "lxde 01-system 10-lxde-application 20-japanese 99-dennou"

