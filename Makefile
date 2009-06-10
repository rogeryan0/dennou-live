BOOTOPTION_LIVE = quiet locale=ja_JP.UTF-8 keyb=jp kmodel=jp106 vga=788 splash
BOOTOPTION_INSTALLER = -- quiet video=vesa:ywrap,mtrr vga=788

all: lxde

lxde: lxde-config-usb build

lxde-config-usb: clean
	lh_config \
	 	--binary-images usb-hdd \
		--bootappend-live "$(BOOTOPTION_LIVE)" \
		--bootappend-install "$(BOOTOPTION_INSTALLER) desktop=lxde" \
		--linux-flavours 486 \
		--packages-lists "lxde 01-system 10-lxde-application 50-japanese" \

gnome: gnome-config-usb build

gnome-config-usb: clean
	lh_config \
	 	--binary-images usb-hdd \
		--bootappend-live "$(BOOTOPTION_LIVE)" \
		--bootappend-install "$(BOOTOPTION_INSTALLER) desktop=gnome" \
		--linux-flavours 686 \
		--packages-lists "gnome 01-system 50-japanese" \

build: 
	sudo lh_build 

clean:
	sudo lh_clean

distclean: clean
	sudo lh_clean --purge
	sudo rm -f *.iso *.img *.list *.packages *.buildlog *.md5sum

