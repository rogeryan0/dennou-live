all: build

config-iso: clean
	lb config --binary-images iso

config-usb: clean
	lb config --binary-images usb-hdd

build: build-iso build-usb
build-iso: config-iso
	sudo lb build
build-usb: config-usb
	sudo lb build

clean:
	sudo lb clean

distclean: clean
	sudo lb clean --purge
	sudo rm -f *.iso *.img *.list *.packages *.buildlog *.md5sum

sync:
	rsync -Cauvlz dennou* dennou-k.gfd-dennou.org:Public/tmp
