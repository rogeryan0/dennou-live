all: usage

usage:
	@echo "make config-iso|config-usb|build|build-iso|build-usb|clean|distclean"

config-iso: distclean
	lb config --binary-images iso

config-usb: distclean
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

sync: sync_desktop
	rsync -Cauvlz dennou* dennou-k.gfd-dennou.org:Public/tmp

sync_spmodel:
	rsync -Cauvz --delete \
	--exclude "SIGEN.htm" \
	--exclude "*.SIGEN" \
	--exclude ".svn" \
	--exclude "LiveCD*" \
	--exclude "Makefile.rd2html" \
	--exclude "TEBIKI.*" \
	--exclude "create-snapshot.sh" \
	--exclude "debian" \
	--exclude "docform" \
	--exclude "ispack-f90*" \
	--exclude "old" \
	--exclude "presen" \
	--exclude "spml" \
	--exclude "spml-*.tar.gz" \
	--exclude "spml_*.tar.gz" \
	--exclude "svnroot" \
	--exclude "tutorial" \
	dennou-k.gfd-dennou.org:/GFD_Dennou_Club/ftp/arch/spmodel/ \
	config/includes.chroot/etc/skel/Desktop/Tutorial/spmodel/WebPages/
	rm -f config/includes.chroot/etc/skel/Desktop/Tutorial/spmodel/WebPages/Makefile
	rm -f config/includes.chroot/etc/skel/Desktop/Tutorial/spmodel/WebPages/html/Makefile
	rsync -Cauvz -L \
	--exclude "*.SIGEN" \
	--exclude "SIGEN.htm" \
	--exclude "old" \
	--exclude "*_20*.odp" \
	--exclude "*_20*.pdf" \
	--exclude "Makefile.rd2html" \
	dennou-k.gfd-dennou.org:/GFD_Dennou_Club/ftp/arch/spmodel/tutorial/ \
	config/includes.chroot/etc/skel/Desktop/Tutorial/spmodel/

sync_gphys:
	rsync -Cauvz --delete \
		--exclude "gphys" \
		--exclude "gphys-cvs_snapshot.tar.gz" \
		--exclude "gphys-*.tar.gz"\
		--exclude "tutorial_old" \
		--exclude "release" \
		--exclude "SIGEN.htm" \
		--exclude "nc-conventions_gphys.xls" \
		--exclude "*.SIGEN" \
		dennou-k.gfd-dennou.org:/GFD_Dennou_Club/ftp/arch/ruby/products/gphys/ \
		config/includes.chroot/etc/skel/Desktop/Tutorial/gphys/

sync_desktop: sync_spmodel sync_gphys
	rsync -Cauvlz --delete \
		config/includes.chroot/etc/skel/Desktop \
		dennou-k.gfd-dennou.org:Public/tmp/

