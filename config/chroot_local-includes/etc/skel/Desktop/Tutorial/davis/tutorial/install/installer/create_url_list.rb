#
# create_url_list.rb : Generate a download URL list for numru-install.rb
#
# Time-stamp: <2006-03-07 22:08:39 koshiro>
#
# Copyright (C) 2006  GFD Dennou Club      <http://www.gfd-dennou.org>
#                     Dennou Ruby Project  <http://ruby.gfd-dennou.org>
#
require "open-uri"
require "net/ftp"
require "yaml"

module NumRu
	module Installer
		def create_latest_url(name, dl_url, lver_chk_url)
			case name
			when "dcl-C"
				regexp = /^dcl-([0-9p\.-]+)-C\.tar\.gz$/
			when "ruby-gnome2-all"
				regexp = /^ruby-gnome2-all-(0.1[0-9p\.-]+)\.tar\.gz$/
			else
				regexp = /^#{name}[\.-]([0-9\.-]+)\.tar\.gz$/
			end
			if /^http:\/\// =~ lver_chk_url
				str = URI(lver_chk_url).read
				flist = []
				str.split(/\n/).each do |line|
					line.scan(/<A HREF="(.*)">(.*)<\/A>/i) do |href, fname|
						href.strip!
						fname.strip!
						flist << fname if File.basename(href) == fname && regexp =~ fname
					end
				end
			elsif /^ftp:\/\// =~ lver_chk_url
				uri = URI(lver_chk_url)
				host = uri.host
				remote_dir = uri.path
				ftp = Net::FTP.new(host)
				ftp.passive = true
				ftp.login("anonymous", nil)
				ftp.chdir(remote_dir)
				line = ftp.list
				flist = []
				line.each do |l|
					fname = l.split[-1]
					flist << fname if regexp =~ fname
				end
			else
				flist = Dir.glob(File.join(lver_chk_url,"*")).collect{|f| File.basename(f)}
				flist.delete_if{|f| regexp !~ f}
			end
			latest_url = File.join(dl_url, flist.max)
		end
		module_function :create_latest_url
	end
end

if __FILE__ == $0
	include NumRu::Installer

	outfile = "pkg_url.lst"

	packages = {
		"numru_installer" => "http://ruby.gfd-dennou.org/products/numru_installer/"+Dir.glob("dist/numru-install-*.rb").max,
		"netcdf" => [create_latest_url("netcdf","http://www.gfd-dennou.org/arch/netcdf/unidata-mirror/","../../../netcdf/unidata-mirror/"), "http://www.gfd-dennou.org/arch/ruby/products/installer/patches/netcdf361_cygwin.patch.gz"],
		"fftw-2" => create_latest_url("fftw-2","http://ftp.fftw.org/","http://ftp.fftw.org/download.html"),
		"fftw-3" => create_latest_url("fftw-3","http://ftp.fftw.org/","http://ftp.fftw.org/download.html"),
		"libdap" => create_latest_url("libdap","ftp://ftp.unidata.ucar.edu/pub/opendap/source/","ftp://ftp.unidata.ucar.edu/pub/opendap/source/"),
		"libnc-dap" => create_latest_url("libnc-dap","ftp://ftp.unidata.ucar.edu/pub/opendap/source/","ftp://ftp.unidata.ucar.edu/pub/opendap/source/"),
		"narray" => create_latest_url("narray","http://rubyforge.org/frs/download.php/7658/","http://narray.rubyforge.org/"),
		"ruby-gnome-all" => create_latest_url("ruby-gnome-all","http://jaist.dl.sourceforge.net/sourceforge/ruby-gnome/","http://prdownloads.sourceforge.net/ruby-gnome/"),
		"ruby-gnome2-all" => create_latest_url("ruby-gnome2-all","http://jaist.dl.sourceforge.net/sourceforge/ruby-gnome2/","http://prdownloads.sourceforge.net/ruby-gnome2/"),
		"dcl-C" => [create_latest_url("dcl-C","http://www.gfd-dennou.org/arch/dcl/","../../../dcl/"), "http://www.gfd-dennou.org/arch/ruby/products/installer/patches/dcl53c_cygwin.patch.gz"],
		"narray_miss" => create_latest_url("narray_miss","http://www.gfd-dennou.org/arch/ruby/products/narray_miss/","../narray_miss/"),
		"ruby-dcl" => create_latest_url("ruby-dcl","http://www.gfd-dennou.org/arch/ruby/products/ruby-dcl/","../ruby-dcl/"),
		"ruby-netcdf" => create_latest_url("ruby-netcdf","http://www.gfd-dennou.org/arch/ruby/products/ruby-netcdf/","../ruby-netcdf/"),
		"ruby-fftw3" => create_latest_url("ruby-fftw3","http://www.gfd-dennou.org/arch/ruby/products/ruby-fftw3/","../ruby-fftw3/"),
		"numru-units" => create_latest_url("numru-units","http://www.gfd-dennou.org/arch/ruby/products/numru-units/","../numru-units/"),
		"numru-misc" => create_latest_url("numru-misc","http://www.gfd-dennou.org/arch/ruby/products/numru-misc/","../numru-misc/"),
		"gphys" => create_latest_url("gphys","http://www.gfd-dennou.org/arch/ruby/products/gphys/","../gphys/"),
		"gave" => create_latest_url("gave","http://www.gfd-dennou.org/arch/ruby/products/gave/","../gave/"),
		"multibitnums" => create_latest_url("multibitnums","http://www.gfd-dennou.org/arch/ruby/products/multibitnums/","../multibitnums/"),
		"gpv" => create_latest_url("gpv","http://www.gfd-dennou.org/arch/ruby/products/gpv/","../gpv/"),
		"met" => create_latest_url("met","http://www.gfd-dennou.org/arch/ruby/products/met/","../met/"),
	}

	File.open(outfile,"w") do |f|
		f.puts(packages.to_yaml)
	end
end
