=begin
numru-install.rb : The Dennou Ruby Products Installer
$Id: numru-install.rb,v 1.48 2006/02/07 17:38:44 koshiro Exp $

Change Log:

* 2006/02/08  KOSHIRO Tsuyoshi <koshiro at gfd-dennou.org>
  - (3.4.6)
  - change URL of narray.
  - change installation check method "installed?" for ruby-fftw3.

* 2005/06/27  KOSHIRO Tsuyoshi <koshiro at gfd-dennou.org>
  - (3.4.5)
  - add installation check by method "installed?" after installation.
  - change module NumRu::Fetch::http_get_list
  - stop using getopts.
  - use an external Ruby process to load ruby library for installation check.

* 2005/05/24  KOSHIRO Tsuyoshi <koshiro at gfd-dennou.org>
  - (3.4.4)
  - remove rubydcl150_cygwin.patch.gz.

* 2005/03/17  KOSHIRO Tsuyoshi <koshiro at gfd-dennou.org>
  - (3.4.3)
  - change download site of ruby-gnome, ruby-gnome2: voxel.net -> JAIST

* 2005/03/03  KOSHIRO Tsuyoshi <koshiro at gfd-dennou.org>
  - (3.4.2)
  - deal with installation failure of the import library of narray on Cygwin + Ruby 1.8.2.

* 2005/03/03  KOSHIRO Tsuyoshi <koshiro at gfd-dennou.org>
  - (3.4.1)
  - use 3.6.1 beta release on Cygwin if the latest version of netcdf is
    3.6.0(-p?).

* 2005/03/03  KOSHIRO Tsuyoshi <koshiro at gfd-dennou.org>
  - (3.4.0)
  - connect only one time to FTP server when checking version of Dennou Ruby
    Products.
  - change URL of fftw: Japan mirror site -> original site

* 2005/03/01  KOSHIRO Tsuyoshi <koshiro at gfd-dennou.org>
  - (3.3.3)
  - deal with changing the installation script of narray_miss.
  - change regular expression for checking latest version of packages.
  - change URL of fftw: original site -> Japan mirror site

* 2005/01/15  KOSHIRO Tsuyoshi <koshiro at gfd-dennou.org>
  - (3.3.2)
  - deal with changing the installation script of gphys.

* 2005/01/06  KOSHIRO Tsuyoshi <koshiro at gfd-dennou.org>
  - (3.3.1)
  - apply rubydcl150_cygwin.patch.gz if needed.
  - change a configure option on Linux and a pacth file on Cygwin for netcdf.
  - use 3.6.1 beta release on Cygwin if the latest version of netcdf is 3.6.0.

* 2004/12/22  KOSHIRO Tsuyoshi <koshiro at gfd-dennou.org>
  - (3.3.0)
  - deal with changing of the tarball name and URL of dcl-C.
  - deal with changing the installation script of gave.

* 2004/08/12  KOSHIRO Tsuyoshi <koshiro at gfd-dennou.org>
  - (3.2.1)
  - bug fix.

* 2004/08/12  KOSHIRO Tsuyoshi <koshiro at gfd-dennou.org>
  - (3.2.0)
  - change class RbLib: deal with changing installation script of narray_miss.

* 2004/08/11  KOSHIRO Tsuyoshi <koshiro at gfd-dennou.org>
  - (3.1.0)
  - change class RubySoft: deal with changing installation script of gave.
  - name of package is changed: misc -> numru-misc
  - bug fix.

* 2004/07/15  KOSHIRO Tsuyoshi <koshiro at gfd-dennou.org>
  - (3.0.0)
  - add packages: fftw, ruby-fftw3, multibitnums, gpv, numru-units,
                  ruby-{gnome|gnome2}-all, gave
  - change module NumRu::Fetch::ftp_fetch, NumRu::Fetch::ftp_get_list
    + add check environment variable 'FTP_PASSIVE_MODE'
  - revise module NumRu::Install
    + add checking of GTK version
    + add class Installer: add self-version-check and self-download function
    + add method 'install' to class RubySoft
    + add class RubyLib, which is superclass of RbExtLib and RbLib
    + change method 'current_ver'(Software, RubySoft): version + GTK version
    + add method 'current_ver_disp' for class Software
    + revise the scheme of checking package dependencies:
      = add method 'get_dep_check_pkgs'
      = revise method 'check_depend'
    + add module function 'upgrade'
  - change package dependencies:
    + rubydcl requires ruby-{gnome|gnome2}-all
    + gphys requires ruby-fftw3 and numru-units
  - 'gpatch' is used, if found, instead of 'patch'.
  - bug fix.

* 2003/12/18  KOSHIRO Tsuyoshi <koshiro at gfd-dennou.org>
  - (2.0.1)
  - bug fix. Thanks to Nishizawa-san.
    + change uncompress command: 'zcat' -> 'gzip -dc'
    + rescue exceptions in Net::HTTP#get (for Ruby 1.6.7)
  - apply netcdf350_cxx_gcc3.patch if needed

* 2003/12/10  KOSHIRO Tsuyoshi <koshiro at gfd-dennou.org>
  - (2.0.0)
  - add packages: misc, met, gphys
  - revise module NumRu::Install
    + select download directory
    + select installation prefix and Ruby libraries directory
    + check file and version dependencies
    + apply patches
    + extra processing for Cygwin

* 2003/06/27  KOSHIRO Tsuyoshi <koshiro at gfd-dennou.org>
  - (1.0.0)
  - first release.
  - Thanks to Gotoken-san: contributed by coding module NumRu::Fetch


(C) 2003-2005  GFD Dennou Club      http://www.gfd-dennou.org/
               Dennou Ruby Project  http://ruby.gfd-dennou.org/
=end

require "ftools"
require "uri"
require "net/http"
require "net/ftp"
Net::HTTP.version_1_2

module NumRu
	DennouRubyProductUrl = "ftp://www.gfd-dennou.org/arch/ruby/products/"

	module Fetch
		FetchError = Class.new(StandardError)

		def http_fetch(uri, header, proxy, out = STDOUT)
			proxyclass = Net::HTTP
			proxyclass = Net::HTTP::Proxy(proxy.host, proxy.port) if proxy

			while true
				host = uri.host
				port = uri.port
				request_uri = uri.request_uri
				http = proxyclass.new(host, port)
				begin
					res = http.get(request_uri, header)
               res = res.shift if res.class == Array
				rescue Net::ProtoRetriableError
					if RUBY_VERSION == "1.6.7"
						res = $!.response
					else
						raise
					end
				rescue TimeoutError, Errno::ETIMEDOUT
					print "\n*Error*  HTTP connection timeout.\n"
					print "         If your Internet connection is via a firewall or proxy, you must set\n"
					print "         the environment variable \"http_proxy\" with the following syntax:\n"
					print "         http_proxy=http://your.proxy.server:8080/\n\n"
					raise
				end
				break if /3../ !~ res.code  # /3../ for redirection
				uri = uri.merge(URI.parse(res["location"]))
			end

			out << res.body
		end
		module_function :http_fetch

		def ftp_fetch(uri, header, proxy, out = STDOUT)
			host = uri.host
			port = uri.port
			user = uri.user || "anonymous"
			pass = uri.password
			remote_file = uri.path
			ftp = Net::FTP.new(host)
			ftp.passive = /^YES$/i =~ ENV["FTP_PASSIVE_MODE"] ? true : nil
			ftp.login(user, pass)
			ftp.getbinaryfile(remote_file, "/dev/null"){ |b|
				out << b
			}
			ftp.close
		end
		module_function :ftp_fetch

		def fetch(uri, header, proxy, out)
			scheme = uri.scheme
			case scheme
			when "http"
				http_fetch(uri, header, proxy, out)
			when "ftp"
				ftp_fetch(uri, header, proxy, out)
			else
				raise NumRu::Fetch::FetchError, "unsupported scheme `#{scheme}'"
			end
		end
		module_function :fetch

		def ftp_get_list(uri, header, proxy, list)
			host = uri.host
			port = uri.port
			user = uri.user || "anonymous"
			pass = uri.password
			remote_dir = uri.path
			ftp = Net::FTP.new(host)
			ftp.passive = /^YES$/i =~ ENV["FTP_PASSIVE_MODE"] ? true : nil
			ftp.login(user, pass)
			ftp.chdir(remote_dir)
			ftp.list.each do |f|
				line = f.split
				if line.include?("->")
					list << line[line.index("->")-1]
				else
					list << line[-1]
				end
			end
			ftp.close
		end
		module_function :ftp_get_list

		def ftp_get_list_dennou_ruby(uri, header, proxy, list)
			host = uri.host
			port = uri.port
			user = uri.user || "anonymous"
			pass = uri.password
			remote_dir = uri.path
			ftp = Net::FTP.new(host)
			ftp.passive = /^YES$/i =~ ENV["FTP_PASSIVE_MODE"] ? true : nil
			ftp.login(user, pass)
			ftp.chdir(remote_dir)
			product_dirs = Array.new
			ftp.list.each do |f|
				product_dirs << f.split[-1] if /^d/ =~ f
			end
			product_dirs.each do |d|
				list[d] = Array.new
				ftp.chdir(d)
				ftp.list.each do |f|
					line = f.split
					if line.include?("->")
						list[d] << line[line.index("->")-1]
					else
						list[d] << line[-1]
					end
				end
				ftp.chdir("..")
			end
			ftp.close
		end
		module_function :ftp_get_list_dennou_ruby

		def http_get_list(uri, header, proxy, list)
			dir_html = ""
			http_fetch(uri, header, proxy, dir_html)
			dir_html.split(/\n/).each do |line|
				line.scan(/<A HREF="(.*)">(.*)<\/A>/i) do |href, fname|
					if href.split(/\//)[-1] == fname
						if href.sub(/#{fname}/,"") == uri.path
							list << fname
						else
							list << href
						end
					end
				end
			end
		end
		module_function :http_get_list

		def get_list(uri, header, proxy, list)
			if uri.to_s == DennouRubyProductUrl
				ftp_get_list_dennou_ruby(uri, header, proxy, list)
			else
				scheme = uri.scheme
				case scheme
				when "http"
					http_get_list(uri, header, proxy, list)
				when "ftp"
					ftp_get_list(uri, header, proxy, list)
				else
					raise NumRu::Fetch::FetchError, "unsupported scheme `#{scheme}'"
				end
			end
		end
		module_function :get_list
	end

	module Install
		require "rbconfig"

		h = ENV["http_proxy"]
		Proxy = h ? URI.parse(h) : nil
		Header = {"accept-language"=>"en"}

		DefaultDownloadDir = Dir.pwd
		DefaultPrefix = "/usr/local"
		DefaultRbSiteLibDir = Config::CONFIG["sitelibdir"]
		DefaultVerListFile = "/usr/local/share/numru/numru_ver.lst"

		if /^2/ =~ `pkg-config gtk+-2.0 --modversion 2>&1`
			GtkMajorVer = "2"
		elsif /^1/ =~ `gtk-config --version 2>&1` && /^1/ =~ `imlib-config --version 2>&1`
			GtkMajorVer = "1"
		else
			GtkMajorVer = "0"
		end

		Patch = (RUBY_PLATFORM !~ /cygwin/ && system("type 'gpatch' > /dev/null 2>&1")) ? "gpatch" : "patch"

		module Installer
			Version = "3.4.6"
			Url = "http://www.gfd-dennou.org/arch/ruby/products/installer/"
			@@latest_ver = ""
			@@latest_fname = ""

			def print_title
				print "\n"
				print "+++", "-"*72, "+++\n"
				print "+++", " "*13, "The Dennou Ruby Products Installer  ver. ", Version, " "*13, "+++\n"
				print "+++", " "*72, "+++\n"
				print "+++", " "*26, "(C) GFD Dennou Club", " "*27, "+++\n"
				print "+++", "-"*72, "+++\n"
				print "\n"
			end
			module_function :print_title

			def latest_ver
				if @@latest_ver == ""
					print "    ", Url, " ...\n"
					list = Array.new
					NumRu::Fetch.get_list(URI.parse(Url), Header, Proxy, list)
					list.delete_if do |f|
						f !~ /^numru-install-([0-9p]+|\.)+\.rb$/
					end
					@@latest_fname = list.max
					@@latest_ver = @@latest_fname.scan(/^numru-install-(.*)\.rb$/).shift.shift
				end
				return @@latest_ver
			end
			module_function :latest_ver

			def download(dl_dir = DefaultDownloadDir)
				self.latest_ver if @@latest_fname == ""
				outfile = File.join(dl_dir, @@latest_fname)
				File.makedirs(dl_dir) unless File.directory?(dl_dir)
				f = open(outfile, "w")
				print "    ", Url, " ...\n"
				begin
					file_uri = URI.parse( File.join(Url, @@latest_fname) )
					NumRu::Fetch.fetch(file_uri, Header, Proxy, f)
					f.close
				rescue
					f.close if f && ! f.closed?
					File.delete(outfile)
					raise "cannot download \"#{@@latest_fname}\".\n"
				end
			end
			module_function :download
		end

		class Software
			NotInstalled = "  -  "
			VersionUnknown = "  ?  "
			@@dennou_ruby_plist = {}

			def check_latest_ver(url)
				if / \| / =~ url
					(check_url, download_url) = url.split(/ \| /)
				else
					check_url = url
					download_url = ""
				end
				if @@dennou_ruby_plist == {}
					print "    ", DennouRubyProductUrl, " ... \n"
					NumRu::Fetch.get_list(URI.parse(DennouRubyProductUrl), Header, Proxy, @@dennou_ruby_plist)
				end
				if @@dennou_ruby_plist.key?(@name)
					list_all = @@dennou_ruby_plist[@name]
				else
					list_all = Array.new
					print "    ", check_url, " ... \n"
					NumRu::Fetch.get_list(URI.parse(check_url), Header, Proxy, list_all)
				end
				if @name == "dcl-C"
					list = list_all.reject do |f|
						File.basename(f) !~ /^dcl-([0-9p]+|\.)+-C\.tar\.gz$/
					end
				else
					list = list_all.reject do |f|
						File.basename(f) !~ /^#{@name}-([0-9p\-]+|\.)+\.tar\.gz$/
					end
				end
				if @name == "ruby-gnome2-all"
					list.map! do |f|
						tmp = f.split(/\./)
						tmp[1] = "0" + tmp[1] if tmp[1].size == 1
                  f = tmp.join(".")
					end
					latest_fname = list.max
					tmp = latest_fname.split(/\./)
					tmp[1] = tmp[1].to_i.to_s
               latest_fname = tmp.join(".")
				else
					latest_fname = list.max
					if RUBY_PLATFORM =~ /cygwin/ && latest_fname =~ /netcdf-3\.6\.0.*\.tar\.gz$/
						list = list_all.reject do |f|
							File.basename(f) !~ /^#{@name}-([0-9]+|\.)+-beta[0-9]+\.tar\.gz$/
						end
						latest_fname = list.max
					end
				end
				if /\// =~ latest_fname
					(download_rdir, latest_fname) = File.split(latest_fname)
				else
					download_rdir = ""
				end
				if download_url == ""
					download_url = URI.join(check_url, download_rdir).to_s
				end
				if @name == "dcl-C"
					latest_ver = latest_fname.scan(/dcl-(.*)-C\.tar\..*/).shift.shift
					src_dir = "dcl-" + latest_ver + "-C"
				else
					latest_ver = latest_fname.scan(/#{@name}(-|\.)(.*)\.tar\..*/).shift[1]
					src_dir = name + "-" + latest_ver
				end
				if latest_ver =~ /-p/
					latest_ver.sub!(/-/, "")
				elsif latest_ver =~ /beta/
					latest_ver.sub!(/(-|\.)*beta/, "b")
				end
				return [ latest_fname, latest_ver, src_dir, check_url, download_url ]
			end
			private :check_latest_ver

			def initialize(name, realname, comm, group, check_cmd, current_ver_method, url, depend={}, patch=[])
				@name = name
				@realname = realname
				@comm = comm
				if group == ""
					@group = self.class.name.split(/::/)[-1]
				else
					@group = group
				end
				@check_cmd = check_cmd
				@current_ver_method = current_ver_method
				@depend = depend
				@patch = patch
				(@latest_fname, @latest_ver, @src_dir, @check_url, @download_url) = check_latest_ver(url)
			end

			attr_reader :name, :realname, :comm, :group, :check_url, :download_url, :depend, :patch, :latest_ver, :latest_fname, :src_dir

			def installed?
				if RUBY_PLATFORM =~ /cygwin/
					if `type '#{@check_cmd}' 2>&1` =~ /not found/
						return false
					else
						return true
					end
				else
					if system("type '#{@check_cmd}' > /dev/null 2>&1")
						return true
					else
						return false
					end
				end
			end

			def current_ver
				if self.installed?
					current_ver = `#{@check_cmd} #{@current_ver_method}`.chop
					current_ver = VersionUnknown if current_ver == ""
					if @name == "dcl-C"
						gtk_ver = `cdclconfig --gtkversion 2>&1`.chop
						if /^Usage/ =~ gtk_ver || gtk_ver == "no"
							gtk_ver = "_gtk0"
						else
							gtk_ver = "_gtk" + gtk_ver
						end
						current_ver = current_ver + gtk_ver
					end
					if current_ver =~ /-p/
						current_ver.sub!(/-/, "")
					elsif current_ver =~ /beta/
						current_ver.sub!(/(-|\.)*beta/, "b")
					end
				else
					current_ver = NotInstalled
				end
				return current_ver
			end

			def current_ver_disp
				current_ver_disp = self.current_ver
				case current_ver_disp
				when /gtk0$/
					current_ver_disp = current_ver_disp[0..-6]
				when /gtk1$/
					current_ver_disp = current_ver_disp[0..-6] + "+"
				when /gtk2$/
					current_ver_disp = current_ver_disp[0..-6] + "*"
				end
				return current_ver_disp
			end

			def get_dep_check_pkgs
				check_pkgs = Array.new
				check_pkg_deps = Hash.new
				@depend.each do |dep, req_ver|
 					(dep_check_pkgs, dep_check_pkg_deps) = dep.get_dep_check_pkgs
					dep_check_pkgs.each do |key|
						val = dep_check_pkg_deps[key]
						if not check_pkgs.include?(key)
							check_pkgs << key
							check_pkg_deps[key] = val
						elsif check_pkg_deps[key] < val
							check_pkg_deps.update({key => val})
						end
					end
					if not check_pkgs.include?(dep)
						check_pkgs << dep
						check_pkg_deps[dep] = req_ver
					elsif check_pkg_deps[dep] < req_ver
						check_pkg_deps.update({dep => req_ver})
					end
				end
				return [ check_pkgs, check_pkg_deps ]
			end

			def check_depend
				dep_cleared = Array.new
				dep_ver_low = Array.new
				dep_gtk_low = Array.new
				dep_failed  = Array.new
				dep_unknown = Array.new
				dep_not_cleared = Array.new
				( check_pkgs, check_pkg_deps ) = self.get_dep_check_pkgs
				check_pkgs.each do |dep|
					req_ver = check_pkg_deps[dep]
					current_ver = dep.current_ver
					if req_ver == ""
						if dep.installed?
							dep_cleared << dep
						else
							dep_ver_low << dep
							dep_failed  << dep
							dep_not_cleared << dep
						end
					else
						if Regexp.new(Regexp.quote(VersionUnknown)) =~ current_ver
							dep_unknown << dep
							dep_not_cleared << dep
							if /gtk[0-2]$/ =~ current_ver
								if GtkMajorVer > current_ver[-1].chr
									dep_gtk_low << dep
									dep_failed  << dep
								end
							end
						elsif current_ver == NotInstalled || current_ver < req_ver
							dep_ver_low << dep
							dep_failed  << dep
							dep_not_cleared << dep
						elsif /gtk[0-2]$/ =~ current_ver
							if GtkMajorVer > current_ver[-1].chr
								dep_gtk_low << dep
								dep_failed  << dep
								dep_not_cleared << dep
							end
						else
							dep_cleared << dep
						end
					end
				end
				dep_result = { "cleared" => dep_cleared,
				               "ver_low" => dep_ver_low,
				               "gtk_low" => dep_gtk_low,
				               "failed"  => dep_failed ,
									"unknown" => dep_unknown,
									"not_cleared" => dep_not_cleared }
				return dep_result
			end

			def download(dl_dir = DefaultDownloadDir)
				spcsize_dl_msg = 15
				outfile = File.join(dl_dir, @latest_fname)
				File.makedirs(dl_dir) unless File.directory?(dl_dir)
				f = open(outfile, "w")
				print @realname, " "*(spcsize_dl_msg - @realname.size), @download_url, " ...\n"
				begin
					file_uri = URI.parse( File.join(@download_url, @latest_fname) )
					NumRu::Fetch.fetch(file_uri, Header, Proxy, f)
					f.close
				rescue
					f.close if f && ! f.closed?
					File.delete(outfile)
					raise "cannot download \"#{@latest_fname}\".\n"
				end
				@patch.each do |p|
					next if p.sub(/.*\//,"") =~ /cygwin/ && RUBY_PLATFORM !~ /cygwin/
					outfile = File.join(dl_dir, p.sub(/.*\//,""))
					f = open(outfile, "w")
					print " "*spcsize_dl_msg, p, " ...\n"
					begin
						file_uri = URI.parse(p)
						NumRu::Fetch.fetch(file_uri, Header, Proxy, f)
						f.close
					rescue
						f.close if f && ! f.closed?
						File.delete(outfile)
						raise "cannot download \"#{p.sub(/.*\//,"")}\".\n"
					end
				end
			end

			def add_path(path)
				ENV["PATH"] = "#{path}:" + ENV["PATH"] unless ENV["PATH"].split(/:/).include?(path)
			end
			private :add_path

			def install(dl_dir = DefaultDownloadDir, prefix = DefaultPrefix)
				error_msg = "Installation failed: " + @realname
				plev = @name == "netcdf" ? "2" : "1"
				pdir = @name == "netcdf" ? "../.." : ".."
				conf_env = ""
				conf_opt = "--prefix=#{prefix}"

				cur_dir = Dir.pwd
				print "    Installing ", @realname, "...\n\n"
				Dir.chdir(dl_dir)
				system("/bin/rm -rf #{@src_dir}") if File.directory?(@src_dir)
				raise error_msg unless system("gzip -dc #{@latest_fname} | tar xvf -")
				Dir.chdir(@src_dir)
				Dir.chdir("src") if @name == "netcdf"
				@patch.each do |p|
					next if p.sub(/.*\//,"") =~ /cygwin/ && RUBY_PLATFORM !~ /cygwin/
					raise error_msg unless system("gzip -dc #{pdir}/#{p.split(/\//)[-1]} | #{Patch} -p#{plev}")
				end
				raise error_msg unless system("#{conf_env} ./configure #{conf_opt}")
				raise error_msg unless system("make")
				File.makedirs(prefix) unless File.directory?(prefix)
				raise error_msg unless system("make install")
				if @name == "fftw"
					raise error_msg unless system("make clean")
					conf_opt = "--enable-float --prefix=#{prefix}"
					raise error_msg unless system("#{conf_env} ./configure #{conf_opt}")
					raise error_msg unless system("make")
					raise error_msg unless system("make install")
				end
				Dir.chdir(cur_dir)
				add_path(prefix+"/bin")
				raise error_msg unless self.installed?
				print "\n    ----> succeeded: ", @realname, "\n\n"
			end
		end

		class RubySoft < Software
			def current_ver
				case @current_ver_method
				when "ModuleConst"
					if self.installed?
						case @name
						when "ruby-gnome-all"
							current_ver = `ruby -e "require 'gtk'; print Gtk::BINDING_VERSION[0]},'.',Gtk::BINDING_VERSION[1],Gtk::BINDING_VERSION[2]"`
						when "ruby-gnome2-all"
							current_ver = `ruby -e "require 'gtk2'; print Gtk::BINDING_VERSION.join('.')"`
						end
					else
						current_ver = NotInstalled
					end
					return current_ver
				else
					verlist = @current_ver_method
					if self.installed?
						current_ver = VersionUnknown
						line = Array.new
						File.open(verlist, "r"){ |f| line = f.readlines }
						line.each do |l|
							unless /^#/ =~ l
								(libname, libver) = l.chop.split
								if libname == @name
									current_ver = libver
									break
								end
							end
						end
						if self.name == "ruby-dcl"
							gtk_ver = `ruby -e "require 'numru/dcl'; print NumRu::DCL.gtk_version[0].to_s"`
							gtk_ver = "0" if gtk_ver == "no"
							current_ver = current_ver + "_gtk" + gtk_ver
						end
					else
						current_ver = NotInstalled
					end
					return current_ver
				end
			end

			def add_rb_load_path(rbsitelibdir)
				new_rbdir = [ "#{rbsitelibdir}/#{Config::CONFIG['arch']}", rbsitelibdir ]
				new_rbdir.each do |rbdir|
					unless $LOAD_PATH.include?(rbdir)
						$LOAD_PATH.unshift(rbdir)
					end
				end
			end
			private :add_rb_load_path

			def save_latest_ver
				verlist = @current_ver_method
				line = Array.new
				File.open(verlist, "r"){ |f| line = f.readlines }
				line.delete_if{ |l| /^#/ !~ l && l.chop.split[0] == @name }
				line << "#{@name}\t#{@latest_ver}\n"
				File.open(verlist, "w") do |f|
					line.each{ |l| f.print l }
				end
			end
			protected :save_latest_ver

			def install(dl_dir = DefaultDownloadDir, rbsitelibdir = DefaultRbSiteLibDir, prefix = DefaultPrefix)
				error_msg = "Installation failed: " + @realname
				cur_dir = Dir.pwd
				print "    Installing ", @realname, "...\n\n"
				Dir.chdir(dl_dir)
				system("/bin/rm -rf #{@src_dir}") if File.directory?(@src_dir)
				raise error_msg unless system("gzip -dc #{@latest_fname} | tar xvf -")
				Dir.chdir(@src_dir)
				@patch.each do |p|
					next if p.sub(/.*\//,"") =~ /cygwin/ && RUBY_PLATFORM !~ /cygwin/
					raise error_msg unless system("gzip -dc ../#{p.split(/\//)[-1]} | #{Patch} -p1")
				end
				case @name
				when "gpv"
					raise error_msg unless system("mv -f install.rb install.rb.orig")
					esc_rbsitelibdir = rbsitelibdir.gsub(/\//, '\/')
					raise error_msg unless system("sed -e 's/libdir = prefix.*$/libdir = \"#{esc_rbsitelibdir}\\/numru\"/' install.rb.orig > install.rb")
					raise error_msg unless system("ruby install.rb #{prefix}")
				when "gave"
					raise error_msg unless system("ruby setup.rb config --prefix=#{prefix} --siterubyver=#{rbsitelibdir}")
					raise error_msg unless system("ruby setup.rb install")
				end
				Dir.chdir(cur_dir)
				add_path(prefix+"/bin")
				add_rb_load_path(rbsitelibdir)
				raise error_msg unless self.installed?
				self.save_latest_ver unless @current_ver_method == "ModuleConst"
				print "\n    ----> succeeded: ", @realname, "\n\n"
			end
		end

		class RubyLib < RubySoft
			def installed?
				if /\&/ =~ @check_cmd
					req_name = @check_cmd.split(/ \& /)
					cmd = ""
					req_name.each do |req|
						cmd += "require '#{req}'; "
					end
					return false unless system("ruby -e \"#{cmd}\" > /dev/null 2>&1")
				else
					req_name = @check_cmd.split(/ \| /)
					req_name.each do |req|
						return false unless system("ruby -e \"require '#{req}'\" > /dev/null 2>&1")
					end
				end
				return true
			end
		end

		class RbExtLib < RubyLib
			def install(dl_dir = DefaultDownloadDir, rbsitelibdir = DefaultRbSiteLibDir, prefix = DefaultPrefix)
				error_msg = "Installation failed: " + @realname
				cur_dir = Dir.pwd
				print "    Installing ", @realname, "...\n\n"
				Dir.chdir(dl_dir)
				system("/bin/rm -rf #{@src_dir}") if File.directory?(@src_dir)
				raise error_msg unless system("gzip -dc #{@latest_fname} | tar xvf -")
				Dir.chdir(@src_dir)
				@patch.each do |p|
					next if p.sub(/.*\//,"") =~ /cygwin/ && RUBY_PLATFORM !~ /cygwin/
					raise error_msg unless system("gzip -dc ../#{p.split(/\//)[-1]} | #{Patch} -p1")
				end
				if @name == "ruby-dcl" || @name == "ruby-netcdf" || @name == "ruby-fftw3"
					conf_opt = Array.new
					conf_opt << "--with-narray-include=#{rbsitelibdir}/#{Config::CONFIG['arch']}"
					if RUBY_PLATFORM =~ /cygwin/
						conf_opt << "--with-narray-lib=#{rbsitelibdir}/#{Config::CONFIG['arch']}"
					end
					if @name == "ruby-netcdf"
						conf_opt << "--with-netcdf-dir=" + File.dirname(`type 'ncgen'`.chop.split[-1]).sub(/\/bin/, "")
					end
					if @name == "ruby-fftw3"
						conf_opt << "--with-fftw3-dir=" + File.dirname(`type 'fftw-wisdom'`.chop.split[-1]).sub(/\/bin/, "")
					end
					until system("ruby extconf.rb #{conf_opt.join(" ")}")
						print "Specify the directory that contains NArray header files ; "
						narray_inc = gets
						conf_opt.each do |cf|
							cf.sub!(/^(--with-narray-(include|lib)=).*$/, $1 + narray_inc.chop)
						end
					end
				else
					raise error_msg unless system("ruby extconf.rb")
				end
				Dir.glob("./**/Makefile").each do |mkfile|
					raise error_msg unless system("mv -f #{mkfile} #{mkfile}.orig")
					esc_rbsitelibdir = rbsitelibdir.gsub(/\//, '\/')
					raise error_msg unless system("sed -e 's/\\$(sitearchdir)\\(\\$(target_prefix)\\)/#{esc_rbsitelibdir}\\/\\$(arch)\\1/' -e 's/\\$(sitelibdir)\\(\\$(target_prefix)\\)/#{esc_rbsitelibdir}\\1/' #{mkfile}.orig > #{mkfile}")
				end
				unless system("make")
					raise error_msg unless system("make")
				end
				raise error_msg unless system("make site-install")
				if @name == "narray" && /cygwin/ =~ RUBY_PLATFORM
					raise error_msg unless File.install("libnarray.a", "#{rbsitelibdir}/#{Config::CONFIG['arch']}", 0644, true)
				elsif @name == "ruby-gnome2-all" && /cygwin/ =~ RUBY_PLATFORM
					Dir.glob("./**/lib*.a").delete_if{|implib| /src2./ =~ implib}.each do |implib|
						raise error_msg unless File.install(implib, "#{rbsitelibdir}/#{Config::CONFIG['arch']}", 0644, true)
					end
				end
				Dir.chdir(cur_dir)
				add_rb_load_path(rbsitelibdir)
				raise error_msg unless self.installed?
				self.save_latest_ver if @current_ver_method != "ModuleConst"
				print "\n    ----> succeeded: ", @realname, "\n\n"
			end
		end

		class RbLib < RubyLib
			def install(dl_dir = DefaultDownloadDir, rbsitelibdir = DefaultRbSiteLibDir, prefix = DefaultPrefix)
				error_msg = "Installation failed: " + @realname
				cur_dir = Dir.pwd
				print "    Installing ", @realname, "...\n\n"
				Dir.chdir(dl_dir)
				system("/bin/rm -rf #{@src_dir}") if File.directory?(@src_dir)
				raise error_msg unless system("gzip -dc #{@latest_fname} | tar xvf -")
				Dir.chdir(@src_dir)
				@patch.each do |p|
					next if p.sub(/.*\//,"") =~ /cygwin/ && RUBY_PLATFORM !~ /cygwin/
					raise error_msg unless system("gzip -dc ../#{p.split(/\//)[-1]} | #{Patch} -p1")
				end
				if @name == "narray_miss"
					raise error_msg unless system("ruby setup.rb config --siterubyver=#{rbsitelibdir}")
					raise error_msg unless system("ruby setup.rb install")
				else
					if @name == "gphys"
						system("mkdir -p #{prefix}/bin") unless File.directory?(File.join(prefix, "bin"))
						raise error_msg unless system("ruby install.rb -d #{rbsitelibdir} -b #{prefix}/bin")
					else
						raise error_msg unless system("ruby install.rb -d #{rbsitelibdir}")
					end
				end
				Dir.chdir(cur_dir)
				add_rb_load_path(rbsitelibdir)
				raise error_msg unless self.installed?
				self.save_latest_ver if @current_ver_method != "ModuleConst"
				print "\n    ----> succeeded: ", @realname, "\n\n"
			end
		end

		def upgrade( software )
			cleared = Array.new
			ver_low = Array.new
			gtk_low = Array.new
			failed  = Array.new
			unknown = Array.new
			not_cleared = Array.new

			software.each do |soft|
				if soft.installed?
					current_ver = soft.current_ver
					if Regexp.new(Regexp.quote(Software::VersionUnknown)) =~ current_ver
						unknown << soft
						not_cleared << soft
						if /gtk[0-2]$/ =~ current_ver && GtkMajorVer > current_ver[-1].chr
							gtk_low << soft
							failed << soft
						end
					elsif soft.current_ver < soft.latest_ver
						ver_low << soft
						failed << soft
						not_cleared << soft
					elsif /gtk[0-2]$/ =~ current_ver && GtkMajorVer > current_ver[-1].chr
						gtk_low << soft
						failed << soft
						not_cleared << soft
					else
						cleared << soft
					end
				else
					cleared << soft
				end
			end
			result = { "cleared" => cleared,
			           "ver_low"  => ver_low,
			           "gtk_low"  => gtk_low,
			           "failed"  => failed,
			           "unknown" => unknown,
			           "not_cleared" => not_cleared }
			return result
		end
		module_function :upgrade
	end
end


if __FILE__ == $0
	include NumRu::Install

	Installer.print_title

	print "Checking the version of this installer...\n\n"
	if Installer.latest_ver > Installer::Version
		print "\n    ----> This installer is version #{Installer::Version}, but version #{Installer.latest_ver} is available.\n"
		print "          You must upgrade to get the latest features and bug fixes.\n\n"
		print "Do you want to download now? [Y/n] ; "
		y_or_n = gets
		if y_or_n =="\n" || y_or_n =~ /^y/i
			print "\n    ----> Yes.\n\n"
			print "Specify a directory to store downloaded installer [", DefaultDownloadDir, "] ; "
			dl_dir = gets
			dl_dir.chop!
			dl_dir = DefaultDownloadDir if dl_dir == ""
			print "\n    Download directory:     #{dl_dir}\n"
			print "\nDo you want to continue? [Y/n] ; "
			y_or_n = gets
			if y_or_n =="\n" || y_or_n =~ /^y/i
				print "\n    ----> Yes.\n\n"
				print "Now downloading...\n\n"
				Installer.download(dl_dir)
				print "\n    ----> succeeded.\n\n"
				print "Please execute new installer.\n\n"
				exit
			end
		end
		print "\n    ----> No.\n\n"
		print "You can get the latest installer from the following URL:\n\n"
		print "    ", Installer::Url, "\n\n"
		exit
	else
		print "\n    ----> succeeded.\n\n"
	end

	verlist = DefaultVerListFile
	print "Checking the version list of installed NumRu libraries...\n"
	unless File.exist?(verlist)
		print "\n"
		print "    ----> The version list is not found in the default directory\n"
		print "          \"", File.dirname(verlist), "\". Do you want to create it?\n\n"
		print "          If this is the first time to use this installer, please answer yes.\n"
		print "          If you want to check another directory than the default one, please\n"
		print "          answer no.\n\n"
		print "Do you want to create the version list? [y/N] ; "
		y_or_n = gets
		if y_or_n =~ /^y/i
			create_verlist = "n"
			until create_verlist == "\n" || create_verlist =~ /^y/i
				print "\n"
				print "    ----> Specify the directory [#{File.dirname(DefaultVerListFile)}] ; "
				verlist_dir = gets
				if verlist_dir == "\n"
					verlist = DefaultVerListFile
				else
					verlist = File.join(verlist_dir.chop, File.basename(DefaultVerListFile))
				end
				print "\n"
				print "    ----> Create a version list \"#{verlist}\" ?\n"
				print "          [Y/n] ; "
				create_verlist = gets
			end
			File.makedirs(File.dirname(verlist)) unless File.directory?(File.dirname(verlist))
			File.open(verlist, "w") do |f|
				f.print "### Version list of NumRu libraries for Dennou Ruby Products Installer ###\n"
				f.print "### Created by Dennou Ruby Products Installer    (C) GFD Dennou Club   ###\n"
			end
		else
			verfile_specify_first = true
			until File.exist?(verlist)
				print "\n"
				unless verfile_specify_first
					print "    ----> The version list is not found.\n          "
				else
					print "    ----> "
				end
				verfile_specify_first = false
				print "Specify the directory for checking [#{File.dirname(DefaultVerListFile)}] ; "
				verlist_dir = gets
				if verlist_dir == "\n"
					verlist = DefaultVerListFile
				else
					verlist = File.join(verlist_dir.chop, File.basename(DefaultVerListFile))
				end
			end
		end
	end
	print "\n"
	print "    ----> succeeded.\n\n"

	print "Connecting remote servers and getting the list of packages...\n\n"

	dcl_c = Software.new(
		"dcl-C", "DCL(C)",
		"DCL graphic library (C version)", "Library",
		"cdclconfig", " --dclversion | sed 's/-C//'",
		"ftp://www.gfd-dennou.org/arch/dcl/",
		{},
		["http://www.gfd-dennou.org/arch/ruby/products/installer/patches/dcl53c_cygwin.patch.gz"]
	)
	netcdf = Software.new(
		"netcdf", "NetCDF",
		"NetCDF file handling libs & commands", "Library",
		"ncgen",
		"-h 2>&1 | sed -n 's/\\(.*version \\)\\(.*\\)\\( of.*\\)/\\2/p'",
		"http://www.gfd-dennou.org/arch/netcdf/unidata-mirror/",
		{},
		["http://www.gfd-dennou.org/arch/ruby/products/installer/patches/netcdf360_cygwin.patch.gz"]
	)
	fftw = Software.new(
		"fftw", "FFTW",
		"C library for FFT", "Library",
		"fftw-wisdom", "-V | sed -n 's/.* version \\(.*\\)\\./\\1/p'",
		"ftp://ftp.fftw.org/pub/fftw/",
		{}
	)
	narray = RbExtLib.new(
		"narray", "NArray",
		"Multi-dimensional numeric array", "",
		"narray", verlist,
		"http://narray.rubyforge.org/ | http://rubyforge.org/frs/download.php/7658/",
		{}
	)
	narray_miss = RbLib.new(
		"narray_miss", "NArrayMiss",
		"Data missing handling for NArray", "",
		"narray_miss", verlist,
		"ftp://www.gfd-dennou.org/arch/ruby/products/narray_miss/",
		{narray => ""}
	)
	if GtkMajorVer == "1"

		rubygnome = RbExtLib.new(
			"ruby-gnome-all", "Ruby-GNOME",
			"Ruby bindings for GNOME", "",
			"gtk | gdk_pixbuf | gdk_imlib", "ModuleConst",
			"http://prdownloads.sourceforge.net/ruby-gnome/ | http://jaist.dl.sourceforge.net/sourceforge/ruby-gnome/",
			{}
		)
	else
		rubygnome2 = RbExtLib.new(
			"ruby-gnome2-all", "Ruby-GNOME2",
			"Ruby bindings for GNOME2", "",
			"gtk2", "ModuleConst",
			"http://prdownloads.sourceforge.net/ruby-gnome2/ | http://jaist.dl.sourceforge.net/sourceforge/ruby-gnome2/",
			{}
		)
	end
	rubydcl = RbExtLib.new(
		"ruby-dcl", "RubyDCL",
		"Ruby wrapper of DCL", "",
		"numru/dcl", verlist,
		"ftp://www.gfd-dennou.org/arch/ruby/products/ruby-dcl/",
		{dcl_c => "5.2.3", narray => "0.5.5", narray_miss => ""}
	)
	if GtkMajorVer == "1"
		rubydcl.depend[rubygnome] = ""
	elsif GtkMajorVer == "2"
		rubydcl.depend[rubygnome2] = ""
	end
	rubynetcdf = RbExtLib.new(
		"ruby-netcdf", "RubyNetCDF",
		"Ruby wrapper of NetCDF", "",
		"numru/netcdf", verlist,
		"ftp://www.gfd-dennou.org/arch/ruby/products/ruby-netcdf/",
		{netcdf => "3.4", narray => "0.5.5", narray_miss => ""}
	)
	rubyfftw3 = RbExtLib.new(
		"ruby-fftw3", "Ruby-FFTW3",
		"Ruby wrapper of FFTW ver.3", "",
		"narray & numru/fftw3", verlist,
		"ftp://www.gfd-dennou.org/arch/ruby/products/ruby-fftw3/",
		{fftw => "3.0", narray => "0.5.5"}
	)
	multibitnums = RbExtLib.new(
		"multibitnums", "Multibitnums",
		"Class of multiple multi-bit data", "",
		"numru/multibitnums", verlist,
		"ftp://www.gfd-dennou.org/arch/ruby/products/multibitnums/",
		{}
	)
	units = RbLib.new(
		"numru-units", "NumRu/Units",
		"Class of units of physical quantities", "",
		"numru/units", verlist,
		"ftp://www.gfd-dennou.org/arch/ruby/products/numru-units/",
		{}
	)
	misc = RbLib.new(
		"numru-misc", "NumRu/Misc",
		"Miscellaneous functions and classes", "",
		"numru/misc", verlist,
		"ftp://www.gfd-dennou.org/arch/ruby/products/numru-misc/",
		{narray => ""}
	)
	met = RbLib.new(
		"met", "NumRu/Met",
		"Meteorological functions and classes", "",
		"numru/met", verlist,
		"ftp://www.gfd-dennou.org/arch/ruby/products/met/",
		{misc => "0.0.4", rubydcl => "1.3.0"}
	)
	gphys = RbLib.new(
		"gphys", "GPhys",
		"Class for Gridded Physical quantities", "",
		"numru/gphys", verlist,
		"ftp://www.gfd-dennou.org/arch/ruby/products/gphys/",
		{narray => "0.5.7", narray_miss => "1.0.2", rubydcl => "1.2.1", rubynetcdf => "0.5.2", rubyfftw3 => "0.2", units => "1.1", misc => "0.0.4"}
	)
	gpv = RubySoft.new(
		"gpv", "GPV",
		"handling tool of GPV data made by JMA", "",
		"gpv2nc", verlist,
		"ftp://www.gfd-dennou.org/arch/ruby/products/gpv/",
		{narray => "0.5.6", rubynetcdf => "0.3.5", multibitnums => ""}
	)
	gave = RubySoft.new(
		"gave", "GAVE",
		"Ruby/GTK+ grid data analyser/viewer", "",
		"gave", verlist,
		"ftp://www.gfd-dennou.org/arch/ruby/products/gave/",
		{rubydcl => "", rubynetcdf => "0.5.5", gphys => "0.2.2"}
	)
	if GtkMajorVer == "1"
		gave.depend[rubygnome] = ""
		software = [dcl_c, netcdf, fftw, narray, narray_miss, rubygnome, rubydcl,
		            rubynetcdf,rubyfftw3, multibitnums, units, misc, met, gphys,
		            gpv, gave]
	else
		gave.depend[rubygnome2] = ""
		software = [dcl_c, netcdf, fftw, narray, narray_miss, rubygnome2,
		            rubydcl, rubynetcdf,rubyfftw3, multibitnums, units, misc,
		            met, gphys, gpv, gave]
	end

	print "\n"
	print "    ----> succeeded.\n\n"

	realname0 = "name"
	charsize = Array.new
	software.each{ |soft| charsize << soft.realname.size }
	spcsize1 = charsize.max + 1
	spcsize1 = realname0.size + 1 if spcsize1 <= realname0.size
	current0  = "current"
	charsize = Array.new
	software.each{ |soft| charsize << soft.current_ver_disp.size }
	spcsize2 = charsize.max + 1
	spcsize2 = current0.size + 1 if spcsize2 <= current0.size
	latest0   = "latest"
	charsize = Array.new
	software.each{ |soft| charsize << soft.latest_ver.size }
	spcsize3 = charsize.max + 1
	spcsize3 = latest0.size + 1 if spcsize3 <= latest0.size
	group0    = "group"
	charsize = Array.new
	software.each{ |soft| charsize << soft.group.size }
	spcsize4 = charsize.max + 2
	spcsize4 = group0.size + 1 if spcsize4 <= group0.size
	comm0     = "summary"

	stop_loop = nil

	while not stop_loop

		if stop_loop == false
			print "Do you want to install another software? [Y/n] ; "
			y_or_n = gets
			if y_or_n == "\n" || y_or_n =~ /^y/i
			print "\n    ----> Yes.\n\n"
			else
				stop_loop == true
				print "\n    ----> No.\n\n"
				break
			end
		end

		print "You can select the following packages:\n\n"
		print "    ",
		      realname0, " "*(spcsize1 - realname0.size),
				current0,  " "*(spcsize2 - current0.size),
				latest0,   " "*(spcsize3 - latest0.size),
				group0,    " "*(spcsize4 - group0.size),
				comm0,     "\n"
		print "    ", "-"*74, "\n"

		disp_legend_gtk1 = false
		disp_legend_gtk2 = false
		software.each_with_index do |soft, j|
			num = (j+1).to_s.strip
			num = " " + num  while num.size < 2
			print num, ") ",
			      soft.realname,    " "*(spcsize1 - soft.realname.size)

			case soft.current_ver_disp[-1].chr
			when "+"
				disp_legend_gtk1 = true
			when "*"
				disp_legend_gtk2 = true
			end
			print soft.current_ver_disp, " "*(spcsize2 - soft.current_ver_disp.size),
			      soft.latest_ver,  " "*(spcsize3 - soft.latest_ver.size),
			      soft.group,       " "*(spcsize4 - soft.group.size),
			      soft.comm,        "\n"
		end
		print "\n    + : compiled with GTK+1\n" if disp_legend_gtk1
		print "\n    * : compiled with GTK+2\n" if disp_legend_gtk2

		num_soft = -1
		first = true
		until num_soft >= 0 && num_soft <= software.size
			print "\n    ----> Invalid number.\n" unless first
			print "\nIf you have already installed these packages in the same prefix and the same"
			print "\nruby site library directory, you can upgrade at once to select \"0\"."
			print "\nOtherwise, select the number of the package you want to install or upgrade.\n"
			print "\nWhich software do you want to install? (Ctrl-d to quit) [0-", software.size.to_s.strip, "] ; "
			num_soft = gets.chop
			if num_soft == ""
				num_soft = -1
			else
				num_soft = num_soft.to_i
			end
			first = false
		end

		if num_soft == 0

			print "\n    ----> Select upgrade.\n\nChecking depedencies...\n\n"
			select_soft_depend = upgrade(software)

			if select_soft_depend["not_cleared"].size == 0
				print "all of the installed packages are already the newest version.\n\n"
				stop_loop = false
				next
			end

		else

			select_soft = software[num_soft-1]
			print "\n    ----> Select ", select_soft.realname, ".\n\n"

			case select_soft.current_ver
			when select_soft.latest_ver, select_soft.latest_ver + "_gtk" + GtkMajorVer
				print "#{select_soft.realname} is already the newest version.\n\n"
				stop_loop = false
				next
			when Regexp.new(Regexp.quote(Software::VersionUnknown))
				print "Version of the selected package is unknown.\n"
				print "Do you want to download/install them? [y/N] ; "
				y_or_n = gets
				if y_or_n =~ /^y/i
					print "\n    ----> Yes.\n\n"
				else
					print "\n    ----> No.\n\n"
					stop_loop = false
					next
				end
			end

			print "Checking depedencies...\n\n"

			if select_soft.name == "dcl-C" && RUBY_PLATFORM =~ /cygwin/
				if `type 'tcsh' 2>&1` =~ /not found/
					print "    This package requires tcsh, but that seems not to be installed in your\n"
					print "    system. First, you have to install tcsh using Cygwin Installer (setup.exe).\n"
					print "    The category of tcsh in the package list is \"Shells\".\n\n"
					stop_loop = false
					next
				end
			end
			if GtkMajorVer == "0"
				case select_soft.name
				when "ruby-gnome2-all", "gave"
					print "    This package requires GTK+ 2.x library, but that seems not to be\n"
					print "    installed in your system. First, you have to install GTK+ 2.x.\n\n"
					print "    GTK+ website is located at:  http://www.gtk.org/\n\n"
					stop_loop = false
					next
				end
			end
			if /gtk[0-2]$/ =~ rubydcl.current_ver
				if select_soft.name == "dcl-C" && GtkMajorVer > rubydcl.current_ver[-1].chr
					print "    This package will be replaced with the GTK+"
					if GtkMajorVer == "1"
						print "1"
					elsif GtkMajorVer == "2"
						print "2"
					end
					print "-powered version,\n"
					print "    but current version is needed by RubyDCL.\n"
					print "    You have also to reinstall RubyDCL.\n\n"
					select_soft = rubydcl
				end
			end

			select_soft_depend = select_soft.check_depend

		end

		unless select_soft_depend["unknown"].size == 0
			print "    Versions of the following packages are unknown."
			print "\n    ", select_soft.realname, " may not work correctly unless these packages are installed." unless num_soft == 0
			print "\n\n        "
			select_soft_depend["unknown"].each do |unknown|
				print unknown.realname, "  "
			end
			print "\n\n    Do you want to download/install them? [y/N] ; "
			y_or_n = gets
			if y_or_n =~ /^y/i
				print "\n        ----> Yes.\n\n"
				inst_soft = select_soft_depend["not_cleared"]
			else
				print "\n        ----> No.\n\n"
				inst_soft = select_soft_depend["failed"]
			end
		else
			inst_soft = select_soft_depend["failed"]
		end
		inst_soft << select_soft unless num_soft == 0
		print "    ----> succeeded.\n\n"

		if select_soft_depend["gtk_low"].size == 0
			print "The following packages will be installed:\n\n    "
			inst_soft.each do |soft|
				print soft.realname, "  "
			end
		else
			unless select_soft_depend["gtk_low"].size == select_soft_depend["not_cleared"].size
				inst_soft_name = inst_soft.map{ |soft| soft.realname }
				gtk_low_soft_name = select_soft_depend["gtk_low"].map{ |soft| soft.realname }
				print_inst_soft_name = inst_soft_name - gtk_low_soft_name
				print "The following packages will be installed:\n\n    "
				print_inst_soft_name.each do |softname|
					print softname, "  "
				end
				print "\n\n"
			end
			print "The following packages will be replaced with the GTK+"
			if GtkMajorVer == "1"
				print "1"
			elsif GtkMajorVer == "2"
				print "2"
			end
			print "-powered version:\n\n    "
			select_soft_depend["gtk_low"].each do |soft|
				print soft.realname, "  "
			end
		end

		print "\n\nSpecify a directory to store downloaded packages [", DefaultDownloadDir, "] ; "
		dl_dir = gets
		dl_dir.chop!
		dl_dir = DefaultDownloadDir if dl_dir == ""

		print "\nDownload directory:     #{dl_dir}\n"

		print "\nDo you want to download them? [Y/n] ; "
		y_or_n = gets
		if y_or_n =="\n" || y_or_n =~ /^y/i
			print "\n    ----> Yes.\n\n"
		else
			print "\n    ----> No.\n\n"
			stop_loop = false
			next
		end

		print "Now downloading...\n\n"
		inst_soft.each do |soft|
			soft.download(dl_dir)
		end
		print "\n    ----> succeeded.\n\n"

		print "Do you want to install them? [Y/n] ; "
		y_or_n = gets
		if y_or_n =="\n" || y_or_n =~ /^y/i
			print "\n    ----> Yes.\n"
		else
			print "\n    ----> No.\n\n"
			stop_loop = false
			next
		end

      ask_prefix = false
		inst_soft.each do |soft|
			if soft.group == "Library" || soft.group == "RubySoft" || soft.name == "gphys"
				ask_prefix = true
				break
			end
		end

      ask_rbsitelibdir = false
		inst_soft.each do |soft|
			if ( soft.group == "RubySoft" || /^Rb/ =~ soft.group )
				ask_rbsitelibdir = true
				break
			end
		end

		if ask_prefix
			print "\nSpecify an installation prefix [", DefaultPrefix, "] ; "
			prefix = gets
			prefix.chop!
			prefix = DefaultPrefix if prefix == ""
		end

		if ask_rbsitelibdir
			print "\nSpecify an installation directory for Ruby libraries [", DefaultRbSiteLibDir, "] ; "
			rbsitelibdir = gets
			rbsitelibdir.chop!
			rbsitelibdir = DefaultRbSiteLibDir if rbsitelibdir == ""
		end

		if ask_prefix
			print "\nInstallation prefix:    #{prefix}\n"
		else
			print "\n"
		end
		if ask_rbsitelibdir
			print "Ruby library directory: #{rbsitelibdir}\n"
		end

		print "\nDo you want to continue? [Y/n] ; "
		y_or_n = gets
		if y_or_n =="\n" || y_or_n =~ /^y/i
			print "\n    ----> Yes.\n\n"
		else
			print "\n    ----> No.\n\n"
			stop_loop = false
			next
		end

		print "Start installation...\n\n"
		inst_soft.each do |soft|
			case soft.group
			when "Library"
				soft.install(dl_dir, prefix)
			when "RubySoft"
				soft.install(dl_dir, rbsitelibdir, prefix)
			when "RbExtLib", "RbLib"
				if soft.name == "gphys"
					soft.install(dl_dir, rbsitelibdir, prefix)
				else
					soft.install(dl_dir, rbsitelibdir)
				end
			end
		end
		print "Installation is completed.\n\n"
		stop_loop = false

	end
	print "Thank you. Enjoy the Dennou Ruby suite! Have a good day!\n"
	print "Make a point of checking your environment variable \"PATH\" and \"RUBYLIB\"!\n\n"
end
