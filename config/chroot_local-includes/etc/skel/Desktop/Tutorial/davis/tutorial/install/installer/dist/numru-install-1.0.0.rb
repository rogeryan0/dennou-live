#! /usr/bin/env ruby

require 'uri'
require 'net/http'
require 'net/ftp'
Net::HTTP.version_1_2

module NumRu
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
				res = http.get(request_uri, header)
				break if /3../ !~ res.code  # /3../ for redirection
				uri = uri.merge(URI::parse(res['location']))
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
			passive = header[:passive]
			ftp = Net::FTP.new(host)
			ftp.passive = passive
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
			passive = header[:passive]
			ftp = Net::FTP.new(host)
			ftp.passive = passive
			ftp.login(user, pass)
			ftp.chdir(remote_dir)
			ftp.list.each{ |f| list << f.split[-1] }
			ftp.close
		end
		module_function :ftp_get_list

		def http_get_list(uri, header, proxy, list)
			dir_html = ""
			http_fetch(uri, header, proxy, dir_html)
			dir_html.split(/\n/).each do |line|
				line.scan(/<A HREF="(.*)">(.*)<\/A>/i) do |href, fname|
					list << fname if href == fname
				end
			end
		end
		module_function :http_get_list

		def get_list(uri, header, proxy, list)
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
		module_function :get_list
	end

	module Install
		require 'getopts'
		getopts(nil, "lang:en")
		h = ENV['http_proxy']

		Proxy = h ? URI::parse(h) : nil
		Header = {"accept-language" => $OPT_lang}

		class Software
			def initialize(name, realname, check, url, depend = [])
				@name = name
				@realname = realname
				@check = check
				@url = url
				@depend = depend
				@uri = URI::parse(@url)
				@latest_ver = ""
				@latest_fname = ""
				@conf_env = ""
				@conf_opt = ""
				@src_dir = ""
			end

			attr_reader :name
			attr_reader :realname
			attr_reader :depend
			attr_accessor :conf_env
			attr_accessor :conf_opt
			attr_accessor :src_dir

			def latest
				if @latest_ver == ""
					list = Array.new
					NumRu::Fetch::get_list(@uri, Header, Proxy, list)
					list.delete_if do |f|
						!( f =~ /^#{@name}(-|\.)([0-9p]+\.)+tar\.(gz|bz2|Z)$/ )
					end
					@latest_fname = list.max
					@latest_ver = @latest_fname.scan(/#{@name}(-|\.)(.*)\.tar\..*/).shift[1]
					@src_dir = @name + "-" + @latest_ver
				end
				return @latest_ver
			end

			def installed?
				system("which #{@check} > /dev/null 2>&1")
			end

			def download(dl_dir = ".")
				outfile = File::join(dl_dir, @latest_fname)
				f = open(outfile, "w")
				begin
					file_uri = URI::parse( File::join(@url, @latest_fname) )
					NumRu::Fetch::fetch(file_uri, Header, Proxy, f)
					f.close
				rescue
					f.close if f && !f.closed?
					File::delete(outfile)
					raise "cannot download \"#{@latest_fname}\".\n"
				end
			end

			def install
				@depend.each do |dep|
					raise "You have to install #{dep.realname} first.\n" if !dep.installed?
				end
				conf_cmd = @conf_env + " ./configure " + @conf_opt
				stat = system("tar zxvf #{@latest_fname} && cd #{@src_dir} && #{conf_cmd} && make && make install")
				raise 'Installation failed: ' + @realname if (!stat)
			end
		end

		class RbExtLib < Software
			def installed?
				begin
					require @check
					return true
				rescue LoadError
					return false
				end
			end

			def install
				@depend.each do |dep|
					raise "You have to install #{dep.realname} first.\n" if !dep.installed?
				end
				stat = system("tar zxvf #{@latest_fname} && cd #{@src_dir} && ruby extconf.rb && make && make site-install")
				raise 'Installation failed: ' + @realname if (!stat)
			end
		end

		class RbExtLibFile < RbExtLib
			def install
				@depend.each do |dep|
					raise "You have to install #{dep.realname} first.\n" if !dep.installed?
				end
				require "rbconfig"
				sitelibdir = Config::CONFIG["sitelibdir"]
				stat = system("tar zxvf #{@latest_fname} && cd #{@src_dir} && install narray_miss.rb #{sitelibdir}")
				raise 'Installation failed: ' + @realname if (!stat)
			end	
		end	
	end
end


if __FILE__ == $0

	narray = NumRu::Install::RbExtLib.new(
		"narray", "NArray", "narray",
		"http://www.ir.isas.ac.jp/~masa/ruby/dist"
	)
	narray_miss = NumRu::Install::RbExtLibFile.new(
		"narray_miss", "NArrayMiss", "narray_miss",
		"ftp://www.gfd-dennou.org/arch/ruby/products/narray_miss",
		[narray]
	)
	netcdf = NumRu::Install::Software.new(
		"netcdf", "NetCDF", "ncdump",
		"http://www.gfd-dennou.org/arch/netcdf/unidata-mirror"
	)
	rubynetcdf = NumRu::Install::RbExtLib.new(
		"ruby-netcdf", "RubyNetCDF", "numru/netcdf",
		"ftp://www.gfd-dennou.org/arch/ruby/products/ruby-netcdf",
		[narray, netcdf]
	)
	dcl_c = NumRu::Install::Software.new(
		"dcl-5\.2-C", "DCL 5.2 C version", "cdclconfig",
		"ftp://www.gfd-dennou.org/arch/dcl"
	)
	rubydcl = NumRu::Install::RbExtLib.new(
		"ruby-dcl", "RubyDCL", "numru/dcl",
		"ftp://www.gfd-dennou.org/arch/ruby/products/ruby-dcl",
		[narray, dcl_c]
	)

	software = [narray, narray_miss, netcdf, rubynetcdf, dcl_c, rubydcl]

	print "\n"
	print "+++------------------------------------------------------+++\n"
	print "+++          the Dennou Ruby Products installer          +++\n"
	print "+++                                                      +++\n"
	print "+++               (C) 2003 GFD-Dennou Club               +++\n"
	print "+++------------------------------------------------------+++\n"
	print "\n"

	print "Connecting the Dennou Server and getting the list of packages...\n"
	software.each{ |soft| soft.latest }

	print "    ----> succeeded.\n\n"

	charsize = Array.new
	software.each{ |soft| charsize << soft.realname.size }
	spcsize = charsize.max + 4

	print "I can install the following:\n\n"
	print " "*spcsize, "latest version    required:\n"
	software.each do |soft|
		print soft.realname, " "*(spcsize - soft.realname.size), soft.latest
		soft.depend.each_with_index do |dep, i|
			if i == 0
				print " "*(18 - soft.latest.size)
			else
				print ", " if i != 0
			end
			print dep.realname
		end
		print "\n"
	end
	print "\n"

	print "Would you like to download them all? ([y]/n) ;"
	dl_all = gets()
	if dl_all == "\n" || dl_all =~ /^y/i
		print "    ----> yes. download all.\n\n"
		print "Now downloading...\n"
		software.each do |soft|
			print "    ", soft.realname, "...\n"
			soft.download
		end
		print "    ----> succeeded.\n\n"
	else
		print "    ----> no.\n\n"
		raise "Abort installation.\n"
	end

	print "Would you like to install them all? ([y]/n) ;"
	inst_all = gets()
	if inst_all == "\n" || inst_all =~ /^y/i
		print "    ----> yes. install all.\n\n"
		inst_all = true
	else
		print "    ----> no. \n\n"
		print "Would you like to select package one by one? ([y]/n) ;"
		inst_1by1 = gets()
		if inst_1by1 == "\n" || inst_1by1 =~ /^y/i
			print "    ----> yes. install interactively.\n\n"
			inst_all = false
		else
			print "    ----> no.\n\n"
			raise "Abort installation.\n"
		end
	end
	print "Start installation...\n\n"
	software.each do |soft|
		if !inst_all
			print " "*spcsize, "latest version    required:\n"
			print soft.realname, " "*(spcsize - soft.realname.size), soft.latest
			soft.depend.each_with_index do |dep, i|
				if i == 0
					print " "*(18 - soft.latest.size)
				else
					print ", " if i != 0
				end
				print dep.realname
			end
			print "\n\n"
			print "Would you like to install ", soft.realname, " ? ([y]/n) ;"
			ans = gets()
			if ans == "\n" || ans =~ /^y/
				print "    ----> yes. install.\n\n"
			else
				print "    ----> no. skip.\n\n"
				next
			end
		end

		print "Installing ", soft.realname, "...\n\n"
		if soft.name == "netcdf"
			soft.conf_env = "CPPFLAGS=\"-DNDEBUG -Df2cFortran\""
			soft.conf_opt = "--prefix=/usr/local"
			soft.src_dir = File::join(soft.src_dir, "src")
		end
		if soft.name == "dcl-5.2-C"
			soft.src_dir = soft.name
		end
		soft.install

		print "\nInstallation completed: ", soft.realname, "\n\n"
	end
	print "    ----> All installation completed.\n\n"

	print "Thank you. Enjoy the Dennou Ruby suite! Have a good day!\n\n"
end
