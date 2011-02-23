=begin
numru-install.rb : The Dennou Ruby Products Installer

Change Log:
* 2003/06/27 KOSHIRO Tsuyoshi <koshiro@gfd-dennou.org>
  - (1.0.0)
  - first release.
* 2003/12/09 KOSHIRO Tsuyoshi <koshiro@gfd-dennou.org>
  - (2.0.0)
  - add packages: misc, met, gphys
  - revise module NumRu::Install
    + select download directory
    + select installation prefix and Ruby libraries directory
    + check file and version dependencies
    + apply patches
    + extra processing for Cygwin

(C) 2003 GFD Dennou Club
=end


require 'ftools'
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
				begin
					res = http.get(request_uri, header)
				rescue TimeoutError
					print "\n*Error*  HTTP connection timeout.\n"
					print "         If your Internet connection is via a firewall or proxy, you must set\n"
					print "         the environment variable \"http_proxy\" with the following syntax:\n"
					print "         http_proxy=http://your.proxy.server:8080/\n\n"
					raise
				end
				break if /3../ !~ res.code  # /3../ for redirection
				uri = uri.merge(URI::parse(res['location']))
			end

			out << res.body
		end
		module_function :http_fetch

		def ftp_fetch(uri, header, proxy, out = STDOUT)
			host = uri.host
			port = uri.port
			user = uri.user || 'anonymous'
			pass = uri.password 
			remote_file = uri.path
			passive = header[:passive]
			ftp = Net::FTP.new(host)
			ftp.passive = passive
			ftp.login(user, pass)
			ftp.getbinaryfile(remote_file, '/dev/null'){ |b|
				out << b
			}
			ftp.close
		end
		module_function :ftp_fetch

		def fetch(uri, header, proxy, out)
			scheme = uri.scheme
			case scheme
			when 'http'
				http_fetch(uri, header, proxy, out)
			when 'ftp'
				ftp_fetch(uri, header, proxy, out)
			else
				raise NumRu::Fetch::FetchError, "unsupported scheme `#{scheme}'"
			end
		end
		module_function :fetch

		def ftp_get_list(uri, header, proxy, list)
			host = uri.host
			port = uri.port
			user = uri.user || 'anonymous'
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
			dir_html = ''
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
			when 'http'
				http_get_list(uri, header, proxy, list)
			when 'ftp'
				ftp_get_list(uri, header, proxy, list)
			else
				raise NumRu::Fetch::FetchError, "unsupported scheme `#{scheme}'"
			end
		end
		module_function :get_list
	end

	module Install
		require 'getopts'
		require 'rbconfig'

		getopts(nil, 'lang:en')
		h = ENV['http_proxy']
		Proxy = h ? URI::parse(h) : nil
		Header = {'accept-language' => $OPT_lang}

		InstallerVersion = '2.0.0'

		DefaultDownloadDir = Dir::pwd
		DefaultPrefix = '/usr/local'
		DefaultRbSiteLibDir = Config::CONFIG['sitelibdir']
		DefaultVerListFile = '/usr/local/share/numru/numru_ver.lst'

		def print_title
			print "\n"
			print '+++', '-'*72, "+++\n"
			print '+++', ' '*13, 'The Dennou Ruby Products Installer  ver. ', InstallerVersion, ' '*13, "+++\n"
			print '+++', ' '*72, "+++\n"
			print '+++', ' '*24, '(C) 2003 GFD Dennou Club', ' '*24, "+++\n"
			print '+++', '-'*72, "+++\n"
			print "\n"
		end
		module_function :print_title

		class Software
			NotInstalled = '  -  '
			VersionUnknown = '  ?  '

			def check_latest_ver(name, url)
				list = Array.new
				print '    ', @url.to_s, " ... \n"
				NumRu::Fetch::get_list(URI::parse(url), Header, Proxy, list)
				if name == 'dcl-5.2-C' or name == 'ruby-dcl'
					list.delete_if do |f|
						not f =~ /^#{name}(-|\.)([0-9p]+|\.)+-gtk2?-[0-9]+\.tar\.(gz|bz2|Z)$/
					end
				else
					list.delete_if do |f|
						not f =~ /^#{name}-([0-9p]+|\.)+\.tar\.(gz|bz2|Z)$/
					end
				end
				latest_fname = list.max
				latest_ver = latest_fname.scan(/#{name}(-|\.)(.*)\.tar\..*/).shift[1]
				if name == 'dcl-5.2-C'
					latest_ver = '5.2.'+ latest_ver.sub(/-gtk2?-[0-9]+/,'')
					src_dir = name + '-' + latest_fname.scan(/.*(gtk2?).*/).shift[0]
				elsif name == 'ruby-dcl'
					latest_ver.sub!(/-gtk2?-[0-9]+/,'')
					src_dir = name + '-' + latest_ver + '-' + latest_fname.scan(/.*(gtk2?).*/).shift[0]
				else
					src_dir = name + '-' + latest_ver
				end
				return [ latest_fname, latest_ver, src_dir ]
			end
			private :check_latest_ver

			def initialize(name, realname, comm, group, check_cmd, url, depend={}, patch=[])
				@name = name
				@realname = realname
				@comm = comm
				@group = group
				@url = url
				@depend = depend
				@patch = patch
				(@latest_fname, @latest_ver, @src_dir) = check_latest_ver(@name, @url)
				@check_cmd = check_cmd
			end

			attr_reader :name, :realname, :comm, :group, :url, :depend, :patch
			attr_reader :latest_ver, :latest_fname, :src_dir

			def installed?
				cmd = @check_cmd.split[0]
				if RUBY_PLATFORM =~ /cygwin/
					if `type '#{cmd}' 2>&1` =~ /not found/
						return false
					else
						return true
					end
				else
					if system("type '#{cmd}' > /dev/null 2>&1")
						return true
					else
						return false
					end
				end
			end

			def current_ver
				if self.installed?
					@current_ver = `#{@check_cmd}`.chop
				else
					@current_ver = NotInstalled
				end
				return @current_ver
			end

			def check_depend
				dep_cleared = Array::new
				dep_failed  = Array::new
				dep_unknown = Array::new
				dep_not_cleared = Array::new
				self.depend.each do |dep, req_ver|
					dep_cleared = (dep_cleared + dep.check_depend['cleared']).uniq
					dep_failed  = (dep_failed  + dep.check_depend['failed']).uniq
					dep_unknown = (dep_unknown + dep.check_depend['unknown']).uniq
					dep_not_cleared = (dep_not_cleared + dep.check_depend['not_cleared']).uniq
					if req_ver == ''
						if dep.installed?
							dep_cleared << dep
						else
							dep_failed << dep
							dep_not_cleared << dep
						end
					else
						if dep.current_ver == VersionUnknown
							dep_unknown << dep
							dep_not_cleared << dep
						elsif dep.current_ver == NotInstalled or dep.current_ver < req_ver
							dep_failed << dep
							dep_not_cleared << dep
						else
							dep_cleared << dep
						end
					end
				end
				dep_result = { 'cleared' => dep_cleared,
				               'failed'  => dep_failed,
									'unknown' => dep_unknown,
									'not_cleared' => dep_not_cleared }
				return dep_result
			end

			def download(dl_dir = DefaultDownloadDir)
				spcsize_dl_msg = 15
				outfile = File::join(dl_dir, @latest_fname)
				File::makedirs(dl_dir) unless File::directory?(dl_dir)
				f = open(outfile, 'w')
				print @realname, ' '*(spcsize_dl_msg - @realname.size), @url, " ...\n"
				begin
					file_uri = URI::parse( File::join(@url, @latest_fname) )
					NumRu::Fetch::fetch(file_uri, Header, Proxy, f)
					f.close
				rescue
					f.close if f and not f.closed?
					File::delete(outfile)
					raise "cannot download \"#{@latest_fname}\".\n"
				end
				@patch.each do |p|
					next if p.sub(/.*\//,'') =~ /cygwin/ and not RUBY_PLATFORM =~ /cygwin/
					outfile = File::join(dl_dir, p.sub(/.*\//,''))
					f = open(outfile, 'w')
					print ' '*spcsize_dl_msg, p, " ...\n"
					begin
						file_uri = URI::parse(p) 
						NumRu::Fetch::fetch(file_uri, Header, Proxy, f)
						f.close
					rescue
						f.close if f and not f.closed?
						File::delete(outfile)
						raise "cannot download \"#{p.sub(/.*\//,'')}\".\n"
					end
				end
			end

			def add_path(path)
				ENV['PATH'] = "#{path}:" + ENV['PATH'] if not ENV['PATH'].split(/:/).include?(path)
			end
			private :add_path

			def install(dl_dir = DefaultDownloadDir, prefix = DefaultPrefix)
				error_msg = 'Installation failed: ' + @realname
				plev = @name == 'netcdf' ? '2' : '1'
				pdir = @name == 'netcdf' ? '../..' : '..'
				if @name == 'netcdf' and RUBY_PLATFORM =~ /cygwin/
					conf_env = 'CXX==""'
				elsif @name == 'netcdf' and RUBY_PLATFORM =~ /linux/
					conf_env = 'CPPFLAGS="-DNDEBUG -Df2cFortran"'
				else
					conf_env = ''
				end
				conf_opt = "--prefix=#{prefix}"

				cur_dir = Dir::pwd
				print '    Installing ', @realname, "...\n\n"
				Dir::chdir(dl_dir)
				system("/bin/rm -rf #{@src_dir}") if File::directory?(@src_dir)
				raise error_msg if not system("zcat #{@latest_fname} | tar xvf -")
				Dir::chdir(@src_dir)
				Dir::chdir('src') if @name == 'netcdf'
				@patch.each do |p|
					next if p.sub(/.*\//,'') =~ /cygwin/ and not RUBY_PLATFORM =~ /cygwin/
					raise error_msg if not system("zcat #{pdir}/#{p.split(/\//)[-1]} | patch -p#{plev}")
				end
				raise error_msg if not system("#{conf_env} ./configure #{conf_opt}")
				raise error_msg if not system('make')
				File::makedirs(prefix) unless File::directory?(prefix)
				raise error_msg if not system('make install')
				Dir::chdir(cur_dir)
				add_path(prefix+'/bin')
				print "\n    ----> succeeded: ", @realname, "\n\n"
			end
		end

		class RubySoft < Software
			def initialize(name, realname, comm, req_name, verlist, url, depend={}, patch=[])
				@name = name
				@realname = realname
				@comm = comm
				@group = self.class.name.split(/::/)[-1]
				@url = url
				@depend = depend
				@patch = patch
				(@latest_fname, @latest_ver, @src_dir) = check_latest_ver(@name, @url)
				@req_name = req_name
				@verlist = verlist
			end

			def installed?
				begin
					require @req_name
					return true
				rescue LoadError
					return false
				end
			end

			def current_ver
				if self.installed?
					@current_ver = VersionUnknown
					line = Array::new
					File::open(@verlist, 'r'){ |f| line = f.readlines }
					line.each do |l|
						unless /^#/ =~ l
							(libname, libver) = l.chop.split
							if libname == @name
								@current_ver = libver
								break
							end
						end
					end
				else
					@current_ver = NotInstalled
				end
				return @current_ver
			end

			def save_latest_ver
				line = Array::new
				File::open(@verlist, 'r'){ |f| line = f.readlines }
				line.delete_if{ |l| not /^#/ =~ l and l.chop.split[0] == @name }
				line << "#{@name}\t#{@latest_ver}\n"
				File::open(@verlist, 'w') do |f|
					line.each{ |l| f.print l }
				end
			end
			protected :save_latest_ver

			def add_load_path(rbsitelibdir)
				new_rbdir = [ "#{rbsitelibdir}/#{Config::CONFIG['arch']}", rbsitelibdir ]
				new_rbdir.each do |rbdir|
					if not $LOAD_PATH.include?(rbdir)
						$LOAD_PATH.unshift(rbdir)
					end
				end
			end
			private :add_load_path
		end

		class RbExtLib < RubySoft
			def install(dl_dir = DefaultDownloadDir, rbsitelibdir = DefaultRbSiteLibDir)
				error_msg = 'Installation failed: ' + @realname
				cur_dir = Dir::pwd
				print '    Installing ', @realname, "...\n\n"
				Dir::chdir(dl_dir)
				system("/bin/rm -rf #{@src_dir}") if File::directory?(@src_dir)
				raise error_msg if not system("zcat #{@latest_fname} | tar xvf -")
				Dir::chdir(@src_dir)
				@patch.each do |p|
					next if p.sub(/.*\//,'') =~ /cygwin/ and not RUBY_PLATFORM =~ /cygwin/
					raise error_msg if not system("zcat ../#{p.split(/\//)[-1]} | patch -p1")
				end
				if @name == 'ruby-dcl' or @name == 'ruby-netcdf'
					conf_opt = Array::new
					conf_opt << "--with-narray-include=#{rbsitelibdir}/#{Config::CONFIG['arch']}"
					if RUBY_PLATFORM =~ /cygwin/
						conf_opt << "--with-narray-lib=#{rbsitelibdir}/#{Config::CONFIG['arch']}"
					end
					if @name == 'ruby-netcdf'
						if RUBY_PLATFORM =~ /cygwin/
							conf_opt << '--with-netcdf-dir=' + File::dirname(`type 'ncgen'`.chop.split[-1]).sub(/\/bin/, '')
						else
							conf_opt << '--with-netcdf-dir=' + File::dirname(`type 'ncgen'`.chop.split[-1]).sub(/\/bin/, '')
						end
					end
					until system("ruby extconf.rb #{conf_opt.join(' ')}")
						print 'Specify the directory that contains NArray header files ; '
						narray_inc = gets
						conf_opt.each do |cf|
							cf.sub!(/^(--with-narray-(include|lib)=).*$/, '\1' + narray_inc.chop)
						end
					end
				else
					raise error_msg if not system('ruby extconf.rb')
				end
				raise error_msg if not system('mv -f Makefile Makefile.orig')
				inst_dir = rbsitelibdir.gsub(/\//, '\/')
				raise error_msg if not system("sed -e 's/\\$(sitearchdir)\\(\\$(target_prefix)\\)/#{inst_dir}\\/\\$(arch)\\1/' -e 's/\\$(sitelibdir)\\(\\$(target_prefix)\\)/#{inst_dir}\\1/' Makefile.orig > Makefile")
				raise error_msg if not system('make')
				if @name == 'narray' and RUBY_PLATFORM =~ /cygwin/ and RUBY_VERSION < '1.7.0'
					raise error_msg if not system('mv -f narray.h.with_ruby16_cygwin narray.h') 
				end
				raise error_msg if not system('make site-install')
				Dir::chdir(cur_dir)
				self.save_latest_ver
				add_load_path(rbsitelibdir)
				print "\n    ----> succeeded: ", @realname, "\n\n"
			end
		end

		class RbLib < RubySoft
			def install(dl_dir = DefaultDownloadDir, rbsitelibdir = DefaultRbSiteLibDir)
				error_msg = 'Installation failed: ' + @realname
				cur_dir = Dir::pwd
				print '    Installing ', @realname, "...\n\n"
				Dir::chdir(dl_dir)
				system("/bin/rm -rf #{@src_dir}") if File::directory?(@src_dir)
				raise error_msg if not system("zcat #{@latest_fname} | tar xvf -")
				Dir::chdir(@src_dir)
				@patch.each do |p|
					next if p.sub(/.*\//,'') =~ /cygwin/ and not RUBY_PLATFORM =~ /cygwin/
					raise error_msg if not system("zcat ../#{p.split(/\//)[-1]} | patch -p1")
				end
				if @name == 'narray_miss'
				  File::makedirs(rbsitelibdir) unless File::directory?(rbsitelibdir)
				  File::install("#{@name}.rb", "#{rbsitelibdir}/#{@name}.rb", 0644, true)
				else
					raise error_msg if not system("ruby install.rb -d #{rbsitelibdir}")
				end
				Dir::chdir(cur_dir)
				self.save_latest_ver
				add_load_path(rbsitelibdir)
				print "\n    ----> succeeded: ", @realname, "\n\n"
			end
		end
	end
end


if __FILE__ == $0
	include NumRu::Install

	system('clear')
	print_title

	verlist = DefaultVerListFile
	print "Checking version list of installed NumRu libraries...\n"
	unless File::exist?(verlist)
		print "\n"
		print "    ----> The version list is not found. If this is the first time to use this\n"
		print '          installer, you must create it. Create? [y/N] ; '
		y_or_n = gets
		if y_or_n =~ /^y/i
			create_verlist = 'n'
			until create_verlist == "\n" or create_verlist =~ /^y/i
				print "\n"
				print "    ----> Specify the directory [#{File::dirname(DefaultVerListFile)}] ; "
				verlist_dir = gets
				if verlist_dir == "\n"
					verlist = DefaultVerListFile
				else
					verlist = File::join(verlist_dir.chop, File::basename(DefaultVerListFile))
				end
				print "\n"
				print "    ----> Create a version list \"#{verlist}\" ?\n"
				print '          [Y/n] ; '
				create_verlist = gets
			end
			File::makedirs(File::dirname(verlist)) unless File::directory?(File::dirname(verlist))
			File::open(verlist, 'w') do |f|
				f.print "### Version list of NumRu libraries for Dennnou Ruby Products Installer ###\n"
				f.print "### Created by Dennnou Ruby Products Installer    (C) GFD Dennou Club   ###\n"
			end
		else
			until File::exist?(verlist)
			print "\n"
			print "    ----> The version list is not found.\n"
			print "          Specify the directory [#{File::dirname(DefaultVerListFile)}] ; "
				verlist_dir = gets
				if verlist_dir == "\n"
					verlist = DefaultVerListFile
				else
					verlist = File::join(verlist_dir.chop, File::basename(DefaultVerListFile))
				end
			end
		end
	end
	print "\n"
	print "    ----> succeeded.\n\n"

	print "Connecting remote servers and getting the list of packages...\n\n"

	dcl_c = Software.new(
		'dcl-5.2-C', 'DCL(C)',
		'DCL graphic library, C version', 'Library',
		"cdclconfig --dclversion | sed 's/-C//'",
		'http://www-mete.kugi.kyoto-u.ac.jp/seiya/dcl/tarball',
		{},
		['http://www.gfd-dennou.org/arch/ruby/tutorial/install/patches/dcl52c_cygwin.patch.gz']
	)
	netcdf = Software.new(
		'netcdf', 'NetCDF',
		'NetCDF lib & commands (file handling)', 'Library',
		"ncgen -h 2>&1 | sed -n 's/\\(.*version \\)\\(.*\\)\\( of.*\\)/\\2/p'",
		'http://www.gfd-dennou.org/arch/netcdf/unidata-mirror',
		{},
		['http://www.gfd-dennou.org/arch/ruby/tutorial/install/patches/netcdf350_cygwin.patch.gz']
	)
	narray = RbExtLib.new(
		'narray', 'NArray',
		'Multi-dimensional numeric array',
		'narray', verlist, 
		'http://www.ir.isas.ac.jp/~masa/ruby/dist',
		{},
		['http://www.gfd-dennou.org/arch/ruby/tutorial/install/patches/narray057.patch.gz']
	)
	narray_miss = RbLib.new(
		'narray_miss', 'NArrayMiss',
		'Data missing handling for NArray',
		'narray_miss', verlist, 
		'ftp://www.gfd-dennou.org/arch/ruby/products/narray_miss',
		{narray => ''}
	)
	rubydcl = RbExtLib.new(
		'ruby-dcl', 'RubyDCL',
		'Ruby wrapper of DCL',
		'numru/dcl', verlist, 
		'http://www-mete.kugi.kyoto-u.ac.jp/seiya/dcl/tarball',
		{dcl_c => '5.2.3', narray => '0.5.5', narray_miss => ''},
		['http://www.gfd-dennou.org/arch/ruby/tutorial/install/patches/ruby-dcl130_cygwin.patch.gz']
	)
	rubynetcdf = RbExtLib.new(
		'ruby-netcdf', 'RubyNetCDF',
		'Ruby wrapper of NetCDF',
		'numru/netcdf', verlist, 
		'ftp://www.gfd-dennou.org/arch/ruby/products/ruby-netcdf',
		{netcdf => '3.4', narray => '0.5.5', narray_miss => ''}
	)
	misc = RbLib.new(
		'misc', 'NumRu/Misc',
		'Miscellaneous functions and classes',
		'numru/misc', verlist, 
		'ftp://www.gfd-dennou.org/arch/ruby/products/misc',
		{narray => ''}
	)
	met = RbLib.new(
		'met', 'NumRu/Met',
		'Meteorological functions and classes',
		'numru/met', verlist, 
		'ftp://www.gfd-dennou.org/arch/ruby/products/met',
		{misc => '0.0.4', rubydcl => '1.3.0'}
	)
	gphys = RbLib.new(
		'gphys', 'GPhys',
		'Class for Gridded Physical quantities',
		'numru/gphys', verlist, 
		'ftp://www.gfd-dennou.org/arch/ruby/products/gphys',
		{rubydcl => '1.2.1', rubynetcdf => '0.5.1', misc => '0.0.3'}
	)

software = [dcl_c, netcdf, narray, narray_miss, rubydcl, rubynetcdf, misc,
            met, gphys]

	print "\n"
	print "    ----> succeeded.\n\n"

	realname0 = 'name'
	charsize = Array.new
	software.each{ |soft| charsize << soft.realname.size }
	spcsize1 = charsize.max + 2
	spcsize1 = realname0.size + 1 if spcsize1 <= realname0.size
	current0  = 'current'
	charsize = Array.new
	software.each{ |soft| charsize << soft.current_ver.size }
	spcsize2 = charsize.max + 2
	spcsize2 = current0.size + 1 if spcsize2 <= current0.size
	latest0   = 'latest'
	charsize = Array.new
	software.each{ |soft| charsize << soft.latest_ver.size }
	spcsize3 = charsize.max + 2
	spcsize3 = latest0.size + 1 if spcsize3 <= latest0.size
	group0    = 'group'
	charsize = Array.new
	software.each{ |soft| charsize << soft.group.size }
	spcsize4 = charsize.max + 2
	spcsize4 = group0.size + 1 if spcsize4 <= group0.size
	comm0     = 'summary'

	stop_loop = nil

	while not stop_loop
	
		if stop_loop == false
			print 'Do you want to install another software? [Y/n] ; '
			y_or_n = gets
			if y_or_n == "\n" or y_or_n =~ /^y/i
				system('clear')
				print_title
			else
				stop_loop == true
				print "\n"
				break
			end
		end

		print "You can select the following packages:\n\n"
		print '    ',
		      realname0, ' '*(spcsize1 - realname0.size),
				current0,  ' '*(spcsize2 - current0.size),
				latest0,   ' '*(spcsize3 - latest0.size),
				group0,    ' '*(spcsize4 - group0.size),
				comm0,     "\n"
		print '    ', '-'*74, "\n"
		software.each_with_index do |soft, j|
			num = (j+1).to_s.strip
			num = ' ' + num  while num.size < 2
			print num, ') ',
			      soft.realname,    ' '*(spcsize1 - soft.realname.size),
					soft.current_ver, ' '*(spcsize2 - soft.current_ver.size),
					soft.latest_ver,  ' '*(spcsize3 - soft.latest_ver.size),
					soft.group,       ' '*(spcsize4 - soft.group.size),
					soft.comm,        "\n"
		end
		num_soft = 0
		first = true
		until num_soft >= 1 and num_soft <= software.size
			print "\n    ----> Invalid number.\n" if not first
			print "\nWhich software do you want to install? (Ctrl-d to quit) [1-", software.size.to_s.strip, '] ; '
			num_soft = gets.chop.to_i
			first = false
		end
		print "\n    ----> Select ", software[num_soft-1].realname, ".\n\n"

		case software[num_soft-1].current_ver
		when software[num_soft-1].latest_ver
			print "Sorry, #{software[num_soft-1].realname} is already the newest version.\n\n"
			stop_loop = false
			next
		when Software::VersionUnknown
			print "Version of the selected package is unknown.\n"
			print "Do you want to install them? [y/N] ; "
			y_or_n = gets
			if y_or_n =~ /^y/i
				print "\n    ----> Yes. install.\n\n"
			else
				print "\n    ----> No.\n\n"
				stop_loop = false
				next
			end
		end

		print "Checking depedencies...\n\n"
		unless software[num_soft-1].check_depend['unknown'].size == 0
			print "    Versions of the following packages are unknown.\n"
			print '    ', software[num_soft-1].realname, " may not work correctly unless these packages are installed.\n\n        "
			software[num_soft-1].check_depend['unknown'].each do |unknown|
				print unknown.realname, '  '
			end
			print "\n\n    Do you want to install them? [y/N] ; "
			y_or_n = gets
			if y_or_n =~ /^y/i
				print "\n        ----> Yes. install.\n\n"
				inst_soft = software[num_soft-1].check_depend['not_cleared']
			else
				print "\n        ----> No.\n\n"
				inst_soft = software[num_soft-1].check_depend['failed']
			end
		else
			inst_soft = software[num_soft-1].check_depend['failed']
		end
		inst_soft << software[num_soft-1]
		print "    ----> succeeded.\n\n"

		print "The following packages will be installed:\n\n    "
		inst_soft.each do |soft|
			print soft.realname, '  '
		end

		print "\n\nSpecify a directory to store downloaded packages [", DefaultDownloadDir, '] ; '
		dl_dir = gets
		dl_dir.chop!
		dl_dir = DefaultDownloadDir if dl_dir == ''

		if inst_soft.map{ |soft| soft.class.name.split(/::/)[-1] }.include?('Software')
			print "\nSpecify an installation prefix [", DefaultPrefix, '] ; '
			prefix = gets
			prefix.chop!
			prefix = DefaultPrefix if prefix == ''
		end

		if inst_soft.map{ |soft| soft.class.superclass.name.split(/::/)[-1] }.include?('RubySoft')
			print "\nSpecify an installation directory for Ruby libraries [", DefaultRbSiteLibDir, '] ; '
			rbsitelibdir = gets
			rbsitelibdir.chop!
			rbsitelibdir = DefaultRbSiteLibDir if rbsitelibdir == ''
		end

		print "\n\nDownload directory:     #{dl_dir}\n"
		if inst_soft.map{ |soft| soft.class.name.split(/::/)[-1] }.include?('Software')
			print "Installation prefix:    #{prefix}\n"
		end
		if inst_soft.map{ |soft| soft.class.superclass.name.split(/::/)[-1] }.include?('RubySoft')
			print "Ruby library directory: #{rbsitelibdir}\n\n"
		end
	
		print "Do you want to continue? [Y/n] ; "
		y_or_n = gets
		if y_or_n =="\n" or y_or_n =~ /^y/i
			print "\n    ----> Yes. install.\n\n"
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

		print "Start installation...\n\n"
		inst_soft.each do |soft|
			case soft.class.name.split(/::/)[-1]
			when 'Software'
				soft.install(dl_dir, prefix)
			when 'RbExtLib', 'RbLib'
				soft.install(dl_dir, rbsitelibdir)
			end
		end
		print "Installation is completed.\n\n"
		stop_loop = false
	end
	print "Thank you. Enjoy the Dennou Ruby suite! Have a good day!\n"
	print "Make a point of checking your environment variable \"PATH\" and \"RUBYLIB\"!\n\n"
end
