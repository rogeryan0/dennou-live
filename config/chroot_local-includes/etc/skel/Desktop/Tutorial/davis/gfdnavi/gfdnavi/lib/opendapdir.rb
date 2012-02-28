require 'open-uri'
require 'numru/gphys'
require 'date'

module NumRu
  class OPeNDAPDir
    def initialize(url)
      @path = url.sub(/\/$/,'')
      @child_dirs = Array.new
      @child_gphys_files = Array.new   # files that can have gphysiable data
      @child_plain_files = Array.new     # external files (non-opendap access)
      open(url) {|f|
	if f.content_type != 'text/html'
         raise(ArgumentError,"#{url}: Not text/html. Cannot be an OPeNDAP Dir")
	end
	opendapdir = false  # to initialize
	while(line = f.gets)
	  if /Parent Directory/i =~ line
	    opendapdir = true
	    break
	  end
	end
	if !opendapdir
	  raise(ArgumentError,"#{url}: is not likely an OPeNDAP directory.")
	end
	while(line = f.gets)
	  case line
	  when /<A HREF="(.*)">(.*)<\/a>\s*(.*)$/
            child = { :url => $1 }
	    label = $2
            time_size_descr = $3
            if /^(\S+\s+\S+)\s+(\S+)/ =~ time_size_descr
              size = $2
              time = $1
              t = DateTime.parse(time)
              child[:mtime] = Time.local(t.year,t.mon,t.day,t.hour,t.min,t.sec)
                                  #^ local works with mysql (how about others?)
              if /^([\d\.]+)([KMG])?$/ =~ size
                if $2.nil?
                  child[:size] = $1.to_i
                else
                  size = $1.to_f
                  size *= 1000 if $2=='K'
                  size *= 1000000 if $2=='M'
                  size *= 10000000 if $2=='G'
                  child[:size] = size.to_i
                end
              elsif '-' == size
                child[:size] = nil
              else
                raise "(BUG) new pattern to support : #{size}"
              end
            end
	    /^(.*)\/([^\/]+)\/?$/ =~ child[:url]
	    dir = $1
	    child[:name] = $2
	    if /\/$/ =~ child[:url]
	      @child_dirs.push(child)
	    elsif /\.html$/ !~ label and /\.html$/ =~ child[:name]
	      child[:url].sub!(/\.html$/,'')
	      child[:name].sub!(/\.html$/,'')
	      if /\.nc$/ =~ label  # we don't need to limit to .nc files, but..
		@child_gphys_files.push(child)
	      end
	    elsif dir != @path
	      @child_plain_files.push(child)
	    else
	      raise "(BUG) Unclassified kind of a child : #{child[:url]}"
	    end
#	    p child
#	    print "#{File.basename(child[:url])}  #{child[:mtime]}  ",child[:size],"\n"
#	    print "@@\n"
	  end
	end
      }
    end

    class << self
      alias open new
    end

    def path
      @path
    end

    def name
      File.basename(path)
    end

    def entries
      if @enttries.nil?
        @enttries = @child_dirs.collect{|c| c[:name]} +
          @child_gphys_files.collect{|c| c[:name]} +
          @child_plain_files.collect{|c| c[:name]}
      end
      @enttries
    end

    def each_dir
      @child_dirs.each{|child|
	dir = OPeNDAPDir.new(child[:url])
	yield(dir, child[:mtime])
      }
      nil
    end

    # Open dods url as NetCDF files and iterate.
    # It is up to the users to close the files.
    def each_gphys_file
      @child_gphys_files.each{|child|
        begin
          file = NetCDF.open(child[:url])
          yield(file.path, child[:mtime], child[:size], file.class)
        rescue NetcdfSyserr
          warn "#{__FILE__}:#{__LINE__}: **Error neglected** Unable to open #{child[:url]}"
        end
      }
      nil
    end

    # name_pattern : regexp to limit by name
    #
    def each_file(name_pattern = nil)
      @child_plain_files.each{|child|
        if name_pattern.nil? or name_pattern =~ child[:name]
          yield(child[:url], child[:mtime], child[:size])
        end
      }
    end

    #def plain_file_paths
    #  @child_plain_files.collect{|child| child[:url]}
    #end

  end
end

if __FILE__ == $0
  include NumRu
  urls = %w(http://coastwatch.chesapeakebay.noaa.gov/dap/edac/qscat_wind/data/
            http://coastwatch.chesapeakebay.noaa.gov/dap/edac/qscat_wind/
            http://stellwagen.er.usgs.gov/cgi-bin/nph-dods/DATAFILES/ECOHAB_II/
            http://dods.ipsl.jussieu.fr/cgi-bin/nph-dods/prism/gridsCF/
            http://test.opendap.org/opendap-3.7/nph-dods/data/ )
#  urls = %w( http://dust.ess.uci.edu/cgi-bin/dods/nph-dods/dodsdata )
#  urls = %w( http://test.opendap.org/opendap-3.7/nph-dods/data/ )

  urls.each do |url|
    dir = OPeNDAPDir.new(url)
    print dir.path,"\n"
    dir.each_dir{|d,mtime| print "  (sub dir) #{d.path} #{mtime}\n"}

    count = 0
    dir.each_gphys_file do |file, mtime, size|
      print "  #{file.path} #{mtime} size:#{size}\n    "
      p GPhys::IO.var_names_except_coordinates(file)
      file.close
      count += 1
      break if count >= 3
    end

    pat = Regexp.union(/readme$/i, /\.txt$/)
    dir.each_file(pat) do |path, mtime, size|
      open(path){ |file| 
        print " ***** ",path, " ", mtime, " ", size, "\n"
        file.gets && print($_) && file.gets && print($_)
      }
    end
  end

end
