module NumRu
  class NetCDF
    # For Marshal.dump. The argument "limit" is ignored.
    def _dump(limit)
      path = self.path
      if /^\// =~ path
        abspath = path
      else
        abspath = Dir.pwd + '/' + path    # Inappropriate after chdir
        raise("No such file: #{abspath}. Directory changed?") if !File.exist?(abspath)
      end
      abspath
    end

    def NetCDF._load(str)
       NumRu::NetCDF.open(str)
    end
  end

  class NetCDFVar
    def _dump(limit)
      # 'mode' and 'share' are not supported
      Marshal.dump([file,name])
    end

    def NetCDFVar._load(str)
      ary = Marshal.load(str)
      NumRu::NetCDFVar.new(*ary)
    end
  end

  class VArrayNetCDF
    def _dump(limit)
      Marshal.dump(@ary) # @ary is a NetCDFVar object
    end

    def VArrayNetCDF._load(str)
      VArrayNetCDF.new(Marshal.load(str))
    end
  end
end
