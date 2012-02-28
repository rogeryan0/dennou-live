module NumRu
  class NuSDaS
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

    def NuSDaS._load(str)
       NumRu::NuSDaS.open(str)
    end
  end

  class NuSDaSVar
    def _dump(limit)
      Marshal.dump([@nusdas,@name]) # @nusdas is a NuSDaS object
    end

    def NuSDaSVar._load(str)
      ary = Marshal.load(str)
      NumRu::NuSDaSVar.new(*ary)
    end
  end

  class VArrayNuSDaS
    def _dump(limit)
      Marshal.dump(@ary) # @ary is a NuSDaSVar object
    end

    def VArrayNuSDaS._load(str)
      VArrayNuSDaS.new(Marshal.load(str))
    end
  end
end
