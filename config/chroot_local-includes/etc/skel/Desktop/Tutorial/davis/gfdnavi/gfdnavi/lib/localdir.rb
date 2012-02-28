require 'numru/gphys'

module NumRu
  class LocalDir

    def initialize(path)
      @dir = Dir.new(path)
    end

    class << self
      alias open new

=begin
      def open(path)
        file = File.open(path)
        if block_given?
          yield(file)
          file.close
          nil
        else
          file
        end
      end
=end
    end

    def path
      @dir.path
    end

    def name
      File.basename(path)
    end

    def entries
      @dir.entries
    end

    def each_dir
      @dir.each do |name|
	path = File.join(self.path,name)
	if name!='.' && name!='..' && File.directory?(path)
	  dir = self.class.new(path)
          mtime = File.mtime(path)
	  yield(dir, mtime)
	end
      end
      nil
    end

    def each_gphys_file
      @dir.each do |name|
	path = File.join(self.path,name)
        if (File.file?(path) || (File.symlink?(path)&&File.file?(File.readlink(path))) ||(/nus$/=~path&&File.directory?(path))) && (klass=GPhys::IO.file2file_class(path))
#          file = klass.open(path)
          mtime = File.mtime(path)
          size = File.size(path)
#          yield(file, mtime, size)
          yield(path, mtime, size, klass)
        end
      end
      nil
    end

    # name_pattern : regexp to limit by name
    #
    def each_file(name_pattern = nil)
      @dir.each do |name|
        path = File.join(self.path,name)
        if File.file?(path) and 
                 (name_pattern.nil? or name_pattern =~ name)
          yield(path, File.mtime(path), File.size(path) )
        end
      end
    end

=begin
    def plain_file_paths
      if @plain_file_paths.nil? 
        @plain_file_paths = Array.new
        @dir.each do |name|
          path = File.join(self.path,name)
          if File.file?(path) && GPhys::IO.file2file_class(path).nil?
            @plain_file_paths.push(path)
          end
        end
      end
      @plain_file_paths 
    end
=end

  end
end

if $0 == __FILE__
  include NumRu
  dir = LocalDir.open(File.expand_path(File.dirname(__FILE__)+'/../public/data/samples/reanalysis'))
  dir.each_dir do |d, mtime|
    p d.path, mtime
    d.each_gphys_file do |file, mtime,size|
      p file.path,mtime,size,GPhys::IO.var_names_except_coordinates(file)
      file.close
    end
  end
  pat = Regexp.union(/readme$/i, /\.yml$/)
  dir.each_file(pat) do |path, mtime, size|
    print "  **** #{path} #{mtime} #{size}\n"
    open(path){|f| print f.gets}
  end
end
