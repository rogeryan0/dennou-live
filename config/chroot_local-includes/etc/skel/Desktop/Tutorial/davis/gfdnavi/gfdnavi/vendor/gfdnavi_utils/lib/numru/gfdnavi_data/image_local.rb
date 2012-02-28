require "numru/gfdnavi_data/image"
require "fileutils"

module NumRu::GfdnaviData
  class ImageLocal < NumRu::GfdnaviData::Image

    OBJECT_CLASS = ::Image

    attr_reader :errors

    def to_png
      @object.to_png
    end

    def copy(path)
      img = Image.new
      img.org_path = self.org_path
      img.path = path
      img.name = File.basename(path)
      FileUtils.cp(fname, img.fname)
      img
    end

    def png=(png)
      unless @new_data
        raise "cannot change data"
      end
      unless String === png && /\A.PNG/ =~ png
        raise "not PNG data"
      end
      @png = png
    end

    def save
      if @new_data
        fname = Node.add_prefix(path)
        if File.exist?(fname)
          @errors = ["File aleady exists"]
          return false
        end
        File.open(fname, "w") do |file|
          file.write @png
        end
      end
      if @object.save
        return true
      else
        @errors = @object.errors.full_messages
        return false
      end
    end
  end
end
