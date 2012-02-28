class Image < NodeEntityAbstract

  def png
    if /\.png\z/ =~ name
      image = File.read(fname)
      image = image.force_encoding('ASCII-8BIT') if image.respond_to?(:force_encoding) # RUBY 1.9
      return image
    else
      return nil
    end
  end
  alias :to_png :png

  def content_type
    case name
    when /\.png\z/i
      'image/png'
    when /\.(?:jpeg|\.jpg)\z/i
      'image/jpeg'
    when /\.gif\z/i
      'image/gif'
    when /\.tiff?\z/i
      'image/tiff'
    when /\.xwd\z/i
      'image/xwindowdump'
    else
      raise "#{name} Not a image file or unsupported image type (which can be easily extended. Contact the gfdnavi developpers.)"
    end
  end

  def destroy_all
    FileUtils.remove(fname)
    super
  end


end
