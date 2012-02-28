require "numru/gfdnavi_data/image"

module NumRu::GfdnaviData
  class ImageRemote < NumRu::GfdnaviData::Image

    def to_png
      get_object("png")
    end

    def png=(png)
      unless @new_data
        raise "cannot change data"
      end
      unless String === png && /\A.PNG/ =~ png
        raise "invalid PNG data"
      end
      @representation["png"] = png
    end

    def update_save_data(hash)
      hash["png"] = @representation["png"]
    end
  end
end
