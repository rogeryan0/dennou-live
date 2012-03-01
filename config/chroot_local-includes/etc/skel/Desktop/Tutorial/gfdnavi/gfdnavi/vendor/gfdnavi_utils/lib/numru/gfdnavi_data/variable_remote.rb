require "numru/gfdnavi_data/variable"
require "numru/gphys"

module NumRu::GfdnaviData
  class VariableRemote < NumRu::GfdnaviData::Variable

    def slice(*index)
      get_object
      if hash = @object["slice"]
        url = hash["url"]
        ind = Regexp.escape(hash["index"])
        url = url.sub(/#{ind}/, index.join(","))
      else
        url = @object[*index]
      end
      if index.length == 1 && Integer === index[-1]
        return create_data(url)
      else
        return create_data_array(url)
      end
    end

    def cut(*args)
      get_object
      if hash = @object["cut"]
        url = hash["url"]
        arg = Regexp.escape(hash["arguments"])
        args = args[0].collect{|k,v| "#{k}=>#{v}"} if (args.length==1 && args[0].kind_of?(Hash))
        url = url.sub(/#{arg}/, args.join(","))
      else
        raise "BUG"
      end
      return create("url"=>url, "type"=>"variable")
    end


    def to_gphys
      Marshal.load(get_object("gphys"))
    end

    def gphys=(gphys)
      unless @new_data
        raise "cannot change data"
      end
      unless NumRu::GPhys === gphys
        raise "gphys must be NumRu::GPhys"
      end
      @representation["gphys"] = gphys
    end

    def update_save_data(hash)
      hash["gphys"] = Marshal.dump(@representation["gphys"])
    end

  end
end
