require "numru/gfdnavi_data/array"

module NumRu::GfdnaviData
  class ArrayRemote < NumRu::GfdnaviData::Array

    def slice(*index)
      get_object
      if hash = @object["slice"]
        url = hash["url"]
        ind = Regexp.escape(hash["index"])
        url = url.sub(/#{ind}/, index.join(","))
        obj = nil
      else
        obj = ::Array.new
        index.each do |i|
          o = @object[i]
          unless o
            raise "index (#{i}) is out of range"
          end
          obj.push o
        end
        url = nil
      end
      if index.length == 1 && Integer === index[-1]
        case @object["type"]
        when /array_(.+)/
          dtype = $1
        when "array"
          return NumRu::GfdnaviData::Remote.parse_url(@object[index[0]]["url"], @user, @password)
        else
          raise "type (#{@object["type"]}) is invalid"
        end
      else
        dtype = "array"
      end
      NumRu::GfdnaviData::Remote.create(url, @user, @password, obj, dtype)
    end
 


#add ##
    def find(query)
      # The url below should be used to access the server:
      #url = "http://#{input_server}data#{input_path}/find(#{query}).xml?show_kwfacets=0&show_spfacets=0&no_result=1&show_pathtree=1" 


      @path =::Array.new
      all=NodeQuery.new # !!! NodeQuery IS NOT ACCESSIBLE FROM GfdnaviData::Remote !!!
      descriptions=::Array.new
      if query == "all"
        $descriptions <<  "all"
      else
        descriptions=query.split(/&/)
      end
      
      descriptions.each { |desc|
        
        if /\Apath=/ =~ desc
          $descriptions[0] = desc
        else
          $descriptions[$i] = desc
          $i=$i+1
        end
      }
      
      qstr = all.make_query($descriptions,user=nil)
      if qstr then
        rnodes=Node.find_by_sql(qstr)
        expres=ExplorerResult.new(-1)
        expres.put_results(rnodes)
        options=Hash.new
        results=all.generate_results(rnodes,expres,$options)
        result=results.to_xml
        result=REXML::Document.new(result)
        
        xpath="//node"
        result.elements.each(xpath) do |e|
          npath=e.elements["path"]
          npath=npath.text
          @path.push(npath)
        end
        obj2 = ::Array.new() 
        
        @path.each{ |path| 
          obj2 +=  Node.find(:all, :conditions => ["path=?",path], :user=>@user) 
        }
        obj = create(obj2)#create_data_array->create
        
        if $options["show_kwfacets"] || $options["show_spfacets"] || ($options["show_kwvalues"]&&$options["show_kwfacets"]) || $options["show_pathtree"] || $options["no_result"]
          return result
        else
          return obj 
        end    
      else
        return 
      end
    end
    
    def analysis(func, *args)
      get_object
      if hash = @object["analysis"]
        url = hash["url"]
        func_name = Regexp.escape(hash["function_name"])
        func_args = Regexp.escape(hash["function_arguments"])
        url = url.sub(/#{func_name}/, func)
        url = url.sub(/#{func_args}/, args.join(","))
        return NumRu::GfdnaviData::Remote.create(url, @user, @password, nil, "array")
      else
        raise "anaysis is not supported for this array"
      end
    end

    def plot(dm, opts = Hash.new)
      get_object
      if !self.location && /#{Regexp.escape(self.path)}/ =~ self.url
        self.location = self.url.sub(/#{Regexp.escape(self.path)}/, "").sub(/\/\[\]\z/, "")
      end
      if hash = @object["plot"]
        url = hash["url"]
        dm_name = Regexp.escape(hash["draw_method_name"])
        dm_opts = Regexp.escape(hash["draw_method_options"])
        url = url.sub(/#{dm_name}/, dm)
        url = url.sub(/#{dm_opts}/, NumRu::GfdnaviData.hash_to_str(opts))
        gd = create("url"=>url, "type"=>"array")
        gd.location = self.location if self.location
        return gd
      else
        raise "plot is not supported for this array"
      end
    end

    def overlay
      get_object
      if !self.location && /#{Regexp.escape(self.path)}/ =~ self.url
        self.location = self.url.sub(/#{Regexp.escape(self.path)}/, "").sub(/\/\[\]\z/, "")
      end
      raise "location could not be obtained" unless self.location
      ary = ::Array.new
      self.each{|plot| ary.push(plot.path)}
      url = self.location + "/[" + ary.join(",") + "]/overlay()"
      gd = create("url" => url, "type" => "image")
      gd.location = self.location if self.location
      return gd
    end

  end
end
