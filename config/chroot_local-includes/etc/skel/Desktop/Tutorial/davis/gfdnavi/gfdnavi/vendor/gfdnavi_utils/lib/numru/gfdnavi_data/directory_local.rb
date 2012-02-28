require "rexml/document"

module NumRu::GfdnaviData
  class DirectoryLocal < NumRu::GfdnaviData::Directory

    OBJECT_CLASS = ::Directory

    def find(query)
      @path = ::Array.new # Array => ::Array
      all=NodeQuery.new
      descriptions= ::Array.new # Array => ::Array
      if query == "all"
        $descriptions <<  "all"
      else
        descriptions=query.split(/&/)
      end
      $descriptions[0] = "path=#{self.path}"
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
        if qstr!="all" then
          tmptable_name="tmptable_#{Time.now.to_i}"
          qstr="create table #{tmptable_name} as #{qstr}"
          Node.connection.execute(qstr)
          results = all.generate_results_fromtmptable(0,tmptable_name,$options,@user)	    
          Node.connection.execute("drop table #{tmptable_name}") #delete temporary table
        else
          results=all.generate_results_fromtmptable(1,tmptable_name,$options,@user)	    
        end
        result=results.to_xml
        if  $options["show_kwfacets"] || $options["show_spfacets"] || ($options["show_kwvalues"]&&$options["show_kwfacets"]) || $options["show_pathtree"] || $options["no_result"]
          return result
        else
          result=REXML::Document.new(result)
          xpath="//node"
          result.elements.each(xpath) do |e|
            npath=e.elements["path"]
            npath=npath.text
            @path.push(npath)
          end
          obj2 = ::Array.new # Array => ::Array
          @path.each{ |path| 
            obj2 +=  Node.find(:all, :conditions => ["path=?",path], :user=>@user) 
          }
          #obj = create_data_array(obj2)
          obj = create(obj2)
          return obj
        end
      else
        return 
      end
    end

    def to_nc
      if ::Directory === @object && @object.downloadable?
        @object.fname
      else
        raise "object is invalid (#{@object.inspect})"
      end
    end



  end
end
