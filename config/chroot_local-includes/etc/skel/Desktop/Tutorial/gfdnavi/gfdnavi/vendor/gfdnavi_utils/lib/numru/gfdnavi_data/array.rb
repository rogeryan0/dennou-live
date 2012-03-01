require "numru/gfdnavi_data/base"

module NumRu::GfdnaviData
  class Array < NumRu::GfdnaviData::Base

    # singleton methods
    def self.[](*objs)
      locate = nil
      objs_new = ::Array.new
      objs.each do |obj|
        case obj
        when NumRu::GfdnaviData::Array
          obj.each do |o|
            objs_new.push o
          end
        when NumRu::GfdnaviData::Base
          objs_new.push obj
        else
          raise "#{obj.class} is not supported"
        end
        if obj.remote?
          loc = obj.location || obj.url.sub(/#{Regexp.escape(obj.path)}/,"")
        elsif obj.local?
          loc = :local
        end
        if locate
          unless locate == loc
            raise "operation over different hosts is not supported yet: #{locate} vs #{loc}"
          end
        else
          locate = loc
        end
      end
      if locate == :local
        NumRu::GfdnaviData::ArrayLocal.new("object" => objs_new)
      else
        url = "#{locate}/[#{objs_new.collect{|obj| obj.path}.join(",")}]"
        user = nil
        pw = nil
        objs_new.each do |obj|
          if u = obj.user
            if user
              raise("users are not the same") if user != obj.user
            else
              user = u
            end
            if pw2 = obj.password
              if pw
                raise("password is not the same") if pw != obj.password
              else
                pw = pw2
              end
            end
          end
        end
        gd = NumRu::GfdnaviData::ArrayRemote.new("url"=>url, "user"=>user)
        gd.password = pw
        gd.location = locate
        gd
      end
    end


    # instance methods
    def [](*index)
      slice(*index)
    end

    def length
      get_attribute("length")
    end

    def each
      length.times do |i|
        yield self[i]
      end
    end

    def each_with_index
      length.times do |i|
        yield self[i], i
      end
    end

    def cut(arg)
      ary = ::Array.new
      self.each do |o|
        ary.push o.cut(arg)
      end
      return NumRu::GfdnaviData::Array[*ary]
    end

    %w(gphys png).each do |name|
      eval <<-EOL, binding, __FILE__, __LINE__+1
      def to_#{name}
        if length == 1
          self[0].to_#{name}
        else
          raise "Method #{name} is called for \#{self.class} of length = \#{length} > 1"
        end
      end
      EOL
    end

  end
end
