require "numru/gfdnavi_data/array"

module NumRu::GfdnaviData
  class ArrayLocal < NumRu::GfdnaviData::Array

    def analysis(func, args=[])
      case func
      when String
        func, user = func.split(",")
        func_org = func
        user_org = user
        user ||= "root"
        func = ::Function.find(:first, :conditions => ["path=?","/usr/#{user}/functions/#{func}"], :user => @user)
        unless func
          raise "function not found: name = #{func_org}, user = #{user_org}"
        end
      when ::Function
      else
        raise "function is invalid: name = #{func_org}, user = #{user_org}"
      end
      case @object
      when VirtualData
        obj = @object.dup
      else
        obj = VirtualData.new(@object)
      end
      args = args.split(",") if String === args
      args = [args] if args.is_a?(Hash)
      obj = obj.analysis(func, *args)
      return create(obj)
    end


    def plot(draw_method, opts={})
      case draw_method
      when String
        dm, user = draw_method.split(",")
        dm_org = dm
        user_org = user
        user ||= "root"
        dm = ::DrawMethod.find(:first, :conditions => ["path=?", "/usr/" << user << "/draw_methods/" << dm], :user => @user)
        unless dm
          raise "draw_method not found: name = #{dm_org}, user = #{user_org}"
        end
      when ::DrawMethod
        dm = draw_method
      else
        raise "draw_method is invalid: name = #{dm_org}, user = #{user_org}"
      end
      case @object
      when VirtualData
        obj = @object.dup
      else
        obj = VirtualData.new(@object)
      end
      if String === opts
        opts = str_to_options(opts)
      end
      obj = obj.plot(dm, opts)
      return create(obj)
    end

    def slice(*index)
      if VirtualData === @object && @object.functions.length == 0 && @object.draw_method.nil?
        if index.max < @object.original_nodes.length
          obj = @object.original_nodes[*index]
        else
          raise "[BUG] index.max==#{index.max} > size of data: #{@object.original_nodes.inspect}"
        end
      else
        obj = @object[*index]
      end
      return NumRu::GfdnaviData::Base === obj ? obj : create(obj)
    end

    def overlay
      return if self.length == 1 # DO NOTHING
      base = self[0]
      unless base.is_a?(NumRu::GfdnaviData::Image)
        raise "NumRu::GfdnaviData::Image is expected: #{obj.class}"
      end
      obj = base.get_object # VirtualData is expected
      raise "VirtualData is expected: #{obj.class}" unless obj.is_a?(VirtualData)
      for i in 1...(self.length)
        unless self[i].is_a?(NumRu::GfdnaviData::Image)
          raise "NumRu::GfdnaviData::Image is expected: #{obj.class}"
        end
        # VirtualData#overlay will call deep_clone
        obj = obj.overlay(self[i])
      end
      return NumRu::GfdnaviData::Local.create(@user, obj)
    end

  end
end
