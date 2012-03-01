require "numru/gphys"
require "register_to_db"
require "numru/gfdnavi_data/variable"

module NumRu::GfdnaviData
  class VariableLocal < NumRu::GfdnaviData::Variable

    OBJECT_CLASS = ::Variable

    attr_reader :error

    def cut(*args)
      if args.length == 0
        raise "argument is invalid"
      elsif args.length == 1
        case args[0]
        when String
          hash = nil
          ary = nil
          args[0].split(",").each{|s|
            if /\A(.+)=>(.+)\z/ =~ s
              if ary
                raise "argument is invalid"
              else
                k = $1
                v = $2
                hash ||= Hash.new
                if /\A(.+)\.\.(.+)\z/ =~ v
                  v = Float($1)..Float($2)
                else
                  v = Float(v)
                end
                hash[k] = v
              end
            else
              if hash
                raise "argument is invalid"
              else
                ary ||= ::Array.new
                ary.push Float(s)
              end
            end
          }
          args = ary || [hash]
        when Hash
        else
          raise "argument is invalid"
        end
      end
      unless @object.respond_to?(:cut)
        obj = VirtualData.new(@object)
      else
        obj = @object
      end
      return create(obj.cut(*args))
    end

    def to_gphys
      @object.to_gphys
    end


    def gphys=(gphys)
      if @new_data
        gphys = Marshal.load(gphys) if String === gphys
        if NumRu::GPhys === gphys
          @gphys = gphys
        else
          raise "gphys must be NumRu::GPhys"
        end
      else
        raise "cannot change data"
      end
    end

    def save
      if @new_data
        vname = @gphys.name
        if vname != File.basename(path)
          @gphys = @gphys.clone
          @gphys.name = vname
        end
        fname = Node.add_prefix(File.dirname(path))
        if File.exist?(fname)
          @errors = ["File already exists"]
          return false
        end
        FileUtils.makedirs(File.dirname(fname))
        file = NumRu::NetCDF.create(fname)
        NumRu::GPhys::IO.write(file, @gphys)
        file.close

        dir = ::Directory.new
        dir.path = File.dirname(path)
        dir.name = File.basename(dir.path)
        dir.owner = @user
        dir.plain_file = true
        unless dir.save
          @errors = dir.errors.full_massages
          return false
        end

        @object.size = @gphys.length
        @object.file = fname
        node = @object.node
        @gphys.att_names.each do |aname|
          register_kattr(aname, @gphys.get_att(aname), node)
        end
      end
      if @object.save
        @data_new = false
        return true
      else
        @errors = @object.errors.full_messages
        return false
      end
    end

  end
end
