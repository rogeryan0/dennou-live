require "numru/vizshot"

module NumRu
  class VizShotGfdnavi < VizShot

    def ==(other)
      return false unless @set == other.instance_variable_get("@set")
      self.instance_variable_get("@plots") == other.instance_variable_get("@plots")
    end

    def get_variables
      unless @variables
        @variables = Array.new
        @plots.each{|pl|
          pl[:variables].each{|path|
            if /^(temporary\:.+\.nc)\/(.+)$/ =~ path
              fname = $1
              vname = $2
              var = Variable.new
              var.file = fname
              var.path = File.join(fname,vname)
            else
              var = Variable.find(:first, :conditions => ["path=?",path],:user=>:all)
            end
            if var
              @variables.push var unless @variables.include?(var)
            else
              raise "[BUG] variable is invalid (#{path})"
            end
          }
        }
      end
      return @variables
    end

    def to_yaml_properties
      props = instance_variables.sort
      props.delete("@variables")
      return props
    end
    
    def get_plots(num=nil)
      if num
        @plots[num].dup
      else
        @plots.collect{|pl| pl.dup}
      end
    end

    def get_size
      [ @set[:admin][:iwidth], @set[:admin][:iheight] ]
    end

    def get_itr
      @set[:fig]['itr']
    end

    def get_viewport
      @set[:fig]['viewport'].join(',')
    end

    # redefined
    def cut_out_data(basename)
      newplots = Array.new
      data_paths = Array.new
      cut0 = slice0 = nil
      @plots.each_with_index{|pl, i|
        gphyses = get_gphyses(pl.dup)
        gphyses = [gphyses] unless gphyses.is_a?(Array)
        method = pl[:method]
        script = pl.delete(:script)
        ndims = self.class.ndims(method)
        gphyses.each_with_index{|gphys,j|
          cut = pl.delete(("cut" << (ext=(j==0 ? "" : (j+1).to_s))).to_sym)
          slice = pl.delete(("slice" << ext).to_sym)
          if j==0
            cut0 = cut
            slice0 = slice
          else
            cut ||= cut0
            slice ||= slice0
          end
          unless script.nil? && cut.nil? || slice.nil? && ndims == gphys.rank
            filename = basename + sprintf("_%03d" + (j==0 ? "" : "_"+(j+1).to_s),i) + '.nc'
            data_paths.push _output_data(gphys,filename,ndims)
            pl = pl.dup
            pl[("file" << ext).to_sym] = filename
            pl[("var" << ext).to_sym] = gphys.name
          end
        }
      }
      return data_paths
    end

    protected

    # redefined
    def get_gphyses(arg)
      vars = arg.delete(:variables)
      unless Array === vars
        raise "variables is valid"
      end
      gphyses = Array.new
      slice = [arg.delete(:slice)]
      cut = [arg.delete(:cut)]
      for i in 1...(vars.length)
        next if i==0
        slice[i] = arg.delete(("slice" << (i+1).to_s).to_sym) || slice[0]
        cut[i] = arg.delete(("cut" << (i+1).to_s).to_sym) || cut[0]
      end
      vars.each_with_index{|path,i|
        if /^(temporary\:.+\.nc)\/(.+)$/ =~ path
          fname = $1
          vname = $2
          v = Variable.new
          v.file = fname
          v.path = File.join(fname,vname)
        else
          v = Variable.find(:first,:conditions=>["path=?",path],:user=>:all)
        end
        if v
          gphys = v.to_gphys
          if i==0 || i==1
            gphys = gphys[*(slice[i])] if slice[i]
            case cut[i]
            when Hash
              h = Hash.new
              gphys.axnames.each do |ax|
                if v = cut[i][ax]
                  h[ax] = v
                end
              end
              if h.any?
                gphys = gphys.cut(h)
              end
            when Array
              gphys = gphys.cut(*(cut[i]))
            end
          end
          gphyses.push gphys
        else
          raise "variable is invalid (#{path})"
        end
      }
      if src = arg.delete(:script)
        safe = $SAFE
        lambda {
#          $SAFE = 2
          eval src
        }.call
        $SAFE = safe
        for i in 1..gphyses.length
          next if i==0
          slice[i] = arg.delete(("slice" << (i+1).to_s).to_sym) || slice[0]
          cut[i] = arg.delete(("cut" << (i+1).to_s).to_sym) || cut[0]
        end
        gphyses1 = []
        gphyses.each_with_index{|gphys,i|
          gphys = gphys[*(slice[i])] if slice[i]
          case cut[i]
          when Hash
            h = Hash.new
            gphys.axnames.each do |ax|
              if v = cut[i][ax]
                h[ax] = v
              end
            end
            if h.any?
              gphys = gphys.cut(h)
            end
            gphyses1.push(gphys)
          when Array
            gphyses1.push(gphys.cut(*(cut[i])))
          else
            gphyses1.push(gphys)
          end
        }
        gphyses = gphyses1
      end
      gphyses = [gphyses] if GPhys === gphyses
      #raise "wrong number of gphyses" if gphyses.length > 2
      return gphyses
    end

    def _output_data(gphys,filename,ndims)
      path = @@dumpdir + filename
      case ndims
      when 1
        gphys = gphys.first1D
      when 2
        gphys = gphys.first2D
      when 3
        gphys = gphys.first3D
      else
        raise "Ploting more-than-3D data is not supported"
      end
      file = NetCDF.create(path)
      GPhys::IO.write(file,gphys)
      file.close
      return path
    end

  end
end


#.*ENDOFLIB


if ActiveRecord::Base.connection.tables.include?("nodes")
DrawMethod.find(:all, :user=>:all).each{|dm|
  if dm.name && dm.script && dm.script!="NULL"
    hash = {
      :name => dm.name,
      :ndims => dm.ndims,
      :nvars => dm.nvars,
      :script => dm.script
    }
    hash[:ggraph] = dm.ggraph if dm.ggraph && dm.ggraph!="NULL"
    NumRu::VizShotGfdnavi.add_extensions(hash)
  end
}
end
