require "numru/gphys"
require "numru/gfdnavi_data"
require "numru/gfdnavi_data/local"
require "no_rdb_base"
require "virtual_data"

class AnalysisColumn < ActiveRecord::ConnectionAdapters::Column

  def type_cast_code(var_name)
    case type
    when :array, :array_int, :array_float
      "VirtualData.value_to_array(#{var_name}, :#{type})"
    when :model
      "#{self.class.name}.value_to_model(#{var_name})"
    else
      super("(#{var_name}=='NULL' ? nil : #{var_name})")
    end
  end

  def self.value_to_model(value)
    return nil if value.nil? || value[:name].nil?
    if Hash === value && value[:class]
      case value[:class]
      when "user"
        return User.find_by_login(value[:name])
      when "function"
        return Function.find(:first, :conditions=>["name=?",value[:name]], :user=>value[:user])
      when "draw_method"
        return DrawMethod.find(:first, :conditions=>["name=?",value[:name]], :user=>value[:user])
      end
    else
      raise "a model is expected"
    end
  end

  def default
    Array===@default ? @default.dup : @default
  end
    

  private
  def simplified_type(type)
    case type
    when /array_int/i
      :array_int
    when /array_float/i
      :array_float
    when /array/i
      :array
    when /model/i
      :model
    else
      super(type)
    end
  end

end

class Analysis < ActiveRecord::NoRdbBase

  ACTION_TYPE = %w( draw analysis )

  common_attrs =
    [
     {:name => "variables", :default => [], :type => "array"},
     {:name => "action_type", :default => ACTION_TYPE.index("draw"), :type => "int"},
     {:name => "axes", :default => {}, :type => "hash"}
    ]
  draw_attrs = VirtualData.class_eval("@@draw_options").map{|k, v|
    v[:name] = "draw_" + k
    v
  } + [
       {:name => "draw_method", :type => "model"},
       {:name => "draw_share", :default => true, :type => "boolean"},
       {:name => "draw_keep", :default => false, :type => "boolean"}        
      ]
  function_attrs =
    [
     {:name => "function", :type => "model"},
     {:name => "function_arguments", :default => [], :type => "array",},
     {:name => "function_variables_order", :default => [], :type => "array"}
    ]

  @@minmaxs =
    {
    "action_type" => [0, ACTION_TYPE.length-1]
  }

  push_column AnalysisColumn.new("user", nil, "model", true)
  (common_attrs +
   draw_attrs +
   function_attrs
   ).each{ |hash|
    push_column AnalysisColumn.new(hash[:name], hash[:default], hash[:type], hash[:optional]==true )
  }

  if ActiveRecord::Base.connection.tables.include?("nodes")
    DrawMethod.find(:all, :user=>:all).each{|dm|
      dm.draw_method_options.each{|dma|
        push_column AnalysisColumn.new( "#{dm.name}_#{dma.name}", dma.default=="NULL"?nil : dma.default, dma.value_type.name, dma.optional)
      }
    }
  end

  def write_attribute(attr_name, value)
    case attr_name.to_s
    when "function"
      unless Function === value
        value =  Function.find(:first, :conditions=>["name=?",value], :user=>user)
      end
      value = {:class => "function", :name => value && value.name, :user => user}
    when "draw_method"
      unless DrawMethod === value
        value = DrawMethod.find(:first, :conditions=>["name=?",value], :user=>user)
      end
      value = {:class => "draw_method", :name => value && value.name, :user => user}
    when "user"
      unless User === value
        value = User.find_by_login(value)
      end
      value = {:class => "user", :name => value && value.login}
    when "draw_variables_order"
      value = variables_order(value)
    when "function_variables_order"
      value = variables_order(value)
    end

    value = nil if value == "NULL"

    super(attr_name, value)

    if (minmax = @@minmaxs[attr_name.to_s])
      val = read_attribute(attr_name)
      if val < minmax[0]
        write_attribute(attr_name, minmax[0].to_s)
      elsif val > minmax[1]
        write_attribute(attr_name, minmax[1].to_s)
      end
    end
    xa = read_attribute("draw_x_axis")
    ya = read_attribute("draw_y_axis")
    za = read_attribute("draw_z_axis")
    ad = read_attribute("draw_anim") && read_attribute("draw_anim_dim")
    case attr_name.to_s
    when "x_axis"
    when "y_axis"
    when "z_axis"
    when "anim_dim"
    when "projection"
      unless VirtualData::DRAW_PROJECTION_ITRS.include?(value.to_i)
        write_attribute("draw_projection", "1")
      end
    when "viewport"
      unless viewport.length==4
        write_attribute("draw_viewport", Analysis.columns_hash["draw_viewport"].default)
      end
    when "map_axis"
      unless map_axis.nil? || (map_axis.is_a?(Array) && map_axis.length==3)
        write_attribute("draw_map_axis", Analysis.columns_hash["draw_map_axis"].default)
      end
    when "map_radius"
      unless map_radius.nil? || map_radius.is_a?(Numeric)
        write_attribute("draw_map_radius", 
                        Analysis.columns_hash["draw_map_radius"].default)
      end
    when "map_fit"
    when "map_window"
      unless map_window.is_a?(Array) && map_window.length==4
        write_attribute("draw_map_window", 
                        Analysis.columns_hash["draw_map_window"].default)
      end
    end
  end

  def self.create_from_path(path,user=nil)
    user_model = user && User.find_by_login(user)
    analysis = Analysis.new
    analysis.user = user_model
    cut = Hash.new
    if/\/overlay\(/ =~ path
      # do nothing because current Analysis cannot reproduce overlayed diagrams
      analysis.variables = []
      analysis.axes = []
      return analysis
    end
    if /\A(.+)\/(plot|analysis)\((.+)\)(\[[-\d,]+\])?(\.\w+)?\z/ =~ path
      path = $1
      type = $2
      md = $3
      if /\A(.+);(.*)\z/ =~ md
        md = $1
        opts = $2
      else
        opts = nil
      end
      md = md.split(",")
      md[1] = "root" unless md[1] 
      case type
      when "plot"
        analysis.action_type = ACTION_TYPE.index("draw")
        unless dm = DrawMethod.find(:first, :conditions => ["path=?", "/usr/#{md[1]}/draw_methods/#{md[0]}"], :user => user_model)
          raise "draw_method is invalid"
        end
        analysis.draw_method = dm
        if opts
          ary = Array.new
          opts.split(",").each do |opt|
            if opt.include?("=")
              ary.push opt.split("=")
            else
              ary[-1][1] << "," << opt
            end
          end
          ary.each do |k,v|
            if /\Aaxes\[(.+)\]\[(.+)\]\z/ =~ k
              # only for the compatibility with the previous URL format
              cut[$1] ||= Hash.new
              cut[$1][$2] = v
              path = path.gsub(/#{k}=#{v},*/,"")
            elsif /\A([^\[]+)(\[.+\].*)\z/ =~ k
              k = $1
              kk = $2
              if analysis.respond_to?(k)
                hash = analysis.send(k)
              elsif analysis.respond_to?("draw_#{k}")
                hash = analysis.send("draw_#{k}")
              else
                raise "invalid option: #{k}"
              end
              vv = nil
              kkk = nil
              while /\A\[([^\]]+)\](.*)\z/ =~ kk
                vv = hash
                kkk = $1
                kk = $2
                hash = (vv[kkk] ||= Hash.new)
              end
              vv[kkk] = v 
            elsif analysis.respond_to?("draw_#{k}=")
              analysis.send("draw_#{k}=", v)
            elsif analysis.respond_to?("#{md[0]}_#{k}=")
              k = "#{md[0]}_#{k}"
              analysis.send("#{k}=", v)
            else
              raise "invalid option: #{k}"
            end
          end
        end
      when "analysis"
        analysis.action_type = ACTION_TYPE.index("analysis")
        unless func = Function.find(:first, :conditions => ["path=?", "/usr/#{md[1]}/functions/#{md[0]}"], :user => user_model)
          raise "function is invalid"
        end
        analysis.function = func
        if opts
          analysis.function_arguments = opts.split(",")
        end
      end
    end
    if /\A(.+)\/cut\(([^\)]+)\)(\[[-\d,]+\])?(\.\w+)?\z/ =~ path
      path = $1
      cut_opts = $2
      cut_opts.split(",").each_with_index{|c,i|
        if /(.+)=>(.+)/ =~ c # Hash
          axis = $1
          val = $2
          range = Hash.new
          if /\A(.+)\.\.(.+)\z/ =~ val
            range["min"] = $1.to_f
            range["max"] = $2.to_f
          else
            range["min"] = val.to_f
          end
          cut[axis] = range
        else
          raise "Illegal option for cut method: #{c}"
        end
      }
    end
    var = NumRu::GfdnaviData::Local.parse_path(path,user)
    ary = Array.new
    if NumRu::GfdnaviData::Array === var
      var.each{|v| ary << v}
    else
      ary[0] = var
    end
    analysis.variables = ary
    analysis.axes = cut
    return analysis
  end

  def variables
    return read_attribute("variables")
  end

  def draw_methods
    dm = read_attribute("draw_method")
    return dm if dm.nil?
    user_model = user && User.find_by_login(user)
    unless dm == DrawMethod.find(:first, :conditions=>["id=>",dm.node.id], :user=>user_model)
      raise "an invalid draw method was set"
    end
    return dm
  end

  def functions
    func = read_attribute("function")
    return func if func.nil?
    user_model = user && User.find_by_login(user)
    unless func ==  Function.find(:first, :conditions=>["id=?",func.id], :user=>user_model)
      raise "an invalid function was set"
    end
    return func
  end


  def variable_clear
    @dimensions = nil
    @common_dimensions = nil
    %w( variables axes draw_x_axis draw_y_axis draw_z_axis draw_variables_order function_variables_order ).each{|name|
      column = column_for_attribute(name)
      default = column.default
      self[name] = default
    }
  end

  def file_and_variable_names
    variables.collect do |var|
      [fname, vname]
    end
  end

  def gphyses
    variables.collect do |var|
      var.to_gphys
    end
  end

  def dimensions
    unless @dimensions
      axes = Hash.new
      @dimensions = Array.new
      self.gphyses.each{|gp|
        gp.rank.times do |i|
          ax = gp.axis(i)
          ax_name = ax.name
          if axes[ax_name]
            ind, units, ary = axes[ax_name]
            pos = ax.pos
            ary2 = pos.val
            unless pos.units == units
              begin
                factor,offset = Units.new(units).factor_and_offset(Units.new(pos.units))
                ary2 = ary*factor + offset
              rescue
              end
            end
            (ary[0]>ary[-1]) && (ary2[0]>ary2[-1]) ? keepreverse=true : keepreverse=false
            if ACTION_TYPE[action_type]=="analysis"
              cmin = [ary.min,ary2.min].max
              cmax = [ary.max,ary2.max].min
              raise "Axis '#{ax_name}' has no common coverage for selected variables" if cmin> cmax
            end
            ary += ary2.to_a
            ary.sort!.uniq!
            ary.reverse! if keepreverse
            if ACTION_TYPE[action_type]=="analysis"
              ary = NArray.to_na(ary)
              ary = ary[(ary.ge(cmin) * ary.le(cmax)).where].to_a
            end
            axes[ax_name] = [ind, units, ary]
          else
            pos = ax.pos
            axes[ax_name] = [axes.length, pos.units, pos.val.to_a]
          end
        end
        axes.each do |name, ary|
          @dimensions[ary[0]] = {:name => name, :units => ary[1], :ary => ary[2]}
        end
      }
    end
    return @dimensions
  end

  def common_dimensions
    unless @common_dimensions
      caxes = Array.new
      @common_dimensions = Array.new
      self.gphyses.each{|gp|
        axnames = gp.axnames
        caxes = ((caxes.length>0) ? caxes : axnames) & axnames
      }
      caxes.each{|cdim|
        dimensions.each_with_index{|dim,indx|
          if dim[:name] == cdim
            newdim = dim.dup
            newdim[:index] = indx
            @common_dimensions.push newdim
          end
        }
      }
    end
    return @common_dimensions
  end

  def index
    #dims = axes
    #if axes.length == 0
      dims = Hash.new
      common_dimenstions.each{|dim|
        dims[dim[:name]] = {"min"=>dim[:ary][0], "max"=>dim[:ary][-1]}
      }
    #end
    twoD = common_dimensions.length > 1
    threeD = common_dimensions.length > 2
    dim0 = common_dimensions[0][:name]
    dim1 = common_dimensions[1][:name] if twoD
    dim2 = common_dimensions[2][:name] if threeD
    unless x_axis
      xa = dim0
      xa = (twoD && y_axis==xa) ? dim1 : xa
      xa = (threeD && z_axis==xa) ? dim2 : xa
      self.x_axis = xa
    end
    if twoD && !y_axis
      ya = dim1
      ya = x_axis==ya ? dim0 : ya
      ya = (threeD && z_axis==ya) ? dim2 : ya
      self.y_axis = ya
    end
    if threeD && !z_axis
      za = dim2
      za = y_axis==za ? dim1 : za
      za = x_axis==za ? dim0 : za
      self.z_axis = za
    end
    hash = Hash.new
    dims.each do |name,range|
      if ACTION_TYPE[action_type]=="analysis" || draw_method.ndims==0 || (name==x_axis || (draw_method.ndims>1&&name==y_axis) || (draw_method.ndims>2&&name==z_axis))
        min = range["min"].to_f
        max = range["max"].to_f
        hash[name] = min==max ? min : min..max
      else
        hash[name] = range["min"].to_f
      end
    end
    return hash
  end

  def get_draw_options
    hash = Hash.new
    if draw_method
      dm = draw_method.name
      draw_method.draw_method_options.each do |opt|
        oname = opt.name
        hash[oname] = self.send("#{dm}_#{oname}")
      end
      VirtualData.draw_options.each do |opt|
        hash[opt] = self.send("draw_#{opt}")
      end
    end
    return hash
  end

  private

  def variables_order(val)
    unless Hash === val
      return nil
    end
    variables_order = Hash.new
    val.each{|k,v|
      if Hash === v
        tmp = Array.new
        v.each{|k1,v1|
          tmp[k1.to_i] = v1.to_i
        }
        variables_order[k.to_i] = tmp
      else
        return nil
      end
    }
    ary = variables_order.values.flatten
    unless ary.max == ary.length-1
      return nil
    end
    ary.length.times{|i|
      unless ary.include?(i)
        return nil
      end
    }
    return variables_order
  end

end
