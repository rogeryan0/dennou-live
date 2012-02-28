require "numru/vizshot_gfdnavi"
require "numru/gfdnavi_data"
require "numru/gfdnavi_data/local"
require "file_gfdnavi"
require "narray_gfdnavi"
require "gphys_gfdnavi"
require "register_to_db"

class VirtualData
  ge544 = (NumRu::DCL::DCLVERSION.split(".").map{|v| v.to_i} <=> [5, 4, 4]) # CHECK DCL VERSION
  DRAW_PROJECTION = \
    [{:itr => 1, :description => "Rectangular uniform coordinate", :map_proj => false},
     {:itr => 2, :description => "Semi-logarithmic coordinate (y axis)", :map_proj => false},
     {:itr => 3, :description => "Semi-logarithmic coordinate (x axis)", :map_proj => false},
     {:itr => 4, :description => "Logarithmic coordinate", :map_proj => false},
     {:itr => 5, :description => "Polar coordinate", :map_proj => false},
     {:itr => 6, :description => "Bipolar coordinate", :map_proj => false},
     # 7 => "elliptic coordinate",
     {:itr => 10, :description => "Equidistant cylindrical projection", :map_proj => true,
       :map_fit =>    {:enable => true, :default => true}, 
       :map_axis =>   {:enable => true, :default => [0, 0, 0]}, 
       :map_window => {:enable => true, :default => [-180, 180, -90, 90]},
       :map_radius => {:enable => false}},
     {:itr => 11, :description => "Mercator projection", :map_proj => true,
       :map_fit =>    {:enable => true, :default => true},
       :map_axis =>   {:enable => true, :default => [0, 0, 0]},
       :map_window => {:enable => true, :default => [-180, 180, -75, 75]},
       :map_radius => {:enable => false}},
     {:itr => 12, :description => "Mollweide projection", :map_proj => true,
       :map_fit =>    {:enable => false}, 
       :map_axis =>   {:enable => true, :default => [0, 0, 0]}, 
       :map_window => {:enable => true, :default => [-180, 180, -90, 90]},
       :map_radius => {:enable => false}},
     {:itr => 13, :description => "Hammer projection", :map_proj => true,
       :map_fit =>    {:enable => false}, 
       :map_axis =>   {:enable => true, :default => [0, 0, 0]}, 
       :map_window => {:enable => true, :default => [-180, 180, -90, 90]},
       :map_radius => {:enable => false}},
     {:itr => 14, :description => "Eckert VI projection", :map_proj => true,
       :map_fit =>    {:enable => false}, 
       :map_axis =>   {:enable => true, :default => [0, 0, 0]}, 
       :map_window => {:enable => true, :default => [-180, 180, -90, 90]},
       :map_radius => {:enable => false}},
     {:itr => 15, :description => "Kitada elliptic projection", :map_proj => true,
       :map_fit =>    {:enable => false}, 
       :map_axis =>   {:enable => true, :default => [0, 0, 0]}, 
       :map_window => {:enable => true, :default => [-180, 180, -90, 90]},
       :map_radius => {:enable => false}},
     {:itr => 16, :description => "Miller projection", :map_proj => true,
       :map_fit =>    {:enable => false}, 
       :map_axis =>   {:enable => true, :default => [0, 0, 0]}, 
       :map_window => {:enable => true, :default => [-180, 180, -90, 90]},
       :map_radius => {:enable => false}},
     {:itr => 17, :description => "Robinson projection", :map_proj => true,
       :map_fit =>    {:enable => false}, 
       :map_axis =>   {:enable => true, :default => [0, 0, 0]}, 
       :map_window => {:enable => true, :default => [-180, 180, -90, 90]},
       :map_radius => {:enable => false}},
     {:itr => 18, :description => "Sanson/sinusoidal projection", :map_proj => true,
       :map_fit =>    {:enable => false}, 
       :map_axis =>   {:enable => true, :default => [0, 0, 0]}, 
       :map_window => {:enable => true, :default => [-180, 180, -90, 90]},
       :map_radius => {:enable => false}},
     {:itr => 19, :description => "Van der Grinten projection", :map_proj => true,
       :map_fit =>    {:enable => false}, 
       :map_axis =>   {:enable => true, :default => [0, 0, 0]}, 
       :map_window => {:enable => true, :default => [-180, 180, -90, 90]},
       :map_radius => {:enable => false}},
     {:itr => 20, :description => "Equidistant conical projection", :map_proj => true,
       :map_fit =>    {:enable => false}, 
       :map_axis =>   {:enable => true, :default => [0, 90, 0]}, 
       :map_window => {:enable => false},
       :map_radius => {:enable => true, :default => 90}},
     {:itr => 21, :description => "Lambert equal-area conical projection", :map_proj => true,
       :map_fit =>    {:enable => false}, 
       :map_axis =>   {:enable => true, :default => [0, 90, 0]}, 
       :map_window => {:enable => false},
       :map_radius => {:enable => true, :default => 90}},
     {:itr => 22, :description => "Lambert conformal conical projection", :map_proj => true,
       :map_fit =>    {:enable => false}, 
       :map_axis =>   {:enable => true, :default => [0, 90, 0]}, 
       :map_window => {:enable => false},
       :map_radius => {:enable => true, :default => 90}},
     {:itr => 23, :description => "Bonne projection", :map_proj => true,
       :map_fit =>    {:enable => false}, 
       :map_axis =>   {:enable => true, :default => [0, 90, 0]}, 
       :map_window => {:enable => false},
       :map_radius => {:enable => true, :default => 90}},
     {:itr => 24, :description => "Polyconic projection", :map_proj => true,
       :map_fit =>    {:enable => false},
       :map_axis =>   {:enable => true, :default => [0, 90, 0]}, 
       :map_window => {:enable => false},
       :map_radius => {:enable => true, :default => 90}},
     {:itr => 30, :description => "Orthographic projection", :map_proj => true,
       :map_fit =>    {:enable => false}, 
       :map_axis =>   {:enable => true, :default => [0, 0, 0]}, 
       :map_window => {:enable => false},
       :map_radius => {:enable => true, :default => 90}},
     {:itr => 31, :description => "Polar stereo projection", :map_proj => true,
       :map_fit =>    {:enable => false}, 
       :map_axis =>   {:enable => true, :default => [0, 90, 0]}, 
       :map_window => {:enable => false},
       :map_radius => {:enable => true, :default => 90}},
     {:itr => 32, :description => "Azimuthal equidistant projection", :map_proj => true,
       :map_fit =>    {:enable => false}, 
       :map_axis =>   {:enable => true, :default => [0, 90, 0]}, 
       :map_window => {:enable => false},
       :map_radius => {:enable => true, :default => 90}},
     {:itr => 33, :description => "Lambert azimuthal equal-area projection", :map_proj => true,
       :map_fit =>    {:enable => false}, 
       :map_axis =>   {:enable => true, :default => [0, 90, 0]}, 
       :map_window => {:enable => false},
       :map_radius => {:enable => true, :default => 90}},
     {:itr => 34, :description => "Gnomonic projection", :map_proj => true,
       :map_fit =>    {:enable => false}, 
       :map_axis =>   {:enable => true, :default => [0, 90, 0]}, 
       :map_window => {:enable => false},
       :map_radius => {:enable => true, :default => 90}}
    ].reject{|prj| (ge544 < 0) && ([16, 17, 18, 19, 24, 34].include?(prj[:itr]))}
  DRAW_PROJECTION_ITRS = DRAW_PROJECTION.map{|prj| prj[:itr]}

  def itr_to_projection(itr)
    DRAW_PROJECTION.each{|proj| return proj if proj[:itr] == itr}
    return nil
  end

  def get_map_proj_default(itr, key)
    proj = itr_to_projection(itr)
    proj.has_key?(key) ? proj[key][:default] : nil
  end

  # GET COLOR MAP NUMBER AND DESCRIPTIONS
  color_maps = Hash.new
  NumRu::DCL::swcmll # READ THE COLOR-MAP LIST BEFORE GETTING DESCRIPTIONS
  NumRu::DCL::swqcmn.times{|i| color_maps[i+1] = NumRu::DCL::swqcmd(i+1)}
  COLOR_MAPS = color_maps # DEFINE AS A CONSTANT

  # AVAILABLE DIAGRAM SIZES AND NAMES IN GUI
  DRAW_SIZE = [{:size => [700, 700], :name => "L"},
               {:size => [550, 550], :name => "M"},
               {:size => [400, 400], :name => "S"},
               {:size => [250, 250], :name => "XS"}]
  
  @@draw_options = {
    "x_axis" => {:type => "string"},
    "y_axis" => {:type => "string"},
    "z_axis" => {:type => "string"},
    "projection" => {:default => 1, :type => "int"},
    "pileup" => {:default => false, :type => "boolean"},
    "size" => {:default => DRAW_SIZE[2][:size], :type => "array_int"},
    "colormap" => {:default => 1, :type => "int"},
    "anim" => {:default => false, :type => "boolean", :optional => true},
    "anim_dim" => {:type => "string", :optional => true},
    "viewport" => {:default => [0.2, 0.8, 0.2, 0.8], :type => "array_float", :optional => true},
    "window" => {:default => [nil, nil, nil, nil], :type => "array_float", :optional => true},
    "map" => {:default => false, :type => "boolean", :optional => true},
    "map_axis" => {:default => nil, :type => "array_float", :optional => true},
    "map_radius" => {:default => nil, :type => "float", :optional => true},
    "map_fit" => {:default => true, :type => "boolean", :optional => true},
    "map_window" => {:default => nil, :type => "array_float", :optional => true},
    "variables_order" => {:default => {}, :type => "hash"},
  }

  attr_accessor :original_nodes, :functions, :draw_method, :draw_slice, :overlayed_plots
  attr_reader :errors, :num_plots
  

  def self.value_to_array(value, val_type)
    if Array === value
      ary = value
    elsif Hash === value
      ary = Array.new
      value.each{|k,v|
        ary[k.to_i] = v
      }
    elsif String === value
      if value.empty?
        return nil
      else
        ary = value.split(/,/) # blank space with zero length will be discarded
      end
    else
      ary = [value]
    end
    case val_type
    when :array_int, "array_int"
      ary.collect!{|v| (v.nil? || ((String === v) && (v.strip == ""))) ? nil : v.to_i} # blank space with non-zero length is treated as nil
    when :array_float, "array_float"
      ary.collect!{|v| (v.nil? || ((String === v) && (v.strip == ""))) ? nil : v.to_f} # blank space with non-zero length is treated as nil
    end
    return ary
  end


  def self.draw_options
    @@draw_options.keys
  end

  def initialize(nodes)
    nodes = [nodes] unless Array === nodes
    nodes_new = Array.new
    nodes.each{|node|
      node = node.entity if Node === node
      case node
      when Directory
        nodes_new += node.variable_nodes
      when NumRu::GfdnaviData::ArrayLocal
        nodes_new += node.get_object
      else
        nodes_new.push node
      end
    }
    @original_nodes = nodes_new
    @num_vars = @original_nodes.length
    @functions = Array.new
    @draw_method = nil
    @draw_slice = nil
    @num_plots = nil
    @names = @original_nodes.collect{|node| node.name}
    @overlayed_plots = Array.new
  end

  def name
    if @draw_method
      return nil
    end
    if @num_vars > 1
      return nil
    end
    return @names[0]
  end

  def analysis!(func, *args)
    unless Function === func
      raise "function is invalid"
    end
    if @draw_method
      raise "cannot apply function after drawing"
    end
    unless @num_vars%func.nvars == 0
      raise "wrong number of variables (#{@num_vars}%#{func.nvars} != 0)"
    end
    unless((args.length <= (al = func.function_arguments.length))||
           ((al == 1)&&(/array/ =~ func.function_arguments[0].value_type.name)))
      raise "wrong number of arguments (#{args.length} for #{al})"
    end
    args = [args] if al == 1
    @functions.push( {:type => :func, :func => func, :args => args} )
    nvars = func.nvars
    nt = @num_vars/nvars
    outs = func.function_outputs
    @num_vars = outs.length * nt
    subs = outs.collect{|out| out.subscript}
    names = Array.new
    nt.times do |n|
      on = @names[n*nvars...(n+1)*nvars].join
      names += subs.collect{|sub| "#{on}_#{sub}"}
    end
    @names = names
    @dimensions = @gphys = @gphys_dummy = nil # CLEAR CACHE
    return self
  end

  def analysis(func, *args)
    return self.deep_clone.analysis!(func, *args)
  end

  def slice!(*ind)
    if ind.length == 0
      raise "index is invalid"
    end
    max = -1
    ind.each{|i|
      case i
      when Integer
        max = i>max ? i : max
      when Range
        max = i.max>max ? i.max : max
      else
        raise "indices must be integer or range: #{ind.join(",")}"
      end
    }
    if max >= @num_vars
      raise "index out of range"
    end
    if @draw_method
      if @draw_slice
        raise "slice already exists"
      end
      @draw_slice = ind
    else
      @functions.push( {:type => :index, :index => ind} )
      @names = @names[*ind]
      @names = [@names] if String === @names
    end
    @num_vars = ind.length
    @dimensions = @gphys = @gphys_dummy = nil # CLEAR CACHE
    return self
  end

  def slice(*ind)
    return self.deep_clone.slice!(*ind)
  end
  alias :[] :slice

  def cut!(*val)
    if val.length == 0
      raise "value is invalid: nil"
    elsif val.length == 1 && val[0].kind_of?(Hash)
      val_new = val[0].dup
      if @num_vars == 1
        #gp = self.to_gphys ###
        #axnames = gp.axnames ###
        dims = self.dimensions #!!!
        axnames = dims.map{|ax| ax["name"]} #!!!
        val_new.dup.each{|k,v|
          if !axnames.include?(k)
            val_new.delete(k)
          elsif v.is_a?(Range)
            #axis = gp.axis(k).pos.val ###
            axis = dims[axnames.index(k)]["ary"] #!!!
            be = [v.begin.to_f, v.end.to_f]
            be = [be[1], be[0]] if be[0] > be[1]
            if ((axis.min.to_f - be[0]).abs <= (be[0] * 1.0e-5).abs) && ((axis.max.to_f - be[1]).abs <= (be[1] * 1.0e-5).abs)
              val_new.delete(k)
            end
          end
        }
        return self if val_new.empty?
      end
      val_new.each{|k,v|
        case v
        when Numeric, Range
        else
          raise "value is invalid: #{v.inspect}"
        end
      }
    else
      val.each{|i|
        case i
        when Numeric, Range, TrueClass, FalseClass
        else
          raise "value is invalid: #{i}"
        end
      }
      val_new = val.dup
    end
    @functions.push( {:type => :cut, :value => val_new} )
    @dimensions = @gphys = @gphys_dummy = nil # CLEAR CACHE
    return self
  end

  def cut(*val)
    return self.deep_clone.cut!(*val)
  end

  def plot!(dm,opts=Hash.new)
    unless DrawMethod === dm
      raise "draw method is invalid"
    end
    unless @num_vars%dm.nvars == 0
      raise "wrong number of variables (#{@num_vars}%#{dm.nvars}!=0)"
    end
    opts_new = Hash.new
    dm.draw_method_options.each{|dmop|
      on = dmop.name
      val = opts.delete(on)
      if val.nil? || ((String === val) && (val.empty? || val == "nil"))
        val =  dmop.optional ? nil : dmop.default
      end
      unless val.nil?
        opts_new[on] = param_cast(val, dmop.value_type.name)
      end
    }
    @@draw_options.each{|on,v|
      # When val == false, that parameter must be sent to draw methods.
      # Only nil and blank can be removed.
      unless (val = opts.delete(on)).nil? || ((String === val) && (val.empty? || val == "nil"))
        opts_new[on] = param_cast(val, v[:type])
      end
    }
    unless opts.empty?
      raise "wrong options were specified: #{opts.keys.join(", ")}"
    end
    @draw_method = [dm,opts_new]
    @num_vars /= dm.nvars
    @num_plots = @num_vars
    if opts_new["pileup"]
      @num_vars = 1
    end
    return self
  end

  def plot(dm, opts=Hash.new)
    return self.deep_clone.plot!(dm, opts)
  end

  def overlay!(plot)
    plot = plot[0] if plot.is_a?(NumRu::GfdnaviData::ArrayLocal)
    plot = plot.get_object if plot.is_a?(NumRu::GfdnaviData::ImageLocal)
    unless plot.is_a?(VirtualData) && (plot.type == "draw")
      raise ArgumentError, "argument is not compatible with overlay: #{plot.inspect}"
    end
    @overlayed_plots ||= Array.new
    @overlayed_plots.push(plot)
    return self
  end

  def overlay(plot)
    return self.deep_clone.overlay!(plot)
  end

  def type
    @draw_method ? "draw" : @functions.length > 0 ? "analysis" : "node"
  end

  def num_vars
    @num_vars
  end
  alias :length :num_vars

  def array?
    return true if @num_vars > 1
    if @draw_method
      return @draw_slice.nil? || @draw_slice.length != 1 || !(Integer === @draw_slice[-1])
    else
      funcs = @functions.dup
      funcs.delete_if{|f| f[:type] == :cut}
      if funcs.empty?
        if @original_nodes.length != 1
          return true
        else
          case (on = @original_nodes[0])
          when NumRu::GfdnaviData::Array
            return true
          when VirtualData
            return on.array?
          when Variable, NumRu::GfdnaviData::Variable
            return false
          else
            raise "invalid type: #{on.class}"
          end
        end
      else
        f = funcs[-1]
        if f[:type] == :index && f[:index].length == 1 && Integer === f[:index][0]
          return false
        end
        return true
      end
    end
  end

  def get(prefix)
    unless files = get_cache # CREATE PNG/NC IF NOT CACHED
      if @draw_method
        suffix = "png"
      else
        suffix = "nc"
      end
      fname = File.temp_name(prefix,"_001.#{suffix}")
      dirname = File.dirname(fname)
      basename = File.basename(fname,"_001.#{suffix}")
      res = get_proc(basename)
      if res[0]
        res = execute(res[1],dirname)
        if res[0]
          files = Dir[File.join(dirname,basename)+"_*.#{suffix}"].sort
          put_cache(files)
        else
          msg = res[1]
          files = nil
        end
      else
        files = nil
        msg = res[1]
      end
    end
    if files && @draw_slice
      files_org = files
      files = Array.new
      @draw_slice.each do |i|
        if i >= files_org.length
          msg = "draw_slice is out of index: #{i} for #{files_org.length}"
          files = nil
          break
        end
        files.push files_org[i]
      end
    end

    return [files, msg]
  end

  def save_as(path, user)
    unless user.kind_of?(User)
      raise "user must be specified"
    end
    files, msg = get(GFDNAVI_WORK_PATH)
    files = Array(files) if files.kind_of?(String)
    path = Array(path) if files.length == 1 && path.kind_of?(String)

    unless path.kind_of?(Array)
      raise "path must be Array of length #{files.length}"
    end

    if @draw_method
      # image
    else
      # variable
      if path.length != 1 || files.length != 1
        raise "BUG"
      end
    end

    prefix = File.join(GFDNAVI_USER_PATH, user.login)
    @errors = Array.new
    files.each_with_index do |file,i|
      if user.super_user? && /\A\// =~ path[i]
        spath = Node.add_prefix(path[i])
      else
        spath = File.join(prefix, path[i])
        path[i] = File.join("/usr", path[i])
      end

      FileUtils.makedirs(File.dirname(spath))
      FileUtils.copy(files[i], spath)
      parent = Directory.find(:first, :conditions => ["path=?",File.dirname(path[i])], :user=>:all)
      unless parent
        ppath = File.dirname(path[i])
        parent = Directory.new
        parent.path = ppath
        parent.name = File.basename(ppath)
        parent.owner = user
        unless parent.save
          @errors += parent.errors.full_messages
          next
        end
      end
      if @draw_method
        meta = Hash.new
        begin
          register_image(spath, self.path, meta, Time.now, File.size(spath), parent, false, false, false)
        rescue
          @errors << $!.message
        end
      else
        begin
          register_file(spath, nil, parent, Time.now, File.size(spath), NumRu::GPhys::IO.file2file_class(spath), false, false, false, false)
        rescue
          @errors << $!.message
        end
      end
    end
    !@errors.any?
  end

  def to_gphys(dummy=false)
    return @gphys if @gphys # GET CACHE
    res = get_proc(nil, true, dummy)
    if res[0]
      res = execute(res[1])
      if res[0]
        obj = Marshal.load(res[2])
        if obj.is_a?(Array) && obj.length == 1
          obj = obj[0]
        end
        unless obj.kind_of?(NumRu::GPhys)
          raise "BUG (#{obj.class} is not a NumRu::GPhys)"
        end
        @gphys = obj unless dummy # PUT CACHE
        return obj
      end
    end
    return nil
  end

  def png
    if @draw_method
      prefix = File.join(GFDNAVI_DIAGRAM_CACHE_PATH)
      files, msg = get(prefix) # the variable "files" is always an Array
      case files
      when Array
        if files.length == 1
          return File.read(files[0])
        else
          return files.collect{|file| File.read(file)}
        end
      when String # maybe nonsense
        return File.read(files)
      else
        raise "BUG: #{msg}, #{files.inspect}"
      end
    end
    return nil
  end
  alias :to_png :png

  def dimensions
    return @dimensions if @dimensions
    dims = Array.new
    g = get_grid
    g.rank.times{|i|
      crd = g.axis(i).pos
      dims.push({"name" => crd.name,
                  "ary" => crd.val,
                  "units" => crd.get_att("units")})
    }
    @dimensions = dims
  end

  def path
    return @path if @path
    str = @original_nodes.collect{|on| on.path}.join(",")
    str = "/[" << str << "]" if @original_nodes.length > 1
    @functions.each_with_index{|f,i|
      case f[:type]
      when :func
        /\A\/usr\/([^\/]+)\/functions\/(.+)\z/ =~ f[:func].path
        fname = $2
        fname << ","+$1 unless $1 == "root"
        args = f[:args] ? ";" << f[:args].join(",") : ""
        str << "/analysis(" << fname << args << ")"
      when :index
        if i==0
          ary = Array.new
          f[:index].each do |idx|
            ary.push @original_nodes[idx]
          end
          str = ary.collect{|on| on.path}.join(",")
        else
          str << "[" << f[:index].join(',') << "]"
        end
      when :cut
        a = f[:value]
        a = a.collect{|k,v| "#{k}=>#{v}"} if a.kind_of?(Hash)
        str << "/cut(" << a.join(',') << ")"
      else
        raise "invalid type"
      end
    }
    if @draw_method
      /\A\/usr\/([^\/]+)\/draw_methods\/(.+)\z/ =~ @draw_method[0].path
      dname = $2
      dname << "," << $1 unless $1 == "root"
      opts = @draw_method[1]
      unless opts && Hash === opts && !opts.empty?
        sopt =  ""
      else
        sopt =  ";" << NumRu::GfdnaviData.hash_to_str(opts)
      end
      str << "/plot(" << dname << sopt << ")"
    end
    if @draw_slice
      str << "[" << @draw_slice.join(",") << "]"
    end
    if @overlayed_plots.length > 0
      str = "/[" << @overlayed_plots.map{|obj| obj.path}.unshift(str).join(",") << "]/overlay()"
    end
    @path = str
    return str
  end

  def to_xml(arg={})
    if @draw_method
      ary = png
      if Array === ary
        flag = true
      else
        flag = false
        ary = [ary]
      end
      ary.each{|str|
        img = Base64.encode64(str)
        hash = {
          "content-type" => "image/png",
          "content-length" => img.length,
          "content-transfer-encoding" => "Base64",
          "content" => img
        }
        ary.push hash
      }
      if flag
        return ary.to_xml(:root => "images")
      else
        return ary[0].to_xml(:root => "image")
      end
    else
      raise "under construction"
    end
  end

  def to_hash(opts={})
    uri_prefix = opts[:uri_prefix]
    hash = Hash.new
    hash["path"] = path
    if @draw_method
      if array?
        @num_vars.times do |i|
          hash[i] =  {"url" => "#{File.join(uri_prefix,"data",path)}[#{i}]"}
        end
        hash["type"] = "array_image"
        hash["length"] = @num_vars
        hash["slice"] = {
          "url" => File.join(uri_prefix, "data", "#{path}[{index}]"),
          "index" => "{index}"
        }
        return hash
      else # not an array => image
#        str = png
=begin
        hash = {
          "content-type" => "image/png",
          "content-length" => str.length,
          "content" => str,
          "path" => "#{path}[0]"
        }
=end
        hash["type"] = "image"
        return hash
      end
    else # no @draw_method
      if array?
        @num_vars.times do |i|
          hash[i] = {"url" => "#{File.join(uri_prefix,"data",path)}[#{i}]"}
        end
        hash["type"] = "array_variable"
        hash["length"] = @num_vars
        hash["slice"] = {
          "url" => File.join(uri_prefix, "data", "#{path}[{index}]"),
          "index" => "{index}"
        }
      else
        hash["type"] = "variable"
      end
      hash["cut"] = {
        "url" => File.join(uri_prefix, "data", path, "cut({arguments})"),
        "arguments" => "{arguments}"
      }
      hash["plot"] = {
        "url" => File.join(uri_prefix, "data", path, "plot({draw_method_name};{draw_method_options})"),
        "draw_method_name" => "{draw_method_name}",
        "draw_method_options" => "{draw_method_options}"
      }
      hash["analysis"] = {
        "url" => File.join(uri_prefix, "data", path, "analysis({function_name};{function_arguments})"),
        "function_name" => "{function_name}",
        "function_arguments" => "{function_arguments}"
      }
      return hash
    end
    return nil
  end

  def to_rb(opts={})
    uri_prefix = opts[:uri_prefix]
    minimal = opts[:minimal]
    suffix = opts[:suffix]
    code = ""
    unless minimal
      code << <<-EOF
require 'numru/gfdnavi_data'
include NumRu

      EOF
    end
    get_path = Proc.new do |on, only_one, suff|
      case on # an item in @original_nodes
      when Variable
        str = "GfdnaviData.open(\"#{File.join(uri_prefix, "data", on.path)}\")"
        str = "data#{suff} = #{str}" if minimal
      else
        str = ""
        str << "data#{suff} = begin \n" if minimal
        str << indent_code(on.to_rb(opts.dup.update(:minimal=>true, :suffix=>suff)), 2)
        str << "end \n" if minimal
      end
      str
    end
    if @original_nodes.length == 1
      code << get_path.call(@original_nodes[0],true,suffix) << "\n"
    else
      a = Array.new
      @original_nodes.each_with_index do |on,i|
        suff = suffix.to_s + "_" + i.to_s
        code << get_path.call(on, false, suff) << "\n"
        a.push "data#{suff}"
      end
      str = a.join(", ")
      code << "data#{suffix} = GfdnaviData::Array[#{str}] \n"
    end


    @functions.each{|f|
      case f[:type]
      when :func
        /\A\/usr\/([^\/]+)\/functions\/(.+)\z/ =~ f[:func].path
        fname = $2
        fname << "," << $1 unless $1 == "root"
        args = f[:args] ? ",'#{f[:args].join("','")}'" : ""
        code << "data#{suffix} = data#{suffix}.analysis('#{fname}'#{args})\n"
      when :index
        code << "data#{suffix} = data#{suffix}[#{f[:index].join(',')}]\n"
      when :cut
        a = f[:value]
        a = a.collect{|k,v| "'#{k}'=>#{v}"} if a.kind_of?(Hash)
        code << "data#{suffix} = data#{suffix}.cut(#{a.join(',')})\n"
      else
        raise "invalid type"
      end
    }
    if @draw_method
      /\A\/usr\/([^\/]+)\/draw_methods\/(.+)\z/ =~ @draw_method[0].path
      dname = $2
      dname << "," << $1 unless $1 == "root"
      opts = @draw_method[1]
      unless opts && Hash === opts && !opts.empty?
        sopt =  ""
      else
        sopt = ", opts"
        code << "\nopts = Hash[\n"
        opts.sort.each{|k,v|
          code << "  '#{k}' => #{v.inspect},\n"
        }
        code << "]\n"
      end
      code << "data#{suffix} = data#{suffix}.plot('#{dname}'#{sopt})\n"
    end

    if @draw_slice
      code << "data#{suffix} = data#{suffix}[#{@draw_slice.join(",")}]\n"
    end

    if @overlayed_plots.length > 0
      code << "plots = [data] \n"
      @overlayed_plots.each_with_index{|ovrly, i|
        code << "\n# Overlayed plot #{i}\n\n"
        code << "def overlayed_plot_#{i}\n"
        code << indent_code(ovrly.to_rb(:uri_prefix => uri_prefix, :minimal => true, :suffix => suffix), 2)
        code << "end\n"
        code << "plots.push overlayed_plot_#{i} \n"
      }
      code << "data = GfdnaviData::Array[*plots].overlay \n"
    end

    unless minimal
      code << "\n\n=begin\n"
      if @num_vars == 1
        code << "data = [data]\n"
      end
      if @draw_method
        code << <<EOF
# The following loop, if uncommented out, will save images 
# in PNG file(s) named gfdnavi_000.png, gfdnavi_001.png,..
# in the current directory.
data.each_with_index do |plot,i|
  path = "gfdnavi_%03d.png"%i
  File.open(path, "wb"){|f| f.write(plot.to_png) }
  print "Saved image in file ", path, "\\n"
end
EOF
      else
        code << <<EOF
# The following loop, if uncommented out, will save data
# in a NetCDF file named gfdnavi.nc in the current directory.
require "numru/gphys"
path = "gfdnavi.nc"
file = NumRu::NetCDF.create(path)
print "Saving data in file ", path, " ...\\n"
data.each do |dat|
  NumRu::GPhys::IO.write(file, dat.to_gphys)
end
file.close
print "...Saved.\\n"
EOF
      end
      code << "=end\n"
    end
    return code
  end

  def indent_code(code, indent = 2)
    code.split("\n").map{|l| " " * indent + l}.join("\n") << "\n"
  end
  private :indent_code

  # get all Variables from VirtualData object.
  def virtual_nodes
    NumRu::GfdnaviData::Local.get_variable_nodes(@original_nodes)
  end

  def get_grid
    return @gphys.instance_variable_get(:@grid) if @gphys # GET CACHE
    return @gphys_dummy.instance_variable_get(:@grid) if @gphys_dummy # GET CACHE
    vars = Array.new
    script = gen_code(nil, vars, 0, true)
    gphyses = vars.collect{|node|
      node = node.entity if Node === node
      node = node.get_object if NumRu::GfdnaviData::VariableLocal === node
      case node
      when Variable
        gphys = node.to_gphys(true) # create a GPhysDummy when the arg is true
        grid = gphys.instance_variable_get(:@grid)
      when VirtualData
        grid = node.get_grid
      else
        raise "not supported"
      end
      NumRu::GPhysDummy.new(grid, NumRu::VArrayDummy.new)
    }
=begin
    Thread.new{
      $SAFE = 2
      begin
        eval script
      rescue => e
        Thread.main.raise e
      end
    }.join
=end
    flag, msg, out = execute(Proc.new{eval_script(script, binding); gphyses})
    if flag
      gphyses =  Marshal.load(out)
    else
      raise msg
    end
    gphyses = [gphyses] unless gphyses.is_a?(Array)
    unless gphyses.length == 1
      raise "number of gphyses is not one"
    end
    @gphys_dummy = gphyses[0] # PUT CACHE
    return gphyses[0].instance_variable_get(:@grid)
  end

  def get_vizshot_proc(basename, flag=false, dummy=false, slave=false)
    get_proc(basename, flag, dummy, slave) if @draw_method
  end

  protected

  def deep_clone
    vd = VirtualData.new(@original_nodes)
    vd.instance_variable_set(:@num_vars, @num_vars)
    vd.instance_variable_set(:@num_plots, @num_plots)
    vd.instance_variable_set(:@functions, @functions.dup)
    vd.instance_variable_set(:@draw_method, @draw_method)
    vd.instance_variable_set(:@draw_slice, @draw_slice)
    vd.instance_variable_set(:@names, @names.dup)
    vd.instance_variable_set(:@overlayed_plots, @overlayed_plots.dup)
    vd.instance_variable_set(:@dimensions, @dimensions.dup) if @dimensions
    vd.instance_variable_set(:@gphys, @gphys.dup) if @gphys
    vd.instance_variable_set(:@gphys_dummy, @gphys_dummy.dup) if @gphys_dummy
    return vd
  end

  private

  def eval_script(script, bind)
    begin
      eval script, bind, "script"
    rescue
      # add line number to the left of each line
      script.split("\n").each_with_index do |line, n|
        STDERR.print "%3d: "%(n+1) << line << "\n"
      end
      raise $!
    end
  end


  def param_cast(val, type)
    type_org = type
    type = type.to_sym if type.is_a?(String)
    case type
    when :int
      return val.to_i
    when :float
      return val.to_f
    when :boolean
      return val == "1" || val == "t" || val == "true" || val == true
    when :string, :hash
      return val
    else
      if /\Aarray/ =~ type_org
        return self.class.value_to_array(val, type)
      else
        raise "invalid type: #{type} #{val}"
      end
    end
  end


  def get_cache
    pa = path.sub(/\[\d+\]\Z/,"")
    dc = DiagramCache.find(:first, :conditions => ["path=?",pa])
    if dc
      return YAML.load(dc.files_yaml)
    else
      return nil
    end
  end

  def put_cache(files)
    pa = path.sub(/\[\d+\]\Z/,"")
    dc = DiagramCache.new(:path => pa, :files_yaml => files.to_yaml)
    dc.save!
    return dc
  end

=begin
  def get_grid
    vars = Array.new
    script = gen_code(nil, vars, 0, true)
    gphyses = vars.collect{|node|
      node = node.entity if Node === node
      node = node.get_object if NumRu::GfdnaviData::VariableLocal === node
      case node
      when Variable
        gphys = node.to_gphys
        grid = gphys.instance_variable_get(:@grid)
      when VirtualData
        grid = node.get_grid
      else
        raise "not supported"
      end
      NumRu::GPhysDummy.new(grid, NumRu::VArrayDummy.new)
    }
    flag, msg, out = execute(Proc.new{eval script; gphyses})
    if flag
      gphyses =  Marshal.load(out)
    else
      raise msg
    end
    gphyses = [gphyses] unless gphyses.is_a?(Array)
    unless gphyses.length == 1
      raise "number of gphyes is not one"
    end
    return gphyses[0].instance_variable_get(:@grid)
  end
=end

  def check_opts_array(ary, len, msg)
    unless (Array === ary) && (ary.length == len)
      raise ArgumentError, "option #{msg} must be an Array of length #{len}: #{ary.inspect}"
    end
  end

  def get_proc(basename, flag=false, dummy=false, slave=false)
    if @draw_method && !flag
      dm, opts = @draw_method
      opts = opts.dup
      vars = Array.new
      script = gen_code(dm.nvars,vars)
      plot = Hash.new
      plot[:method] = dm.vizshot_method.to_sym

      # PARSE OPTIONS ##########################
      size = opts.delete("size") || @@draw_options["size"][:default]
      check_opts_array(size, 2, "size")
      size = size.collect{|c| c.to_i}

      colormap = opts.delete("colormap") || @@draw_options["colormap"][:default]
      unless COLOR_MAPS[colormap]
        raise ArgumentError, "option colormap is invalid: #{colormap}"
      end

      itr = (opts.delete("projection") || @@draw_options["projection"][:default]).to_i
      unless DRAW_PROJECTION_ITRS.include?(itr)
        raise ArgumentError, "option projection is invalid: #{itr}"
      end

      if(viewport = opts.delete("viewport") || @@draw_options["viewport"][:default])
        check_opts_array(viewport, 4, "viewport")
        viewport = viewport.collect{|c| c.to_f}
        unless viewport.min >= 0 && viewport.max <= 1 && viewport[0] < viewport[1] && viewport[2] < viewport[3]
          raise ArgumentError, "option viewport is invalid: #{viewport.inspect}"
        end
      end

      if(window = opts.delete("window") || @@draw_options["window"][:default])
        check_opts_array(window, 4, "window")
        window = window.collect{|c| c.nil? ? nil : c.to_f}
      end

      if(get_map_proj_default(itr, :map_axis))
        map_axis = opts.delete("map_axis") || get_map_proj_default(itr, :map_axis)
        check_opts_array(map_axis, 3, "map_axis")
        3.times{|i|
          map_axis[i] ||= get_map_proj_default(itr, :map_axis)[i]
          map_axis[i] = map_axis[i].to_f
        }
      end

      map_radius = opts.delete("map_radius") || get_map_proj_default(itr, :map_radius)

      map_fit = opts.delete("map_fit")
      map_fit = get_map_proj_default(itr, :map_fit) if map_fit.nil? # map_fit is boolean

      if(get_map_proj_default(itr, :map_window))
        map_window = opts.delete("map_window") || get_map_proj_default(itr, :map_window)
        check_opts_array(map_window, 4, "map_window")
        4.times{|i|
          map_window[i] ||= get_map_proj_default(itr, :map_window)[i]
          map_window[i] = map_window[i].to_f
        }
      end
      
      if y_axis = opts.delete("y_axis") # NOT 1D-PLOT
        x_axis = opts.delete("x_axis")
        z_axis = opts.delete("z_axis")
      end
      vo = nil
      dm.nvars > 1 && (vo = opts.delete("variables_order"))
      vo = vo.sort.collect{|val| val[1]} if vo.is_a?(Hash) # TENTATIVE WORKAROUND
      vo = [vo] if vo && !(vo.is_a?(Array) && vo[0].is_a?(Array))
      # END PARSE OPTIONS ######################

      dm.draw_method_options.each{|dmop|
        unless (val = opts.delete(key = dmop.name)).nil?
          if dmop.parser == "vizshot"
            plot.update(key.to_sym => val)
          else
            plot.update(key => val)
          end
        end
      }
      proc = Proc.new{
        viz = NumRu::VizShotGfdnavi.new(:iwidth => size[0], 
                                        :iheight => size[1],
                                        :iclrmap => colormap,
                                        :basename => basename)
        unless slave
          viz.set_fig("itr" => itr, 
                      "viewport" => viewport, 
                      "window" => window, 
                      'map_axis' => map_axis,
                      'map_radius' => map_radius,
                      'map_fit' => map_fit,
                      'map_window' => map_window) 
        end
        viz.set_tone("auto" => (itr != 5 && itr <= 10)) 
        @overlayed_plots.each{|ovrly|
          if ovrly.num_plots != 1 # for the moment
            raise "number of plots to be overlayed is not one."
          end
        }
        @num_plots.times{|i|
          plot[:variables] = vars[i].collect{|var| var.path}
          plot[:script] = script + "gphyses = gphyses[#{dm.nvars*i},#{dm.nvars}] if gphyses.is_a?(Array)\n"
          if vo
            ga = vo[i].collect{|j| "gphyses[#{j}]"}.join(", ")
            plot[:script] << "gphyses = [#{ga}]\n"
          end
=begin
          if dm.ndims == 2 && y_axis
            plot[:script] << <<EOL
gphyses = gphyses.collect do |g|
  g = g.cut(#{cut.inspect})
  ax = g.axnames
  g.transpose(ax.index("#{x_axis}"), ax.index("#{y_axis}"))
end
EOL
            cut = nil
          end
          plot[:cut] = cut if cut
=end
          case dm.ndims
          when 2
            plot[:axes] = [x_axis, y_axis] if x_axis && y_axis
          when 3
            plot[:axes] = [x_axis, y_axis, z_axis] if x_axis && y_axis && z_axis
          end
          pileup = opts["pileup"]
          pileup = (pileup==true || pileup==1)
          plot[:newfrm] = (i==0 || pileup==false) && !slave
          viz.plot(plot.dup)
          @overlayed_plots.each{|ovrly|
            res = ovrly.get_vizshot_proc(basename, flag, dummy, true)
            viz = viz.add(res[1].call) if res[0]
          }
        }
        if slave
          viz
        else
          viz.execute(:image_dump => true)
          nil
        end
      }
    else
      vars = Array.new
      script = gen_code(nil, vars)
      proc = Proc.new{
        NumRu::GPhys::read_size_limit_2 = GPHYS_READ_SIZE_LIMIT_2
        NumRu::GPhys::read_size_limit_1 = GPHYS_READ_SIZE_LIMIT_1

        gphyses = vars.collect{|var|
          NumRu::GPhys::IO.open(var.fname,var.vname)
        }
        if dummy
          gphyses.collect{|gp| 
            NumRu::GPhysDummy.new(gp.instance_variable_get(:@grid), NumRu::VArrayDummy.new)
          }
        end
        Thread.new{
#          $SAFE = 2
          eval_script(script, binding)
        }.join

        unless flag
          ofname = basename+"_001.nc"
          file = NumRu::NetCDF.create(ofname)
          begin
            $SAFE = 2
            gphyses = [gphyses] unless gphyses.kind_of?(Array)
            gphyses.each{|gphys|
              NumRu::GPhys::IO.write(file,gphys)
            }
          ensure
            file.close
          end
          nil
        else
          gphyses.dup
        end
      }
    end

    return true, proc

  end # get_proc


  def gen_code(nvars, vars, id=0, dummy=false)
    if id == 0 && nvars
      vars_org = vars
      vars = Array.new
    end
    script = "gphyses#{id} = Array.new\n"
    @original_nodes.each_with_index{|node,i|
      case node
      when Variable
        script << "gphyses#{id}.push gphyses[#{vars.length}]\n"
        vars.push node
      when VirtualData
        id2 = id*10+i
        script << node.gen_code(var,id2)
        script << "gphyses#{id} += proc#{id2}.call\n"
      when NumRu::GfdnaviData::VariableLocal
        if dummy
          script << "gphyses#{id}.push gphyses[#{vars.length}]\n"
          vars.push node.get_object
        else
          if node.user
            script << "gphyses#{id}.push NumRu::GfdnaviData::Local.parse_path('#{node.path}', User.find(:first,:conditions=>['login=?','#{node.user.login}'])).to_gphys\n"
          else
            script << "gphyses#{id}.push NumRu::GfdnaviData::Local.parse_path('#{node.path}').to_gphys\n"
          end
        end
      when NumRu::GfdnaviData::VariableRemote
        script << "gphyses#{id}.push NumRu::GfdnaviData::Remote.parse_url('#{node.url}', '#{node.user}', '#{node.password}').to_gphys\n"
      else
        raise "[BUG] unsuported data class"
      end
      if id == 0
        if nvars
          if vars.length <= nvars
            vars_org.push vars
            vars = Array.new
          else
            raise "[BUG] vars.length is invalid"
          end
        end
      end
    }
    script << "proc#{id} = Proc.new{\n"
    @functions.each{|func|
      case func[:type]
      when :func
        argv = func[:args] || Array.new
        func = func[:func]
        nvars = func.nvars
        fargs = func.function_arguments
        ary = Array.new
        nvars.times{|i| ary << "gphys#{i}"}
        args ||= Array.new
        fargs.each_with_index{|fa,i|
          ary << "arg#{i}"
          args[i] = (argv[i] || YAML.load(fa.default)).inspect
        }
        script << <<"EOF"
    $SAFE = 2
    new_gphyses = Array.new
    proc = Proc.new{|#{ary.join(",")}|
       #{func.script}
    }
    gphyses#{id} = [gphyses#{id}] unless gphyses#{id}.kind_of?(Array)
    (gphyses#{id}.length/#{nvars}).times{|i|
       new_gphyses += proc.call(#{s=Array.new;nvars.times{|ii|s.push("gphyses"+id.to_s+"[i*#{nvars}+#{ii}]")};args.each{|arg|s.push arg};s.join(",")})
    }
    gphyses#{id} = new_gphyses
EOF
      when :index
        script << "    gphyses#{id} = gphyses#{id}[#{func[:index].join(",")}]\n"
      when :cut
        a = func[:value]
        a = a.collect{|k,v| "'#{k}'=>#{v}"} if a.kind_of?(Hash)
        script << <<EOF
    if gphyses#{id}.kind_of?(Array)
      gphyses#{id}.collect!{|gphys| gphys.cut(#{a.join(",")})}
    else
      gphyses#{id} = gphyses#{id}.cut(#{a.join(",")})
    end
EOF
      end
    }
    script << <<"EOF"
       gphyses#{id}
     }
EOF
    if id==0
      script << "gphyses = proc#{id}.call\n"
    end
    return script
  end


  def execute(proc, dir=nil, nice=19)
    proc1 = Proc.new {
      Dir.chdir(dir) if dir
      obj = proc.call
      begin
        Marshal.dump(obj)
      rescue TypeError
        if obj.is_a?(NumRu::GPhys)
          Marshal.dump(obj.copy) # not NArray nor NetCDF
        end
      end
    }
    case Config::CONFIG["host_os"]
    when /linux/, /darwin/
      flag, msg, err, out = execute_fork(proc1, nice)
    when /win32/, /cygwin/
      flag, msg, err, out = execute_thread(proc1)
    else
      raise "not supported"
    end
    if msg
      msg += err
#      msg = msg.delete_if{|line| (/^\s*from / =~ line) || /\.\.\. \d+ levels\.\.\./ =~ line}.collect{|line| line.sub(/[\/\w\.]*\.rb:\d*:in /, "") }
      msg = msg.join("")
    end
    unless flag
      raise msg
    end
    return [flag,msg,out]
  end

  def execute_thread(proc)
    pwd = Dir.pwd
    begin
      t = Thread.new{
        proc.call
      }.join
      flag = true
      msg = nil
      err = []
      out = t.value
    rescue => e
      flag = false
      msg = ["execution failed: #{e.to_s}\n"]
      bt =  e.backtrace
      err = ["#{bt[0]}: #{e.to_s} (#{e.class.name})"]
      bt[1..-1].each{|m|
        err.push "\tfrom #{m}"
      }
      out = nil
    end
    Dir.chdir(pwd)
    return [flag, msg, err, out]
  end

  def execute_fork(proc,nice=19)
    flag = false
    rr,rw = IO.pipe
    er,ew = IO.pipe
    pid = fork {
      rr.close
      er.close
      STDERR.reopen(ew)
      begin
        Process.setpriority(Process::PRIO_PROCESS, 0, nice)
      rescue Errno::EACCES
      end
      begin
        out = proc.call
        rw.print out
      ensure
        rw.close
        ew.close
      end
      exit 0
    }
    rw.close
    ew.close
    out = ""
    while (buffer = rr.read(1024))
      out << buffer
    end
    begin
      pid, status = Process.wait2(pid)
    rescue Errno::ECHILD
    end
    ActiveRecord::Base.connection.reconnect!
    case
    when status.nil?
      flag = true
      msg = nil
    when status.signaled?
      msg = ["Killed by signal #{status.termsig}\n"]
    when status.exited?
      if (es = status.exitstatus) == 0
        flag = true
        msg = nil
      else
        msg = ["error occured with status #{es}\n"]
      end
    else
      msg = ["stoped with unknown status (#{status.to_i})\n"]
    end
    err = er.readlines
    rr.close
    er.close
    $stderr.print err.join("") if err.length > 0
    return [flag, msg, err, out]
  end

end

