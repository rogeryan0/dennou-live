require "numru/ggraph"
require "yaml"

module NumRu
  class VizShot

    @@ms_windows = false
    def self.windows
      @@ms_windows = true
    end

    @@dumpdir = './'
    def self.dumpdir=(dirname)
      @@dumpdir = dirname.sub(/([^\/])$/,'\1/')   # to end with '/'
    end

    @@basename_default = 'vizshot_dump'   ## DCL::swcget('FNAME').strip 
    def self.basename_default=(basename)
      @@basename_default = basename
    end

    @@plot_methods = Hash.new
    @@plot_methods[:plot_1d] = {:ndims=>1,:nvars=>1,:pre_defined=>true}
    @@plot_methods[:line] = {:ndims=>1,:nvars=>1,:pre_defined=>true}
    @@plot_methods[:mark] = {:ndims=>1,:nvars=>1,:pre_defined=>true}
    @@plot_methods[:tone_cont] = {:ndims=>2,:nvars=>1,:pre_defined=>true}
    @@plot_methods[:tone] = {:ndims=>2,:nvars=>1,:pre_defined=>true}
    @@plot_methods[:contour] = {:ndims=>2,:nvars=>1,:pre_defined=>true}
    @@plot_methods[:vector] = {:ndims=>2,:nvars=>2,:pre_defined=>true}
    @@plot_methods[:timeplot] = {:ndims=>3,:nvars=>1,:pre_defined=>true}
    @@plot_methods[:profile] = {:ndims=>1,:nvars=>1,:pre_defined=>true}
    @@plot_methods[:scatter] = {:ndims=>1,:nvars=>2,:pre_defined=>true}

    #<< class methods >>

    class << self
      def has_plot_method?(methodname)
	@@plot_methods.has_key?(methodname.to_sym)
      end
      def plot_methods
	@@plot_methods.keys
      end
      def ndims(methodname)
	@@plot_methods[methodname.to_sym][:ndims]
      end
      def nvars(methodname)
	@@plot_methods[methodname.to_sym][:nvars]
      end
      def pre_defined?(methodname)
	@@plot_methods[methodname.to_sym][:pre_defined]
      end

      def add_extensions(arg)
        case arg
        when Array
          return arg.collect{|hash| add_extensions(hash)}
        when Hash
          hash = arg
        else
          raise ArgumentError, "argument must be Hash"
        end
        name = hash[:name]
	hash[:pre_defined] = false
        return nil if @@plot_methods[name]
        ndims = hash[:ndims] || raise("ndims must be set")
        nvars = hash[:nvars] || raise("nvars must be set")
        script = hash[:script] || raise("script must be set")
        vars = ""
        nvars.times{|i| vars << "gphys" << (i==0 ? "" : i.to_s) << ","}
        str =<<-"EOS"
          def #{name}(#{vars} opt)
            #{script}
          end
          private :#{name}
        EOS
        class_eval(str)
        if ggraph = hash[:ggraph]
          str =<<-"EOS"
            def #{name}(#{vars} newframe=true, options=nil)
              #{ggraph}
            end
            module_function :#{name}
          EOS
          GGraph.module_eval(str)
        end
        @@plot_methods[name.to_sym] = hash
        print "registrated plot method: #{name}\n" if $VERBOSE
        return hash
      end
    end

    #<< methods >>

    def initialize(opt = nil)
      opt = opt ? opt.dup : Hash.new
      @set = Hash.new
      @set[:admin] = Hash.new
      @set[:admin][:iwidth]  = ( opt.delete(:iwidth)  || 700 )
      @set[:admin][:iheight] = ( opt.delete(:iheight) || 700 )
      @set[:admin][:iclrmap] = ( opt.delete(:iclrmap) || 1 )
      @set[:admin][:xdiv]    = ( opt.delete(:xdiv) || 1 )
      @set[:admin][:ydiv]    = ( opt.delete(:ydiv) || 1 )
      @set[:admin][:basename] = ( opt.delete(:basename) || @@basename_default)

      if opt.length > 0
	raise ArgumentError,"Unsupported option(s) #{opt.keys.inspect}" 
      end

      @plots = Array.new
    end

    def plot(arg)
      i = 0
      while(file=arg[sym = ("file"+(i==0 ? "" : (i+1).to_s)).to_sym])
        if String===file && !(/http:\/\// =~ file)
          arg[sym] = File.expand_path(file)
        end
        i += 1
      end
      @plots.push(arg) 
      @plots.length - 1      # index of the current plot
    end

    def add(vizshot)
      viz_new = self.semi_deep_clone
      vizshot.plots.each{|plot| viz_new.plot(plot)}
      viz_new
    end

    def replace_plot!(index, arg)
      @plots[index].update(arg)
    end

    def set_fig(opt)
      @set[:fig] = opt
    end

    def set_map(opt)
      @set[:map] = opt
    end

    def set_axes(opt)
      @set[:axes] = opt
    end

    def set_tone(opt)
      @set[:tone] = opt
    end

    def set_contour(opt)
      @set[:contour] = opt
    end
#------------------------------- Added at 080207
    def set_profile(opt)
      @set[:profile] = opt
    end
#---------------------------------------------

    def execute(opt=nil)
      if opt  # must be a Hash
	opt = opt.dup
	postscript = opt.delete(:postscript)
	image_dump = opt.delete(:image_dump)
	if opt.length > 0
	  raise ArgumentError,"Unsupported option(s) #{opt.keys.inspect}" 
	end
      end

      setup(postscript,image_dump)
      first = true
      @plots.each do |p| 
        exec_plot(p, first)
        first = false
      end
      finish
    end

    def dump_code(basename=nil, all_in_one=false, exec_opt=nil)
      @set[:admin][:basename] = basename if basename
      path = @@dumpdir + @set[:admin][:basename] + '.rb'
      File.open(path,'w'){|f| f.print(gen_code(all_in_one, exec_opt))}
      path
    end

    def dump_code_and_data(basename=nil, all_in_one=false, exec_opt=nil)
      @set[:admin][:basename] = basename if basename
      viz = self.dup
      data_paths = viz.cut_out_data(@set[:admin][:basename])
      code_path = viz.dump_code(basename, all_in_one, exec_opt)
      [code_path] + data_paths
    end

    def gen_code(all_in_one = false, exec_opt=nil)
      if all_in_one
        libsrc = File.open(__FILE__)
        code = ""
        while (line=libsrc.gets)
          break if /^#.*ENDOFLIB/ =~ line
          code << line
        end
        code << "####################\n"
      else
        code = 'require "numru/vizshot"'
      end
      ext_methods = Array.new
      @plots.each{|pl|
        method = pl[:method]
        pm = @@plot_methods[method.to_sym]
        unless pm[:pre_defined]
          ext_methods.push pm unless ext_methods.include?(pm)
        end
      }
      code += "\next_methods = <<'YML'\n\n#{ext_methods.to_yaml}\nYML\n\n"
      code += "\nset = <<YML\n\n#{@set.to_yaml}\nYML\n\n"
      code += "\nplots = <<YML\n\n#{@plots.to_yaml}\nYML\n\n"
      ecode = <<-EOS
        plots = YAML.load(plots)
        set = YAML.load(set)
        ext_methods = YAML.load(ext_methods)
        NumRu::VizShot.add_extensions(ext_methods)
        viz = NumRu::VizShot.new(set[:admin])
        viz.set_fig(set[:fig]) if set[:fig]
        viz.set_map(set[:map]) if set[:map]
        viz.set_axes(set[:axes]) if set[:axes]
        viz.set_contour(set[:contour]) if set[:contour]
        viz.set_tone(set[:tone]) if set[:tone]
        viz.set_profile(set[:profile]) if set[:profile]
        plots.each{|p| viz.plot(p)}
        viz.execute(#{exec_opt.inspect})
      EOS
      ecode.gsub!(/^        /,'')
      code << ecode
    end

    ###################################################
    protected

    def semi_deep_clone
      viz_new = self.class.new(@set[:admin].dup)
      viz_new.set_fig(@set[:fig]) if @set[:fig]
      viz_new.set_map(@set[:map]) if @set[:map]
      viz_new.set_axes(@set[:axes]) if @set[:axes]
      viz_new.set_contour(@set[:contour]) if @set[:contour]
      viz_new.set_tone(@set[:tone]) if @set[:tone]
      viz_new.set_profile(@set[:profile]) if @set[:profile]
      @plots.each{|plot| viz_new.plot(plot.dup) }
      return viz_new
    end

    def plots
      @plots
    end

    # for dump_code_and_data
    def cut_out_data(basename)
      newplots = Array.new
      data_paths = Array.new
      @plots.each_with_index do |pl, i|
        gphyses = get_gphyses(pl.dup)
        gphyses = [gphyses] unless gphyses.is_a?(Array)
        cut = [pl.delete(:cut)]
        slice = [pl.delete(:slice)]
        for j in 1...(gphyses.length)
          cut[j] = pl.delete(("cut"+(j+1).to_s).to_sym) || cut[0]
          slice[j] = pl.delete(("slice"+(j+1).to_s).to_sym) || slice[0]
        end
        method = pl[:method]
        gphyses.each_with_index{|gphys,j|
          if !cut[j] && !slice[j] && self.class.ndims(method) == gphys.rank
            # no slicing needed
            newplots[i] = pl
          else
            filename = basename + sprintf("_%03d"+(j==0 ? "" : "_"+(j+1).to_s),i) + '.nc'
            path = @@dumpdir + filename
            pl = pl.dup
            pl[("file"+(j==0 ? "" : (j+i).to_s)).to_sym] = filename
            case self.class.ndims(method)
            when 1
              gphys = gphys.first1D
            when 2
              gphys = gphys.first2D
            when 3
              gphys = gphys.first3D
            else
              raise "Ploting more-than-3D data is not supported"
            end
            data_paths.push(path)
            file = NetCDF.create(path)
            GPhys::IO.write(file,gphys)
            file.close
            newplots[i] = pl
          end
        }
      end
      @plots = newplots

      data_paths
    end

    ###################################################
    private

    def get_gphyses(arg)
      gphyses = Array.new
      i = 0
      while(true)
        file = arg.delete("file#{ext=(i==0 ? "" : (i+1).to_s)}".to_sym)
        var = arg.delete("var#{ext}".to_sym)
        slice  = arg.delete("slice#{ext}".to_sym)
        cut = arg.delete("cut#{ext}".to_sym)
        if gphyses.empty?
          file0 = file
          var0 = var
          slice0 = slice
          cut0 = cut
        else
          file = file0 if !file && var
          var = var0 if file && !var 
          slice ||= slice0
          cut ||= cut0
        end

        if file && var
          gphys = GPhys::IO.open(file,var)
          gphys = gphys[*slice] if slice
          case cut
          when Hash
            gphys = gphys.cut(cut)
          when Array
            gphys = gphys.cut(*cut)
          end
        else
          break
        end
        gphyses.push(gphys)
        i += 1
      end
      raise ArgumentError, "Need to specify a variable by :file and :var" if gphyses.empty?
      
      gphyses
    end

    def exec_plot(arg, first)
      arg = arg.dup
      arg[:newfrm] = first if arg[:newfrm]==nil

      method = arg.delete(:method)    # :tone_cont, :line, ..

      gphyses = get_gphyses(arg)
      gphyses = [gphyses] unless gphyses.is_a?(Array)

      if axes = arg.delete(:axes)
        gphyses = gphyses.collect{|gphys|
          ax = gphys.axnames
          iax = axes.collect{|d| ax.index(d)}
          raise("variable does not have axis #{iax.inspect}") if iax.index(nil)
          gphys.transpose(*iax)
        }
      end

      opt = arg

      pm = @@plot_methods[method.to_sym]
      unless pm
        raise "#{method.to_s} is not defined"
      end
      self.send(*(gphyses.unshift(method.to_s).push(opt)))

    end

    def setup(postscript=nil,image_dump=nil)
      DCL::swiset("iwidth",  @set[:admin][:iwidth])
      DCL::swiset("iheight", @set[:admin][:iheight])
      DCL::swiset("iclrmap", @set[:admin][:iclrmap])
      if postscript
	raise "DCL on MS Windows does not support PostScript file O" if @@ms_windows
        DCL::gropn(2)
      else
        if image_dump
	  if !@@ms_windows
	    DCL::swcset("fname", @@dumpdir + @set[:admin][:basename])
	    DCL::swlset("lwnd", false)
	    DCL::gropn(4)
	  else
	    DCL::swlset("ldump", true)
	    DCL::gropn(1)
	  end
	else
	  DCL::gropn(1)
        end
      end
      DCL::sglset("lfull", true)
      DCL::sgpset("isub", 96)
      DCL::gllset("lmiss", true)

      if ( @set[:admin][:xdiv] > 1 || @set[:admin][:ydiv] > 1)
	DCL::sldiv('y',@set[:admin][:xdiv],@set[:admin][:ydiv])
      end

      GGraph::set_fig(@set[:fig]) if @set[:fig]
      GGraph::set_map(@set[:map]) if @set[:map]
      GGraph::set_axes(@set[:axes]) if @set[:axes]
      GGraph::set_contour(@set[:contour]) if @set[:contour]
      GGraph::set_tone(@set[:tone]) if @set[:tone]
      GGraph::set_profile(@set[:profile]) if @set[:profile]
    end
 
    def finish
      DCL::grcls
    end

    def tone_cont(gphys, opt)
      newfrm = opt.delete(:newfrm)
      contour = opt.delete(:contour)
      tone = opt.delete(:tone)
      color_bar = opt.delete(:color_bar)
      color_bar_options = opt.delete(:color_bar_options)
      if color_bar_options && !color_bar_options.is_a?(Hash)
	raise ":color_bar_options must be a Hash"
      end

      if @set[:fig] && (itr=@set[:fig]['itr']) && itr>=10
	lon = gphys.coord(0)
	lat = gphys.coord(1)
	if lon.min >= 120 && lon.max <= 150 && lat.min >= 20 && lat.max <=50
	  GGraph::set_map({'coast_japan'=>true})
	else
	  GGraph::set_map({'coast_world'=>true})
	end
	if itr==31
	  DCL.sgpset('lclip',true) 
	  GGraph.set_map('vpt_boundary'=>3)
	end
      end

      tone = contour = true if !tone && !contour   # should be what is meant

# TENTATIVE WORKAROUD
=begin
      if tone && contour
        GGraph::tone_and_contour(gphys, newfrm, opt)
      elsif tone 
        opt.delete('coloring') # ad-hoc workaround
        GGraph::tone(gphys, newfrm, opt)
      elsif contour
        GGraph::contour(gphys, newfrm, opt)
      end
      newfrm = false
=end
      if tone
        opt.delete('coloring') # ad-hoc workaround
        opt['nlev'] = 99 if(opt['nlev'].is_a?(Numeric) && (opt['nlev'] > 99))
        GGraph::tone(gphys, newfrm, opt)
        newfrm = false
      end
      if contour
        opt.delete('tonc') # ad-hoc workaround
        opt['nlev'] = 50 if(opt['nlev'].is_a?(Numeric) && (opt['nlev'] > 50))
        GGraph::contour(gphys, newfrm, opt)
        newfrm = false
      end
      if tone && color_bar
        cbopt = color_bar_options || Hash.new
        cbopt['log'] = true if opt['log']
        if opt['nlev'] && opt['nlev'] > 20
          cbopt['tickintv'] ||= opt['nlev'].to_i / 10
        end
        GGraph::color_bar(cbopt)
      end
      newfrm
    end

    def vector(gpx, gpy, opt)
      newfrm = opt.delete(:newfrm)
      GGraph::set_unit_vect_options('vyuoff'=>-0.1,'vxuoff'=>0.07)
      GGraph::vector(gpx, gpy, newfrm, opt)
    end

    def plot_1d(gphys, opt)
      newfrm = opt.delete(:newfrm)
      contour = opt.delete(:contour) 
      line = opt.delete(:line) 
      mark = opt.delete(:mark)
      profile = opt.delete(:profile)

      line = mark = true if !line && !mark   # should be what is meant

      lineindex = opt.delete(:lineindex)
      linetype = opt.delete(:linetype)
      markerindex = opt.delete(:markerindex)
      markertype = opt.delete(:markertype)
      markersize = opt.delete(:markersize)

      if line 
        lineopt = opt.dup
        lineopt["index"] = lineindex if lineindex
        lineopt["type"] = linetype if linetype
        GGraph::line(gphys, newfrm, lineopt)
        newfrm = false
      end
      if mark
        markeropt = opt.dup
        markeropt["index"] = markerindex if markerindex
        markeropt["type"] = markertype if markertype
        markeropt["size"] = markersize if markersize
        GGraph::mark(gphys, newfrm, markeropt)
        newfrm = false
      end
      if profile
        GGraph::profile(gphys, newfrm, opt)
        newfrm = false
      end

      newfrm
    end

    def tone(gphys, opt)
      tone_cont(gphys, opt.update(:tone=>true,:contour=>false))
    end
    def contour(gphys, opt)
      tone_cont(gphys, opt.update(:tone=>false,:contour=>true))
    end
    def line(gphys, opt)
      plot_1d(gphys, opt.update(:line=>true,:mark=>false))
    end
    def mark(gphys, opt)
      plot_1d(gphys, opt.update(:line=>false,:mark=>true))
    end
## Added at 080207 ##
    def profile(gphys, opt)
      plot_1d(gphys, opt.update(:profile=>true,:line=>false,:mark=>false))
    end
######################
## Added at 080131 ##
    def timeplot(gphys, opt)
      color_bar = opt.delete(:color_bar)
      color_bar_options = opt.delete(:color_bar_options)
      if color_bar_options && !color_bar_options.is_a?(Hash)
	raise ":color_bar_options must be a Hash"
      end
      newfrm = opt.delete(:newfrm)
      GGraph::timeplot(gphys, newfrm, opt)
      newfrm = false

      cbopt = color_bar_options || Hash.new
      cbopt['log'] = true if opt['log']
      GGraph::color_bar(cbopt)
      newfrm
    end
######################

  end
end

########## ENDOFLIB ##########

if __FILE__ == $0

  if ARGV.length == 0
    raise <<-EOS

      USAGE: % ruby #{__FILE__} menu [dumpdir]
      where menu=0,1,2,..

    EOS
  end

  test_menu = ARGV[0].to_i
  NumRu::VizShot.dumpdir = ARGV[1] if ARGV[1]

  path_samples = "../../../../data/samples/reanalysis/ncep"
  t_file = File.join(path_samples, "T.jan.nc")
  uv_file = File.join(path_samples, "UV.jan.nc")

  case test_menu

  when 0
    extdef = YAML.load( DATA.read )
    NumRu::VizShot.add_extensions( extdef )  # New one registered but not used

    NumRu::VizShot.plot_methods.each do | method|
      print "#{method}\t#{NumRu::VizShot.ndims(method)}\t#{NumRu::VizShot.nvars(method)}\t#{NumRu::VizShot.pre_defined?(method)}\n"
    end
    p NumRu::VizShot.has_plot_method?(:tone)

    viz = NumRu::VizShot.new(:iwidth=>700,:iheight=>500)
    viz.set_fig('viewport'=>[0.15,0.85,0.2,0.55])
    viz.set_tone('tonc'=>true)
    viz.plot( :method => :tone_cont,
	      :file => t_file, 
	      :var=> "T", :cut => {'level'=>400},
	      :color_bar=>true, "title"=>"T and [U,V]" )
    viz.plot( :method => :vector,
	      :file  => uv_file,
	      :var=> "U",  :var2=> "V" , :cut => {'level'=>400},
	      "unit_vect" => true )  
	      # You can give GGraph options using string keys.

    paths = viz.dump_code_and_data(nil, true)
    print "\n!!! Code and data stored in #{paths.inspect} !!!\n\n"

    viz.execute

  when 100   # executable after executing case 0

    system('mv vizshot_dump_000.nc tmp.nc') || raise("mv failed. Call after 0")
    viz = NumRu::VizShot.new(:iwidth=>700,:iheight=>500)
    viz.set_fig('itr'=>10, 'viewport'=>[0.15,0.85,0.2,0.55])
    viz.plot( :method => :tone_cont,
	      :file => "tmp.nc", 
	      :var=> "T", 
	      :color_bar=>true)

    path = viz.dump_code(nil, true, :image_dump => true)
    print "\n!!! Code written in #{path} !!!\n\n"

    viz.execute(:image_dump => true)

  when 2

    viz = NumRu::VizShot.new
    viz.set_fig('itr'=>31, 'viewport'=>[0.15,0.85,0.15,0.85])
    viz.plot( :method => :tone_cont,
	      :file => t_file, 
	      :var=> "T", :cut => {'lat'=>10..90,'level'=>600},
	      :color_bar=>true)

    path = viz.dump_code
    print "\n!!! Code written in #{path} !!!\n\n"

    viz.execute

  when 3

    viz = NumRu::VizShot.new
    viz.plot( :method => :plot_1d,
	      :file => t_file, 
	      :var=> "T", :cut => {'lon'=>135}, "index"=>2,
	      :line => true, :mark => true )

    path = viz.dump_code
    print "\n!!! Code written in #{path} !!!\n\n"

    viz.execute

  when 4

    # you can use :method=>:line instead of :method=>:plot_1d && :line=>true

    viz = NumRu::VizShot.new
    viz.plot( :method => :line,
	      :file => t_file, 
	      :var=> "T", :cut => {'lon'=>135}, "index"=>2, "type"=>2 )

    path = viz.dump_code
    print "\n!!! Code written in #{path} !!!\n\n"

    viz.execute

  when 5

    viz = NumRu::VizShot.new(:iwidth=>700,:iheight=>500,:xdiv=>2,:ydiv=>2)
    viz.set_fig('viewport'=>[0.15,0.85,0.2,0.55])
    [1000,600,400,200].each do |lev|
      viz.plot( :method => :tone_cont, :newfrm=>true,
	        :file => t_file, 
	        :var=> "T", :cut => {'level'=>lev},
	        :color_bar=>true)
    end

    path = viz.dump_code
    print "\n!!! Code written in #{path} !!!\n\n"

    viz.execute

  when 6

    viz = NumRu::VizShot.new(:iwidth=>700,:iheight=>500,:xdiv=>2,:ydiv=>2)
    viz.set_fig('viewport'=>[0.2,0.8,0.27,0.6])
    viz.plot( :method => :tone_cont, :newfrm=>true,
	      :file => t_file, 
	      :var=> "T", :cut => {'level'=>1000},
	     :color_bar=>true, :color_bar_options=>{'vlength'=>0.25})
    viz.plot( :method => :tone_cont, :newfrm=>true,
	      :file => t_file, 
	      :var=> "T", :cut => {'level'=>1000},
	      :color_bar=>true, :color_bar_options=>{'labelintv'=>1})
    viz.plot( :method => :tone_cont, :newfrm=>true,
	      :file => t_file, 
	      :var=> "T", :cut => {'level'=>1000},
	      :color_bar=>true, 
              :color_bar_options=>{'landscape'=>true, 'labelintv'=>1,
               'vlength'=>0.6})

    path = viz.dump_code
    print "\n!!! Code written in #{path} !!!\n\n"

    viz.execute

  when 7
    # test user-defined method #

    extdef = YAML.load( DATA.read )
    NumRu::VizShot.add_extensions( extdef )

    viz = NumRu::VizShot.new(:iwidth=>500,:iheight=>500)
    viz.plot( :method => :scatter,
	      :file  => uv_file,
	      :var=> "U",  :var2=> "V" , :cut => {'lat'=>0, 'level'=>400} )  

    paths = viz.dump_code_and_data
    print "\n!!! Code and data stored in #{paths.inspect} !!!\n\n"

    viz.execute

  when 8

    # overlay contour on tone on a map projection
    viz = NumRu::VizShot.new
    viz.set_fig('itr'=>10, 'viewport'=>[0.15,0.85,0.15,0.85])
    viz.plot( :method => :tone,
	      :file => uv_file, 
	      :var=> "U", :cut => {'level'=>1000},
	      :color_bar=>true)
    viz.plot( :method => :contour,
	      :file  => uv_file,
	      :var=> "V", :cut => {'level'=>1000} )  

    path = viz.dump_code
    print "\n!!! Code written in #{path} !!!\n\n"

    viz.execute

  when 9

    # overlay vector on tone_cont on a map projection
    viz = NumRu::VizShot.new
    viz.set_fig('itr'=>10, 'viewport'=>[0.15,0.85,0.15,0.85])
    viz.plot( :method => :tone_cont,
	      :file => t_file, 
	      :var=> "T", :cut => {'level'=>1000},
	      :color_bar=>true)
    viz.plot( :method => :vector,
	      :file  => uv_file,
	      :var=> "U",  :var2=> "V" , :cut => {'level'=>1000} )  

    path = viz.dump_code
    print "\n!!! Code written in #{path} !!!\n\n"

    viz.execute

  end

end


__END__
### The following part is an YAML to define an extension method ###
:name: scatter
:ndims: 1
:nvars: 2
:script: |
  newfrm = opt.delete(:newfrm)
  GGraph::scatter(gphys, gphys1, newfrm, opt)

:ggraph: |
  gropn_1_if_not_yet
  unless defined?(@@scater_options)
    @@scater_options = Misc::KeywordOptAutoHelp.new(
      ['title', nil, 'Title of the figure'],
      ['annotate', true, 'if false, do not put texts on the right margin even when newframe==true'],
      ['index', 1, 'mark index'],
      ['type', 2, 'mark type'],
      ['size', 0.01, 'marks size']
    )
  end
  opts = @@scater_options.interpret(options)
  gphys = gphys.first1D.copy
  gphys1 = gphys1.first1D.copy
  len = gphys.length
  unless len == gphys1.length
    raise ArgumentError, "length of gphys and gphys1 do not agree with each other"
  end
  x = gphys.data
  y = gphys1.data
  if newframe
    fig(x,y)
    axes(x,y)
    title( opts['title'] )
    annotate(gphys.lost_axes) if opts['annotate']
  end
  DCL::uumrkz(x.val, y.val, opts['type'], opts['index'], opts['size'])
