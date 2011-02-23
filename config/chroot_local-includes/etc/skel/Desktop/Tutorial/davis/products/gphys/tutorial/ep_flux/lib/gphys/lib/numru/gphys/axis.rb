require "numru/gphys/varray"
require "numru/gphys/varraynetcdf"
require "numru/gphys/varraycomposite"

module NumRu

   class Axis

      def initialize(cell=false,bare_index=false,name=nil)
	 @init_fin = false         # true/false (true if initializatn finished)
	 @name = name              # String
	 @pos = nil                # VArray (to set it can be deferred if
                                   # @cell to support mother axes)
	 @cell = cell              # true/false
	 @cell_center = nil        # VArray (defined if @cell)
         @cell_bounds = nil        # VArray (defined if @cell)
         @bare_index = bare_index  # true/false(if true @cell is meaningless)
         @aux = nil                # Hash of VArray (auxiliary quantities)
      end

      def inspect
	 "<axis pos=#{@pos.inspect}>"
      end

      def name=(nm)
	 @name=nm
      end
      def name
	 @name || "noname"
      end

      def cell?
	 @cell
      end
      def cell_center?
	 @cell && @pos.equal?(@cell_center)
      end
      def cell_bounds?
	 @cell && @pos.equal?(@cell_bounds)
      end
      def bare_index?
	 @bare_index
      end

      def flatten
	 # return VArrays contained in a flat array
	 out = Array.new
	 out.push(@pos) if @pos
	 out.push(@cell_center) if @cell_center && !cell_center?
	 out.push(@cell_bounds) if @cell_bounds && !cell_bounds?
	 if @aux
	    @aux.each{|k,v|
	       out.push(v)
	    }
	 end
	 out
      end

      def copy
	 # deep clone onto memory
	 out = Axis.new(cell?, bare_index?, name)
	 if cell?
	    out.set_cell( @cell_center.copy, @cell_bounds.copy )
	    if cell_center?
	       out.set_pos_to_center
	    elsif cell_bounds?
	       out.set_pos_to_bounds
	    end
	 else
	    out.set_pos( @pos.copy )
	 end
	 if @aux
	    @aux.each{|k,v|
	       out.set_aux(k, v.copy)
	    }
	 end
	 out
      end

      def pos=(pos)
	 if !@cell
	    if ! pos.is_a?(VArray)
	       raise ArgumentError,"arg not a VArray: #{pos.class}"  
	    end
	    if pos.rank != 1
	      raise ArgumentError,"rank of #{pos.name} (#{pos.rank}) is not 1" 
	    end

	    @pos=pos
	    @init_fin = true
	    if ! @name
	       @name = pos.name
	    end
	 else
	    raise "This method is not available for a cell axis. "+
	          "Use set_pos_to_center or set_pos_to_bounds instead."
	 end
	 pos
      end

      def set_pos(pos)
	 self.pos= pos
	 @name = pos.name
	 self
      end

      def pos
	 raise "pos has not been set" if !@pos
	 @pos
      end
      def cell_center
	 @cell_center
      end
      def cell_bounds
	 @cell_bounds
      end

      def length
	 if @pos
	    @pos.length
	 else
	    raise "length is not determined until pos is set"
	 end
      end

      def set_cell(center, bounds, name=nil)
	 # it is the user's obligation to ensure that center and bounds
         # have the same units etc.

	 # < error check >

	 if ! @cell; raise "method not available for a non-cell axis"; end

	 if ! center.is_a?(VArray)
	    raise ArgumentError,"1st arg not a VArray: #{center.class}"  
	 end
	 if center.rank != 1
	    raise ArgumentError,"center: rank of #{center.name} (#{center.rank}) is not 1" 
	 end

	 if ! bounds.is_a?(VArray)
	    raise ArgumentError,"2nd arg not a VArray: #{bounds.class}"  
	 end
	 if bounds.rank != 1
	    raise ArgumentError,"bounds: rank of #{bounds.name} (#{bounds.rank}) is not 1" 
	 end

	 if( center.length != bounds.length-1 )
	    raise "center.length != bounds.length-1"
	 end

	 # < do the job >

	 @cell_center = center
	 @cell_bounds = bounds
	 if name
	    @name=name
	 end
	 @init_fin = true       # To set @pos is deferred at this moment.
                                # use set_pos_to_(bounds|center) to make 
                                # the object fully available
         self
      end

      def set_aux(name,vary)
	 if !name.is_a?(String) 
	    raise ArgumentError,"1nd arg: not a String"
	 end

	 if ! vary.is_a?(VArray)
	    raise ArgumentError,"2nd arg not a VArray: #{vary.class}"  
	 end
	 if vary.rank != 1
	    raise ArgumentError,"rank of #{vary.name} (#{vary.rank}) is not 1" 
	 end

	 if !@aux; @aux = Hash.new; end

	 @aux[name] = vary
      end
      def get_aux(name)
	 @aux[name]
      end
      def aux_names
	 @aux ? @aux.keys : []
      end

      def to_gphys(datavary1d=nil)
	# To form a 1-dimensional GPhys
	# (Dependent on GPhys&Grid, unlike other methods Axis,
        # so the test program is in gphys.rb)
	# Arguments
	#   * datavary1d (nil or VArray): 1D VArray with the same
        #     size as the axis. If omitted, @pos (coordinate variable
        #     of the variable) is used.
	if !datavary1d
	  datavary1d = @pos.copy
	else
	  if datavary1d.rank != 1 || datavary1d.length != length
	      raise ArgumentError, "Must be 1D and same size"
	  end
	end
	GPhys.new( Grid.new(self), datavary1d )
      end

      def [] (slicer)
	 if ! @pos
	    raise "pos has not been set. Forgot set_pos_to_(center|bounds)?"
	 end

	 case slicer
	 when Fixnum
	    pos=@pos[slicer]
	    cval = ( pos.val.is_a?(Numeric) ? pos.val : pos.val[0] )
	    sval = sprintf("%g",cval)
	    info_lost = "#{self.name}=#{sval}"
	    if (units = pos.get_att('units'))   # substitution
	      info_lost += " "+units
	    end
	    return info_lost
	 when Range, true
	    # re-organize the range to support slicing of variables
	    # with a 1 larger / smaller length.
	    if true===slicer
	       range=0..-1
	    else
	       range = slicer
	    end
	    first = range.first
	    last = range.exclude_end? ? range.last-1 : range.last
	    if first < 0
	       first += self.length  # first will be counted from the beginning
	    end
	    if last >= 0
	       last -= self.length   # last will be counted from the end
	    end
	    slicer = first..last
	    newax=Axis.new(@cell,@bare_index)
	 when Hash
	    range = slicer.keys[0]
	    step = slicer[range]
	    range = 0..-1 if range==true
	    first = range.first
	    last = range.exclude_end? ? range.last-1 : range.last
	    if first < 0
	       first += self.length  # first will be counted from the beginning
	    end
	    if last >= 0
	       last -= self.length   # last will be counted from the end
	    end
	    slicer = { first..last, step }
	    newax=Axis.new(false,@bare_index)  # always not a cell axis
         when NArray, Array
	    newax=Axis.new(false,@bare_index)  # always not a cell axis
	 else
	    raise ArgumentError, "Axis slicing with #{slicer.inspect} is not available"
	 end

	 if newax.bare_index? || !newax.cell?
	    pos=@pos[slicer]
	    newax.set_pos( pos )
	 elsif newax.cell?
	    center=@cell_center[slicer]
	    bounds=@cell_bounds[slicer]
	    newax.set_cell( center, bounds )
	    if self.cell_center?
	       newax.set_pos_to_center
	    elsif self.cell_bounds?
	       newax.set_pos_to_bounds
	    end
	 end
	 if @aux
	    @aux.each{ |name, vary|
#	       if (aux=vary[slicer]).rank != 0
		  newax.set_aux(name, vary[slicer])
#	       else
#		  print "WARNING #{__FILE__}:#{__LINE__} auxiliary VArray #{name} is eliminated\n"
#	       end
	    }
	 end
	 newax
      end

      def cut(coord_cutter)
	_cut_(false, coord_cutter)
      end
      def cut_rank_conserving(coord_cutter)
	_cut_(true, coord_cutter)
      end

      def _cut_(conserve_rank, coord_cutter)

	# assume that the coordinates are monotonic (without checking)

	slicer = Array.new
	ax = self.pos.val

	case coord_cutter
	when true
	  slicer=true
	when Range
	  # find the grid points included in the range
	  range = coord_cutter
	  range_ary = [range.first, range.last]
	  xmin = range_ary.min
	  xmax = range_ary.max
	  wh = ( (ax >= xmin) & (ax <= xmax) ).where
	  if wh.length == 0
	    raise "Range #{range} does not include any grid point of #{dim}-th axis"
	  end
	  idmin = wh.min
	  idmax = wh.max
	  slicer=idmin..idmax
	when Numeric
	  # find the nearst point
	  pt = coord_cutter       
	  dx = (ax-pt).abs
	  minloc = _minloc_(dx)
	  if conserve_rank
	    slicer=minloc..minloc
	  else
	    slicer=minloc
	  end
	when Array, NArray
	  # find the nearst points
	  ary = coord_cutter
	  slicer = ary.collect{ |pt|
	    dx = (ax-pt).abs
	    minloc = _minloc_(dx)
	  }
	else
	  raise TypeError, "(#{coord_cutter.inspect}) is not accepted as a coordinate selector"
	end

	[ self[slicer], slicer ]
      end
      private :_cut_

      def _minloc_(a)  # private to be used in _cut_ 
        # Developper's MEMO: Such a method should be supported in NArray
        minloc = 0  # initialization
        for i in 0...(a.length-1)   # here, do not assume monotonic
	  minloc = i+1 if a[i+1]<a[i]
	end
	minloc
      end
      private :_minloc_

      def set_cell_guess_bounds(center, name=nil)
	 # derive bounds with a naive assumption (should be OK for
	 # an equally separated axis).
	 if ! center.is_a?(VArray) || center.rank != 1
	    raise ArgumentError, "1st arg: not a VArray, or its rank != 1" 
	 end
	 vc = center.val
	 vb = NArray.float( vc.length + 1 )
	 vb[0] = vc[0] - (vc[1]-vc[0])/2   # Assume this!!
	 for i in 1...vb.length
	    vb[i] = 2*vc[i-1] - vb[i-1]    # from vc[i-1] = (vb[i-1]+vb[i])/2
	 end
	 bounds = VArray.new(vb, center)  # borrow attributes from center
	 set_cell(center, bounds, name)
      end

      def set_pos_to_center
	 raise "The method is not available for a non-cell axis" if ! @cell
	 @pos = @cell_center
	 if !@name
	    @name = @cell_center.name
	 end
	 self
      end

      def set_pos_to_bounds
	 raise "The method is not available for a non-cell axis" if ! @cell
	 @pos = @cell_bounds
	 if !@name
	    @name = @cell_bounds.name
	 end
	 self
      end

      def Axis.defined_operations
	 @@operations
      end

      @@operations = ["integrate","average"]

      def integrate(ary,dim)
	 if !@integ_wieght; _set_default_integ_weight; end
	 sh = (0...ary.rank).collect{|i| (i==dim) ? @integ_weight.length : 1 }
	 mn = sprintf("%g", ( UNumeric===(a=@pos.min) ? a.val : a) )
         mx = sprintf("%g", ( UNumeric===(a=@pos.max) ? a.val : a) )
	 return [ ( ary * @integ_weight.reshape(*sh) ).sum(dim), 
	          "integrated  "+@name+":#{mn}..#{mx}" ]
      end

      def average(ary,dim)
	 if !@avg_weight; _set_default_avg_weight; end
	 sh = (0...ary.rank).collect{|i| (i==dim) ? @avg_weight.length : 1 }
	 mn = sprintf("%g", ( UNumeric===(a=@pos.min) ? a.val : a) )
         mx = sprintf("%g", ( UNumeric===(a=@pos.max) ? a.val : a) )
	 return [ ( ary * @avg_weight.reshape(*sh) ).sum(dim), 
	          "averaged  "+@name+":#{mn}..#{mx}" ]
      end


      #########  private methods ####################################

      private

      def _set_default_integ_weight

	 raise "Initialization is not completed" if ! @init_fin
	 if ! @pos
	    raise "pos has not been set. Call set_pos_to_(center|bounds)." 
	 end

	 # < define numerical integration / averaging >

	 if( @bare_index )
	    @integ_weight = NArray.int(@pos.length).fill!(1)
	    #dist = weight.length
	 elsif ( !@cell || (@cell && @pos.equal?(@cell_bounds)) )
	    # --- use trapezoidal formula ---
	    posv = @pos.val
	    if posv.length == 1
	       raise "cannot define the default integration when length==1 and non-cell"
	    end
	    #dist = (posv[-1]-posv[0]).abs
	    @integ_weight = posv.dup
	    @integ_weight[0] = (posv[1]-posv[0]).abs/2
	    @integ_weight[-1] = (posv[-1]-posv[-2]).abs/2
	    @integ_weight[1..-2] = (posv[2..-1] - posv[0..-3]).abs/2
	 else
	    # --- assume that the center values represents the averages ---
	    bd = @cell_bounds.val
	    @integ_weight = (bd[1..-1] - bd[0..-2]).abs
	    #dist = (bd[-1]-bd[0]).abs
	 end

      end

      def _set_default_avg_weight

	 raise "Initialization is not completed" if ! @init_fin
	 if ! @pos
	    raise "pos has not been set. Call set_pos_to_(center|bounds)." 
	 end

	 # < define numerical integration / averaging >

	 if( @bare_index )
	    @avg_weight = NArray.int(@pos.length).fill!(1)
	    dist = @avg_weight.length
	    @avg_weight /= dist
	 elsif ( !@cell || (@cell && @pos.equal?(@cell_bounds)) )
	    # --- use trapezoidal formula ---
	    posv = @pos.val
	    if posv.length == 1
	       @avg_weight = NArray[1]
            else
	       dist = (posv[-1]-posv[0]).abs
	       @avg_weight = posv.dup
	       @avg_weight[0] = (posv[1]-posv[0]).abs/2
	       @avg_weight[-1] = (posv[-1]-posv[-2]).abs/2
	       @avg_weight[1..-2] = (posv[2..-1] - posv[0..-3]).abs/2
	       @avg_weight /= dist
	    end
	 else
	    # --- assume that the center values represents the averages ---
	    bd = @cell_bounds.val
	    @avg_weight = (bd[1..-1] - bd[0..-2]).abs
	    dist = (bd[-1]-bd[0]).abs
	    @avg_weight /= dist
	 end

      end

      #######

   end
end

###################################################
## < test >

if $0 == __FILE__
   include NumRu
   xc = VArray.new( NArray.float(10).indgen! + 0.5 ).rename("x")
   xb = VArray.new( NArray.float(11).indgen! ).rename("xb")
   axpt = Axis.new().set_pos(xc)
   axcel = Axis.new(true).set_cell(xc,xb)
   axcel_c = axcel.dup.set_pos_to_center
   axcel_b = axcel.dup.set_pos_to_bounds
   axcel2 = Axis.new(true).set_cell_guess_bounds(xc)
   p "axcel",axcel, "axcel2",axcel2
   print "########\n"
   p axpt.pos.val
   p axcel_c.pos.val
   p axcel_b.pos.val
   z = VArray.new( NArray.float(xc.length, 2).random! )
   w = VArray.new( NArray.float(xc.length).indgen! )
   w2 = VArray.new( NArray.float(xb.length).indgen!-0.5 )
   p z.val
   p axpt.average(z,0), axcel_c.average(z,0)
   p z.sum(0)/10
   p  axpt.average(w,0), axcel_c.average(w,0), 
      axpt.integrate(w,0), axcel_c.integrate(w,0), 
      axcel_b.integrate(w2,0)
   # axcel.set_default_algorithms  # this is to fail
   print "////////\n"
   p axpt[1..3].pos.val
   p axcel_c[1..3].cell_center.val, axcel_c[1..3].cell_bounds.val
   p axcel_b[1..3].cell_center.val, axcel_b[1..3].cell_bounds.val
   p axpt[1]
   p axcel_b[5]
   p axcel_b.cut(4.3)
   axcut, slicer = axcel_c.cut(3.0..7.0)
   p axcut.copy, slicer
   p axcel_b.cut([3.2, 2.5, 6.9])
   exit
   p axpt
   axcel_c.set_aux('aux1',xc*4)
   p axcel_c.flatten
   p axcel_c.copy.flatten
end
