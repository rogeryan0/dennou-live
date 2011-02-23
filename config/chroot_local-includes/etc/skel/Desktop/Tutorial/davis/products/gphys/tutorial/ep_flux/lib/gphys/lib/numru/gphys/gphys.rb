=begin
=class NumRu::GPhys

==Instance Methods
---first1D
    Returns a 1D subset selecting the first elements of 2nd, 3rd, ..
    dimensions, i.e., self[true, 0, 0, ...]. (For graphics)

    ARGUMENTS
    * (none)

    RETURN VALUE
    * a GPhys

---first2D
    Returns a 2D subset selecting the first elements of 3rd, 4th, ..
    dimensions, i.e., self[true, true, 0, 0, ...]. (For graphics)

    ARGUMENTS
    * (none)

    RETURN VALUE
    * a GPhys
=end

require "numru/gphys/grid"
require "numru/misc/md_iterators"

module NumRu
   class GPhys

      include NumRu::Misc::MD_Iterators

      def initialize(grid, data)
	 raise ArgumentError,"1st arg not a Grid" if ! grid.is_a?(Grid)
	 raise ArgumentError,"2nd arg not a VArray" if ! data.is_a?(VArray)
	 if ( grid.shape_current != data.shape_current )
	    raise ArgumentError, "Shapes of grid and data do not agree. " +
	       "#{grid.shape_current.inspect} vs #{data.shape_current.inspect}"
	 end
	 @grid = grid
	 @data = data
      end

      attr_reader :grid, :data
      protected :grid

      def grid_copy
	# deep clone of the grid
	@grid.copy
      end

      def copy
	 # deep clone onto memory
	 GPhys.new( @grid.copy, @data.copy )
      end

      def inspect
	 "<GPhys grid=#{@grid.inspect}\n   data=#{@data.inspect}>"
      end

      def name
	 data.name
      end
      def name=(nm)
	 data.name=nm
      end
      def rename(nm)
	data.name=nm
	self
      end

      def val
	 @data.val
      end
      def val=(v)
	 @data.val= v
      end
      def replace_val(v)
	 @data.replace_val(v)
      end

      def att_names
	@data.att_names
      end
      def get_att(name)
	@data.get_att(name)
      end
      def set_att(name, val)
	@data.set_att(name, val)
      end
      def del_att(name)
	@data.del_att(name)
      end
      alias put_att set_att

      def units
	@data.units
      end
      def units=(units)
	@data.units= units
      end

      def convert_units(to)
	# ==NOTE: 
	#    * VArray#convert_units does not copy data if to == @data.units
	#    * @grid is shared with self (no duplication)
        #    Thus, use GPhys#copy to separate all sub-objects (deep clone).
	data = @data.convert_units(to)  
	GPhys.new(@grid, data)
      end

      def [](*slicer)
	 if slicer.length==1 && slicer[0].is_a?(Hash) && 
	    slicer[0].keys[0].is_a?(String)
	   slicer = __process_hash_slicer(slicer[0])
	 else
	   slicer = __rubber_expansion( slicer )
	 end
	 GPhys.new( @grid[*slicer], @data[*slicer] )
      end

      def []=(*args)
	 val = args.pop
	 slicer = args
	 if slicer.length==1 && slicer[0].is_a?(Hash) && 
	    slicer[0].keys[0].is_a?(String)
	   slicer = __process_hash_slicer(slicer[0])
	 else
	   slicer = __rubber_expansion( slicer )
	 end
	 val = val.data if val.is_a?(GPhys)
	 @data[*slicer] = val
      end

      def __process_hash_slicer(hash)
	raise ArgumentError, "Expect a Hash" if !hash.is_a?(Hash)
	if (hash.keys - axnames).length > 0
	  raise ArgumentError,"One or more of the hash keys "+
	    "(#{hash.keys.inspect}) are not found in the axis names "+
            "(#{axnames.inspect})."
	end
	axnames.collect{|nm| hash[nm] || true}   # slicer for []/[]=
      end
      private :__process_hash_slicer

      def cut( *args )
	newgrid, slicer = @grid.cut( *args )
	GPhys.new( newgrid, @data[ *slicer ] )
      end

      def cut_rank_conserving( *args )
	newgrid, slicer = @grid.cut_rank_conserving( *args )
	GPhys.new( newgrid, @data[ *slicer ] )
      end

      Axis.defined_operations.each do |method|
         eval <<-EOS, nil, __FILE__, __LINE__+1
	 def #{method}(dim_or_dimname, *extra_args)
            vary, grid = @grid.#{method}(@data, dim_or_dimname, *extra_args)
	    GPhys.new( grid, vary )
	 end
	 EOS
      end

      def axnames
	 @grid.axnames
      end
      def rank
	@grid.rank
      end
      def axis(i)
	@grid.axis(i)
      end
      def coord(i)
	@grid.axis(i).pos
      end
      def lost_axes
	 @grid.lost_axes
      end
      def dim_index( dimname )
	 @grid.dim_index( dimname )
      end

      ## For graphics -->
      def first2D
	raise "rank less than 2" if rank < 2
	self[true,true,*([0]*(rank-2))]
      end
      def first1D
	raise "rank less than 1" if rank < 1
	self[true,*([0]*(rank-1))]
      end
      ## <-- For graphics

      def coerce(other)
	case other
	when Numeric
	  na_other = self.data.val.fill(other)   # Not efficient!
           #developpers memo: perhaps need to let VArray know GPhys to avoid it
           #スカラーでrankゼロの GPhys を定義する手もあるがちょっと難しい。
           #NArrayScalarでも shape は [1] を返してしまうし。
	  va_other, = self.data.coerce(na_other)
	  c_other = GPhys.new( @grid, va_other )
	when Array, NArray
	  va_other, = self.data.coerce(other)
	  c_other = GPhys.new( @grid, va_other )
	when VArray
	  c_other = GPhys.new( @grid, other )
	else
	  raise "Cannot coerse #{other.class}"
	end
	[c_other, self]
      end

      def shape_coerce(other)
	 #
	 # for binary operations
	 #
	 if self.rank == other.rank
	    # nothing to do
	    [other, self]
	 else
	    if self.rank < other.rank
	       shorter = self
	       longer = other
	       i_am_the_shorter = true
	    else
	       shorter = other
	       longer = self 
	       i_am_the_shorter = false
	    end
	    reshape_args = 
	       __shape_matching( shorter.shape_current, longer.shape_current, 
				shorter.axnames, longer.axnames )
	    shorter = shorter.data.copy.reshape!(*reshape_args)
	    ##def shorter.data; self; end  # singular method!
	    if i_am_the_shorter
	       [longer, shorter]
	    else
	       [shorter, longer]
	    end
	 end
      end

      def transpose(*dims)
	grid = @grid.transpose(*dims)
	data = @data.transpose(*dims)
	GPhys.new( grid, data )
      end

      for f in VArray::Math_funcs
	 eval <<-EOS, nil, __FILE__, __LINE__+1
	 #def GPhys.#{f}(gphys)
         #   raise ArgumentError, "Not a GPhys" if !gphys.is_a?(GPhys)
	 #   GPhys.new( gphys.grid, VArray.#{f}(gphys.data) )
	 #end
	 def #{f}(*arg)
	    GPhys.new( self.grid, self.data.#{f}(*arg) )
	 end
         EOS
      end
      for f in VArray::Binary_operators
	 eval <<-EOS, nil, __FILE__, __LINE__+1
	 def #{f.delete(".")}(other)
            if other.is_a?(GPhys)
	       other, myself = self.shape_coerce(other)
               if myself.is_a?(GPhys)
		 if other.is_a?(GPhys)
		   vary = myself.data#{f} other.data
		 else
		   vary = myself.data#{f} other
		 end
		 GPhys.new( myself.grid.copy, vary )
	       else
		 if other.is_a?(GPhys)
		   vary = myself#{f} other.data
		 else
		   vary = myself#{f} other
		 end
		 GPhys.new( other.grid.copy, vary )
	       end
	    else
	       vary = self.data#{f} other
	       GPhys.new( @grid.copy, vary )
	    end
	 end
	 EOS
      end
      for f in VArray::Binary_operatorsL
	 eval <<-EOS, nil, __FILE__, __LINE__+1
	 def #{f.delete(".")}(other)
            # returns NArray
            self.data#{f}(other.is_a?(GPhys) ? other.data : other)
	 end
	 EOS
      end
      for f in VArray::Unary_operators
	 eval <<-EOS, nil, __FILE__, __LINE__+1
	 def #{f}
            vary = #{f.delete("@")} self.data
	    GPhys.new( @grid.copy, vary )
	 end
	 EOS
      end
      for f in VArray::NArray_type1_methods
	 eval <<-EOS, nil, __FILE__, __LINE__+1
	 def #{f}(*args)
	    GPhys.new( self.grid.copy, self.data.#{f}(*args) )
	 end
	 EOS
      end
      for f in VArray::NArray_type2_methods
	 eval <<-EOS, nil, __FILE__, __LINE__+1
	 def #{f}(*args)
	    self.data.#{f}(*args)
	 end
	 EOS
      end
      for f in VArray::NArray_type3_methods
	 eval <<-EOS, nil, __FILE__, __LINE__+1
	 def #{f}(*args)
            args = args.collect{|i| @grid.dim_index(i)}
	    result = self.data.#{f}(*args)
            if Numeric===result || UNumeric===result
	      result
	    else
	      GPhys.new( self.grid.delete_axes(args, "#{f}"), result )
	    end
	 end
	 EOS
      end

      def shape_current
	 @data.shape_current
      end
      alias shape shape_current

      ############## < private methods > ##############

      private
      def __rubber_expansion( args )
	if (id = args.index(false))  # substitution into id
          # false is incuded
	  alen = args.length
	  if args.rindex(false) != id
	    raise ArguemntError,"only one rubber dimension is permitted"
	  elsif alen > rank+1
	    raise ArgumentError, "too many args"
	  end
	  ar = ( id!=0 ? args[0..id-1] : [] )
	  args = ar + [true]*(rank-alen+1) + args[id+1..-1]
	end
	args
      end

      def __shape_matching( shs, shl, axnms, axnml )
	 # shs : shape of the shorter
	 # shl : shape of the longer
	 # axnms : axis names of the shorter
	 # axnml : axis names of the longer
	 #
	 # Return value is reshape_args, which is to be used 
	 # as shorter.reshape( *reshape_args )

	 # < matching from the first element >
	 shlc = shl.dup
	 table = Array.new
	 last_idx=-1
	 shs.each do |len|
	    i = shlc.index(len)
	    if !i
	       raise "cannot match shapes #{shs.inspect} and #{shl.inspect}"
	    end
	    idx = i+last_idx+1
	    table.push(idx)
	    (i+1).times{ shlc.shift }
	    last_idx = idx
	 end

	 # < matching from the last element >
	 shlc = shl.dup
	 rtable = Array.new
	 shs.reverse_each do |len|
	    i = shlc.rindex(len)
	    if !i
	       raise "cannot match shapes #{shs.inspect} and #{shl.inspect}"
	    end
	    idx = i
	    rtable.push(idx)
	    (shlc.length-idx).times{ shlc.pop }
	 end
	 rtable.reverse!

	 if table != rtable
	    # < matching ambiguous => try to match by name >

	    real_table = table.dup  # just to start with.
                                    # rtable will be merged in the following

	    shs.each_index do |i|
	       #print axnms[i]," ",axnml[ table[i] ]," ",axnml[ rtable[i] ],"\n"
	       if axnms[i] == axnml[ rtable[i] ]
		  real_table[i] = rtable[i]
	       elsif  axnms[i] != axnml[ table[i] ]
		  raise "Both matchings by orders and by names failed for the #{i}-th axis #{axnms[i]}."
	       end
	    end
	    
	    table = real_table

	 end

	 # < derive the argument for the reshape method >

	 reshape_args = Array.new
	 shl.length.times{ reshape_args.push(1) }
	 for i in 0...table.length
	    reshape_args[ table[i] ] = shs[i]
	 end

	 reshape_args
      end

   end
end


######################################################
## < test >
if $0 == __FILE__
   include NumRu
   vx = VArray.new( NArray.float(10).indgen! + 0.5 ).rename("x")
   vy = VArray.new( NArray.float(6).indgen! ).rename("y")
   xax = Axis.new().set_pos(vx)
   yax = Axis.new(true).set_cell_guess_bounds(vy).set_pos_to_center
   grid = Grid.new(xax, yax)

   z = VArray.new( NArray.float(vx.length, vy.length).indgen! )
   p z.val
   w = VArray.new( NArray.float(vx.length, vy.length).indgen!/10 ) # .random!

   gpz = GPhys.new(grid,z)
   gg = gpz[true,[0,2,1]]
   p '###',gg.val
   p gg[true,1].val
   p gg['y'=>1].val
   gg['y'=>1] = 999
   p gg.val
   p gg.coord(0).val, gg.coord(1).val
   p gg.cut([1.2,3.8],1.1).val

   gpw = GPhys.new(grid,w)
   p '@@@',gpw.average(1).val
   p ( (gpz + gpw).val )

   vz = VArray.new( NArray.float(6).indgen! ).rename("z")
   zax = Axis.new().set_pos(vz)

   grid3 = Grid.new(xax,yax,zax)
   gridz = Grid.new(zax)
   z3 = VArray.new( NArray.float(vx.length, vy.length, vz.length).indgen! )
   q = VArray.new( NArray.float(vz.length).indgen!*100 )
   gpz3 = GPhys.new(grid3,z3)
   gpq = GPhys.new(gridz,q)
   p ( (gpz3 + gpq).val )
   p ( (gpz + gpq).val )
   p ( (gpz3 + gpz).val )

   print "#######\n"
   p gpz.val, gpz[2..5,2..3].val
   gpz[2..5,2..3]=999
   p gpz.val
   p gpz
   p gpz.sort.val

   print "*****\n"
   gpz.each_subary_at_dims(1){|sub|
     p sub.val
   }
   print "===\n"
   gpz_m0=gpz.mean(0)
   p gpz.val, gpz_m0.val, gpz_m0.lost_axes
   p gpz.mean, gpz.max
   p gpz.mean("x").val

   print "transpose\n"
   p gpz3.axnames, gpz3.val, 
     gpz3.transpose(2,0,1).axnames, gpz3.transpose(2,0,1).val

   print "manual cyclic extention\n"
   p(sp=gpz3.shape)
   gpz3_cext = gpz3[ [0...sp[0],0], false ]
   p gpz3_cext.coord(0).val, gpz3_cext.val

   print "axis to gphys\n"
   ax = gpz3.axis(1)
   p ax.to_gphys
   p ax.to_gphys( ax.cell_bounds[0..-2].copy )

   print "convert units\n"
   gpz3.units = 'm'
   p gpz3.units
   gpz3k = gpz3.convert_units('km')
   p gpz3k.units, gpz3.val, gpz3k.val
end
