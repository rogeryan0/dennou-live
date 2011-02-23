require "numru/gphys/axis"

module NumRu

   class Grid

      def initialize( *axes )
	 @axes = Array.new
	 axes.each{|ag|
	    if ag.is_a?(Axis)
	       @axes.push(ag)
	    else
	       raise ArgumentError, "each argument must be an Axis"
	    end
	 }
	 @lost_axes = Array.new   # Array of String
	 @rank = @axes.length
	 @axnames = Array.new
	 __check_and_set_axnames
      end

      def inspect
	 "<#{rank}D grid #{@axes.collect{|ax| ax.inspect}.join("\n\t")}>"
      end

      def __check_and_set_axnames
	 @axnames.clear
	 @axes.each{|ax| 
	    nm=ax.name
	    if @axnames.include?(nm)
	       raise "Two or more axes share a name: #{nm}"
	    end
	    @axnames.push(nm)
	 }
      end
      private :__check_and_set_axnames

      attr_reader :rank

      def axnames
	@axnames.dup
      end
      def lost_axes
	@lost_axes.dup
      end

#      def axis(i)
#	 @axes[i]
#      end

      def axis(dim_or_dimname)
	 ax_dim(dim_or_dimname)[0]
      end

      alias get_axis axis

      def dim_index(dimname)
	 ax_dim(dimname)[1]
      end

      def set_axis(dim_or_dimname,ax)
	 @axes[ ax_dim(dim_or_dimname)[1] ] = ax
      end

      def set_lost_axes( lost )
	 @lost_axes = lost   # Array of String
         self
      end
      def add_lost_axes( lost )
	 @lost_axes = @lost_axes + lost   # Array of String
         self
      end

      def delete_axes( at, deleted_by=nil )
	case at
        when String
          at = [dim_index(at)]
	when Numeric
	  at = [at]
	when Array
          at = at.collect{|x|
            case x
            when String
              dim_index(x)
            when Integer
              x
            else
              raise ArgumentError,"'at' must consist of Integer and/or String"
            end
          }
	else
	  raise TypeError, "1st arg not an Array"
	end

	at.collect!{|pos| 
	  if pos < 0
	    pos + a.length
	  else
	    pos
	  end
	}
	at.sort!
	newaxes = @axes.dup
	at.reverse.each{|pos| 
	  del = newaxes.delete_at(pos)
	  if !del
	    raise ArgumentError, "dimension #{pos} does not exist in a #{rank}D Grid"
	  end
	}

	newgrid = Grid.new( *newaxes )
	newgrid.set_lost_axes( @lost_axes.dup )

	if !deleted_by 
	  msg = '(deleted) '
	else
	  raise TypeError, "2nd arg not a String" if !deleted_by.is_a?(String)
	  msg = '('+deleted_by+') '
	end
	lost = at.collect{|pos|
	  mn = sprintf("%g", ( UNumeric===(a=@axes[pos].pos.min) ? a.val : a) )
	  mx = sprintf("%g", ( UNumeric===(a=@axes[pos].pos.max) ? a.val : a) )
	  msg + 
	  "#{@axes[pos].name}:#{mn}..#{mx}"
	}

	newgrid.add_lost_axes( lost )
	newgrid
      end

      def copy
	 # deep clone onto memory
	 out = Grid.new( *@axes.collect{|ax| ax.copy} )
	 out.set_lost_axes( @lost_axes.dup )
	 out
      end

      def shape
	 @axes.collect{|ax| ax.length}
      end
      alias shape_current shape

      def [] (*slicer)
	 if slicer.length == 0
	    # make a clone
	    axes = Array.new
	    (0...rank).each{ |i| axes.push( @axes[i][0..-1] ) }
	    Grid.new( *axes )
	 else
	    slicer = __rubber_expansion(slicer)
	    if slicer.length != rank
	       raise ArgumentError,"# of the args does not agree with the rank"
	    end
	    axes = Array.new
	    lost = self.lost_axes.dup  #Array.new
	    for i in 0...rank
	       ax = @axes[i][slicer[i]]
	       if ax.is_a?(Axis)      # else its rank became zero (lost)
		  axes.push( ax )
               else
		  lost.push( ax )
	       end
	    end
	    grid = Grid.new( *axes )
	    grid.set_lost_axes( lost ) if lost.length != 0
	    grid
	 end
      end

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
      private :__rubber_expansion

      def cut(*args)
	_cut_(false, *args)
      end
      def cut_rank_conserving(*args)
	_cut_(true, *args)
      end

      def _cut_(conserve_rank, *args)

	# assume that the coordinates are monotonic (without checking)

	if args.length==1 && args[0].is_a?(Hash)
	  # specification by axis names
	  spec = args[0]
	  if (spec.keys - axnames).length > 0
	    raise ArgumentError,"One or more of the hash keys "+
	      "(#{spec.keys.inspect}) are not found in the axis names "+
              "(#{axnames.inspect})."
	  end
	  args = axnames.collect{|ax| spec[ax] || true}
	end

	args = __rubber_expansion(args)

	if rank != args.length
	  raise ArgumentError, "# of dims doesn't agree with the rank(#{rank})"
	end

	slicer = Array.new

	for dim in 0...rank
	  ax = @axes[dim]
	  if conserve_rank
	    dummy, slicer[dim] = ax.cut_rank_conserving(args[dim])
	  else
	    dummy, slicer[dim] = ax.cut(args[dim])
	  end
	end

	[ self[*slicer], slicer ]
      end
      private :_cut_

      def exclude(dim_or_dimname)
	 dim = dim_index(dim_or_dimname)
	 axes = @axes.dup
	 axes.delete_at(dim)
	 Grid.new( *axes )
      end

      def change_axis(dim, axis)
	 axes = @axes.dup
	 lost = @lost_axes.dup
	 if axis.is_a?(Axis)
	    axes[dim] = axis
	 else
	    lost.push(axis) if axis.is_a?(String)
	    axes.delete_at(dim)
	 end
	 Grid.new( *axes ).add_lost_axes( lost )
      end

      def change_axis!(dim_or_dimname, axis)
	 if axis.is_a?(Axis)
	    @axes[ ax_dim(dim_or_dimname)[1] ] = axis
	 else
	    @lost_axes.push(axis) if axis.is_a?(String)
	    @axes.delete_at( ax_dim(dim_or_dimname)[1] )
	 end
	 @rank = @axes.length
	 __check_and_set_axnames
	 self
      end

      def insert_axis(dim_or_dimname, axis)
	 dim = ax_dim(dim_or_dimname)[1]
	 axes = @axes.dup
	 if axis.is_a?(Axis)
	    axes[dim+1,0] = axis    # axes.insert(dim, axis) if ruby 1.7
	 else
	    # do nothing
	 end
	 Grid.new( *axes )
      end

      def insert_axis!(dim_or_dimname, axis)
	 dim = ax_dim(dim_or_dimname)[1]
	 if axis == nil
	    # do nothing
	 else
	    @axes[dim+1,0] = axis    # @axes.insert(dim, axis) if ruby 1.7
	    @rank = @axes.length
	    __check_and_set_axnames
	 end
	 self
      end

      def transpose( *dims )
	if dims.sort != NArray.int(rank).indgen!.to_a
	  raise ArgumentError, 
            "Args must a permutation of 0..rank-1 (eg, if 3D 2,1,0; 1,0,2;etc)"
	end
	axes = Array.new
	for i in 0...rank
	  axes[i] = @axes[dims[i]]
	end
	Grid.new(*axes)
      end

      # Define operations along each axis --- The following defines
      # instance methods such as "average" and "integrate":

      Axis.defined_operations.each do |method|
      	 eval <<-EOS, nil, __FILE__, __LINE__+1
	 def #{method}(vary, dim_or_dimname, *extra_args)
	    ax, dim = self.ax_dim(dim_or_dimname)
	    va, new_ax = ax.#{method}(vary, dim, *extra_args)
            if va.is_a?(Numeric) || va.is_a?(UNumeric)
              va
	    else
              [va, self.change_axis(dim, new_ax)]
	    end
	 end
	 EOS
      end

      ######### < protected methods > ###########

      protected

      def ax_dim(dim_or_dimname)
	 if dim_or_dimname.is_a?(Integer)
	    dim = dim_or_dimname
	    if dim < 0 || dim >= rank
	       raise ArgumentError,"rank=#{rank}: #{dim}th grid does not exist"
	    end
	 else
	    dim = @axnames.index(dim_or_dimname)
	    if !dim
	       raise ArgumentError, "Axis #{dim_or_dimname} is not contained"
	    end
	 end
	 [@axes[dim], dim]
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
   p "average along x-axis:", grid.average(z,0)[0].val, 
     grid.average(z,"x")[0].val
   p "average along y-axis:", grid.average(z,1)[0].val, 
     grid.average(z,"y")[0].val
   p "grid set by an operation:", (g = grid.average(z,1)[1]).rank, g.shape

   p grid.shape, grid.axis(0).pos.val, grid.axis(1).pos.val
   subgrid = grid[1..3,1..2]
   p subgrid.shape, subgrid.axis(0).pos.val, subgrid.axis(1).pos.val
   p grid[3,2].lost_axes

   p grid

   gr,slice = grid.cut(1.0..4.0, 3.2)
   p "%%",gr.copy,slice,gr.lost_axes
   gr,slice = grid.cut_rank_conserving(-10,false)
   p "%%",gr.copy,slice,gr.lost_axes

   p grid[0,0]

   p Grid.new(xax).average(vx,0)  # --> scalar

   p "+++++"
   p grid.delete_axes(0).lost_axes
   p grid.delete_axes([0,1]).lost_axes
   p grid.delete_axes([0,1], 'mean').lost_axes

   p grid, grid.transpose(1,0)

end

