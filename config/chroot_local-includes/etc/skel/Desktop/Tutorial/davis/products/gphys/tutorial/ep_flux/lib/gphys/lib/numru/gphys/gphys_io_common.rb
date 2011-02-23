=begin
=module NumRu::GPhys::IO_Common

THIS MODULE IS ONLY FOR INTERNAL USAGE. 
(Does not work stand alone.)

Functions independent of specific file formart.
To be used by IO, NetCDF_IO, GrADS_IO etc. 

A test program is included in gphys_netcdf.rb

=end

module NumRu
  class GPhys
    module IO_Common

      module_function

      def each_along_dims_write(gphyses, files, loopdims, io_module)
	case gphyses
	when Array
	  shape = gphyses[0].shape
	  for i in 1...gphyses.length
	    if shape != gphyses[i].shape
	      raise "Shape of the GPhys objects must be the same"
	    end
	  end
	else
	  gphyses = [gphyses]     # put in an Array (if a single GPhys)
	end
	gp = gphyses[0]

	if !files.is_a?(Array)
	  files = [files]     # put in an Array (if a single File)
	end

	if !loopdims.is_a?(Array)
	  loopdims = [loopdims]  # put in an Array (if a single Integer/String)
	end
	if loopdims.length == 0
	  raise ArgumentError, "No loop dimension is specified "+
	    " -- In that case, you don't need this iterator."
	end

	#if loopdims.min<0 || loopdims.max>=gp.rank
	#  raise ArguemntError,"Invalid dims #{loopdims.inspect} for #{gp.rank}D array"
	#end

	loopdimids = Array.new
	loopdimnames = Array.new
	loopdims.each{|d|
	  case d
	  when Integer
	    if d < 0
	      d += gp.rank
	    end
	    loopdimids.push( d )
	    loopdimnames.push( gp.axis(d).name )
	  when String
	    loopdimids.push( gp.dim_index(d) )
	    loopdimnames.push( d )
	  else
	    raise ArgumentError,"loopdims must consist of Integer and/or String"
	  end
	}

	sh = Array.new
	len = 1
	loopdimids.each{|i|
	  sh.push(gp.shape[i])
	  len *= gp.shape[i]
	}

	cs = [1]
	(1...sh.length).each{|i| cs[i] = sh[i-1]*cs[i-1]}
	idx_hash = Hash.new
	for i in 0...len do
	  loopdimnames.each_with_index{|d,j| 
	    idx_hash[d] = ((i/cs[j])%sh[j])..((i/cs[j])%sh[j]) # rank preserved
	  }
	  subs = gphyses.collect{|g| g[idx_hash] }
	  results = yield(*subs)
	  if !results.is_a?(Array)
	    raise "The return value of the block must be an Array of GPhys" 
	  end
	  if i == 0
	    fl = files.shift
	    results_whole = Array.new
	    for j in 0...results.length
	      rs = results[j]
	      grid = rs.grid_copy
	      loopdimnames.each{|nm|
                # replaces with original axes (full length)
		if !grid.axnames.include?( nm )
		  raise "Dimension '#{nm}' has been eliminated. "+
                        "You must keep all loop dimensions." 
		end
		grid.set_axis(nm,gphyses[0].axis(nm))
	      }
	      io_module.write_grid(fl, grid)
	      fl = files.shift if files.length >= 1
	      results_whole.push( 
		io_module.def_var(fl, rs.name, rs.data.ntype, 
				  grid.axnames, rs.data)
	      )
	    end
	  end
	  idx = grid.axnames.collect{|k| idx_hash[k] || true}
	  for j in 0...results.length
	    rs = results[j]
	    results_whole[j][*idx] = rs.data
	  end
	end
	return results_whole

      end
    end      # module IO_Common
  end      # class GPhys
end      # module NumRu
