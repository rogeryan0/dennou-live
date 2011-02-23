require "numru/gphys/gphys_netcdf_io"
require "numru/gphys/gphys_grads_io"

module NumRu
  class GPhys
    module IO
      module_function

      ## // module functions to be defined in specific IO modules -->
      def open(file, varname)
	file2specific_module(file)::open(file, varname)
      end

      def write(file, gphys, name=nil)
	file2specific_module(file)::write(file, gphys, name)
      end

      def write_grid(file, grid_or_gphys)
	# usually not needed (internally called by write)
	file2specific_module(file)::write_grid(file, grid_or_gphys)
      end

      def each_along_dims_write(gphyses, files, *loopdims, &block)
	IO_Common::each_along_dims_write(gphyses, files, loopdims, 
					 file2specific_module(files), &block)
      end
      ## <-- module functions to be defined in specific IO modules //

      ## // file type selctor -->
      NETCDF = "NETCDF"
      GRADS = "GRADS"
      @@iomdl =     {NETCDF => GPhys::NetCDF_IO, 
                     GRADS => GPhys::GrADS_IO}
      @@nc_pattern = [/\.nc$/]
      @@gr_pattern = [/\.ctl$/]

      def file2type(file)
 	case file
	when Array, NArray
	  file2type(file[0])   # inspect the first element (ignoring the rest)
	when NetCDF
	  NETCDF
	when GrADS_Gridded
	  GRADS
	when Regexp
	  NETCDF     # So far, only NetCDF_IO supports Regexp. 
	when *@@nc_pattern
	  NETCDF
	when *@@gr_pattern
	  GRADS
	else
	  nil
	end
      end

      def file2specific_module(file)
	@@iomdl[ file2type(file) ]
      end

      ['nc','gr'].each{|c|
	eval <<-EOS
	  def add_#{c}_pattern(*regexps)
	    regexps.each{ |regexp|
	      raise TypeError,"Regexp expected" unless Regexp===regexp
	      @@#{c}_pattern.push(regexp)
	    }
	    nil
	  end
	EOS
      }

      ['nc','gr'].each{|c|
	eval <<-EOS
	  def set_#{c}_pattern(*regexps)
	    regexps.each{ |regexp|
	      raise TypeError,"Regexp expected" unless Regexp===regexp
	    }
	    @@#{c}_pattern = regexps
	    nil
	  end
	EOS
      }
      ## <-- file type selctor //

    end      # module IO
  end      # class GPhys
end      # module NumRu

######################################################
if $0 == __FILE__
  include NumRu

  puts "\n** test NETCDF **\n"

  file = "../../../testdata/T.jan.nc"
  temp = GPhys::IO.open(file,"T")
  p temp.name, temp.shape_current
  p temp.val.class
  temp2 = temp[true,true,2]
  p temp2.name, temp2.shape_current
  
  temp_xmean = temp.average(0)
  p temp.val

  temp_edy = ( temp - temp_xmean )
  p '###',temp_edy.name,temp_edy.val[0,true,true]
  p 'deleted attributes:', temp.data.att_names - temp_edy.data.att_names
  p '@@@',temp
  p '///',temp.copy
  p '+++',temp2

  puts "\n** test write (tmp.nc) **"
  file2 = NetCDF.create('tmp.nc')
  p v = temp_edy.axis(0).pos[0..-2].copy.rename('lonlon')
  temp_edy.axis(0).set_aux('test',v)
  temp_edy.axis(0).set_aux('test2',(v/2).rename('lonlon2'))
  temp_edy.axis(0).set_aux('test2',(v/2).rename('lonlon3')[0..-2])
  GPhys::IO.write(file2,temp_edy)
  file2.close
  file3 = NetCDF.create('tmp2.nc')
  GPhys::IO.write(file2,temp_xmean)
  file3.close

  p '** test each_along_dims* **'

  f=NetCDF.create('tmpE1.nc')
  GPhys::IO.each_along_dims_write( temp, f, 1, 2 ){|sub|
    [sub.mean(0)]
  }
  f.close
  f=NetCDF.create('tmpE2.nc')
  GPhys::IO.each_along_dims_write([temp,temp_edy], f, "level"){|s1,s2|
    [s1.mean(0),s2.mean(1).rename('T_edy')]
  }
  f.close
  f=NetCDF.create('tmpE0.nc')
  GPhys::IO.write( f, temp.mean(0) )
  f.close

  print `ncdump tmpE0.nc > tmpE0; ncdump tmpE1.nc > tmpE1 ; diff -u tmpE[01]`

  puts "\n** test GRADS (and write into NETCDF) **\n"

  file = "../../../testdata/T.jan.ctl"
  temp = GPhys::IO.open(file,"T")
  p temp.name, temp.shape_current
  temp2 = temp[true,true,2,0]
  p temp2.name, temp2.shape_current

  temp_xmean = temp.average(0)
  p temp.val

  temp_edy = ( temp - temp_xmean )
  p '$$$',temp_edy.name,temp_edy.val[0,true,true,0]
  p '@@@',temp
  p '///',temp.copy
  p '+++',temp2

  puts "\n** test write (tmp.nc) **"
  require "numru/gphys/gphys_netcdf_io"
  file2 = NetCDF.create('tmp.nc')
  p v = temp_edy.axis(0).pos[0..-2].copy.rename('lonlon')
  temp_edy.axis(0).set_aux('test',v)
  temp_edy.axis(0).set_aux('test2',(v/2).rename('lonlon2'))
  temp_edy.axis(0).set_aux('test2',(v/2).rename('lonlon3')[0..-2])
  GPhys::IO.write(file2,temp_edy)
  file2.close
  file3 = NetCDF.create('tmp2.nc')
  GPhys::IO.write(file2,temp_xmean)
  file3.close

end
