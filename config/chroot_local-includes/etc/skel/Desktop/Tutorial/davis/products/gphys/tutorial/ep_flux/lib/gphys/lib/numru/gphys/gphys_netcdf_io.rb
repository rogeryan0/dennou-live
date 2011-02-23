require "numru/gphys/gphys"
require "numru/gphys/gphys_io_common"

=begin
=module NumRu::GPhys::NetCDF_Convention_Users_Guide

(To be written.)

=module NumRu::GPhys::NetCDF_IO

a NetCDF read/write helper by automatically interpreting conventions

==Module functions
---open(files, varname)
    a GPhys constructor from a NetCDF file (or multiple NetCDF files).

    ARGUMENTS
    * files (String, NetCDF, NArray, or Regexp): file specifier.
      A single file is specified by a String (containing the path),
      of a NetCDF. Multiple files can be specified by a NArray of
      String or NetCDF or by a Regexp to match paths. In that case,
      data and axes are represented by VArrayComposite.
    * varname (String): name of the variable.

    RETURN VALUE
    * a GPhys

    EXAMPLES
    * From a single file:
        file = NetCDF.open('hogehoge.nc')
        gphys = GPhys::NetCDF_IO(file, 'temp')

        file = NetCDF.open('hogehoge.nc', 'a')     # writable
        gphys = GPhys::NetCDF_IO(file, 'temp')

    * From a single file:
        gphys = GPhys::NetCDF_IO('hogehoge.nc', 'temp')

        gphys = GPhys::NetCDF_IO('/data/netcdf/hogehoge.nc', 'temp')

      If you use a String to specify a file path, the file is opened as 
      read-only.

    * To use data separated into multiple files. Suppose that you have
      hoge_yr2000.nc, hoge_yr2001.nc, and hoge_yr2002.nc in the current
      directory. You can open it by using a regular expression as follows:

        gphys = GPhys::NetCDF_IO(/hoge_yr(\d\d\d\d).nc/, 'temp')

      Here, the parentheses to enclose \d\d\d\d is NEEDED. 

      The same thing can be done as follows by using Array or NArray:

        files = ['hoge_yr2000.nc', 'hoge_yr2001.nc', 'hoge_yr2002.nc']
        gphys = GPhys::NetCDF_IO(files, 'temp')

        files = NArray['hoge_yr2000.nc', 'hoge_yr2001.nc', 'hoge_yr2002.nc']
        gphys = GPhys::NetCDF_IO(files, 'temp')

    * Same as above but to use the full path:

        gphys = GPhys::NetCDF_IO(/\/data\/nc\/hoge_yr(\d\d\d\d).nc/, 'temp')

      Here, the directory separator '/' is escaped as '\/'.

    * To use data separated into multiple files. Suppose that you have
      hoge_x0y0.nc, hoge_x1y0.nc, hoge_x0y1.nc, hoge_x1y1.nc, where
      the data is separated 2 dimensionally into 2*2 = 4 files.

        gphys = GPhys::NetCDF_IO(/hoge_x(\d)y(\d).nc/, 'temp')

      Note that 2 pairs of parentheses are needed here. Alternatively,
      you can also do it like this:

        files = NArray[ ['hoge_x0y0.nc', 'hoge_x1y0.nc'],
                        ['hoge_x0y1.nc', 'hoge_x1y1.nc'] ]
        gphys = GPhys::NetCDF_IO(files, 'temp')

---write(file, gphys, name=nil)

    Write a GPhys into a NetCDF file. The whole data under the GPhys 
    (such as coordinate vars) are written self-descriptively.

    ARGUMENTS
    * file (NetCDF): the NetCDF file to write in. Must be writable of course.
    * gphys (GPhys): the GPhys to write.
    * name (String): (optional) name in the new file -- if you want to save
      with a different variable name than that of gphys.

    RETURN VALUE
    * nil

---write_grid(file, grid_or_gphys)

    Same as ((<write>)) but for writing only the contents of the grid.
    (Used in ((<write>)).)

    ARGUMENTS
    * file (NetCDF): the NetCDF file to write in. Must be writable of course.
    * grid_or_gphys (Grid or GPhys):

    RETURN VALUE
    * nil

---each_along_dims_write(gphyses, files, *loopdims){...}  # a block is expected

    Iterator to process GPhys objects too big to read on memory at once.
    Makes a loop (loops) by dividing the GPhys object(s) (((|gphyses|)))
    with the dimension(s) specified by ((|loopdims|)), and the results
    (which is the return value of the block) are written in ((|files|)).

    ARGUMENTS
    * gphyses (GPhys or Array of GPhys): GPhys object(s) to be processed
    * files (NetCDF or Array of NetCDF): the file in which the results are
      written. The number of the file must be smaller than or equalt to 
      the number of resultant GPhys objects (following the multiple assignment
      rule of Ruby).
    * loopdims (Array of String or Integer) : name (when String) or
      count starting from zero (when Integer) 
    * expected block : Number of arguments == number of GPhys objects in
      ((|gphyses|)). Expected return value is an Array of GPhys objects
      to be written ((|files|)).

    RETURN VALUE
    * GPhys objects in which the results are written

    ERRORS

    The following raise exceptions (in adition to errors in arguments).

    * Shape of the GPhys objects in ((|gphyses|)) are not the same.
    * Return value of the block is not an Array of GPhys.
    * Dimension(s) used for looping (((|loopdims|))) is(are) eliminated
      from the retunred GPhys objects.

    USAGE

    * EXAMPLE 1

      Suppose that you want to do the following:

        in = GPhys::NetCDF_IO.open(infile, varname)
        ofile = NetCDF.create(ofilename)
        out = in.mean(0)
        GPhys::NetCDF_IO.write( ofile, out )
        ofile.close

      The data object (((|in|))) is read on memory and an averagin is made.
      If the size of the data is too big to read on memory at once, you can
      divid this process by using this iterator. The following gives the 
      same result as above, but the processing is made for each subset
      divided at the last dimension (represented by -1, as in the negative
      indexing of Array).

        in = GPhys::NetCDF_IO.open(infile, varname)
        ofile = NetCDF.create(ofilename)
        out = GPhys::NetCDF_IO.each_along_dims_write(in, ofile, -1){|in_sub|
          [ in_sub.mean(0) ]
        }
        ofile.close

      In this case, each_along_dims_write makes a loop by substituting
      ((|in[false,0..0]|)), ((|in[false,1..1]|)), ((|in[false,2..2]|)),.. 
      into the argument of the block (((|in_sub|))). Thus, the return
      value of the block (here, ((|[ in_sub.mean(0) ]|))) consists of
      ((|in[false,0..0].mean(0)|)), ((|in[false,1..1].mean(0)|)),.. .
      This iterator creates a GPhys object in ((|out|)) that 
      represents the whole part of the results (here, ((|in.mean(0)|))), and 
      write the resultant subsets in it one by one. Therefore, the output file
      is filled correctly when exiting the iterator.

      Note that the subset (((|in_sub|))) retains the last dimension
      but the length is 1 becasue of the slicing by Range (0..0, 1..1,..).
      Therefore, the subset has the same rank as the original.
      The output GPhys objects, as given by the return value of the block,
      must have the dimension retained, since the dimension (whose length 
      is one) is replaced by the original one when written in the file.
      Therefore, THE FOLLOWING CAUSE AN ERROR (an exception is raised):

        out = GPhys::NetCDF_IO.each_along_dims_write(in, ofile, 0){|in_sub|
          [ in_sub.mean(0) ]
        }

      Here, looping is made by the first dimension (0), but it is eliminated 
      from the result by averaging with the same dimension. (Also, note
      that this averaging is non-sense, since the length of the first
      dimension of the subset is 1).

    * EXAMPLE 2

      You can specify mutiple dimensions for looping to further
      decrease the size of data to read on memory:

        GPhys::NetCDF_IO.each_along_dims_write(in, ofile, -2, -1){|in_sub|
          ...
        }

      Also, you can specify the loop dimension(s) by name(s):

        GPhys::NetCDF_IO.each_along_dims_write(in, ofile, "y"){|in_sub|
          ...
        }

        GPhys::NetCDF_IO.each_along_dims_write(in, ofile, "y", "z"){|in_sub|
          ...
        }

    * EXAMPLE 3

      You can give multiple objects in the iterotor if they
      have the same shape (in future, this restriction may been loosened),
      as follows:

        in1 = GPhys::NetCDF_IO.open(infile1, varname1)
        in2 = GPhys::NetCDF_IO.open(infile2, varname2)
        in3 = GPhys::NetCDF_IO.open(infile3, varname3)
        ofile = NetCDF.create(ofilename)
        outA, outB = \
          GPhys::NetCDF_IO.each_along_dims_write([in1,in2,in3], ofile, -1){
            |isub1,isub2,isub3|
            osubA = (isub1*isub2).mean(0)
            osubB = (isub2*isub3).mean(1)
            [ osubA, osubB ]
          }
        ofile.close

      In this case, two output objects (outA and outB) are made 
      from the three input objects (in1,in2,in3) and written in a
      single file (ofile). If you want to separate into two files, 
      you can do it like this: 

        in1 = GPhys::NetCDF_IO.open(infile1, varname1)
        in2 = GPhys::NetCDF_IO.open(infile2, varname2)
        in3 = GPhys::NetCDF_IO.open(infile3, varname3)
        ofile1 = NetCDF.create(ofilename1)
        ofile2 = NetCDF.create(ofilename2)
        outA, outB = \
          GPhys::NetCDF_IO.each_along_dims_write([in1,in2,in3], [ofile1,ofile2], -1){
            |isub1,isub2,isub3|
            osubA = (isub1*isub2).mean(0)
            osubB = (isub2*isub3).mean(1)
            [ osubA, osubB ]
          }
        ofile.close

---set_convention(convention)
    Set a NetCDF convention to be interpreted.

    ARGUMENTS
    * convention (Module): the convention

    RETURN VALUE
    * convention (Module)

---convention
    Returns the current NetCDF convention to be interpreted.

    RETURN VALUE
    * convention (Module)

=end

module NumRu

  class GPhys

    module NetCDF_IO

      module_function

      def open(files, varname)
	files, ncvar0 = __interpret_files( files, varname )
	data = __files2varray( files, varname )
	convention = NetCDF_Conventions.find( ncvar0.file )
	axposnames = convention::coord_var_names( ncvar0 )
	rank = data.rank
	bare_index = [ false ] * rank # will be true if coord var is not found

        axes = Array.new
	var_names = ncvar0.file.var_names
	for i in 0...rank
	  if var_names.include?(axposnames[i])
	    axpos = __files2varray( files, axposnames[i], i )
	  else
	    bare_index[i]=true
	    na = NArray.float(ncvar0.shape_current[i]).indgen!
	    axpos = VArray.new( na )
	  end
	  cell_center, cell_bounds_name = convention::cell_center?( axpos )
	  cell_bounds, cell_center_name = convention::cell_bounds?( axpos )
	  cell = cell_center || cell_bounds
	  axis = Axis.new(cell,bare_index[i])
	  if !cell
	    axis.set_pos( axpos )
	  else
	    if cell_center
	      if cell_bounds_name
		varray_cell_bounds = __files2varray(files, cell_bounds_name, i)
		axis.set_cell(axpos, varray_cell_bounds).set_pos_to_center
	      else
		p "cell bounds are guessed"
		axis.set_cell_guess_bounds(axpos).set_pos_to_center
	      end
	    else  # then it is cell_bounds
	      if cell_center_name
		varray_cell_center = __files2varray(files, cell_center_name, i)
		axis.set_cell(varray_cell_center, axpos).set_pos_to_bounds
	      else
		p "cell center is guessed"
		axis.set_cell_guess_center(axpos).set_pos_to_bounds
	      end
	    end
	  end
	  
	  aux = convention::aux_var_names( axpos )
	  if aux
	    aux.each{|k,varname| 
	      axis.set_aux( k, __files2varray(files,varname,i) )
	    }
	  end
	  
	  axes[i] = axis
	end

	grid = Grid.new( *axes )

	GPhys.new(grid,data)
      end

      def write_grid(file, grid_or_gphys)
	(0...(grid_or_gphys.rank)).each{|i|
	  ax = grid_or_gphys.axis(i)
	  dimname = ax.pos.name
	  length = ax.pos.length
	  isfx = 0
	  altdimnames = Hash.new
	  ax.flatten.each{ |va|
	    if va.length == length
	      dimnames = [dimname]
	    else
	      if (nm=altdimnames[va.length])
		dimnames = [nm]
	      else
		dimnames = [ (altdimnames[va.length] = dimname+isfx.to_s) ]
		isfx += 1
	      end
	    end
	    if !(already=file.var(va.name))
	      VArrayNetCDF.write(file, va, nil, dimnames )
	    else
              #< var with the same name exists --> check it >
              if va.shape_current != already.shape_current
		raise "#{va.name} exisits but the shape is different" 
	      #elsif va[0].val != already[0].val #(not allowed in the def mode)
	      #	raise "#{va.name} exisits but the first element doesn't agree" 
	      end
	    end
	  }
	}
	nil
      end

      def def_var(file, name, ntype, dimensions, vary=nil)
	VArrayNetCDF.def_var(file, name, ntype, dimensions, vary)
      end

      def write(file, gphys, name=nil)
        write_grid(file, gphys)
	NetCDF_Conventions.add_history(file, "#{self}.write "+gphys.name)
	VArrayNetCDF.write(file, gphys.data, name, gphys.axnames)
	nil
      end

      def each_along_dims_write(gphyses, files, *loopdims, &block)
	IO_Common::each_along_dims_write(gphyses, files, loopdims, NetCDF_IO,
					 &block)
      end

      ############################################################

      def __files2varray( files, varname, dim=nil )
	if files.is_a?(NetCDF)
	  # Single file. Returns a VArrayNetCDF. dim is ignored.
	  file = files
	  var = file.var( varname )
	  raise "variable '#{varname}' not found in #{file}" if !var
	  VArrayNetCDF.new( var )
	elsif files.is_a?(NArray)
	  # Suppose that files is a NArray of NetCDF. Returns a VArrayCompsite.
	  if dim.is_a?(Integer) && dim>=0 && dim<files.rank
	    files = files[ *([0]*dim+[true]+[0]*(files.rank-dim-1)) ]
	  end
	  varys = NArray.object( *files.shape )
	  for i in 0...files.length
	    var = files[i].var( varname )
	    raise "variable '#{varname}' not found in #{files[i].path}" if !var
	    varys[i] = VArrayNetCDF.new( var )
	  end
	  if files.length != 1
	    VArrayComposite.new( varys )
	  else
	    varys[0]
	  end
	else
	  raise TypeError, "not a NetCDF or NArray"
	end
      end

      def __interpret_files( files, varname )
	case files
	when NetCDF
	  ncvar0 = files.var( varname )
	when String
	  files = NetCDF.open(files)
	  ncvar0 = files.var( varname )
	when NArray, Array, Regexp
	  files = NArray[ *files ] if files.is_a?(Array)
	  files = GPhys::NetCDF_IO.__to_na_of_netcdf( files )
	  files = GPhys::NetCDF_IO.__files_dim_matching( files, varname )
	  ncvar0 = files[0].var( varname )
	else
	  raise TypeError, "argument files: not a NetCDF, String, NArray, Array, or Regexp"
	end
	[files, ncvar0]
      end

      def __files_dim_matching( files, varname )
	# files: NArray of NetCDF

	# < read the first file and check its rank >

	file0 = files[0]
	ncvar0 = file0.var(varname)
	if files.rank > ncvar0.rank
	  raise ArgumentError, "rank of files > rank of data"
	end
	convention = NetCDF_Conventions.find( ncvar0.file )
	axposnames = convention::coord_var_names(ncvar0)

	#< find correspoding dimensions >

	j2i = Array.new

	for ifl in 0...files.rank
	  for jdt in 0...ncvar0.rank
	    axpos_last = nil
	    for kfl in 0...files.shape[ifl]
	      nvax = files[*([0]*ifl+[kfl]+[0]*(files.rank-ifl-1))].var(axposnames[jdt])
	      raise "No coordinate variable: #{axposname}' not found" if !nvax
	      axpos = VArrayNetCDF.new( nvax )
	      if kfl > 0
		if axpos.length != axpos_last.length
		  # not equal ==> this is the dimension looking for
		  j2i[jdt] = ifl
		  break
		else
		  axv0 = axpos.val[0]
		  axv_last0 = axpos_last.val[0]
		  if axv0 != axv_last0
		    # not equal ==> this is the dimension looking for
		    j2i[jdt] = ifl
		    break
		  end
		end
	      end
	      axpos_last = axpos
	    end
	    break if j2i.include?(ifl)
	  end
	  if files.shape[ifl]>1 && !j2i.include?(ifl)
	    raise "No dimension correpodence found for #{ifl}th dim" 
	  end
	end

	for d in files.rank...ncvar0.rank
	  j2i[ (j2i.index(nil) || files.rank) ] = d
	  files.newdim!(d)
	end
	files = files.transpose(*j2i)

	files
      end

      def __to_na_of_netcdf( files )
	case files
	when NArray
	  case files[0]
	  when NetCDF
	    files.each{|f|
	      raise TypeError, "non-NetCDF obj included" if !f.is_a?(NetCDF)
	    }
	  when String
	    files.each{|f|
	      raise TypeError, "non-String obj included" if !f.is_a?(String)
	    }
	    for i in 0...files.length
	      files[i] = NetCDF.open(files[i])
	    end
	  end
	when Regexp
	  pat = files
	  if  /^(.*)\\?\/(.*)$/ =~ (pat.source)
	    d=$1
	    f=$2
	    dir = d.gsub(/\\/,'') + '/'
	    pat = Regexp.new(f)
	  else
	    dir = './'
	  end
	  match_data = Array.new
	  fnames = Array.new
	  first_time = true
	  Dir.open(dir).each{|fn| 
	    if pat =~ fn
	      fnames.push(fn)
	      ary = Regexp.last_match.to_a
	      ary.shift  # ==> ary == [$1,$2,...]
              ary.each_with_index{|v,i|
		match_data[i] = Array.new if !match_data[i]
		match_data[i].push(v) if !match_data[i].include?(v)
	      }
	      if first_time
		body = fn

		first_time = false
	      end
	    end
	  }
	  if match_data.length == 0
	    raise ArgumentError, "found no files that matches #{files.source}"
	  end
	  match_data.each{|ary| ary.sort!}
	  shape = match_data.collect{|ary| ary.length}
	  files = NArray.object(*shape)
	  fnames.each{|fn|
	    pat =~ fn
	    ary = Regexp.last_match.to_a
	    ary.shift  # ==> ary == [$1,$2,...]
            index = Array.new        
            ary.each_with_index{|v,i|
	      index[i] = match_data[i].index(v)
	    }
	    files[*index] = NetCDF.open(dir+fn)
	  }
	else
	  raise TypeError, "unsupported type #{pat.class}"
	end
	files
      end

    end
  end
end

######################################################
if $0 == __FILE__
   include NumRu

   begin
     file = NetCDF.open("../../testdata/T.jan.nc")
   rescue
     file = NetCDF.open("../../../testdata/T.jan.nc")
   end
   temp = GPhys::NetCDF_IO.open(file,"T")
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
   GPhys::NetCDF_IO.write(file2,temp_edy)
   file2.close
   file3 = NetCDF.create('tmp2.nc')
   GPhys::NetCDF_IO.write(file2,temp_xmean)
   file3.close

   p '** test composite **'

   temp = GPhys::NetCDF_IO.open(file,"T")
   GPhys::NetCDF_IO.write( f=NetCDF.create('tmp00.nc'), temp[0..5,0..9,true] )
   f.close
   GPhys::NetCDF_IO.write( f=NetCDF.create('tmp01.nc'), temp[0..5,10..15,true])
   f.close
   GPhys::NetCDF_IO.write( f=NetCDF.create('tmp10.nc'), temp[6..9,0..9,true])
   f.close
   GPhys::NetCDF_IO.write( f=NetCDF.create('tmp11.nc'), temp[6..9,10..15,true])
   f.close
   files = /tmp(\d)(\d).nc/
   p gpcompo = GPhys::NetCDF_IO.open( files, 'T' )
   p gpcompo.coord(0).val
   p gpcompo[false,0].val


  p '** test each_along_dims* **'

  f=NetCDF.create('tmpE1.nc')
  GPhys::NetCDF_IO.each_along_dims_write( temp, f, 1, 2 ){|sub|
    [sub.mean(0)]
  }
  f.close
  f=NetCDF.create('tmpE2.nc')
  GPhys::NetCDF_IO.each_along_dims_write([temp,temp_edy], f, "level"){|s1,s2|
    [s1.mean(0),s2.mean(1).rename('T_edy')]
  }
  f.close
  f=NetCDF.create('tmpE0.nc')
  GPhys::NetCDF_IO.write( f, temp.mean(0) )
  f.close

  print `ncdump tmpE0.nc > tmpE0; ncdump tmpE1.nc > tmpE1 ; diff -u tmpE[01]`
end
