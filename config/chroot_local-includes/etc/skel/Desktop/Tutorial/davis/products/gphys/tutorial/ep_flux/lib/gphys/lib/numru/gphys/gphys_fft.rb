=begin
=extention of class NumRu::GPhys -- Fast Fourier transformation and its applications

This manual documents the methods of NumRu::GPhys defined in gphys_fft.rb

=methods
---fft(backward=false, *dims)
    Fast Fourier Transformation (FFT) by using  
    (((<FFTW|URL:http://www.fftw.org>))) ver 3 or ver 2.
    A FFTW ver.2 interface is included in NArray, while
    to use FFTW ver.3, you have to install separately.
    Dimension specification by the argument dims is available
    only with ver.3. By default, FFT is applied to all dimensions.

    The transformation is complex. If the input data is not complex,
    it will be coerced to complex before transformation.

    When the FT is forward, the result is normalized 
    (i.e., divided by the data number), unlike the default behavior of
    FFTW.
    
    Each coordinate is assumed to be equally spaced without checking.
    The new coordinate variables will be set equal to wavenumbers,
    derived as 2*PI/(length of the axis)*[0,1,2,..], where the length
    of the axis is derieved as (coord.val.max - coord.val.min)*(n+1)/n.

    ARGUMENTS
    * backward (true of false) : when true, backward FT is done;
      otherwise forward FT is done.
    * dims (integers) : dimensions to apply FFT

    RETURN VALUE
    * a GPhys

    EXAMPLE
      gphy.fft           # forward, for all dimensions
      gphy.fft(true)     # backward, for all dimensions
      gphy.fft(nil, 0,1) # forward, for the first and second dimensions.
      gphy.fft(true, -1) # backward, for the last dimension.

---detrend(dim1[,dim2[,...]])
    Remove means and linear trends along dimension(s) specified.
    Algorithm: 1st order polynomial fitting.

    ARGUMENTS
    * dim? (Integer of String): the dimension along which you want to remove
      trends.

    RETURN VALUE
    * a GPhys

    EXAMPLE
    * See ((<detrend>)).

---cos_taper(dim1[,dim2[,...]])
    Cosine tapering along dimension(s) specified.

    Algorithm: to multiply with the half cosine curves at the both 
    1/10 ends of the data.

         cos taper shape:
                  _____________
                _/             \_
              ->   <-       ->   <-
               T/10           T/10
            half-cosine     half-cosine
              shaped         shaped

    The spectra of tapered data should be multilied by 1/0.875,
    which is stored as GPhys::COS_TAPER_SP_FACTOR (==1/0.875).

    ARGUMENTS
    * dim? (Integer of String): the dimension along which you want to remove
      trends.

    RETURN VALUE
    * a GPhys

    EXAMPLE
       dim = 0    # for the 1st dimension
       fc = gphys.detrend(dim).cos_taper(dim).fft(nil,dim)
       sp = fc.abs**2 * GPhys::COS_TAPER_SP_FACTOR

---spect_zero_centering(dim)
    Shifts the waveneumber axis to cover from -K/2 to K/2 instead of 
    from 0 to K-1, where the wavenumber is simbolically treated as integer,
    which is actually not the case, though. Since the first (-K/2) and
    the last (K/2) elements are duplicated, both are divided by 2.
    Therefore, this method is to be used for spectra (squared quantity)
    rather than the raw Fourier coefficients. (That is why the method name
    is prefixed by "spect_").

    The method is applied for a single dimension (specified by the argument
    dim). If you need to apply for multiple dimensions, use it for multiple
    times.

    ARGUMENTS
    * dim (integer): the dimension you want to shift spectra elements.
      Count starts from zero.

    RETURN VALUE
    * a GPhys

    EXAMPLE
    * To get a spectra of a variable var along the 1st and 2nd dimensions:

        fc = var.fft(nil, 0,1)    # --> Fourier coef
        sp = ( fc.abs**2 ).spect_zero_centering(0).spect_zero_centering(1)

      Note that spect_zero_centering is applied after taking |fc|^2.

    * Same but if you want to have the 2nd dimension one-sided:

        fc = var.fft(nil, 0,1)
        sp = ( fc.abs**2 ).spect_zero_centering(0).spect_one_sided(1)

    * Similar to the first example but for cross spectra:

        fc1 = var1.fft(nil, 0,1)
        fc2 = var2.fft(nil, 0,1)
        xsp = (fc1 * fc2.conj).spect_zero_centering(0).spect_zero_centering(1)

---spect_one_sided(dim)
    Similar to ((<spect_zero_centering>)) but to make one-sided spectra.
    Namely, to convert from 0..K-1 to 0..K/2. To be applied for spectra;
    wavenumber 2..K/2-1 are multiplied by 2.

    ARGUMENTS
    * dim (integer): the dimension you want to shift spectra elements.
      Count starts from zero.

    RETURN VALUE
    * a GPhys

    EXAMPLE
    * See the 2nd example of ((<spect_zero_centering>)).

---rawspect2powerspect(*dims)
    Converts raw spectra obtained by gphys.fft.abs**2 into
    power spectra by dividing by wavenumber increments
    along the dimensions spcified by dims.

    ARGUMENTS
    * dims (integers): the dimensions corresponding to wavenumbers.

    RETURN VALUE
    * a GPhys

    EXAMPLE
    * Suppose a 2 (or more) dimensional data gphys.

        fc = gphys.fft(nil, 0, 1)
        sp = fc.abs**2
        ps = sp.rawspect2powerspect(0,1)

      Here, sp is the raw spectum of gphys, and ps is the power spectrum.
      The Parseval relation for them are as follows:

        (gphys**2).mean == sp.sum
                        == pw.sum*dk*dl (== \int pw dk dl, mathematically),

      where, dk = (pw.coord(0)[1] - pw.coord(0)[0]), and
      dl = (pw.coord(1)[1] - pw.coord(1)[0]).
=end

begin
  require "numru/fftw3"
rescue LoadError
end
require "numru/gphys/gphys"

module NumRu
  class GPhys
    @@fft_forward = -1
    @@fft_backward = 1

    COS_TAPER_SP_FACTOR = 1.0 / 0.875  # Spectral factor for the cosine taper.
                                       # Specta should be multiplied by this.
    def cos_taper(*dims)
      if dims.length < 1
	raise ArgumentError,'You have to specify one or more dimensions'
      end
      dims.sort!.uniq!
      val = self.data.val
      dims.each{|dim|
	dim = dim_index(dim) if dim.is_a?(String)
	dim += rank if dim < 0
	raise ArgumentError,"dim #{dim} does not exist" if dim<0 || dim>rank
        nx = shape[dim]
	wgt = NArray.float(nx).fill!(1)
        x = 10.0 / nx * (NArray.float(nx).indgen!+0.5) 
	wskl = x.lt(1).where
	wskr = x.gt(9).where
	wgt[wskl] = 0.5*( 1.0 - NMath::cos(Math::PI*x[wskl]) )
	wgt[wskr] = 0.5*( 1.0 - NMath::cos(Math::PI*x[wskr]) )
	wgt.reshape!( *([1]*dim + [nx] + [1]*(rank-dim-1)) )
	val = val*wgt
      }
      to_ret = self.copy
      to_ret.data.val = val
      to_ret
    end

    def detrend(*dims)
      if dims.length < 1
	raise ArgumentError,'You have to specify one or more dimensions'
      end
      dims.sort!.uniq!
      val = self.data.val
      dims.each{|dim|
	dim = dim_index(dim) if dim.is_a?(String)
	dim += rank if dim < 0
	raise ArgumentError,"dim #{dim} does not exist" if dim<0 || dim>rank
	if val.is_a?(NArray)
	  x = self.coord(dim).val
	  x.reshape!( *([1]*dim + [x.length] + [1]*(rank-dim-1)) )
	  vmean = val.mean(dim)
	  vxmean = (val*x).mean(dim)
	  xmean = x.mean(dim)
	  x2mean = (x*x).mean(dim)
	  denom = x2mean-xmean**2
	  if denom != 0
	    a = (vxmean - vmean*xmean)/denom
	    b = (vmean*x2mean - vxmean*xmean)/denom
	  else
	    a = 0
	    b = vmean
	  end
	elsif val.is_a?(NArrayMiss)
	  x = self.coord(dim).val
	  x.reshape!( *([1]*dim + [x.length] + [1]*(rank-dim-1)) )
	  x = NArrayMiss.to_nam( NArray.new(x.typecode, *val.shape) + x,
				 val.get_mask ) 
	  vmean = val.mean(dim)
	  vxmean = (val*x).mean(dim)
	  xmean = x.mean(dim)
	  x2mean = (x*x).mean(dim)
	  denom = x2mean-xmean**2
	  meq0 = denom.eq(0).to_na(0)    # ==0 and not masked
	  mne0 = denom.ne(0).to_na(0)    # !=0 and not masked
          denom.set_mask(mne0)    # only nonzero part will be used to divide:
	  a = (vxmean - vmean*xmean)/denom
	  b = (vmean*x2mean - vxmean*xmean)/denom
	  a[meq0] = 0
	  b[meq0] = vmean[meq0]
	end
	a.newdim!(dim) if !a.is_a?(Numeric)
	b.newdim!(dim) if !b.is_a?(Numeric)
	val = val - a*x-b
      }
      to_ret = self.copy
      to_ret.data.val = val
      to_ret
    end

    def fft(backward=false, *dims)
      fftw3 = false
      if defined?(FFTW3)
	fftw3 = true
      elsif !defined?(FFTW)
	raise "Both FFTW3 and FFTW are not installed."
      end
      if backward==true
	dir = @@fft_backward
      elsif !backward
	dir = @@fft_forward
      else
	raise ArgumentError,"1st arg must be true or false (or, equivalenty, nil)"
      end

      # <FFT>

      gfc = self.copy  # make a deep clone
      if fftw3
	fcoef = FFTW3.fft( gfc.data.val, dir, *dims )
      else
	# --> always FFT for all dimensions
	if dims.length == 0
	  raise ArgumentError,
	    "dimension specification is available only if FFTW3 is installed"
	end
	fcoef = FFTW.fftw( gfc.data.val, dir )
      end
      if dir == @@fft_forward
	if dims.length == 0
	  fcoef = fcoef / fcoef.length       # normalized if forward FT
	else
	  sh = fcoef.shape
	  len = 1
	  dims.each{|d|
	    raise ArgumentError, "dimension out of range" if sh[d] == nil
	    len *= sh[d]
          }
	  fcoef = fcoef / len
        end
      end
      gfc.data.replace_val( fcoef )

      # <coordinate variables>
      for i in 0...gfc.rank
	if dims.length == 0 || dims.include?(i) || dims.include?(i+rank)
	  __predefined_coord_units_conversion(gfc.coord(i))
	  cv = gfc.coord(i).val
	  n = cv.length
	  clen = (cv.max - cv.min) * n / (n-1)
	  wn = (2*Math::PI/clen) * NArray.new(cv.typecode,cv.length).indgen!
	  if (!backward)
	    gfc.coord(i).set_att('origin_in_real_space',cv[0..0])
	  else 
	    if ( org = gfc.coord(i).get_att('origin_in_real_space') )
	      wn += org[0]
	      ###gfc.coord(i).del_att('origin_in_real_space')
	    end
	  end
	  gfc.coord(i).replace_val(wn)
	  gfc.coord(i).units = gfc.coord(i).units**(-1)
	  __coord_name_conversion(gfc.coord(i), backward)
	end
      end

      # <fini>
      gfc
    end

    def __predefined_coord_units_conversion(coord)
      case coord.units
      when Units["degree"]
	val = coord.val
	coord.replace_val( val * (Math::PI/180) )
	coord.units = "radian"
      end
    end
    private :__predefined_coord_units_conversion

    def __coord_name_conversion(coord, backward)

      if !backward   #--> forward

	( ln = coord.get_att('long_name') ) &&
	  coord.set_att('long_name','wavenumber - '+ln) 

	case coord.name
	when 'x'
	  coord.name = 'k'
	when 'y'
	  coord.name = 'l'
	when 'z'
	  coord.name = 'm'
	  # when 'lon','longitude'
	  #  coord.name = 's'
	when 't','time'
	  if coord.units === Units['s-1']   # compatible_with?
	    coord.name = 'omega'
	    coord.set_att('long_name', 'angular frequency')
	  end
	end

      else  #--> backward

	if ( ln = coord.get_att('long_name') )
	  case ln
	  when /^wavenumber -/
	    coord.set_att( 'long_name', ln.sub(/^wavenumber - */,'') ) 
	  when /angular frequency/
	    coord.set_att( 'long_name', 'time' ) 
	  end
	end

	case coord.name
	when 'k'
	  coord.name = 'x'
	when 'l'
	  coord.name = 'y'
	when 'm'
	  coord.name = 'z'
	when 'omega'
	  coord.name = 'time'
	end
      end
    end
    private :__coord_name_conversion

    def spect_zero_centering(dim)
      dim = dim + self.rank if dim<0
      len = self.shape[dim]
      b = self[ *( [true]*dim + [[(len+1)/2..len-1,0..len/2],false] ) ].copy
      s1 = [true]*dim + [0, false]
      s2 = [true]*dim + [-1, false]
      if (len % 2) == 0   #--> even number
        b[*s2] = b[*s1] = b[*s1]/2      # the ends are duplicated --> halved
      end
      b.coord(dim)[0..len/2-1] = -b.coord(dim)[len/2+1..-1].val[-1..0]
      b
    end

    def spect_one_sided(dim)
      dim = dim + self.rank if dim<0
      len = self.shape[dim]
      b = self[ *([true]*dim + [0..len/2,false]) ] * 2
      b[*([true]*dim + [0,false])] = b[*([true]*dim + [0,false])] / 2
      if (self.shape[dim] % 2) == 0  # --> even number
        b[*([true]*dim + [-1,false])] = b[*([true]*dim + [-1,false])] / 2
      end
      b
    end

    def rawspect2powerspect(*dims)
      # developpers memo: Needs Units conversion.
      factor = nil
      dims.each{|dim|
	ax = self.coord(dim)
	dwn = UNumeric.new( ((ax[-1].val - ax[0].val)/(ax.length - 1)).abs,
			    ax.units )
        if !factor
	  factor = dwn**(-1)
	else
	  factor = factor / dwn
	end
      }
      self * factor
    end
  end
end

######################################################
## < test >
if $0 == __FILE__
  include NumRu

  # < make a GPhys from scratch >
  vx = VArray.new( NArray.float(11).indgen! * (3*Math::PI/11) ).rename("x")
  vx.units = 'km'
  vy = VArray.new( NArray.float(8).indgen! * (3*Math::PI/8) ).rename("y")
  vy.units = 'km'
  xax = Axis.new().set_pos(vx)
  yax = Axis.new(true).set_cell_guess_bounds(vy).set_pos_to_center
  grid = Grid.new(xax, yax)
  a = NArray.float(vx.length, vy.length)
  a[] = NMath.sin(vx.val.refer.newdim(1)) * NMath.cos(vy.val.refer.newdim(0))
  v = VArray.new( a )
  v.units = 'm/s'
  gpz = GPhys.new(grid,v)

  print "Original:\n"
  p gpz.val
  fc = gpz.fft
  print "2D FFT & abs:\n"
  p fc.val.abs, fc.units.to_s, fc.coord(0).units.to_s
  print "Check the invertivility: ",
        (fc.fft(true) - gpz).abs.max, ' / ', gpz.abs.max, "\n"
  sp = fc.abs**2
  print "Check parsevals relation: ",
        sp.sum, ' / ', (gpz**2).mean, "\n"

  spex = sp.spect_zero_centering(0)
  spex2 = spex.spect_one_sided(1)
  print "   sidedness changed -->  ",spex.sum,",  ",spex2.sum,"\n"

  if defined?(FFTW3)
    fc = gpz.fft(nil, 0)
    print "1D FFT & abs:\n"
    p fc.val.abs
    print "Check the invertivility: ",
          (fc.fft(true, 0) - gpz).abs.max, ' / ', gpz.abs.max, "\n"
    sp = fc.abs**2
    print "Check parsevals relation: ",
          sp.sum(0).mean, ' / ', (gpz**2).mean, "\n"
  end

  print "\n** Check detrend **\n"
  print "when NArray...\n"
  EPS = 5e-14
  a.indgen!
  v = VArray.new( a )
  gp = GPhys.new(grid,v)
  gpdt = gp.detrend(0)
  if gpdt.val.max <EPS; print "test succeeded\n";else; raise "test failed";end
  gpdt = gp.detrend(1)
  if gpdt.val.max <EPS; print "test succeeded\n";else; raise "test failed";end
  print "  -- NArrayMiss\n"
  mask = a.le(47)
  am = NArrayMiss.to_nam(a, mask)
  v = VArray.new( am )
  gp = GPhys.new(grid,v)
  gpdt = gp.detrend(0)
  #if gpdt.val.max <EPS; print "test succeeded\n";else; raise "test failed";end
  p "The following should be basically zero:",gpdt.val
  gpdt = gp.detrend(1)
  #if gpdt.val.max <EPS; print "test succeeded\n";else; raise "test failed";end
  p "The following should be basically zero:",gpdt.val

  print "\n** Check cos_taper **\n"
  a = NArray.float(30,10).fill!(1)
  v = VArray.new( a )
  xax = Axis.new().set_pos(VArray.new(NArray.float(30).indgen!).rename("x"))
  yax = Axis.new().set_pos(VArray.new(NArray.float(10).indgen!).rename("y"))
  gp = GPhys.new( Grid.new(xax, yax), v )
  gpct = gp.cos_taper(0)
  gpct = gp.cos_taper(0,1)
  p gpct.val
  p GPhys::COS_TAPER_SP_FACTOR, 1/GPhys::COS_TAPER_SP_FACTOR
end

