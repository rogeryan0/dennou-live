require "numru/gphys"
require "derivative_ver0"

  include NumRu
  include NMath

  #### define method for tests. 
  ##
  # genarate narray

  def gen_x(n)
    2*PI*NArray.sfloat(n).indgen!/(n-1)
  end
  def gen_y(n)
    PI*(NArray.sfloat(n).indgen!/(n-1)-0.5)
  end
  def gen_z1(n)
    NArray.sfloat(n).indgen!/(n-1)
  end

  def make_gp3D(f,x,y,z)
    vax = VArray.new( x, 
		     {"long_name"=>"longitude", "units"=>"rad"},
		     "lon" )
    vay = VArray.new( y, 
		     {"long_name"=>"latitude", "units"=>"rad"},
		     "lat" )
    vaz = VArray.new( z, 
		     {"long_name"=>"altitude", "units"=>"m"},
		     "z" )
    axx = Axis.new.set_pos(vax)
    axy = Axis.new.set_pos(vay)
    axz = Axis.new.set_pos(vaz)
    data = VArray.new( f,
		      {"long_name"=>"temperature", "units"=>"K"},
		      "t" )
    return GPhys.new( Grid.new(axx, axy, axz), data)
  end

  def show_attr(bef_deriv, aft_deriv)
    fm = "%-15s%-15s%-10s%s"
    printf(fm, "<attr-name>", "<before>", "<after>", "\n")
    printf(fm, "name", bef_deriv.data.name, aft_deriv.data.name, "\n")
    aft_deriv.data.att_names.each{|nm| 
      printf(fm, nm, bef_deriv.data.get_att(nm).to_s, 
	             aft_deriv.data.get_att(nm).to_s, "\n")
    }
  end

  def test(dim)
    nx = 10; ny = 10; nz = 10
    x = gen_x(nx)
    y = gen_y(ny)
    z1 = gen_z1(nz)    
    f = sin(x).reshape(nx,1,1) * sin(y).reshape(1,ny,1) * z1.reshape(1,1,nz)
    gp = make_gp3D(f, x, y, z1)
    deriv = GPhys::Derivative::cderiv(gp, dim)
    dfdx1 = deriv.data.val
    case dim
    when 0, -3, "lon"
      dfdx2 = cos(x).reshape(nx,1,1) * sin(y).reshape(1,ny,1) * z1.reshape(1,1,nz)
    when 1, -2, "lat"
      dfdx2 = sin(x).reshape(nx,1,1) * cos(y).reshape(1,ny,1) * z1.reshape(1,1,nz)
    when 2, -1, "z"
      dfdx2 = sin(x).reshape(nx,1,1) * sin(y).reshape(1,ny,1) 
    end
    p(dfdx1) if $VERBOSE
    diff = (dfdx1 - dfdx2)[1..-2].abs
    err = diff.mean
    print "dfdx - kaiseki_kai (except boundary): "
    print err, "\t", diff.max,"\n"
    print "**** check attribute ****\n"
    show_attr(gp, deriv)
  end

  ## main routine of test ---------------------------------------------

  
  print "******** dimname == 'lat' ********\n"
  test("lat")
  print "******** dim == 0(lon) ********\n"
  test(0)
  print "******** dim == -1(z) ********\n"
  test(-1)
 
