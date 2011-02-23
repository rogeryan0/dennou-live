require 'numru/gphys'
require 'derivative_ver1'

############################################################

=begin
=module NumRu::GPhys::Derivative in derivative.rb

==Index
* ((<module NumRu::GPhys::Derivative>))
  * ((<threepoint_O2nd_deriv>))
    * First derivative (2nd Order difference use three point.)
  * ((<cderiv>))
    * First derivative (using center difference method)

=module NumRu::GPhys::Derivative

Module functions of Derivative Operater for GPhys.

---threepoint_O2nd_deriv(gp, dim_or_dimname, bc=NumRu::Derivative::LINEAR_EXT))

    Derivate (({z})) respect to (({dim})) th dimension with 2nd Order difference. 
    return an NArray which result of the difference ((<z>)) divided difference
    (({x})) (in other wards, 
     (s**2*z_{i+1} + (t**2 - s**2)*f_{i} - t**2*f_{i-1}) / (s*t*(s + t)):
     now s represents (x_{i} - x_{i-1}) ,t represents (x_{i+1} - x_{i})
     and _{i} represents the suffix of {i} th element in the ((<dim>)) th 
     dimension of array. ).

    ARGUMENTS
    * z (NArray): a NArray which you want to derivative.
    * x (NArray): a NArray represents the dimension which derivative respect 
      to. z.rank must be 1.
    * dim (Numeric): a Numeric represents the dimention which derivative 
      respect to. you can give number count backward (((<dim>))<0), but 
      ((<z.rank ¡Üdim>)) must be > 0. 
    * bc (Numeric) : a Numeric which represent boundary condition.
      now only LINEAR_EXT(=1) supported. LINEAR_EXT load ((<b_expand_linear_ext>)) 
      which extend boundary with lenear value.

    RETURN VALUE
    * O2nd_deriv_data (NArray): 
      (s**2*z_{i+1} + (t**2 - s**2)*f_{i} - t**2*f_{i-1}) / (s*t*(s + t))

---cderiv(gp, dim_or_dimname, bc=NumRu::Derivative::LINEAR_EXT)

    Derivate (({gp})) respect to (({dim})) th or (({dimname})) dimension 
    with center difference. return a GPhys which result of the difference 
    ((<gp>)) divided difference axis. 
    ( in other wards,  (z_{i+1} - z_{i-1}) / (x_{i+1} - x_{i-1}): now x is 
    axis array which you wants to respects to, _{i} represents the suffix 
    of {i} th element in the ((<dim>)) th dimension of array. ).

    ARGUMENTS
    * gp (GPhys): a GPhys which you want to derivative.
    * dim_or_dimname (Numeric or String): a Numeric or String represents
      the dimension which derivate respect to. you can give number count 
      backward (((<dim>))<0), but ((<z.rank ¡Üdim>)) must be > 0. 
    * bc (Numeric) : a Numeric which represent boundary condition.
      now only NumRu::Derivative::LINEAR_EXT(=1) supported. LINEAR_EXT load 
      ((<NumRu::Derivative::b_expand_linear_ext>)) which extend boundary 
      with linear value.

    RETURN VALUE
    * a GPhys

=end
############################################################

module NumRu
  class GPhys
    module Derivative

      module_function

      def threepoint_O2nd_deriv(gp,dim,bc=NumRu::Derivative::LINEAR_EXT)
	# <<get dimention number>>
	if (dim.is_a?(Numeric) && dim < 0)
	  dim += gp.rank
	elsif dim.is_a?(String)
	  dim = gp.axnames.index(dim) 
	end

	# <<derivate gp>>
	v_data = gp.data;       v_x = gp.coord(dim)            # get varray
	n_data = v_data.val;    n_x = v_x.val                  # get narray
	n_dgpdx = NumRu::Derivative::threepoint_O2nd_deriv(n_data,n_x,dim,bc) 
                                                               # derivate n_data
        name = "d#{gp.name}_d#{v_x.name}"                      # ex. "dT_dx"
	v_dgpdx = VArray.new(n_dgpdx, gp.data, name)           # make varray 
	g_dgpdx = GPhys.new( gp.grid_copy, v_dgpdx )           # make gphys 

        # <<set attribute>>
	u_data = v_data.units;  u_x = v_x.units                # get units
	g_dgpdx.units = u_data/u_x                             # set units
	if v_data.get_att('long_name') && v_x.get_att('long_name')
	  long_name = "d_#{v_data.get_att('long_name')}_d_#{v_x.get_att('long_name')}"
	else
	  long_name = name
	end
	g_dgpdx.data.set_att("long_name",long_name)            # set long_name
	return g_dgpdx
      end

      def cderiv(gp,dim,bc=NumRu::Derivative::LINEAR_EXT)
	# <<get dimention number>>
	if (dim.is_a?(Numeric) && dim < 0)
	  dim += gp.rank
	elsif dim.is_a?(String)
	  dim = gp.axnames.index(dim) 
	end

	# <<derivate gp>>
	v_data = gp.data;       v_x = gp.coord(dim)            # get varray
	n_data = v_data.val;    n_x = v_x.val                  # get narray
	n_dgpdx = NumRu::Derivative::cderiv(n_data,n_x,dim,bc) # derivate n_data
        name = "d#{gp.name}_d#{v_x.name}"                      # ex. "dT_dx"
	v_dgpdx = VArray.new(n_dgpdx, gp.data, name)           # make varray 
	g_dgpdx = GPhys.new( gp.grid_copy, v_dgpdx )           # make gphys 

        # <<set attribute>>
	u_data = v_data.units;  u_x = v_x.units                # get units
	g_dgpdx.units = u_data/u_x                             # set units
	if v_data.get_att('long_name') && v_x.get_att('long_name')
	  long_name = "d_#{v_data.get_att('long_name')}_d_#{v_x.get_att('long_name')}"
	else
	  long_name = name
	end
	g_dgpdx.data.set_att("long_name",long_name)            # set long_name
	return g_dgpdx
      end

    end
  end  
end


######################################################
## < test >
if $0 == __FILE__

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

end
