require "numru/gphys"                    
require "gphys_derivative_ver0"          

include NumRu
                                         
t = GPhys::IO.open('T.jan.nc','T')       
dtdx = GPhys::Derivative.cderiv(t,'lon') 
newfile = NetCDF.create('dtdx.jan.nc')   
GPhys::IO.write(newfile, dtdx)           
newfile.close                            
