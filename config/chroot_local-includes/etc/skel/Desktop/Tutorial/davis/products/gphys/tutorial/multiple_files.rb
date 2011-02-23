require "numru/ggraph"
include NumRu
gp = GPhys::IO.open('T.jan.nc', 'T')

# < create test files: divide into 4 files (2 by 2) >

GPhys::IO.write( f=NetCDF.create('tmp00.nc'), gp[0..17,0..9,{0..6,6}] )
f.close
GPhys::IO.write( f=NetCDF.create('tmp01.nc'), gp[0..17,10..-1,{0..6,6}])
f.close
GPhys::IO.write( f=NetCDF.create('tmp10.nc'), gp[18..-1,0..9,{0..6,6}])
f.close
GPhys::IO.write( f=NetCDF.create('tmp11.nc'), gp[18..-1,10..-1,{0..6,6}])
f.close

# < open two-dimentionally divided data >

files = /tmp(\d)(\d).nc/
p gpcompo = GPhys::IO.open( files, 'T' )

# < test graphics >

DCL.gropn(1)
DCL.sgpset('lcntl', false) ; DCL.uzfact(0.7)
DCL.sldiv('y',2,1)
GGraph.contour( gpcompo )
GGraph.contour( gpcompo[false,1] )
DCL.grcls

# < clean up >

File.unlink('tmp00.nc')
File.unlink('tmp01.nc')
File.unlink('tmp10.nc')
File.unlink('tmp11.nc')
