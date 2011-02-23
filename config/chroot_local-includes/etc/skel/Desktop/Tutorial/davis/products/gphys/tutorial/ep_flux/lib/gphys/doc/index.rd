=begin

=Reference manuals of the GPhys library

==GPhys and its components

====GPhys Structure

* ((<class NumRu::VArray|URL:varray.html>)) :
  Virtual Array class, which holds multi-dimensional data of various
  format and media (on memory or in file).

  * ((<class NumRu::VArrayComposite|URL:varraycomposite.html>)) :
    Subclass of VArray to bundle multiple VArrays by "tiling".

* ((<class NumRu::Axis>)) : Sorry, yet to be written.
* ((<class NumRu::Grid>)) : Sorry, yet to be written.
* ((<class NumRu::GPhys>)) : Sorry, yet to be written.

====Miscleneous Dependent Libraries

* ((<class NumRu::UNumeric|URL:unumeric.html>)) :
  Numeber with units (combination of Numeric and Units)
* ((<class NumRu::SubsetMapping>)) : Sorry, yet to be written.
* ((<class NumRu::Attribute>)) : Sorry, yet to be written.
* ((<class NumRu::CoordMapping|URL:coordmapping.html>)) :
  * ((<class NumRu::LinearCoordMapping|URL:coordmapping.html>)) :

====External File Handling

* ((<module NumRu::GPhys::NetCDF_IO|URL:gphys_netcdf_io.html>)) :
  NetCDF data input/output.
* ((<module NumRu::NetCDF_Conventions, 
  module NumRu::NetCDF_Convention_Users_Guide etc.
  |URL:netcdf_convention.html>)) :
  NetCDF conventon handler.
* ((<module NumRu::GPhys::GrADS_IO|URL:gphys_grads_io.html>)) :
  GrADS data input/output.
* ((<class NumRu::GrADS_Gridded|URL:grads_gridded.html>)) :
  Class to handle GrADS data.

====Extensions

* ((<coodinate transformation|URL:coordtransform.html>)) :
  Extention of the NumRu::GPhys class for coodinate transformation.
* ((<FFT|URL:gphys_fft.html>)) :
  Extention of the NumRu::GPhys class 
  for the Fast Fourier Transformation and its applications

==Applications

To use the following, you have to ((|require|)) explicitly. 

* ((<module NumRu::GGraph|URL:ggraph.html>)) :
  The graphic library.

=end
