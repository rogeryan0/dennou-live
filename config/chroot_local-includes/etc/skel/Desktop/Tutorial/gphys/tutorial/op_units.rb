require "numru/gphys"
include NumRu
temp = GPhys::NetCDF_IO.open('T.jan.nc', 'T')
p( temp.data.get_att('units') )
p( (temp*temp).data.get_att('units') )
p( (temp*temp).units.to_s )

R = UNumeric[ 287.04, 'J.K-1.kg-1' ]   # Gas const of dry air
p( (R*temp).data.get_att('units') )  # Bad, because temperature should be in K
zeroK = UNumeric[ 0.0, 'K' ]
tempK = zeroK + temp
p( (R*tempK).data.get_att('units') )
