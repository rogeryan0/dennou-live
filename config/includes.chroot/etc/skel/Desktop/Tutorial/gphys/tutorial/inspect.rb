require "numru/gphys"
include NumRu
p gphys = GPhys::NetCDF_IO.open('T.jan.nc', 'T')
p 'name = '+gphys.name, 'rank:',gphys.rank, 'shape:', gphys.shape
print '[1st dim]  name:', gphys.coord(0).name, 
      '  long_name:',gphys.coord(0).get_att('long_name'),
      '  units:',gphys.coord(0).get_att('units'),
      "  min,max:#{gphys.coord(0).min},#{gphys.coord(0).max}\n"
prs = gphys.coord(2).val
t_jpn = gphys.cut(135,35,true).val
print "\n#{gphys.coord(0).name}\t#{gphys.name}\n"
for i in 0...prs.length do
  print prs[i], "\t", t_jpn[i], "\n"
end
