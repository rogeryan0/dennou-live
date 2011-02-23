require "numru/netcdf"
include NumRu

infile = NetCDF.open("/home/seiya/gdcl/gdcl/U.199901.nc") # 入力ファイル
outfile = NetCDF.create("./u.199901.nc")   # 出力ファイル

dim = Hash.new
var = Hash.new
["lon","lat","time"].each{|name|
  dim[name] = outfile.def_dim(name, infile.dim(name).length)
  var[name] = outfile.def_var(name,"sfloat",[dim[name]])
  infile.var(name).each_att{|att| att.copy(var[name])}
}

var["dat"] = outfile.def_var("dat","sfloat",[dim["lon"],dim["lat"],dim["time"]])
infile.var("dat").each_att{|att| att.copy(var["dat"])}

infile.each_att{ |a| a.copy(outfile) }

outfile.enddef

["lon","lat","time"].each{|name|
  var[name].put(infile.var(name).get)
}

data = infile.var("dat").get("start"=>[0,0,9,0],"end"=>[-1,-1,9,-1])[true,true,0,true]

rmiss = infile.var("dat").att("missing_value").get[0]

for i in NArray.int(500000).random!(144*73*124).to_a.uniq.sort
  data[i] = rmiss
end

var["dat"].put(data)

outfile.close
infile.close
