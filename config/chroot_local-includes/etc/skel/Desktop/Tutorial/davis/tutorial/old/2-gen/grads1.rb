require "numru/netcdf"

include NumRu

data = Hash.new
dims = Hash.new
longname = Hash.new
unit = Hash.new

open('grads.ctl','r'){|file|
  var = false
  last = nil
  while line = file.gets do

    ary = line.split
    if line =~ /^\s/ then
      data[last] = data[last]+ary
    elsif line =~ /^VARS/ then
      var = true
    elsif line =~ /^ENDVARS/ then
      var = false
    elsif var then
      ary = line.split
      name = ary[0]
      str = ary[3..-1].join(' ')
      longname[name] = str[0..str.index('[')-2]
      unit[name] = str[str.index('[')+1..-3]
    else
      last = ary[0]
      data[last] = ary[1..-1]
    end

  end

}

data.each_key{|key|
  if key =~ /^.DEF$/
    ary = data[key]
    name = key[0..0].downcase
    if ary[1] == 'LINEAR'
      dims[name] = NArray.sfloat(ary[0].to_i).indgen!(ary[2].to_f,ary[3].to_f)
    elsif ary[1] == 'LEVELS'
      dims[name] = NArray.to_na( ary[2..-1].collect{|str| str.to_f} )
    end
    data.delete(key)
  elsif key == 'TITLE' then
    data[key] = data[key].join(' ')
  elsif key == 'UNDEF'
    data[key] = data[key][0].to_f
  end
}

p data
p dims
p longname
p unit
