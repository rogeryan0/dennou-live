# /usr/bin/env ruby

require "numru/netcdf"
include NumRu

module NumRu
 class NetCDF
  def copy(outfilename)
    outfile = NetCDF.create(outfilename)

    self.each_dim{ |d|
    #  outfile.def_dim(d.name, d.length_ul0)
      outfile.def_dim(d.name, d.length)    # 次元の定義
    }

    vlist = []                             # 変数リスト
    self.each_var{ |v|
    #  vdims = v.dim_names.collect{ |nm| outfile.dim(nm) }
    #  vout = outfile.def_var(v.name, v.vartype, vdims ) 
                                           # 変数の定義
      vout = outfile.def_var(v.name, v.vartype, outfile.dims(v.dim_names) )
      v.each_att{ |a| a.copy(vout) }       # 属性のコピー
      vlist.push(vout)                     # 変数リストの最後に加える
    }
    
    self.each_att{ |a| a.copy(outfile) } # グローバル属性のコピー

    outfile.enddef

    self.each_var{ |v|
      vout = vlist.shift                   # 変数リストの先頭を取り出し
      vout.put(v.get)                      # 変数のコピー
      p vout.name
    }

    outfile.close
  end
 end 
end

file = NetCDF.open("T.jan.nc") # 入力ファイル
file.copy("nccopy.nc")                 # 出力ファイル
file.close

#print `diff T.jan.nc nccopy.nc`
