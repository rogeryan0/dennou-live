# /usr/bin/env ruby

require "numru/netcdf"
include NumRu

module NumRu
 class NetCDF
  def copy(outfilename)
    outfile = NetCDF.create(outfilename)

    self.each_dim{ |d|
    #  outfile.def_dim(d.name, d.length_ul0)
      outfile.def_dim(d.name, d.length)    # ���������
    }

    vlist = []                             # �ѿ��ꥹ��
    self.each_var{ |v|
    #  vdims = v.dim_names.collect{ |nm| outfile.dim(nm) }
    #  vout = outfile.def_var(v.name, v.vartype, vdims ) 
                                           # �ѿ������
      vout = outfile.def_var(v.name, v.vartype, outfile.dims(v.dim_names) )
      v.each_att{ |a| a.copy(vout) }       # °���Υ��ԡ�
      vlist.push(vout)                     # �ѿ��ꥹ�ȤκǸ�˲ä���
    }
    
    self.each_att{ |a| a.copy(outfile) } # �����Х�°���Υ��ԡ�

    outfile.enddef

    self.each_var{ |v|
      vout = vlist.shift                   # �ѿ��ꥹ�Ȥ���Ƭ����Ф�
      vout.put(v.get)                      # �ѿ��Υ��ԡ�
      p vout.name
    }

    outfile.close
  end
 end 
end

file = NetCDF.open("T.jan.nc") # ���ϥե�����
file.copy("nccopy.nc")                 # ���ϥե�����
file.close

#print `diff T.jan.nc nccopy.nc`
