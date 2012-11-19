require 'narray'
require 'numru/gphys'
require 'derivative_ver1'

module NumRu
  class GPhys
    module Derivative

      module_function

      def cderiv(gp,dim,bc=NumRu::Derivative::LINEAR_EXT)
	# <<微分する次元を解析(名前 -> 数)>>
	if (dim.is_a?(Numeric) && dim < 0)   # 後ろからカウント -> 前からカウント
	  dim += gp.rank
	elsif dim.is_a?(String)              # 名前 -> 数
	  dim = gp.axnames.index(dim)        
	end

	# <<derivate gp>>
	v_data = gp.data;       v_x = gp.coord(dim)            # データと軸の VArray を取得
	n_data = v_data.val;    n_x = v_x.val                  #      ::      NArray を取得
	n_dgpdx = NumRu::Derivative::cderiv(n_data,n_x,dim,bc) # 微分結果(NArray) を取得
        name = "d#{gp.name}_d#{v_x.name}"                      # データの名前 ( ex. "dT_dx" )
	v_dgpdx = VArray.new(n_dgpdx, gp.data, name)           # 微分結果の VArray を構成
	g_dgpdx = GPhys.new( gp.grid_copy, v_dgpdx )           #            GPhys  を構成

        # <<set attribute>>
	u_data = v_data.units;  u_x = v_x.units                # データと軸それぞれの単位を取得
	g_dgpdx.units = u_data/u_x                             # 微分結果の単位( データ / 軸)
	if v_data.get_att('long_name') && v_x.get_att('long_name') # long_name 属性設定
	  long_name = "d_#{v_data.get_att('long_name')}_d_#{v_x.get_att('long_name')}"
	else
	  long_name = name
	end
	g_dgpdx.data.set_att("long_name",long_name)            # 微分結果の GPhys に long_name を設定
	return g_dgpdx                                        
      end

    end
  end  
end
