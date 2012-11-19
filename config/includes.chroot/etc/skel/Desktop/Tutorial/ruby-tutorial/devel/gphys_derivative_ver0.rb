require 'narray'
require 'numru/gphys'
require 'derivative_ver1'

module NumRu
  class GPhys
    module Derivative

      module_function

      def cderiv(gp,dim,bc=NumRu::Derivative::LINEAR_EXT)
	# <<��ʬ���뼡�������(̾�� -> ��)>>
	if (dim.is_a?(Numeric) && dim < 0)   # ����饫����� -> �����饫�����
	  dim += gp.rank
	elsif dim.is_a?(String)              # ̾�� -> ��
	  dim = gp.axnames.index(dim)        
	end

	# <<derivate gp>>
	v_data = gp.data;       v_x = gp.coord(dim)            # �ǡ����ȼ��� VArray �����
	n_data = v_data.val;    n_x = v_x.val                  #      ::      NArray �����
	n_dgpdx = NumRu::Derivative::cderiv(n_data,n_x,dim,bc) # ��ʬ���(NArray) �����
        name = "d#{gp.name}_d#{v_x.name}"                      # �ǡ�����̾�� ( ex. "dT_dx" )
	v_dgpdx = VArray.new(n_dgpdx, gp.data, name)           # ��ʬ��̤� VArray ����
	g_dgpdx = GPhys.new( gp.grid_copy, v_dgpdx )           #            GPhys  ����

        # <<set attribute>>
	u_data = v_data.units;  u_x = v_x.units                # �ǡ����ȼ����줾���ñ�̤����
	g_dgpdx.units = u_data/u_x                             # ��ʬ��̤�ñ��( �ǡ��� / ��)
	if v_data.get_att('long_name') && v_x.get_att('long_name') # long_name °������
	  long_name = "d_#{v_data.get_att('long_name')}_d_#{v_x.get_att('long_name')}"
	else
	  long_name = name
	end
	g_dgpdx.data.set_att("long_name",long_name)            # ��ʬ��̤� GPhys �� long_name ������
	return g_dgpdx                                        
      end

    end
  end  
end
