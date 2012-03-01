#
# GPhys �����Ѥ����������ޤ����褹�륯�饹�饤�֥��
#
class GPhys_cont
  require "numru/ggraph" ; include NumRu

  # �ե�����̾ (����)
  FILENAME = 'T.jan.nc'

  # ���褹���ѿ�
  attr_reader   :var

  #
  # ����������ѤΥ᥽�å�. ���� _var_ �ˤ����褹���ѿ���
  # Ϳ���ޤ�.
  #
  def initialize(var='T')
    @var  = var
  end

  #
  # �� ���Ԥ�������, ����������ʬ��̵�뤵��ޤ�.
  #

  #
  # �������ޤ������¹Ԥ��ޤ�.
  #
  def cont
    gphys = GPhys::IO.open(FILENAME, @var)
    DCL.gropn(1) ; DCL.sgpset('lcntl', false) ; DCL.uzfact(0.7)
    GGraph.contour( gphys)
    DCL.grcls
  end
end

if __FILE__ == $0
  gphys = GPhys_cont.new
  gphys.cont
end
