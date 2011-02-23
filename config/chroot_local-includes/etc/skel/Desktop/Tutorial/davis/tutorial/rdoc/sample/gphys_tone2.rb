require "gphys_cont2"
#
#= GPhys �����Ѥ��ƿ��ɤ�ޤ����褹�륯�饹�饤�֥��
#
#Authors::   ���� ����
#Version::   1.2 2006-03-08 morikawa
#Copyright:: Copyright (C) GFD Dennou Club, 2006. All rights reserved.
#License::   Ruby �饤���󥹤˽��
#
#-- (#-- ���� #++ �ޤǤ���ʬ�� RDoc �ϲ�ᤷ�ޤ���.)
#"=", "==", ""===" �ϸ��Ф���ɽ���ޤ�.
#
#= ���Ф���٥�1
#== ���Ф���٥�2
#=== ���Ф���٥�3
#++
#
#���Υ��饹�Υ����ѡ����饹�� GPhys_cont �Ǥ�.
#new �᥽�åɤǽ������Ԥ�, tone �᥽�åɤ������Ԥ��ޤ�.
#
#--
# �⥸�塼��̾��᥽�å�̾�Ϥ��Τޤޥ⥸�塼���᥽�åɤؤΥ�󥯤�
# �Ѵ�����ޤ�.
#++
#
#=== ���ͻ���
#
#* http://ruby.gfd-dennou.org
#  1. GPhys[http://www.gfd-dennou.org/library/ruby/products/gphys/]
#  2. {2006 ǯ ��Ǿruby���ߥʡ�����Ǿdavis/ruby�������å�}[http://www.gfd-dennou.org/library/ruby/workshop200603/]
#
#--
#==�ꥹ�Ȥ�ɽ���˴ؤ���
#
#�ꥹ�Ȥϰʲ��Τ褦�ʵ��椬�դ����ѥ饰��դǤ�.
#
# - '*' �⤷���� '-' �����̤Υꥹ��
# - ����+�ԥꥪ�ɤ��ֹ��դ��ꥹ��
# - ����ե��٥å�+�ԥꥪ�ɤǥ���ե��٥åȥꥹ��
#
#
#== ��󥯤˴ؤ���
#
# http:, mailto:, ftp:, www. �ǻϤޤ�ƥ����Ȥϥ����֤ؤΥ�󥯤���
# Ƚ�̤���ޤ�.
#
# label[url] �η����Ǥ�ϥ��ѡ���󥯤�ĥ��ޤ�. ���ξ��� lavel ��ɽ
# ������, url �������Ȥʤ�ޤ�. label ��ʣ����ñ���ޤ�Ǥ����� 
# (���ܸ�ξ��Ϥ��ä���ȤäƤ�������), ���̤�Ȥ�, <em>{multi word
# label}[</em>url<em>]</em>�Ȥ��Ƥ�������.
#++
#
#=== ��ȯ����
#
#* 1.2 2006-03-08
#  * ��Ƿ�⤵��Υ����Ȥ򲼤�, ��Ԥ�饤����, ��ȯ�����
#    ���­���Ƥߤ�.
#
#* 1.1 2006-03-07
#  * �Ȥꤢ�����������Ƥߤ�.
#
class GPhys_tone < GPhys_cont

  # �ޤ˿��ɤ��Ԥ����ɤ����Υե饰.
  # ���Υե饰�� false �� nil �ˤ������, GPhys_cont#cont ��
  # Ʊ�ͤ�ư��ޤ�.
  #
  attr_accessor :draw_tone

  #
  #=== ����������ѥ᥽�å�
  #
  #GPhys_cont#new �򻲾Ȥ��Ƥ�������.
  #
  #--
  # �̤Υ⥸�塼����Υ᥽�åɤإ�󥯤������
  # "<i>�⥸�塼��̾</i>#<i>�᥽�å�̾</i>" �Ȼ��ꤷ�ޤ�
  #++
  #
  def initialize
    super
    @draw_tone = true
  end

  #=== ����᥽�å�
  #
  #���ɤ�ޤ����褹��᥽�å�. �������ޤΤߤ����褷��������
  #GPhys_cont#cont �����Ѥ��Ƥ�������.
  #
  #_itr_ :: ���褹��ݤ��Ͽ����ˡ����ꤷ�ޤ�. ���ͤ�Ϳ���Ƥ�������.
  #         �ǥե���Ȥ� 1 �ˤʤäƤ��ޤ�. �ֹ�����ˡ�δط��˴ؤ��Ƥ�
  #         http://www.gfd-dennou.org/library/dcl/dcl-f90/doc/term/2d.htm
  #         �򻲾Ȥ�������
  #
  #�֤���:: ��� true ���֤�ޤ�.
  #
  def tone(itr=1)
    gphys = GPhys::IO.open(FILENAME, @var)
    DCL.gropn(1) ; DCL.sgpset('lcntl', false) ; DCL.uzfact(0.7)
    GGraph.set_fig( 'itr'=>(itr == nil) ? 1 : itr.to_i)
    GGraph.tone( gphys ) if @draw_tone
    GGraph.contour( gphys, !@draw_tone )
    DCL.grcls
    return true
  end
end

if __FILE__ == $0
  gphys = GPhys_tone.new
  gphys.tone
end
