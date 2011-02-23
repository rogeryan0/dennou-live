=begin

= RDoc �ˤ�뼫ư�ɥ����������

# * ��������
#   * 2006/08/16 (��������) �������륷���Ȥβ���.
#   * 2006/03/09 (��������) ((<���󥹥ȡ���γ�ǧ>)) ���ɲ�
#   * 2006/03/08 (��������) ��Ƿ�⤵��Υ����Ȥ򸵤˽���
#   * 2006/03/07 (��������) �Ȥꤢ��������
#   * 2006/03/05 (��������) ����

== RDoc �Ȥ�?

Ruby Documentation System (RDoc) �Ȥ�, Ruby �ǽ񤫤줿�����������ɤ���
�ɥ�����Ȥ�ư��������, Ruby ���Τ���°����ɸ��饤�֥���1�ĤǤ�.

RDoc �� Ruby �����������ɤ���Ϥ�, ���饹, �⥸�塼��, �᥽�åɤ����
��ȴ���Ф�, include �� require �˴ؤ��Ʋ�ᤷ�ޤ�. �����Ƥ���������
�Ȥ���ľ���˽񤫤줿�����Ȥ�ʻ�礷, HTML �ɥ�����Ȥ���Ϥ���
��. �ܤ�����((<���ͻ���>)) 1,2 �򻲾Ȥ�������.

== ����

�ʲ��Ǥ�, �ޤ� RDoc �Υ��󥹥ȡ����Ԥ��ޤ�. ������ Ruby �Ǵ�ñ�ʥ���
���饤�֥��������, RDoc ���Ѥ��Ƥ��Υץ���फ��ɥ�����Ȥ���
�����Ƥߤޤ�.

����Ȥ���, Ruby ���ΤΥ��󥹥ȡ���ϹԤäƤ����Ƥ�������.


== ���󥹥ȡ���

: Ruby �򥽡��������ɤ��饤�󥹥ȡ��뤷�����

  RDoc ��, Ruby �ΥС������ 1.8.4 �Ǥϴ���ɸ��饤�֥��Ȥ��� Ruby
  ���Τ�Ʊ������Ƥ���Ϥ��Ǥ�. Ruby �Υۡ���ڡ���
  ( ((<���ͻ���>)) 3 ) ���饽���������ɤ�
  ��������ɤ��ƥ���ѥ��뤷�����ˤ� RDoc ����˥��󥹥ȡ��뤵���
  ���ޤ�.

: �Х��ʥ�ѥå����������Ѥ�����

  �㤨�� Fedora, Vine, Debian �ʤɤǤ�, ruby �Ȥ����ѥå������Ȥ��̤�
  rdoc �Ȥ����ѥå��������Ѱդ���Ƥ���Τ����ӥ��󥹥ȡ����Ԥ�ɬ��
  ������ޤ�. (����¾�ξ���̤Ĵ���Ǥ�. �����ޤ���).

  * Fedora Core �ξ��

      # yum install rdoc

  * Vine, Debian �ξ��

      # apt-get install rdoc

=== ���󥹥ȡ���γ�ǧ

���Υǥ��쥯�ȥ�������, ���Υǥ��쥯�ȥ���˰�ư���Ƥ�������.  ����
�ǥ��쥯�ȥ����, ((*rdoc*)) �Ȥ������ޥ�ɤ��¹ԤǤ��뤳�Ȥ��ǧ����
��������.

  $ mkdir rdoc_test
  $ cd rdoc_test
  $ rdoc

�ʲ��Τ褦�ʥ�å�������ɽ������, doc �Ȥ����ǥ��쥯�ȥ꤬����
����Ƥ���� OK �Ǥ�.

    Generating HTML...

    Files:   0
    Classes: 0
    Modules: 0
    Methods: 0
    Elapsed: 0.262s


== ����ץ� Ruby ������ץȺ���

�ޤ�, GPhys ���Ѥ��ƴ�ñ�ʥ��饹�饤�֥���������ޤ��礦. (������
�����Υ��塼�ȥꥢ��Ǵ��� GPhys �����Ѳ�ǽ�ʾ��֤��Ȳ��ꤷ�Ƥ��ޤ�).
�ʲ��Υץ�����������Ƥ�������.  ((<URL:sample/gphys_cont1.rb>))

    class GPhys_cont
      require "numru/ggraph" ; include NumRu

      FILENAME = 'T.jan.nc'

      attr_reader   :var

      def initialize(var='T')
        @var  = var
      end
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

(({if __FILE__ == $0 })) �ʹߤ���ʬ��, ���Υץ�����¹Ԥ����ݤ�
�ᥤ��ʸ��������ޤ�. ���Υץ����ϼºݤˤ�
((<GPhys ���塼�ȥꥢ�� -- 6. �Ȥꤢ�����Ļ벽|URL:http://ruby.gfd-dennou.org/products/gphys/tutorial/body-j.html#h2:toriaezu>))
�Τ褦��ư��ޤ�. �ʲ��Υǡ����ե�������������ɤ�����, �嵭��
Ruby ������ץȤ� ruby �Ǽ¹Ԥ��ƤߤƤ�������.

  * ((<���������: NetCDF�ե����� T.jan.nc|URL:http://ruby.gfd-dennou.org/products/gphys/tutorial/T.jan.nc>))

       $ ruby gphys_cont1.rb

�⤦1��, ���Υ��饹��Ѿ��������饹�饤�֥���������Ƥߤޤ��礦.
�ʲ��Υץ�����������Ƥ�������.
((<URL:sample/gphys_tone1.rb>))

    require "gphys_cont1"
    class GPhys_tone < GPhys_cont

      attr_accessor :draw_tone

      def initialize
        super
        @draw_tone = true
      end
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

���Υץ�����, gphys_cont1.rb �ο��ɤ��ǤǤ�.

       $ ruby gphys_tone1.rb


== RDoc �ˤ��ɥ����������

=== �ޤ� rdoc ��ȤäƤߤ�

�Ǥϼ���, RDoc ���Ѥ��Ƥ��Υ��饹�饤�֥��Υ�ե���󥹥ޥ˥奢���
��ư�������Ƥߤޤ��礦. gphys_cont1.rb, gphys_tone1.rb ���֤��Ƥ���ǥ�
�쥯�ȥ�ǰʲ��Υ��ޥ�ɤ�¹Ԥ��Ƥ�������.

   $ rdoc gphys_cont1.rb gphys_tone1.rb --main GPhys_cont

: ���� "gphys_cont1.rb gphys_tone1.rb"

  �оݤȤʤ�ե�����Ǥ�. �����Ǥ��������Ƥ��ޤ���, �ե��������������
  �����ϥ����ȥǥ��쥯�ȥ�ʲ���¸�ߤ������Ƥ� "rb" �Ȥ�����ĥ�Ҥ�
  ���ĥե�����򸡺����ޤ�.

: ���� "--main GPhys_cont"

  �ᥤ��ڡ�������ꤷ�ޤ�. �����Ǥϥ��饹 GPhys_cont ��ᥤ��ڡ�����
  ���ꤷ�ޤ�.

���Υ��ޥ�ɤˤ��, doc �Ȥ����ǥ��쥯�ȥ꤬��������, ������� RDoc ��
��äƺ������줿�ɥ�����Ȥ����Ϥ��줿�Ϥ��Ǥ�. �֥饦����
doc/index.html �򸫤Ƥߤޤ��礦. �ʲ��Τ褦�ʥڡ�����ɽ�������Ϥ��Ǥ�.

  * ((<GPhys_cont (������̵��)|URL:sample/doc1>))

((<RDoc �ˤ��������줿�ɥ������ (������̵��)|"IMG:rdoc-image1.png">))

���ʤ� 3 ʬ�䤵�줿�ե졼��˥ե�����, ���饹����ӥ⥸�塼��, �᥽��
�ɤΥꥹ�Ȥ�ɽ������Ƥ��ޤ�. ������ʬ�ˤ� GPhys_cont ���饹�����Ƥ�
ɽ������Ƥ��ޤ�. ���Υե졼��Υ᥽�å�̾����ʬ�򥯥�å������, ����
�������ɤ�ɽ������ޤ�.


=== �����Ȥ�񤭹���Ǥߤ�

�����������ɤ˥����Ȥ������ळ�Ȥ�, �ɥ�����Ȥˤ��¿����
������ղä��Ƥߤޤ��礦. gphys_cont1.rb �˥����Ȥ��ɲä���
�ʲ��Υե������������ޤ��礦.

((<URL:sample/gphys_cont2.rb>))

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

���饹��᥽�å������ľ���˽񤫤�Ƥ��륳���Ȥ��ơ��Υɥ�����Ȥ�
���Ʋ�ᤵ��ޤ�. �ʤ�, ���Ԥ򤤤줿�ʳ��Ǥ�������������ʬ (��Υ���
�������ɤǸ����ȡ֢� ���Ԥ�������...�פ���ʬ) �ϥɥ�����ȤȤ��Ʋ��
����ޤ���.

�Ǥ�, ���� rdoc ���ޥ�ɤ�¹Ԥ��Ƥߤޤ��礦.

   $ rdoc gphys_cont2.rb gphys_tone1.rb --main GPhys_cont --charset euc-jp

: ���� "--charset euc-jp"

  ��������������˥ޥ���Х��Ȥ�ʸ�� (���ܸ�ʤ�) ���ޤޤ�����ɬ��
  ���Υ��ץ�������ꤷ�Ƥ�������. ��������������Υޥ���Х���ʸ����
  ʸ�������ɤ˹�碌, "euc-jp", "shift_jis", "iso-2022-jp" �Τ��Ť줫
  ����ꤷ�ޤ�.

���٤�, �ʲ��Τ褦�ʥڡ�������������ޤ�.

  * ((<GPhys_cont (�����Ȥ���)|URL:sample/doc2>))

((<RDoc �ˤ��������줿�ɥ������ (�����Ȥ���)|"IMG:rdoc-image2.png">))

��������������Υ��饹��᥽�åɤξ����˽񤫤줿�����Ȥ��ɥ�����Ȥ�
ȿ�Ǥ���Ƥ���Τ�ʬ����ޤ�.


== RDoc �������ʵ�ǽ��ȤäƤߤ�

=== ���������ν���

RDoc �Υ��������Ϥ��ʤ꼫���˽񤯤��Ȥ��Ǥ��ޤ���,
������ʽ������ǽ�ˤʤäƤ��ޤ�.

gphys_tone1.rb �˥����Ȥ��ɲä����ʲ��Υե������������ޤ��礦.

((<URL:sample/gphys_tone2.rb>))

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


�Ǥ�, ���� rdoc ���ޥ�ɤ�¹Ԥ��Ƥߤޤ��礦.

   $ rdoc gphys_cont2.rb gphys_tone2.rb --main GPhys_tone --charset euc-jp

���٤�, �ʲ��Τ褦�ʥڡ�������������ޤ�.

  * ((<GPhys_tone|URL:sample/doc3>))

((<RDoc �ˤ��������줿�ɥ������ (��������)|"IMG:rdoc-image3.png">))


�⥸�塼���᥽�åɤʤɤ˼�ưŪ�˥�󥯤��Ϥ��, ���Ф�, �ꥹ��ɽ��
���Ԥ��Ƥ��뤳�Ȥ��狼��ޤ�.

�����Τ���ν񼰤˴ؤ���, �����������ɤ˲��⤬�յ����Ƥ���Τǻ��Ȥ���
��������. ���ܤ��������, ((<���ͻ���>)) 1, 2 �� "MarkUp" ����ʬ��
�Ȥ��Ƥ�������. ��������������ǲ��⤬�񤭹���Ǥ��� (({'#--'})) �� 
(({'#++'})) ����ʬ�˴ؤ��Ƥ� RDoc ��̵�뤹�뤿��, �ɥ�����Ȥ�ȿ�Ǥ�
��ޤ���.


=== �����ʥ��ץ����

rdoc ���ޥ�ɤΥ��ץ����Τ���, �嵭���������ʤ��ä������ʤ�Τ򤤤�
�Ĥ��Ҳ𤷤ޤ�. ���ܤ��������, ((<���ͻ���>)) 1, 2 �� "Usage" �ޤ�
�� "�Ȥ���" ����ʬ�򻲾Ȥ��Ƥ�������.

: --all, -a

  private °���Υ᥽�åɤ�ɥ�����Ȥ�ɽ�����ޤ�.
  ��ȯ�Ը����Υɥ�����ȤȤ����������⤷��ޤ���.

: --diagram, -d

  ���饹�ηѾ��ط��ʤɤ����������ɽ�����ޤ�.
  ((<Dot|URL:http://www.research.att.com/sw/tools/graphviz/>))
  ��ɬ�פˤʤ�ޤ�. (Fedora, Vine, Debian �ʤ�� graphviz
  �ѥå������Υ��󥹥ȡ�������Ѳ�ǽ�ˤʤ�ޤ�).

: --inline-source, -S

  �����������ɤ�ɽ����ݥåץ��åפǤϤʤ�, �ڡ������
  ɽ������褦�ˤ��ޤ�.

: --op, -o dir

  dir �ǥ��쥯�ȥ�˥ɥ�����Ȥ���Ϥ��ޤ�.

: --title, -t text

  text �� HTML �Υ����ȥ�����ꤷ�ޤ�.

�ʲ��Υ��ޥ�ɤǺ��������ɥ�����Ȥ�ܤ��Ƥ����ޤ�.
(���ޥ�ɥץ��ץȤ� DOS ������Ѥ��Ƥ����������ԥڤ��䤹���褦��,
���Ԥ��ʤ���Τ�ܤ��Ƥ����ޤ�)

   $ rdoc gphys_cont2.rb gphys_tone2.rb --main GPhys_tone \
         --charset euc-jp --inline-source --diagram \
         --title "GPhys_tone and GPhys_cont Documentation"

   $ rdoc gphys_cont2.rb gphys_tone2.rb --main GPhys_tone --charset euc-jp --inline-source --diagram --title "GPhys_tone and GPhys_cont Documentation"

* ((<GPhys_tone and GPhys_cont Documentation|URL:sample/doc4>))




== RDoc �ɥ�����ȤΥ���ץ�

RDoc ���Ѥ��ƺ������줿�ɥ�����Ȥ򤤤��Ĥ��Ҳ𤷤ޤ�.

* ((<Ruby Standard Library Documentation|URL:http://www.ruby-doc.org/stdlib/>))
* ((<Ruby on Rails|URL:http://api.rubyonrails.org/>))


== ���ͻ���

(1) ((<rdoc: Ruby Standard Library Documentation|URL:http://www.ruby-doc.org/stdlib/libdoc/rdoc/rdoc/index.html>))
(2) ((<���Ӱ�ʿ����ˤ��嵭�ڡ��������ܸ���|URL:http://www.kmc.gr.jp/~ohai/rdoc.ja.html>))
(3) ((<���֥������Ȼظ�������ץȸ��� Ruby ���ܲȥ�����|URL:http://www.ruby-lang.org/>))

=end
