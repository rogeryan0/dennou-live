=begin

= Ruby/GSL

((:<font size="-1">:))
* �Խ���: �͸�����, ����������
* �ǽ�����: 2006/03/08 (����������)
* ��������: 2005/02/12 (�͸�����)
((:</font>:))

((:<hr>:))
((:<ol class="contents">:))
Contents
<<< index.hindex.rd
((:</ol>:))
((:<hr>:))

== Ruby/GSL �Ȥ�

GSL(GNU Scientific Library) �Ϥ���̾���̤�ʳص��ѷ׻��饤�֥���
���ޤ��ޤʿ��ͷ׻�ˡ�δؿ����������󽸤���Ƥ��ޤ���((<URL:http://www.gnu.org/software/gsl/>))
ANSI C �ǵ��Ҥ���Ƥ��ơ�C �� C++ ����ƤӽФ��ޤ���


Ruby/GSL �Ϲ�Ωŷʸ��ξ��ꤵ�󤬥��ƥʥ󥹤��Ƥ�����
GSL �� Ruby ��åѡ��Ǥ�. �ǿ���(2006/03/08 ����) 1.7 �Ǥ�
GSL 1.7 �����Ƥδؿ������夷�Ƥ��ޤ�. ((<URL:http://rb-gsl.rubyforge.org>)) 

* ����
  * ̵�������ѤǤ���
  * C �Ǥ����ܿͤ�¿������(���ܸ�ɥ�����Ȥ�¿��¸��)
  * Ruby �Ǥΰ����Υ᥽�åɤϰ���, ����ͤ� NArray ��Ȥ뤳�Ȥ��Ǥ���
    * ���󥹥ȡ�����������ɬ�פ���
  * ����饤�֥�����°
  
���Υ��塼�ȥꥢ��Ǥ� Ruby/GSL �λ�����򤪸������ޤ�.   

== �Х��ʥ�ѥå������ǥ��󥹥ȡ���
*Debian, Vine

  # apt-get install rb-gsl

*Fedora Core

  # yum install rb-gsl

*FreeBSD

  # cd /usr/ports/math/ruby-gsl
  # make install

*Cygwin

ruby-gsl �Ȥ����ѥå���������ǾRuby �ץ������Ȥ����󶡤���Ƥ��ޤ����ܤ�����((<������|URL:http://dennou-k.gfd-dennou.org/arch/ruby/products/cygwin/index-j.html>))��

== �����������ɤ��饤�󥹥ȡ���

=== plotutils �Υ��󥹥ȡ���

�ޤ��� gsl �ǻȤ��Ƥ�������饤�֥��򥤥󥹥ȡ��뤷�ޤ�.
(rb-gsl ��ɬ�ܤǤϤʤ��餷���ΤǤ���, ���󥹥ȡ���Ǥ�����Τǰ��.)
((<Plotutils �����ڡ���(GNU)|URL:ftp://ftp.gnu.org/gnu/plotutils/>))����

* plotutils-2.4.1.tar.gz

���������ɤ��ޤ�.

Ÿ���������, Ÿ�����줿�ǥ��쥯�ȥ�˰ܤ�ޤ�. ������

  % ./configure

�� Makefile ���������ޤ�. ���λ��ǥե���ȤΥ��󥹥ȡ���ѥ�(/usr/local/)
�ʳ��ξ��˥��󥹥ȡ��뤷��������

  % ./configure --prefix=<���󥹥ȡ�����Υѥ�>

�ʤɤȤ��ޤ��礦. configure ������ä���
  
  % make

���ޤ�. ���֤�������Τǹ���Ǥ������Ԥ��ޤ��礦. ����ä���
  
  # make install

���ޤ�(���ˤ�äƥ����ѡ��桼���ˤʤäƤ�������). ��ñ�Ǥ���. 

=== GSL �Υ��󥹥ȡ���

((<GSL �����ڡ���(GNU)|URL:ftp://ftp.gnu.org/gnu/gsl/>))����

* gsl-1.7.tar.gz

���������ɤ��ޤ��礦.

Ÿ���������, Ÿ�����줿�ǥ��쥯�ȥ�˰ܤ�ޤ�. ������ plotutils Ʊ�ͤ�

  % ./configure or ./configure --prefix=<���󥹥ȡ�����Υѥ�>
  % make
  # make install

�Ȥ��ޤ�. ����ǽ�λ�Ǥ�.

=== rb-gsl �Υ��󥹥ȡ���

���� Ruby/GSL �򥤥󥹥ȡ��뤷�ޤ�.
((<RubyGSL �����ڡ���|URL:http://rb-gsl.rubyforge.org/>))�� 3. Installation, 2 �� Download ����
((<URL:http://rubyforge.org/frs/?group_id=285>))�����Ӥޤ�. ��������

* rb-gsl-1.7.0.tar.gz

���������ɤ��ޤ��礦.

��Ȥ����񸻤�Ÿ������, Ÿ�������ǥ��쥯�ȥ�˰ܤ�ޤ�.

�ޤ������ꤷ�ޤ�. NArray �� gsl �����ѤǤ���褦�ˤ���ˤ�

  % ruby setup.rb config -- --with-narray-include=<narray.h's path>

�Ȥ��ޤ�. ���λ� narray.h �Υѥ����ۤ˻��ꤷ�ʤ��ƤϤʤ�ޤ���.
�ޤ� plotutils �� gsl �Υ��󥹥ȡ����褬�ǥե���ȤΥ��ɥѥ��ˤʤ�����
�ʲ����ͤ����ꤷ�Ƥ��ʤ���Фʤ�ޤ���.

  % export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:<���󥹥ȡ�����Υѥ�>/lib
  % export C_INCLUDE_PATH=$C_INCLUDE_PATH:<���󥹥ȡ�����Υѥ�>/include

�¹Ԥ����ݤ�, narray.h �����ɤ��줿���Ȥ��ǧ������

  % ruby setup.rb setup

�Ȥ��ޤ�. ̵����λ������  
  
  # ruby setup.rb install

���ޤ�. ������, rb_gsl.so �����Ĥ���뤫�⤷��ޤ���. ���λ��ϼ�ư��

  # cd ext; install rb_gsl.so <���󥹥ȡ�����Υѥ�>

�ȼ¹Ԥ��ưܤ��Ƥ��ޤ�. �ʾ�ǥ��󥹥ȡ���Ͻ�λ�Ǥ�.

����Ǥϥ���ץ��¹Ԥ��Ƥߤޤ��礦. ����ץ��Ÿ�������ǥ��쥯�ȥ��
samples/ �ʲ��ˤ���ޤ�. ((<��ե���󥹥ޥ˥奢��|URL:http://rb-gsl.rubyforge.org/ref.html>))
��į��ĤĤ����ߤΥץ�����¹Ԥ��Ƥߤޤ��礦.

== ������


=== EOF ����
Ruby/SSL2 �����ꥹ����ץȤ򻲹ͤˤ��Ƥ��ޤ�. ��������»�ͤϹ�θ���Ƥޤ���.

((<eof_gsl.rb|URL:eof_gsl.rb>)): GSL::Eigen::symmv(���оι���θ�ͭ�͸�ͭ�٥��ȥ�����) ������
=end
=begin html
<textarea cols="80" rows="10" wrap="hard" class="source">
require "narray"
require "gsl"
 
module Analysis
 
def covariance(x,y)
  if x.length != y.length then
    print "error\n"
    exi
  end

  len = x.length

  sum = x.mul_add(y,0)
  n = len

  return  sum/(n-1)
end

def covariance_matrix( x )
  if x.rank !=2
    raise "covariance_matrix: x.rank must be 2"
  end

  dim = x.shape[0]  
  cov = ( NArray.sfloat(dim, dim).fill!(0.0) ).to_gm
  total = 0

  for i in 0..dim-1
    for j in 0..i
      elm = covariance(x[i,true],x[j,true])
      cov[j, i] = elm  unless i == j
      cov[i, j] = elm 
      total += elm
    end
  end
  return cov, total
end

def eof( x )

  if x.shape.length!=2 then
    print "err\n"
    exit
  end

  dim,nle = x.shape

  p "calc anomary"
  x = x - x.mean(1)

  p "make covariance matrix"
  cova, total = covariance_matrix(x)

  p "calc eigen value"
  val, vec = GSL::Eigen::symmv(cova)
  vec = vec.transpose.to_na

  [val/total,vec]
end

module_function :eof, :covariance_matrix, :covariance

end
</textarea>
=end html
=begin


((<eof.rb|URL:eof.rb>))eof_gsl.rb ��������� Analysis::eof ��Ƥ֥�����ץ�.
=end
=begin html
<textarea cols="80" rows="10" wrap="hard" class="source">
require "narray"
require "gsl"
require "eof_gsl.rb"
require "numru/dcl"

include NumRu

def rand_nmal(m, s, n)

  # �����������
  rnd_std_nmal = NArray.sfloat(n).randomn 
  rnd_nmal     = rnd_std_nmal * s + m 

  return rnd_nmal
end

n = 1000
x = NArray.float(2,n)

a = rand_nmal(0,0.4,n)  # ʿ�� 0, ʬ�� 0.4 ���������
b = rand_nmal(0,0.2,n)  # ʿ�� 0, ʬ�� 0.2 ���������

theta = Math::PI/6
x[0,true] = a*Math::cos(theta) - b*Math::sin(theta)
x[1,true] = a*Math::sin(theta) + b*Math::cos(theta)

val,vec = Analysis.eof(x)

DCL::gropn(1)
DCL::grfrm
DCL::grswnd(-1.5,1.5,-1.5,1.5)
DCL::uspfit
DCL::grstrf
DCL::usdaxs
DCL::sgpmzu(x[0, true],x[1,true],1,1,0.01)
for i in 0..1
  fact = val[i]
  DCL::sgplzu([-vec[0,i]*fact,vec[0,i]*fact],[-vec[1,i]*fact,vec[1,i]*fact],1,3)
end
DCL::grcls
</textarea>
=end html
=begin

eof.rb �μ¹Է��

((:<center><IMG SRC="eof.png"></center>:))
    
=end


