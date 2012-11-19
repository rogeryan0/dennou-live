require "narray"
require "numru/derivative"

include NumRu
include NMath

##############################################################

# << 1 ����������Ф���ƥ��� >>
def test1(x1)                             
  f1 = sin(x1)
  dfdx1 = Derivative::cderiv(f1, x1, 0)   # �׻����
  dfdx2 = cos(x1)                         # ���ϲ�
  p(dfdx1) if $VERBOSE                    # ��Ĺ�⡼�ɻ��Τ�ɽ��
  diff = (dfdx1 - dfdx2)[1..-2].abs       # ���ϲ�Ȥκ�(�����ʳ�)
  err = diff.mean                         # ʿ�Ѹ�
  print "dfdx - analytic (except boundary): "
  print "[mean] ", err, "\t", "[max] ", diff.max,"\n"
  return err
end

# << ¿����������Ф���ƥ��� >>
def test2
  nx = 21
  x = 2*PI*NArray.float(nx).indgen!/nx
  f = sin(2*PI*NArray.float(nx,nx,nx).indgen!/nx)
  dfdx1 = Derivative::cderiv(f, x, 0)     # �������饫�����
  dfdx2 = Derivative::cderiv(f, x, -3)    # �������饫�����
  p(dfdx1) if $VERBOSE
  p diff = (dfdx1 - dfdx2).abs.max        

  # <<���������å��ƥ���>>
  begin                                   # ����Υ�󥯳�����ꤷ�����.
    dfdx3 = Derivative::cderiv(f, x, -4)  
  rescue
    print "test exception successful\n"
  end
end

#<< �������� >>
def gen_x(nx)                             # ���ֳ֥���å�
  2*PI*NArray.float(nx).indgen!/(nx-1)    
end
def gen_x2(nx)                            # �����ֳ֥���å�
  2*PI*exp(-NArray.float(nx).indgen!/(nx-1))
end

##############################################################

print "**** equally spaced grid ****\n"

print "**** single-D ****\n"
er1 = test1( gen_x(11) )
er2 = test1( gen_x(21) )
print "error change from nx=11->21: ", er2/er1,"\n"

print "**** multi-D ****\n"
test2

print "**** non-uniform grid ****\n"
p 'x(11):',gen_x2(11),'x(21):',gen_x2(21)
er1 = test1( gen_x2(11) )
er2 = test1( gen_x2(21) )
print "error change from nx=11->21: ", er2/er1,"\n"
