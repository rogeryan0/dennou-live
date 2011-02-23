require "narray"
require "numru/derivative"

include NumRu
include NMath

##############################################################

# << 1 次元配列に対するテスト >>
def test1(x1)                             
  f1 = sin(x1)
  dfdx1 = Derivative::cderiv(f1, x1, 0)   # 計算結果
  dfdx2 = cos(x1)                         # 解析解
  p(dfdx1) if $VERBOSE                    # 冗長モード時のみ表示
  diff = (dfdx1 - dfdx2)[1..-2].abs       # 解析解との差(境界以外)
  err = diff.mean                         # 平均誤差
  print "dfdx - analytic (except boundary): "
  print "[mean] ", err, "\t", "[max] ", diff.max,"\n"
  return err
end

# << 多次元配列に対するテスト >>
def test2
  nx = 21
  x = 2*PI*NArray.float(nx).indgen!/nx
  f = sin(2*PI*NArray.float(nx,nx,nx).indgen!/nx)
  dfdx1 = Derivative::cderiv(f, x, 0)     # 前方からカウント
  dfdx2 = Derivative::cderiv(f, x, -3)    # 後方からカウント
  p(dfdx1) if $VERBOSE
  p diff = (dfdx1 - dfdx2).abs.max        

  # <<引数チェックテスト>>
  begin                                   # 配列のランク外を指定した場合.
    dfdx3 = Derivative::cderiv(f, x, -4)  
  rescue
    print "test exception successful\n"
  end
end

#<< 配列生成 >>
def gen_x(nx)                             # 等間隔グリッド
  2*PI*NArray.float(nx).indgen!/(nx-1)    
end
def gen_x2(nx)                            # 不等間隔グリッド
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
