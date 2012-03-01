require "narray"                                                                                       

############################################################

=begin
=module NumRu::Derivative 

==todo
* support other boundary conditions.

==Index
* ((<module NumRu::Derivative>))
  * ((<cderiv>))
    * First derivative (center difference use two point.)
  * ((<b_expand_linear_ext>))
    * return array extended boundaries with linear extention.
  * ((<cdiff>))
    * return difference. (center difference)

==module NumRu::Derivative

Module functions of Derivative Operater for NArray.

---cderiv(z, x, dim, bc=LINEAR_EXT)

    Derivate (({z})) respect to (({dim})) th dimension with center difference.
    return an NArray which result of the difference ((<z>)) divided difference
    (({x})) ( in other wards,  (z_{i+1} - z_{i-1}) / (x_{i+1} - x_{i-1}):
    now _{i} represents the suffix of {i} th element in the ((<dim>)) th
    dimension of array. ).

    ARGUMENTS
    * z (NArray): a NArray which you want to derivative.
    * x (NArray): a NArray represents the dimension which derivative respect
      to. z.rank must be 1.
    * dim (Numeric): a Numeric represents the dimention which derivative
      respect to. you can give number count backward (((<dim>))<0), but
      ((<z.rank ＋dim>)) must be > 0.
    * bc (Numeric) : a Numeric which represent boundary condition.
      now only LINEAR_EXT(=1) supported. LINEAR_EXT load
      ((<b_expand_linear_ext>)) which extend boundary with lenear value.

    RETURN VALUE
    * cderiv_data (NArray): (z_{i+1} - z_{i-1}) / (x_{i+1} - x_{i-1})

---b_expand_linear_ext(z, dim)

    expand boundary with linear value. extend array with 1 grid at each
    boundary with ((<dim>)) th dimension, and assign th value which diffrential
    value between a grid short of boundary and boundary grid in original array.
    (on other wards, 2*z_{0}-z_{1} or 2*z_{n-1}-z_{n-2}: now _{i} represents the
     suffix of {i} th element in the ((<dim>)) th dimension of array. ).


    ARGUMENTS
    * z (NArray): a NArray which you want to expand boundary.
    * dim (Numeric): a Numeric represents the dimention which derivative
      respect to. you can give number count backward (((<dim>))<0), but
      ((<z.rank ＋dim>)) must be > 0.

    RETURN VALUE
    * expand_data (NArray):

---cdiff(x, dim)

    Diffrence operater. return an NArray which a difference ((<x>))
    ( in other wards, (x_{i+1} - x_{i-1}): now _{i} represents the suffix of
    {i} th element in the ((<dim>)) th dimension of array. ).

    ARGUMENTS
    * x (NArray): a NArray which you want to get difference.
    * dim (Numeric): a Numeric representing the dimention which derivative
      respect to. you can give number count backward (((<dim>))<0), but
      ((<z.rank ＋dim>)) must be > 0.

    RETURN VALUE
    * cdiff_data (NArray): (x_{i+1} - x_{i-1})

=end
############################################################

                                                                                                       
module NumRu                                                                                           
  module Derivative                                                                                    
    module_function                                                                                    
                                                                                                       
    #<<モジュール定数>>                                                                                
    LINEAR_EXT = 1                                 # 境界値の補完方法(線形補完)                        
                                                                                                       
    def cderiv(z, x, dim, bc=LINEAR_EXT)           # z: データ, x: 軸, dim                             
      dim += z.rank if dim<0                                                                           
      if dim >= z.rank || dim<0                                                                        
        raise ArgumentError,"dim value (#{dim}) must be smaller than z.rank and >= 0"                  
      end                                                                                              
      if x.rank != 1                                                                                   
        raise ArgumentError,"rank of x (#{x.rank}) must be 1"                                          
      end                                                                                              
                                                                                                       
      # <<境界を拡張>>                                                                                 
      case bc                                                                                          
      when LINEAR_EXT                                                                                  
        ze = b_expand_linear_ext(z,dim)            # データの dim 次元目を線形拡張.                    
      else                                                                                             
        raise ArgumentError,"unsupported boundary condition #{bc}."                                    
      end                                                                                              
      xe = b_expand_linear_ext(x,0)                # 軸の境界を線形拡張.                               
                                                                                                       
      # <<差分演算>>                                                                                   
      dz = cdiff(ze,dim)                           # dz[i] = ze[n+1] - ze[n-1]
      dx = cdiff(xe,0)                             # dx[i] = xe[n+1] - xe[n-1]
      if dx.rank != dz.rank                        # 軸配列のランクをデータ配列と揃える              
        dx = dx.reshape(*([1]*dim + [true] + [1]*(dz.rank-1-dim)))                                     
      end                                                                                              
      dzdx = dz/dx                                 # 差分を計算                                        
      return dzdx                                                                                      
    end                                                                                                
                                                                                                       
    def b_expand_linear_ext(z,dim)                                                                     
     if z.shape[dim] < 2                                                                               
        raise ArgumentError,"the size of z's #{dim} th dimention (#{z.shape[dim]}) must be >= 2"       
      end                                                                                              
      val0  = z[*([true]*dim +  [0] + [false])]    # 配列の先頭の値                                    
      val1  = z[*([true]*dim +  [1] + [false])]    #       先頭から 2 番目                             
      valm1 = z[*([true]*dim + [-1] + [false])]    #       最後尾                                      
      valm2 = z[*([true]*dim + [-2] + [false])]    #       最後尾から 2 番目                           
                                                                                                       
      # 境界拡張                                                                                       
      ze = z[*([true]*dim   + [[0,0..(z.shape[dim]-1),0]]  + [false])] # 両端をそれぞれ 1 グリッド拡張 
      ze[*([true]*dim + [0]  + [false])] = 2*val0-val1                 # 先頭のグリッドに値を線形補完  
      ze[*([true]*dim + [-1] + [false])] = 2*valm1-valm2               # 最後尾                        
      return ze                                                                                        
    end                                                                                                
                                                                                                       
    def cdiff(z,dim)                                                                                   
      z1 = z[*([true]*dim   + [2..-1] + [false])]                                                      
      z2 = z[*([true]*dim   + [0..-3] + [false])]                                                      
      cz = z1-z2                                   # cz[i] = z[n+1] - z[n-1]                           
      return cz                                                                                        
    end                                                                                                
                                                                                                       
  end                                                                                                  
end                                                                                                   

################################################################
## << test >>

if $0 == __FILE__

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
    err = diff.mean                         # 平均エラー
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

end
