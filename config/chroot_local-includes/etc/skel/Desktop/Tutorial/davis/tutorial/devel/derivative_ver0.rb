require "narray"                                                                                       
                                                                                                       
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
