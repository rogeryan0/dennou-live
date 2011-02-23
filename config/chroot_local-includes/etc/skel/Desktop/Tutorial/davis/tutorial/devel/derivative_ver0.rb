require "narray"                                                                                       
                                                                                                       
module NumRu                                                                                           
  module Derivative                                                                                    
    module_function                                                                                    
                                                                                                       
    #<<�⥸�塼�����>>                                                                                
    LINEAR_EXT = 1                                 # �����ͤ��䴰��ˡ(�����䴰)                        
                                                                                                       
    def cderiv(z, x, dim, bc=LINEAR_EXT)           # z: �ǡ���, x: ��, dim                             
      dim += z.rank if dim<0                                                                           
      if dim >= z.rank || dim<0                                                                        
        raise ArgumentError,"dim value (#{dim}) must be smaller than z.rank and >= 0"                  
      end                                                                                              
      if x.rank != 1                                                                                   
        raise ArgumentError,"rank of x (#{x.rank}) must be 1"                                          
      end                                                                                              
                                                                                                       
      # <<�������ĥ>>                                                                                 
      case bc                                                                                          
      when LINEAR_EXT                                                                                  
        ze = b_expand_linear_ext(z,dim)            # �ǡ����� dim �����ܤ�������ĥ.                    
      else                                                                                             
        raise ArgumentError,"unsupported boundary condition #{bc}."                                    
      end                                                                                              
      xe = b_expand_linear_ext(x,0)                # ���ζ�����������ĥ.                               
                                                                                                       
      # <<��ʬ�黻>>                                                                                   
      dz = cdiff(ze,dim)                           # dz[i] = ze[n+1] - ze[n-1]
      dx = cdiff(xe,0)                             # dx[i] = xe[n+1] - xe[n-1]
      if dx.rank != dz.rank                        # ������Υ�󥯤�ǡ��������·����              
        dx = dx.reshape(*([1]*dim + [true] + [1]*(dz.rank-1-dim)))                                     
      end                                                                                              
      dzdx = dz/dx                                 # ��ʬ��׻�                                        
      return dzdx                                                                                      
    end                                                                                                
                                                                                                       
    def b_expand_linear_ext(z,dim)                                                                     
     if z.shape[dim] < 2                                                                               
        raise ArgumentError,"the size of z's #{dim} th dimention (#{z.shape[dim]}) must be >= 2"       
      end                                                                                              
      val0  = z[*([true]*dim +  [0] + [false])]    # �������Ƭ����                                    
      val1  = z[*([true]*dim +  [1] + [false])]    #       ��Ƭ���� 2 ����                             
      valm1 = z[*([true]*dim + [-1] + [false])]    #       �Ǹ���                                      
      valm2 = z[*([true]*dim + [-2] + [false])]    #       �Ǹ������� 2 ����                           
                                                                                                       
      # ������ĥ                                                                                       
      ze = z[*([true]*dim   + [[0,0..(z.shape[dim]-1),0]]  + [false])] # ξü�򤽤줾�� 1 ����åɳ�ĥ 
      ze[*([true]*dim + [0]  + [false])] = 2*val0-val1                 # ��Ƭ�Υ���åɤ��ͤ������䴰  
      ze[*([true]*dim + [-1] + [false])] = 2*valm1-valm2               # �Ǹ���                        
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
