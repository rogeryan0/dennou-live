=begin
= ���ݲ��ϥ饤�֥��γ�ȯ��ˡ

�͸�����, ��Ƿ����

�ǽ�����: 2005 ǯ 03 �� 23 ��

((:<hr>:))

== �ܼ�

((:<ol class="contents">:))
<<< index.hindex.rd
((:</ol>:))

((:<hr>:))

== �Ϥ����

�ܥ��塼�ȥꥢ��Ǥ� 
GPhys::EP_Flux �������Ѥ����Ƥ�����ʬ�黻�⥸�塼�������
NArray ����� GPhys �����Ѥ���
Ruby �ˤ�뵤�ݲ��ϥ⥸�塼��γ�ȯ�λ�����
����ɤäƾҲ𤷤Ƥ����ޤ�.

����������˴�Ϣ���뾮��(TIPS)�ؤΥ�󥯤��󼨤��ޤ�.
���ݲ��ϥ饤�֥����������������ʵ��ʤɤ�
�Ҳ𤷤Ƥ��ޤ��Τ�ʻ���ƻ��Ȥ�������.
TIPS ���ܥ��塼�ȥꥢ��κǸ����ˤޤȤ�ƷǺܤ��Ƥ��ޤ�.

�ʤ�, �Ҳ𤷤Ƥ��륳���ɤϾ嵭�⥸�塼��ΰ�����
���塼�ȥꥢ���Ѥ˴�ά��������ΤǤ�. 

== �⥸�塼��λ��ͤ����

�ޤ��⥸�塼��λ��ͤ���ޤ�. 
�����Ǥϰʲ��λ��ͤΥ⥸�塼���������뤳�Ȥˤ��ޤ�.

  * �ǥ���Ⱥ�ɸ�Ǥ���ʬ�黻�⥸�塼��.
  * NArray ���о�
  * ��������᥽�å�
    * cderiv(�����ʬ)
      * ����: �ǡ�������(NArray), ������(NArray), �оݼ���(Numeric)
      * �֤���: ��ʬ�����ǡ�������(NArray)
    
�⥸�塼��ζ���̾�� NumRu::Derivative �Ȥ��ޤ�. NumRu ��
NUMericalRUby ��ά��, ���ͷ׻���Ļ벽��Ϣ�Υ饤�֥���
�����⥸�塼����֤Ǥ�. 

== NArray �Ǥκ���
=== �᥽�åɤ����
  
# ===== �᥽�åɤ����
#+ �᥽�åɤ����

���ͤˤ������äƥ᥽�åɤ�������ޤ�. �ʲ��� NumRu::Derivative �� 0 �ǤǤ�.
�桼�����ƤӽФ��᥽�åɤ� cderiv �Ǥ�. 
Ϳ����줿�����ξü�ζ����� 1 ����åɳ�ĥ�����ͤ������䴰��(b_expand_linear_ext),
�Ĥ��ǥǡ�������, ������������������ʬ��׻����ޤ�(cdiff).
�Ǹ�ˤ����ξ����֤��ޤ�.

((<derivative_ver0.rb|URL:derivative_ver0.rb>)):
((:<textarea cols="105" rows="20" wrap="hard" class="source">:))
01 require "narray"
02
03 module NumRu
04   module Derivative
05     module_function
06
07     #<<�⥸�塼�����>>
08     LINEAR_EXT = 1                                 # �����ͤ��䴰��ˡ(�����䴰)
09
10     def cderiv(z, x, dim, bc=LINEAR_EXT)           
11       dim += z.rank if dim<0
12       if dim >= z.rank || dim<0
13         raise ArgumentError,"dim value (#{dim}) must be smaller than z.rank and >= 0"
14       end
15       if x.rank != 1
16         raise ArgumentError,"rank of x (#{x.rank}) must be 1" 
17       end
18
19       # <<�������ĥ>>
20       case bc
21       when LINEAR_EXT
22         ze = b_expand_linear_ext(z,dim)            # �ǡ����� dim �����ܤ�������ĥ.
23       else
24         raise ArgumentError,"unsupported boundary condition #{bc}."
25       end
26       xe = b_expand_linear_ext(x,0)                # ���ζ�����������ĥ.
27
28       # <<��ʬ�黻>>
29       dz = cdiff(ze,dim)                           # �ǡ����κ�ʬ��׻�
30       dx = cdiff(xe,0)                             # ���κ�ʬ��׻�
31       if dx.rank != dz.rank                        # ��������Υ�󥯤�ǡ��������·����
32         dx = dx.reshape(*([1]*dim + [true] + [1]*(dz.rank-1-dim)))
33       end
34       dzdx = dz/dx                                 # ��ʬ��׻�
35       return dzdx
36     end
37
38     def b_expand_linear_ext(z,dim)
39      if z.shape[dim] < 2
40         raise ArgumentError,"the size of z's #{dim} th dimention (#{z.shape[dim]}) must be >= 2"
41       end
42       val0  = z[*([true]*dim +  [0] + [false])]    # �������Ƭ����
43       val1  = z[*([true]*dim +  [1] + [false])]    #       ��Ƭ���� 2 ����
44       valm1 = z[*([true]*dim + [-1] + [false])]    #       �Ǹ���
45       valm2 = z[*([true]*dim + [-2] + [false])]    #       �Ǹ������� 2 ����
46
47       # ������ĥ & �����䴰
48       ze = z[*([true]*dim   + [[0,0..(z.shape[dim]-1),0]]  + [false])] # ξü�� 1 ����åɳ�ĥ
49       ze[*([true]*dim + [0]  + [false])] = 2*val0-val1                 
50       ze[*([true]*dim + [-1] + [false])] = 2*valm1-valm2               
51       return ze
52     end
53
54     def cdiff(z,dim)
55       z1 = z[*([true]*dim   + [2..-1] + [false])]
56       z2 = z[*([true]*dim   + [0..-3] + [false])]
57       cz = z1-z2                                   # cz[i] = z[n+1] - z[n-1]
58       return cz
59     end
60
61   end
62  end                                                                                                   
((:</textarea>:)) 

((*TIPS*))

((<�㳰���� - �����Υ����å�>)) /
((<����μ����λ�����ˡ>)) /
((<NArray Ʊ�Τ� 2 ��黻>))

=== �ƥ���
# ===== �ƥ���
#+ �ƥ���

�嵭����������᥽�åɤ�ƥ��Ȥ��ޤ�. �ƥ��Ȥ˺ݤ��ưʲ�����������å����뤳�Ȥˤ��ޤ�.

* �黻����
  * ���ϲ�Ȥκ�
  * �ʻ�������¸��
  * �����ֳֳʻҤ��Ф���黻���
* ¿��������ؤ��б�
* ���������å������㳰����

�ʲ��˥ƥ��ȥ�����ץȤ򼨤��ޤ�. �ºݤ���������Ǥϰ���ܤ��Ȥ˥ƥ��Ȥ����, ��ǧ����
�����μ¤Ǥ�. �ޤȤ�Ƽ¹Ԥ���Ȥɤ��˥Х����Ź��ޤ�Ƥ���Τ��狼���
���ʤäƤ��ޤ��ޤ�.

  
((<test_derivative_ver0.rb|URL:test_derivative_ver0.rb>)):
((:<textarea cols="105" rows="20" wrap="hard" class="source">:))
01 require "narray"
02 require "derivative_ver0"
03
04 include NumRu
05 include NMath
06
07 ##############################################################
08
09 # << 1 ����������Ф���ƥ��� >>
10 def test1(x1)
11   f1 = sin(x1)
12   dfdx1 = Derivative::cderiv(f1, x1, 0)   # �׻����
13   dfdx2 = cos(x1)                         # ���ϲ�
14   p(dfdx1) if $VERBOSE                    # ��Ĺ�⡼�ɻ��Τ�ɽ��
15   diff = (dfdx1 - dfdx2)[1..-2].abs       # ���ϲ�Ȥκ�(�����ʳ�)
16   err = diff.mean                         # ʿ�Ѹ�
17   print "dfdx - analytic (except boundary): "
18   print "[mean] ", err, "\t", "[max] ", diff.max,"\n"
19   return err
20 end
21
22 # << ¿����������Ф���ƥ��� >>
23 def test2
24   nx = 21
25   x = 2*PI*NArray.float(nx).indgen!/nx
26   f = sin(2*PI*NArray.float(nx,nx,nx).indgen!/nx)
27   dfdx1 = Derivative::cderiv(f, x, 0)     # �������饫�����
28   dfdx2 = Derivative::cderiv(f, x, -3)    # �������饫�����
29   p(dfdx1) if $VERBOSE
30   p diff = (dfdx1 - dfdx2).abs.max
31
32   # <<���������å��ƥ���>>
33   begin                                   # ����Υ�󥯳�����ꤷ�����.
34     dfdx3 = Derivative::cderiv(f, x, -4)
35   rescue
36     print "\n"
37   end
38 end
39
40 #<< �������� >>
41 def gen_x(nx)                             # ���ֳ֥���å�
42   2*PI*NArray.float(nx).indgen!/(nx-1)
43 end
44 def gen_x2(nx)                            # �����ֳ֥���å�
45   2*PI*exp(-NArray.float(nx).indgen!/(nx-1))
46 end
47
48 ##############################################################
49
50 print "**** equally spaced grid ****\n"
51
52 print "**** single-D ****\n"
53 er1 = test1( gen_x(11) )
54 er2 = test1( gen_x(21) )
55 print "error change from nx=11->21: ", er2/er1,"\n"
56
57 print "**** multi-D ****\n"
58 test2
59
60 print "**** non-uniform grid ****\n"
61 p 'x(11):',gen_x2(11),'x(21):',gen_x2(21)
62 er1 = test1( gen_x2(11) )
63 er2 = test1( gen_x2(21) )
64 print "error change from nx=11->21: ", er2/er1,"\n"
((:</textarea>:)) 

�嵭������ץȤ򥳥ޥ�ɥ饤�󤫤�¹Ԥ���ȥƥ��ȷ�̤����Ϥ���ޤ�.
������, �⥸�塼�����������ե�����(derivative_ver0.rb) �������ȥǥ��쥯�ȥ�ˤʤ��ƤϤʤ�ޤ���.

  % ruby test_derivative_ver0.rb 

  **** equally spaced grid ****
  **** single-D ****
  dfdx - analytic (except boundary): [mean] 0.03922348996 [max] 0.06451071621
  dfdx - analytic (except boundary): [mean] 0.0100170063  [max] 0.01636835692
  error change from nx=11->21: 0.2553828409
  **** multi-D ****
  0.0
  test exception successful
  
  **** non-uniform grid ****
  "x(11):"
  NArray.float(11):
  [ 6.28319, 5.68526, 5.14424, 4.6547, 4.21175, 3.81094, 3.44829, 3.12014, ... ]
  "x(21):"
  NArray.float(21):
  [ 6.28319, 5.97675, 5.68526, 5.40799, 5.14424, 4.89335, 4.6547, 4.42769, ... ]
  dfdx - analytic (except boundary): [mean] 0.01898250849 [max] 0.03216015526
  dfdx - analytic (except boundary): [mean] 0.004934445822        [max] 0.01194461777
  error change from nx=11->21: 0.2599469835

�嵭�η�̤���, �ʻ��������ܤˤʤ�ȸ����� 0.25 �ܤȤʤ�, ���������ʬ�᥽�åɤ�
����Ū�� 2 �����٤Ǥ��뤳�Ȥ��狼��ޤ�. ¿����������Ф���������������Ƥ���,
�����Υ����å����������ʤ���Ƥ��ޤ�.
�ʤ��������ֳֳʻ�������Ф����̤��������Ͼ������Ǥ���,
���ޤ���Ϳ���������������ɤ��ä��ȹͤ����ޤ�.

�ʾ�η�̤���ƥ��Ȥ������Ǥ�. 
�������᥽�åɤ��դ��ä�����Ϥ������٥ƥ��Ȥ��ɲä��Ƥ����ޤ��礦. �ޤ���¸�Υ᥽�åɤ˼��ä������
���碌�ƥƥ��Ȥ�񤭴����ޤ�. �ƥ��Ȥ����ˤ�, ���Ŀ��Ť��Ѥ߽Ťͤ뤳�Ȥ����פǤ�.

((<�⥸�塼��ե�����˥ƥ��Ȥ�������>)) / 
((<��˥åȥƥ��ȥե졼���� ���Ѥ����ƥ���>))

=== �ɥ�����ȵ���
# ===== �ɥ�����ȵ���
#+ �ɥ�����ȵ���

��������᥽�åɤΥƥ��Ȥ�����������ɥ�����Ȥ�񤭤ޤ��礦.
Ruby �Ǥϥ�����ץȥե������������������տޤ������
���줿�ɥ�����ȥե����ޥå� RD ��¸�ߤ��ޤ�.
����⥹����ץȤ� RD �ǥɥ�����Ȥ�������Ǥ��ޤ��ޤ�.
RD �ν񼰤ˤĤ��Ƥ�((<�ɥ�����ơ�������󥯽�|URL:http://www.gfd-dennou.org/arch/ruby/doc-link-j.htm>))
�� RD �ι�Υ�󥯤򻲾Ȥ�������.

�ʲ��˥ɥ�����Ȥ���������������򼨤��ޤ�. �����ǤϱѸ�ǽ񤤤Ƥޤ���, ���ܸ�Ǥ⹽���ޤ���.
(Ruby �桼���δ֤ǤϱѸ�Υɥ�����Ȥ��Ѱդ��Ƥ����Ȥ����Τ�����Τ褦�Ǥ�.)

((<derivative_ver1.rb|URL:derivative_ver1.rb>)):
=end
=begin html
<textarea cols="105" rows="20" wrap="hard" class="source">
001  require "narray"                                                     
002                                                                       
003  ############################################################         
004                                                                        
005  =begin                                                               
006  =module NumRu::Derivative                                            
007                                                                        
008  ==todo                                                               
009  * support other boundary conditions.                                 
010                                                                       
011  ==Index                                                              
012  * ((<module NumRu::Derivative>))                                     
013    * ((<cderiv>))                                                     
014      * First derivative (center difference use two point.)            
015    * ((<b_expand_linear_ext>))                                        
016      * return array extended boundaries with linear extention.        
017    * ((<cdiff>))                                                      
018      * return difference. (center difference)                         
019                                                                       
020  ==module NumRu::Derivative                                           
021                                                                       
022  Module functions of Derivative Operater for NArray.                  
023                                                                       
024  ---cderiv(z, x, dim, bc=LINEAR_EXT)                                  
025                                                                       
026      Derivate (({z})) respect to (({dim})) th dimension with center di
027      return an NArray which result of the difference ((<z>)) divided d
028      (({x})) ( in other wards,  (z_{i+1} - z_{i-1}) / (x_{i+1} - x_{i-
029      now _{i} represents the suffix of {i} th element in the ((<dim>))
030      dimension of array. ).                                           
031                                                                       
032      ARGUMENTS                                                        
033      * z (NArray): a NArray which you want to derivative.             
034      * x (NArray): a NArray represents the dimension which derivative 
035        to. z.rank must be 1.                                          
036      * dim (Numeric): a Numeric represents the dimention which derivat
037        respect to. you can give number count backward (((<dim>))<0), b
038        ((<z.rank ��dim>)) must be > 0.                                
039      * bc (Numeric) : a Numeric which represent boundary condition.   
040        now only LINEAR_EXT(=1) supported. LINEAR_EXT load             
041        ((<b_expand_linear_ext>)) which extend boundary with lenear val
042                                                                       
043      RETURN VALUE                                                     
044      * cderiv_data (NArray): (z_{i+1} - z_{i-1}) / (x_{i+1} - x_{i-1})
045                                                                       
046  ---b_expand_linear_ext(z, dim)                                       
047                                                                       
048      expand boundary with linear value. extend array with 1 grid at ea
049      boundary with ((<dim>)) th dimension, and assign th value which d
050      value between a grid short of boundary and boundary grid in origi
051      (on other wards, 2*z_{0}-z_{1} or 2*z_{n-1}-z_{n-2}: now _{i} rep
052       suffix of {i} th element in the ((<dim>)) th dimension of array.
053                                                                       
054                                                                       
055      ARGUMENTS                                                        
056      * z (NArray): a NArray which you want to expand boundary.        
057      * dim (Numeric): a Numeric represents the dimention which derivat
058        respect to. you can give number count backward (((<dim>))<0), b
059        ((<z.rank ��dim>)) must be > 0.                                
060                                                                       
061      RETURN VALUE                                                     
062      * expand_data (NArray):                                          
063                                                                       
064  ---cdiff(x, dim)                                                     
065                                                                       
066      Diffrence operater. return an NArray which a difference ((<x>))  
067      ( in other wards, (x_{i+1} - x_{i-1}): now _{i} represents the su
068      {i} th element in the ((<dim>)) th dimension of array. ).        
069                                                                       
070      ARGUMENTS                                                        
071      * x (NArray): a NArray which you want to get difference.         
072      * dim (Numeric): a Numeric representing the dimention which deriv
073        respect to. you can give number count backward (((<dim>))<0), b
074        ((<z.rank ��dim>)) must be > 0.                                
075                                                                       
076      RETURN VALUE                                                     
077      * cdiff_data (NArray): (x_{i+1} - x_{i-1})                       
078                                                                       
079  =end                                                                 
080  ############################################################         
081                                                                       
082                                                                       
083  module NumRu                                                         
084    module Derivative                                                  
085      module_function                                                  
086                                                                       
087      #<<�⥸�塼�����>>                                              
088      LINEAR_EXT = 1                                 # �����ͤ��䴰��ˡ
089                                                                       
090      def cderiv(z, x, dim, bc=LINEAR_EXT)           # z: �ǡ���, x: ��
091        dim += z.rank if dim<0                                         
092        if dim >= z.rank || dim<0                                      
093          raise ArgumentError,"dim value (#{dim}) must be smaller than 
094        end                                                            
095        if x.rank != 1                                                 
096          raise ArgumentError,"rank of x (#{x.rank}) must be 1"        
097        end                                                            
098                                                                       
099        # <<�������ĥ>>                                               
100       case bc                                                        
101       when LINEAR_EXT                                                
102         ze = b_expand_linear_ext(z,dim)            # �ǡ����� dim �� 
103       else                                                           
104         raise ArgumentError,"unsupported boundary condition #{bc}."  
105       end                                                            
106       xe = b_expand_linear_ext(x,0)                # ���ζ�����������
107                                                                      
108       # <<��ʬ�黻>>                                                 
109       dz = cdiff(ze,dim)                           # dz[i] = ze[n+1] 
110       dx = cdiff(xe,0)                             # dx[i] = xe[n+1] 
111       if dx.rank != dz.rank                        # ������Υ�󥯤�
112         dx = dx.reshape(*([1]*dim + [true] + [1]*(dz.rank-1-dim)))   
113       end                                                            
114       dzdx = dz/dx                                 # ��ʬ��׻�      
115       return dzdx                                                    
116     end                                                              
117                                                                      
118     def b_expand_linear_ext(z,dim)                                   
119      if z.shape[dim] < 2                                             
120         raise ArgumentError,"the size of z's #{dim} th dimention (#{z
121       end                                                            
122       val0  = z[*([true]*dim +  [0] + [false])]    # �������Ƭ����  
123       val1  = z[*([true]*dim +  [1] + [false])]    #       ��Ƭ���� 2
124       valm1 = z[*([true]*dim + [-1] + [false])]    #       �Ǹ���    
125       valm2 = z[*([true]*dim + [-2] + [false])]    #       �Ǹ�������
126                                                                      
127       # ������ĥ                                                     
128       ze = z[*([true]*dim   + [[0,0..(z.shape[dim]-1),0]]  + [false])
129       ze[*([true]*dim + [0]  + [false])] = 2*val0-val1               
130       ze[*([true]*dim + [-1] + [false])] = 2*valm1-valm2             
131       return ze                                                      
132     end                                                              
133                                                                      
134     def cdiff(z,dim)                                                 
135       z1 = z[*([true]*dim   + [2..-1] + [false])]                    
136       z2 = z[*([true]*dim   + [0..-3] + [false])]                    
137       cz = z1-z2                                   # cz[i] = z[n+1] -
138       return cz                                                      
139     end                                                              
140                                                                      
141   end                                                                
142 end                                                                  
143                                                                      
144 ################################################################     
145 ## << test >>                                                        
146                                                                      
147 if $0 == __FILE__                                                    
148                                                                      
149   include NumRu                                                      
150   include NMath                                                      
151                                                                      
152   ##############################################################     
153                                                                      
154   # << 1 ����������Ф���ƥ��� >>                                   
155   def test1(x1)                                                      
156     f1 = sin(x1)                                                     
157     dfdx1 = Derivative::cderiv(f1, x1, 0)   # �׻����               
158     dfdx2 = cos(x1)                         # ���ϲ�                 
159     p(dfdx1) if $VERBOSE                    # ��Ĺ�⡼�ɻ��Τ�ɽ��   
160     diff = (dfdx1 - dfdx2)[1..-2].abs       # ���ϲ�Ȥκ�(�����ʳ�) 
161     err = diff.mean                         # ʿ�ѥ��顼             
162     print "dfdx - analytic (except boundary): "                      
163     print "[mean] ", err, "\t", "[max] ", diff.max,"\n"              
164     return err                                                       
165   end                                                                
166                                                                      
167   # << ¿����������Ф���ƥ��� >>                                   
168   def test2                                                          
169     nx = 21                                                          
170     x = 2*PI*NArray.float(nx).indgen!/nx                             
171     f = sin(2*PI*NArray.float(nx,nx,nx).indgen!/nx)                  
172     dfdx1 = Derivative::cderiv(f, x, 0)     # �������饫�����       
173     dfdx2 = Derivative::cderiv(f, x, -3)    # �������饫�����       
174     p(dfdx1) if $VERBOSE                                             
175     p diff = (dfdx1 - dfdx2).abs.max                                 
176                                                                      
177     # <<���������å��ƥ���>>                                         
178     begin                                   # ����Υ�󥯳�����ꤷ 
179       dfdx3 = Derivative::cderiv(f, x, -4)                           
180     rescue                                                           
181       print "test exception successful\n"                            
182     end                                                              
183   end                                                                
184                                                                      
185   #<< �������� >>                                                    
186   def gen_x(nx)                             # ���ֳ֥���å�         
187     2*PI*NArray.float(nx).indgen!/(nx-1)                             
188   end                                                                
189   def gen_x2(nx)                            # �����ֳ֥���å�       
190     2*PI*exp(-NArray.float(nx).indgen!/(nx-1))                       
191   end                                                                
192                                                                      
193   ##############################################################     
194                                                                      
195   print "**** equally spaced grid ****\n"                            
196                                                                      
197   print "**** single-D ****\n"                                       
198   er1 = test1( gen_x(11) )                                           
199   er2 = test1( gen_x(21) )                                           
200   print "error change from nx=11->21: ", er2/er1,"\n"                
201                                                                      
202   print "**** multi-D ****\n"                                        
203   test2                                                              
204                                                                      
205   print "**** non-uniform grid ****\n"                               
206   p 'x(11):',gen_x2(11),'x(21):',gen_x2(21)                          
207   er1 = test1( gen_x2(11) )                                          
208   er2 = test1( gen_x2(21) )                                          
209   print "error change from nx=11->21: ", er2/er1,"\n"                
210                                                                      
211 end
</textarea>
=end html
=begin 

���Ĥϰ�ĤΥ᥽�åɤ��������뤴�Ȥ˵��Ҥ��Ƥ������ȤǤ�.
���ޤ��ޤȾ��������դ�­���Ƥޤ�.
�᥽�åɤλ��ͤ��ѹ����뤿�Ӥ˹������Ƥ����Τ���פǤ�.
�⥸�塼�����Τ��������Ƥ���񤳤��Ȼפ��Ȥ��Ȥ����Ѥ��ܤ˲񤤤ޤ�.
(�������Ƥ�˺��Ƥ��ޤ��Ȥ����ʾ�˥���١�����󤬾夬��ޤ���.)

�ʾ���ä� NumRu::Derivative ver 1 �δ����Ǥ�. ����Ͽ������᥽�åɤ��ɲä�����,
��¸�Υ᥽�åɤ���ɤ����ꤷ�ƥ⥸�塼��Υ֥�å��奢�åפ�ޤ�ޤ�.
( �����Ǥ���ʬ�黻�⥸�塼��Ǥ�
cderiv �˲ä��� 3 �����������ٺ�ʬ(threepoint_O2nd_deriv) ��
�������Ƥ��ޤ�. )
��������, �嵭�ι���( �᥽�å����/���� --> �ƥ��� --> �ɥ�����ȵ���) �򷫤��֤��ޤ�.


== GPhys �� �κ���

�ʾ��, ��ʬ�᥽�åɤϤǤ��ޤ���. ������, ���ΤޤޤǤ�, ��ɸ�ȥǡ�����
��ޤȤޤ�ˤʤäƤ��餺, ñ�̤�̾���⤢��ޤ���. ������ GPhys �Ǥ��
��ޤ��礦. GPhys ���֥������Ȥ��оݤ���ʬ��Ԥ�, ��̤�GPhys�Ȥ�����
���ޤ�. ��ʬ����Ȱ��̤�ñ�̤��Ѥ��ޤ��Τ�, ��������ư���������
�褦�ˤ��ޤ��礦. �������뤳�Ȥ�, ���ʵ���Ū�ʥǡ������鼫�ʵ���Ū��
�׻���̤��Ԥ����뤳�Ȥ������褦�ˤʤ�ޤ�. �����, ���� 1-2 �Ԥ�
�ޤˤ��뤳�Ȥ�, �ե�����˼��ʵ���Ū�ʷ��ǽ��Ϥ��뤳�Ȥ�Ǥ��ޤ��Τ�,
�ȤƤ������Ǥ�.

�¸���ˡ�Ȥ��Ƥ�, (1) GPhys �˥᥽�åɤ��ɲä���, (2) �⥸�塼��
�ؿ��ˤ���, �Σ��Ĥ��ͤ����ޤ���, �����Ǥϸ�Ԥ���Ѥ��ޤ�.
���ԤΤۤ��������ʤΤǤ���, ̵�Ǥ˼�ʬ�ǥ᥽�å��ɲä򤹤��, 
GPhys �����դ��Υ᥽�åɤȼ������ɲä����᥽�åɤζ��̤��񤷤�
�ʤ꤬���Ȥ���û��⤢��ޤ�. �ʤ�, ����� GPhys �ܲȤ˼������
�㤤�����Ȥ����Τ�����ޤ�����, �Ѷ�Ū�˺�Ԥޤ�Ϣ���Ƥ�������.
(Ϣ�����((<��Ǿ Ruby �Υ᡼��󥰥ꥹ��|URL:http://dennou-k.gfd-dennou.org/arch/ruby/home-j.htm#mailinglist>))��)

=== �᥽�å����

#+ GPhys �ι�¤������

#* GPhys �� �ǡ�������(VArray)�ȳʻҥ��饹(Grid) ����ʤ�
#  * Grid ���饹�ϼ����饹(Axis) �����
#  * Axis ���饹�� VArray + ������(����) �����

�������� NumRu::Derivative �����Ѥ��� GPhys �� Derivative ��������ޤ�.
��ʬ�黻�Υ��󥸥�� NumRu::Derivative �ˤ�Ǥ���ˤ���,
GPhys �ǤǤ�ʪ���̤�ñ�̤ȼ�̾�ʤɤ�°����α�դ��ޤ�.

((<gphys_derivative_ver0.rb|URL:gphys_derivative_ver0.rb>)):
=end
=begin html
<textarea cols="105" rows="20" wrap="hard" class="source">
01 require 'narray'
02 require 'numru/gphys'
03 require 'derivative_ver1'
04
05 module NumRu
06   class GPhys
07     module Derivative
08
09       module_function
10
11       def cderiv(gp,dim,bc=NumRu::Derivative::LINEAR_EXT)
12         # <<��ʬ���뼡�������(̾�� -> ��)>>
13         if (dim.is_a?(Numeric) && dim < 0)   # ����饫����� -> �����饫�����
14           dim += gp.rank
15         elsif dim.is_a?(String)              # ̾�� -> ��
16           dim = gp.axnames.index(dim)
17         end
18
19         # <<gp ����ʬ>>
20         v_data = gp.data;       v_x = gp.coord(dim)            # �ǡ����ȼ��� VArray �����
21         n_data = v_data.val;    n_x = v_x.val                  #      ::      NArray �����
22         n_dgpdx = NumRu::Derivative::cderiv(n_data,n_x,dim,bc) # ��ʬ���(NArray) �����
23         name = "d#{gp.name}_d#{v_x.name}"                      # �ǡ�����̾�� ( ex. "dT_dx" )
24         v_dgpdx = VArray.new(n_dgpdx, v_data, name)            # ��ʬ��̤� VArray ����
25         g_dgpdx = GPhys.new( gp.grid_copy, v_dgpdx )                        GPhys  ����
26
27         # <<°������>>
28         u_data = v_data.units;  u_x = v_x.units                # �ǡ����ȼ����줾���ñ�̤����
29         g_dgpdx.units = u_data/u_x                             # ��ʬ��̤�ñ��( �ǡ��� / ��)
30         if v_data.get_att('long_name') && v_x.get_att('long_name') # long_name °������
31           long_name = "d_#{v_data.get_att('long_name')}_d_#{v_x.get_att('long_name')}"
32         else
33           long_name = name
34         end
35         g_dgpdx.data.set_att("long_name",long_name)            # ��ʬ���� GPhys �� long_name ������
36         return g_dgpdx
37       end
38
39     end
40   end
41 end
</textarea>
=end html
=begin

��ʬ�黻��ʬ�� 22 ���ܤ����Ǥ�. �ä� 27 ���ܰʹߤϥǡ�����ñ�̤� long_name °���Υ���
��ԤäƤ��ޤ�. 

((* TIPS *))

((<VArray/GPhys �κ����>))

=== �ƥ���(���ϲ���Ѥ���)

GPhys �ǤΥƥ��ȤǤϰʲ���������դ��ޤ�.
�׻������Υ����å��� NArray �Ǥ����ǹԤäƤ��ޤ��Τ�
�����ǤϤ��ޤ���.

* °��, ñ��
  * ��ʬ����Ǥ��줾�����
* �����λ���
  * String/Numeric �����ǻ���Ǥ��뤫

�ʲ��˥ƥ��ȥ�����ץȤ򼨤��ޤ�.
Ĺ���Ǥ���, ���ϳ���ޤἫ�ʵ���Ū�� GPhys �ǡ����򥼥�����Ф���
�뤫��Ǥ�. �ǽ餫�鼫�ʵ���Ū�� NetCDF �� GrADS �ե�������Υǡ���
���ɤ����ʬ��������ʤ�, ��Ū��û���ʤ�ޤ�(������
((<4.3.3 ��|URL:http://www.gfd-dennou.org/arch/ruby/products/gphys/tutorial/ep_flux/#4.3.3>))
����) .


((<test_gphys_derivative_ver0.rb|URL:test_gphys_derivative_ver0.rb>)):
((:<textarea cols="105" rows="20" wrap="hard" class="source">:))
01  require "numru/gphys"
02  require "gphys_derivative_ver0"
03
04  include NumRu
05  include NMath
06
07  #### define method for tests. 
08  ##
09  # genarate narray
10
11  def gen_x(n)
12    2*PI*NArray.sfloat(n).indgen!/(n-1)
13  end
14  def gen_y(n)
15    PI*(NArray.sfloat(n).indgen!/(n-1)-0.5)
16  end
17  def gen_z1(n)
18    NArray.sfloat(n).indgen!/(n-1)
19  end
20
21  def make_gp3D(f,x,y,z)
22    vax = VArray.new( x, 
23		     {"long_name"=>"longitude", "units"=>"rad"},
24		     "lon" )
25    vay = VArray.new( y, 
26		     {"long_name"=>"latitude", "units"=>"rad"},
27		     "lat" )
28    vaz = VArray.new( z, 
29		     {"long_name"=>"altitude", "units"=>"m"},
30		     "z" )
31    axx = Axis.new.set_pos(vax)
32    axy = Axis.new.set_pos(vay)
33    axz = Axis.new.set_pos(vaz)
34    data = VArray.new( f,
35		      {"long_name"=>"temperature", "units"=>"K"},
36		      "t" )
37    return GPhys.new( Grid.new(axx, axy, axz), data)
38  end
39
40  def show_attr(bef_deriv, aft_deriv)
41    fm = "%-15s%-15s%-10s%s"
42    printf(fm, "<attr-name>", "<before>", "<after>", "\n")
43    printf(fm, "name", bef_deriv.data.name, aft_deriv.data.name, "\n")
44    aft_deriv.data.att_names.each{|nm| 
45      printf(fm, nm, bef_deriv.data.get_att(nm).to_s, 
46	             aft_deriv.data.get_att(nm).to_s, "\n")
47    }
48  end
49
50  def test(dim)
51    nx = 10; ny = 10; nz = 10
52    x = gen_x(nx)
53    y = gen_y(ny)
54    z1 = gen_z1(nz)    
55    f = sin(x).reshape(nx,1,1) * sin(y).reshape(1,ny,1) * z1.reshape(1,1,nz)
56    gp = make_gp3D(f, x, y, z1)
57    deriv = GPhys::Derivative::cderiv(gp, dim)
58    dfdx1 = deriv.data.val
59    case dim
60    when 0, -3, "lon"
61      dfdx2 = cos(x).reshape(nx,1,1) * sin(y).reshape(1,ny,1) * z1.reshape(1,1,nz)
62    when 1, -2, "lat"
63      dfdx2 = sin(x).reshape(nx,1,1) * cos(y).reshape(1,ny,1) * z1.reshape(1,1,nz)
64    when 2, -1, "z"
65      dfdx2 = sin(x).reshape(nx,1,1) * sin(y).reshape(1,ny,1) 
66    end
67    p(dfdx1) if $VERBOSE
68    diff = (dfdx1 - dfdx2)[1..-2].abs
69    err = diff.mean
70    print "dfdx - kaiseki_kai (except boundary): "
71    print err, "\t", diff.max,"\n"
72    print "**** check attribute ****\n"
73    show_attr(gp, deriv)
74  end
75
76  ## main routine of test ---------------------------------------------
77
78  
79  print "******** dimname == 'lat' ********\n"
80  test("lat")
81  print "******** dim == 0(lon) ********\n"
82  test(0)
83  print "******** dim == -1(z) ********\n"
84  test(-1)                                                                       
((:</textarea>:))


�嵭������ץȤ򥳥ޥ�ɥ饤�󤫤�¹Ԥ���ȥƥ��ȷ�̤����Ϥ���ޤ�.
derivative_ver0.rb, gphys_derivative_ver0.rb �������ȥǥ��쥯�ȥ�ˤʤ��ƤϤʤ�ޤ���.
�ǽ�ιԤǷٹ𤬽Фޤ���, gphys-0.3.5 �� NumRu::Derivative �����  GPhys::Derivative   
�����˼����ޤ�Ƥ��뤿��Ǥ�.
  
  % ruby test_gphys_derivative_ver0.rb 
  
  ./derivative_ver0.rb:8: warning: already initialized constant LINEAR_EXT
  ******** dimname == 'lat' ********
  dfdx - kaiseki_kai (except boundary): 0.01307030336     0.1701432467
  **** check attribute ****
  <attr-name>    <before>       <after>
  name           t              dt_dlat
  long_name      temperature    d_temperature_d_latitude
  units          K              K.rad-1
  ******** dim == 0(lon) ********
  dfdx - kaiseki_kai (except boundary): 0.01806355861     0.07927459478
  **** check attribute ****
  <attr-name>    <before>       <after>
  name           t              dt_dlon
  long_name      temperature    d_temperature_d_longitude
  units          K              K.rad-1
  ******** dim == -1(z) ********
  dfdx - kaiseki_kai (except boundary): 3.220621713e-08   7.748603821e-07
  **** check attribute ****
  <attr-name>    <before>       <after>
  name           t              dt_dz
  long_name      temperature    d_temperature_d_altitude
  units          K              K.m-1

=== �ƥ���(�ºݤΥǡ������Ѥ���)

�����Ǥ� GPhys �ѥå��������տ魯��ƥ��ȥǡ���(T.jan.nc)���Ѥ���
�ƥ��Ȥ򤷤ޤ��礦. ���Υǡ�����NCEP �Ʋ��Ϥˤ��, ����ε�����1��ε����ͤǤ�.
�ƥ��ȥǡ����ξܺ٤�
((<GPhys �Υ��塼�ȥꥢ��:�ܥ��塼�ȥꥢ����Ѥ���ǡ���|URL:http://www.gfd-dennou.org/arch/ruby/products/gphys/tutorial/body-j.html#h2:data>))
������������.

�ʲ��ƥ��ȥ�����ץȤǤ�.

((<test_gphys_derivative_ver1.rb|URL:test_gphys_derivative_ver1.rb>)):
  01  require "numru/gphys"
  02  require "gphys_derivative_ver0"
  03  
  04  include NumRu
  05  
  06  t = GPhys::IO.open('T.jan.nc','T')      
  07  dtdx = GPhys::Derivative.cderiv(t,'lon')
  08  newfile = NetCDF.create('dtdx.jan.nc')   
  09  GPhys::IO.write(newfile, dtdx)          
  10  newfile.close                           
  
T.jan.nc ����ѿ� "T" �η�����ʬ�� dtdx.jan.nc �Ȥ����ե��������¸���Ƥ��ޤ�.
��ʬ�����ä� 1 ��(05 ����)�Ǥ��뤳�Ȥ����ܤǤ�. ���������
���پ�η�����ʬ���׻��Ǥ��ޤ�. �嵭�Υ�����ץȤ�¹Ԥ����������줿 netCDF �ե��������Ȥ򸫤Ƥߤޤ��礦.

  % ruby test_gphys_derivative_ver1.rb
  % ncdump -h dtdx.jan.nc |less

  tcdf dtdx.jan {
  dimensions:
              lon = 36 ;
              lat = 19 ;
              level = 9 ;
  variables:
              float lon(lon) ;
                      lon:long_name = "Longitude" ;
                      lon:units = "degrees_east" ;
                      lon:actual_range = 0.f, 357.5f ;
              float lat(lat) ;
                      lat:long_name = "Latitude" ;
                      lat:units = "degrees_north" ;
                      lat:actual_range = 90.f, -90.f ;
              float level(level) ;
                      level:GRIB_id = 100s ;
                      level:positive = "down" ;
                      level:long_name = "Level" ;
                      level:units = "millibar" ;
                      level:actual_range = 10.f, 1000.f ;
                      level:GRIB_name = "hPa" ;
              double dT_dlon(level, lat, lon) ;
                      dT_dlon:parent_stat = "Other\n",
          "-" ;
                      dT_dlon:var_desc = "Air Temperature\n",
          "A" ;
                      dT_dlon:missing_value = -9.96921e+36f ;
                      dT_dlon:long_name = "d_Temperature_d_Longitude" ;
                      dT_dlon:least_significant_digit = 0s ;
                      dT_dlon:scale_factor = 1.f ;
                      dT_dlon:units = "degC.degrees_east-1" ;
                      dT_dlon:level_desc = "Multiple levels\n",
          "F" ;
                      dT_dlon:actual_range = -72.66724f, -24.35f ;
                      dT_dlon:dataset = "CDC Derived NCEP Reanalysis Products\n",
          "AC" ;
                      dT_dlon:add_offset = 0.f ;
                      dT_dlon:precision = 0s ;
                      dT_dlon:statistic = "Long Term Mean\n",
          "C" ;
  
  // global attributes:
             :history = "2005-03-11 16:14:03 JST daktu32> NumRu::GPhys::NetCDF_IO.write dT_dlon" ;
  }
    
  
���� gphys ��°�� gpview �ǥ��󥿡��ޤ򸫤Ƥߤޤ��礦.

  % gpview dtdx.jan.nc@dT_dlon

((:<center><A HREF="./dtdx.jan.png"><IMG SRC="dtdx.jan.png" HEIGHT=300 WIDTH=400></A></center>:))

���Τ褦��, ñ�̤ʤɤ�°���Υ����� 1 �ԤǤǤ���褦�ˤʤ�ޤ���.

=== �ɥ�����ȵ���

NArray ��Ʊ�ͤ˥ɥ�����Ȥ�Ĥ��ƴ����Ǥ�.

((<gphys_derivative_ver1.rb|URL:gphys_derivative_ver1.rb>)):
=end
=begin html
<textarea cols="105" rows="20" wrap="hard" class="source">
001 require 'numru/gphys'
002 require 'derivative_ver1'
003 
004 ############################################################
005 
006 =begin
007 =module NumRu::GPhys::Derivative in derivative.rb
008 
009 ==Index
010 * ((<module NumRu::GPhys::Derivative>))
011   * ((<cderiv>))                                                                  
012     * First derivative (using center difference method)                           
013                                                                                   
014 =module NumRu::GPhys::Derivative                                                  
015                                                                                   
016 Module functions of Derivative Operater for GPhys.                                
017
018 ---cderiv(gp, dim_or_dimname, bc=NumRu::Derivative::LINEAR_EXT)                      
019                                                                                       
020     Derivate (({gp})) respect to (({dim})) th or (({dimname})) dimension             
021     with center difference. return a GPhys which result of the difference            
022     ((<gp>)) divided difference axis.                                                
023     ( in other wards,  (z_{i+1} - z_{i-1}) / (x_{i+1} - x_{i-1}): now x is           
024     axis array which you wants to respects to, _{i} represents the suffix            
025     of {i} th element in the ((<dim>)) th dimension of array. ).                     
026                                                                                      
027     ARGUMENTS                                                                        
028     * gp (GPhys): a GPhys which you want to derivative.                              
029     * dim_or_dimname (Numeric or String): a Numeric or String represents             
030       the dimension which derivate respect to. you can give number count             
031       backward (((<dim>))<0), but ((<z.rank ��dim>)) must be > 0.                    
032     * bc (Numeric) : a Numeric which represent boundary condition.                   
033       now only NumRu::Derivative::LINEAR_EXT(=1) supported. LINEAR_EXT load          
034       ((<NumRu::Derivative::b_expand_linear_ext>)) which extend boundary             
035       with linear value.                                                             
036                                                                                      
037     RETURN VALUE                                                                     
038     * a GPhys                                                                        
039                                                                                      
040 =end                                                                                 
041 ############################################################                         
042                                                                                      
043 module NumRu                                                                         
044   class GPhys                                                                         
045     module Derivative                                                                
046                                                                                      
047       module_function                                                                
048                                                                                      
049       def threepoint_O2nd_deriv(gp,dim,bc=NumRu::Derivative::LINEAR_EXT)             
050      # <<get dimention number>>                                                      
051      if (dim.is_a?(Numeric) && dim < 0)                                              
052        dim += gp.rank                                                                
053      elsif dim.is_a?(String)                                                         
054        dim = gp.axnames.index(dim)                                                   
055      end                                                                             
056                                                                                      
057      # <<derivate gp>>                                                               
058      v_data = gp.data;       v_x = gp.coord(dim)            # get varray             
059      n_data = v_data.val;    n_x = v_x.val                  # get narray             
060      n_dgpdx = NumRu::Derivative::threepoint_O2nd_deriv(n_data,n_x,dim,bc)           
061                                                                # derivate n_data     
062         name = "d#{gp.name}_d#{v_x.name}"                      # ex. "dT_dx"         
063      v_dgpdx = VArray.new(n_dgpdx, gp.data, name)           # make varray            
064      g_dgpdx = GPhys.new( gp.grid_copy, v_dgpdx )           # make gphys             
065                                                                                      
066         # <<set attribute>>                                                          
067      u_data = v_data.units;  u_x = v_x.units                # get units              
068      g_dgpdx.units = u_data/u_x                             # set units              
069      if v_data.get_att('long_name') && v_x.get_att('long_name')                      
070        long_name = "d_#{v_data.get_att('long_name')}_d_#{v_x.get_att('long_name')}"  
071      else                                                                            
072        long_name = name                                                              
073      end                                                                             
074      g_dgpdx.data.set_att("long_name",long_name)            # set long_name          
075      return g_dgpdx                                                                  
076       end                                                                            
077                                                                                      
078       def cderiv(gp,dim,bc=NumRu::Derivative::LINEAR_EXT)                            
079      # <<get dimention number>>                                                      
080      if (dim.is_a?(Numeric) && dim < 0)                                              
081        dim += gp.rank                                                                
082      elsif dim.is_a?(String)                                                         
083        dim = gp.axnames.index(dim)                                                   
084      end                                                                             
085                                                                                      
086      # <<derivate gp>>                                                               
087      v_data = gp.data;       v_x = gp.coord(dim)            # get varray             
088      n_data = v_data.val;    n_x = v_x.val                  # get narray             
089      n_dgpdx = NumRu::Derivative::cderiv(n_data,n_x,dim,bc) # derivate n_data        
090         name = "d#{gp.name}_d#{v_x.name}"                      # ex. "dT_dx"         
091      v_dgpdx = VArray.new(n_dgpdx, gp.data, name)           # make varray            
092      g_dgpdx = GPhys.new( gp.grid_copy, v_dgpdx )           # make gphys             
093                                                                                      
094         # <<set attribute>>                                                          
095      u_data = v_data.units;  u_x = v_x.units                # get units              
096      g_dgpdx.units = u_data/u_x                             # set units              
097      if v_data.get_att('long_name') && v_x.get_att('long_name')                      
098        long_name = "d_#{v_data.get_att('long_name')}_d_#{v_x.get_att('long_name')}"  
099      else                                                                            
100        long_name = name                                                              
101      end                                                                             
102      g_dgpdx.data.set_att("long_name",long_name)            # set long_name          
103      return g_dgpdx                                                                  
104       end                                                                            
105                                                                                      
106     end                                                                              
107   end                                                                                
108 end                                                                                  
109                                                                                      
110                                                                                      
111 ######################################################                               
112 ## < test >                                                                          
113 if $0 == __FILE__                                                                    
114                                                                                      
115   include NumRu                                                                      
116   include NMath                                                                      
117                                                                                      
118   #### define method for tests.                                                      
119   ##                                                                                 
120   # genarate narray                                                                  
121                                                                                      
122   def gen_x(n)                                                                       
123     2*PI*NArray.sfloat(n).indgen!/(n-1)                                              
124   end                                                                                
125   def gen_y(n)                                                                       
126     PI*(NArray.sfloat(n).indgen!/(n-1)-0.5)                                          
127   end                                                                                
128   def gen_z1(n)                                                                      
129     NArray.sfloat(n).indgen!/(n-1)                                                   
130   end                                                                                
131                                                                                      
132   def make_gp3D(f,x,y,z)                                                             
133     vax = VArray.new( x,                                                             
134                    {"long_name"=>"longitude", "units"=>"rad"},                       
135                    "lon" )                                                           
136     vay = VArray.new( y,                                                             
137                    {"long_name"=>"latitude", "units"=>"rad"},                        
138                    "lat" )                                                           
139     vaz = VArray.new( z,                                                             
140                    {"long_name"=>"altitude", "units"=>"m"},                          
141                    "z" )                                                             
142     axx = Axis.new.set_pos(vax)                                                      
143     axy = Axis.new.set_pos(vay)                                                      
144     axz = Axis.new.set_pos(vaz)                                                      
145     data = VArray.new( f,                                                            
146                     {"long_name"=>"temperature", "units"=>"K"},                      
147                     "t" )                                                            
148     return GPhys.new( Grid.new(axx, axy, axz), data)                                 
149   end                                                                                
150                                                                                      
151   def show_attr(bef_deriv, aft_deriv)                                                
152     fm = "%-15s%-15s%-10s%s"                                                         
153     printf(fm, "<attr-name>", "<before>", "<after>", "\n")                           
154     printf(fm, "name", bef_deriv.data.name, aft_deriv.data.name, "\n")               
155     aft_deriv.data.att_names.each{|nm|                                               
156       printf(fm, nm, bef_deriv.data.get_att(nm).to_s,                                
157                    aft_deriv.data.get_att(nm).to_s, "\n")                            
158     }                                                                                
159   end                                                                                
160                                                                                      
161   def test(dim)                                                                      
162     nx = 10; ny = 10; nz = 10                                                        
163     x = gen_x(nx)                                                                    
164     y = gen_y(ny)                                                                    
165     z1 = gen_z1(nz)                                                                  
166     f = sin(x).reshape(nx,1,1) * sin(y).reshape(1,ny,1) * z1.reshape(1,1,nz)         
167     gp = make_gp3D(f, x, y, z1)                                                      
168     deriv = GPhys::Derivative::cderiv(gp, dim)                                       
169     dfdx1 = deriv.data.val                                                           
170     case dim                                                                         
171     when 0, -3, "lon"                                                                
172       dfdx2 = cos(x).reshape(nx,1,1) * sin(y).reshape(1,ny,1) * z1.reshape(1,1,nz)   
173     when 1, -2, "lat"                                                                
174       dfdx2 = sin(x).reshape(nx,1,1) * cos(y).reshape(1,ny,1) * z1.reshape(1,1,nz)   
175     when 2, -1, "z"                                                                  
176       dfdx2 = sin(x).reshape(nx,1,1) * sin(y).reshape(1,ny,1)                        
177     end                                                                              
178     p(dfdx1) if $VERBOSE                                                             
179     diff = (dfdx1 - dfdx2)[1..-2].abs                                                
180     err = diff.mean                                                                  
181     print "dfdx - kaiseki_kai (except boundary): "                                   
182     print err, "\t", diff.max,"\n"                                                   
183     print "**** check attribute ****\n"                                              
184     show_attr(gp, deriv)                                                             
185   end                                                                                
186                                                                                      
187   ## main routine of test ---------------------------------------------              
188                                                                                      
189                                                                                      
190   print "******** dimname == 'lat' ********\n"                                       
191   test("lat")                                                                        
192   print "******** dim == 0(lon) ********\n"                                          
193   test(0)                                                                            
194   print "******** dim == -1(z) ********\n"                                           
195   test(-1)                                                                           
196                                                                                      
197 end                                                                                  
</textarea>
=end html
=begin

== ����

�ʾ����ʬ�黻�⥸�塼�뤬�������ޤ���.
�ºݤ� GPhys::EP_Flux ������Ѥ����Ƥ�����ʬ�黻�⥸�塼���
�嵭�� cderiv �˲ä��������ֳֳʻҤ��Ф��� 2 �����٤κ�ʬ��Ϳ����
�᥽�å�(threepoint_O2_deriv) ���ä�äƤ��ޤ���, ����Ū�ˤϾ嵭�Ǽ������Ȥ���Ǥ�.

���ʣ���ʥ饤�֥��κ��������Ū�ˤϤ����ǾҲ𤷤�ή��ǳ�ȯ��ʤ�Ƥ����ޤ�.
�ճ��ȴ�ñ���Ȼפ�줿�ΤǤϤʤ��Ǥ��礦��?
��ʬ�Ǥ�Ǥ�������, �ȴ������ͤϤ��ҿ����ʥ饤�֥���������ƤߤƤ�������.
�����֤��ˤʤ�ޤ���, GPhys �ܲȤ˼�����Ǥ�餤������Τ��Ǥ��ޤ�����
�Ѷ�Ū�˺�Ԥޤ�Ϣ���Ƥ�������.
�ޤ�, �������Ƥ��ʤ��Ƥ⤳��ʤ�κ�äƤޤ�, �Ȥ��� PR ���紿�ޤǤ�.

((:<hr>:))

== (��Ͽ) TIPS

�����Ǥ���ʸ�ǾҲ𤷤��饤�֥�ꥳ��������о줹�뾮��(TIPS)�ˤĤ���
����򤷤ޤ�. ������ TIPS �ϵ��ݲ��ϥ饤�֥�������Τ�������
�����Ǥ��Τ�, ���ҤȤ⻲�ͤˤ��Ƥ�������.

=== �㳰���� - �����Υ����å�

4.2.1 ����󼨤��������ɤγƥ᥽�å����������Ƭ�Ǥϰ����Υ����å����Ԥ��Ƥ��ޤ�.
��������곰�ΰ������Ϥ��줿���ϡ��㳰�פ�夲��Ȥ�����ΤǤ�.
Ruby �Ǥ��㳰������������ͤ��Ȥ߹��ޤ�Ƥ��ޤ�.
�Ȥ߹��ߥ᥽�å� raise ���㳰��ȯ��������̿���, �ʲ��ν񼰤ˤ������äƵ��Ҥ��ޤ�.

  raise �㳰���饹, �ٹ�ʸ(String)

�������㳰���饹�ˤϰ����˴ؤ����㳰���饹(ArgumentError)�ʤɤ�����ޤ�.
���ʤߤ��㳰���饹�Ͼ�ά��ǽ��

  raise �ٹ�ʸ(String)

�Τ褦�ˤ�񤱤ޤ�. ���λ��� RuntimeError ���夬��ޤ�.

�Ȥ�����λ����㳰��󤲤����

  if ��Ｐ
  
    raise HogeError, "herohero"
  
  end

��
  
  raise HogeError, "herohero" if ��Ｐ

�Τ褦�˽񤭤ޤ�.
���ܤ����Τꤿ������((<������|URL:http://ruby.gfd-dennou.org/tutorial/rakuraku/exception.html>))
������������.

=== ����μ����λ�����ˡ
 
4.2.1 ����󼨤��������ɤǤϰʲ��Τ褦������μ����λ���λ�����¿�Ѥ���Ƥ��ޤ�. 
��������� z�� dim �����ܤ���Ƭ���ͤ���Ф���ΤǤ�.

  42       val0  = z[*([true]*dim +  [0] + [false])]      # �������Ƭ����
  
  
����ϰʲ���ɽ����Ʊ���Ǥ�.

           val0  = z[true, tru, true, ..., 0, false]      
                     ^^^^^^^^^^^^^^^
                      true �� dim ��
           
������ true �Ϥ��μ����������������Ƥ�ɽ���Ƥ��ޤ�. 
�ޤ� false ��Ǥ�ոĤ� true ��ɽ����rubber(����)�����פǤ�. 
Array �ɤ�����­������ξ�դ򤯤äĤ�, �ݤ����ϸĿ�ʬ�η����֤��Ǥ��뤳��,
�����ƺǸ�ΰ����� * �ΤĤ�������Ǥ�����, ������Ȥ�Ÿ�����뤳�Ȥ����Ѥ��Ƥޤ�.  
( [dennou-ruby:002026] ����)

������ Ruby ���������Ǥλ���λ����ˤĤ��ƿ���Ƥ����ޤ�. Ruby ���������Ƭ����
0, 1, 2... �ȿ����ޤ�. ����������������� -1, -2, .. �Ȥʤ�ޤ�.

  ary = [2, 3, 5, 9, 10]
  p ary[0]  #=> 2
  p ary[-1] #=> 10  
  p ary[1..-2] #=> [3, 5, 9]

=== NArray Ʊ�Τ� 2 ��黻


NArray Ʊ�Τ� 2 ��黻�Ǥ�Ĺ��1�μ�����Ǥ�դ�Ĺ���˼�ưŪ�˳�ĥ����ޤ�.
������򤪸������ޤ��礦. �ʲ��� [3,3] ������� [3,1] ��������¤�
�黻�Ǥ�. ���ξ�� [3,1] ������� 2 �����ܤϼ�ưŪ��Ĺ�� 3 �˳�ĥ
����ޤ�.

 Narray[ [0,1,2],
         [3,4,5],     +   NArray[ [ 0,100,200 ] ]
         [6,7,8]]

 ---> # �����Ǽ�ưŪ�˳�ĥ
 
 Narray[ [0,1,2],         NArray[ [ 0,100,200 ]
         [3,4,5],     +           [ 0,100,200 ]
         [6,7,8]]                 [ 0,100,200 ] ]
 
 ---> # ���
 Narray[ [0,101,202],     
         [3,104,205],     
         [6,107,208]]     

=== �⥸�塼��ե�����˥ƥ��Ȥ�������

4.2.2 ��Ǽ������ƥ��ȥ�����ץȤϥ⥸�塼�����������ե��������Ω�ˤ��ޤ�����,
û��������ץȤʤ�Х⥸�塼�����ΤΥե�����������������褤�Ǥ��礦.
̵�̤˥ե���������䤵���˺Ѥߤޤ���, �⥸�塼����ѹ��˱����ƥƥ��Ȥ��
��������¹Ԥ���Τ���ñ�Ǥ�.

�ƥ��Ȥ�������ˤϥƥ��ȥ����ɤ�

  if $0 == __FILE__

  end

�Ƕ��ߤޤ�. $0 �� ruby ���󥿥ץ꥿�ˤ�äƼ¹Ԥ���Ƥ���
�ե������ɽ���ѿ�, __FILE__ �ϼ��ȤΥ�����ץȥե������ɽ������Ǥ�.
��������ȥ⥸�塼��ե����뼫�Ȥ�¹Ԥ������Τ�, �ƥ��ȥ����ɤ��¹Ԥ���ޤ�. 
�⥸�塼��ե������ require ���Ƥ�ƥ�����ʬ��̵�뤵��ޤ�.

=== ��˥åȥƥ��ȥե졼���� ���Ѥ����ƥ���

Ruby �ˤϥ�˥åȥƥ��Ȥ�ٱ礹�뤿��Υ��饹 RubyUnit(TEST::Unit) ��¸�ߤ��ޤ�.
���������饤�֥����ä��ΤǤ���, Ruby 1.8 ����ɸ��ź�ե饤�֥��Ȥ���
TEST::Unit �Ȥ������饹�ˤʤ�ޤ���.
�Ŀ�Ū�ʴ��ۤȤ��ƿ��ͷ׻��⥸�塼��Υƥ��ȤˤϤ��ޤ�����ʤ�
(ex. ���٤γ�ǧ�ʤ�) �ȻפäƤ���ΤǤ���, ��̣�Τ����������Ѥ��Ƹ��Ƥ�������.
�����ƿ��ͷ׻������Υƥ��Ȥ���ˡ���Ԥ߽Ф�����, �Ѷ�Ū�˿����ФƤ�������.


* ((<RubyUnit �����ڡ���|URL:http://homepage1.nifty.com/markey/ruby/rubyunit/>))
  * Ruby 1.6 �Ϥ����Ѥ�������  
* ((<Ruby ��ե���󥹥ޥ˥奢�� - TEST::Unit |URL:http://homepage1.nifty.com/markey/ruby/rubyunit/>))
  * (Ruby 1.8 �Ϥ����Ѥ�������)
* ((<Ruby��256�ܻȤ�������� ��ƻ��|URL:http://www.amazon.co.jp/exec/obidos/ASIN/4756136877/ref=pd_rhf_f_2/249-5231327-8410745>))
  * RubyUnit ��ȯ�Ԥ��񤤤�������. ���ƤϤ��Ť�.

=== VArray/GPhys �κ����

4.3.2 ��Ǽ����������ɤ� 24-25 ���ܤǤ���ʬ��̤� NArray �򸵤� VArray, ������ GPhys �������Ƥ��ޤ�.

  24         v_dgpdx = VArray.new(n_dgpdx, v_data, name)            # ��ʬ��̤� VArray ����
  25         g_dgpdx = GPhys.new( gp.grid_copy, v_dgpdx )                        GPhys  ����

VArray �ϴ���Ū�˥ǡ������Τ� NArray ��, ���Υǡ�����̾����°�����
��������Ǥ�. VArray ���饹�Υ��󥹥��󥹤���������ˤ�

  VArray.new( �ǡ�������(NArray), °��(Attribute, Hash, or VArray; ��ά��, �ѿ�̾(String; ��ά��) )

�Τ褦�ˤ��ޤ�. �嵭�Τ褦����������� VArray ��Ϳ�����, ����°
�������ԡ������, ��������������� VArray ���֥�������
�˰����Ѥ���ޤ�. °�����ۤ�����������Ȥ��� Hash ��
�Ȥ����ɤ��Ǥ��礦. �ʤ�, NumRu::Attribute��gphys�Υѥå�������
���������Ƥ��ޤ�(�����ά). ����, �軰��
���Ȥ�˾�ά��ǽ��, �ѿ�̾�� "noname" �Ȥʤ�ޤ���nil��Ϳ���Ƥ�
��ά�������Ȥˤʤ�ޤ�.
�嵭����Ǥϥǡ������Τ���ʬ��̤� NArray ���֥�������, °������ʬ����°��������Ѥ�������
�������� name ���Ѥ��Ƥ��ޤ�.


GPhys �ϥǡ�������(VArray) �ȳʻҾ���(Grid) ����ʤ�ޤ�. 
GPhys ���饹�Υ��󥹥�����������ˤ�

  GPhys.new( �ʻҾ���(Grid), �ǡ�������( VArray or VArrayComposit ) )

�Τ褦�ˤ��ޤ�. �����(25 ����)�Ǥ� 24 ���ܤǹ������� VArray ��ǡ������󤷤�
���� GPhys ���֥������ȤΥ���åɤ򥳥ԡ�����( gp.grid_copy )������ GPhys
���֥������Ȥ��������Ƥ��ޤ�. 

         
=end
