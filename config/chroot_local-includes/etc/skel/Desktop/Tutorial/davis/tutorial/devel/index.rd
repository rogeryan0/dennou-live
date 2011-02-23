=begin
= 気象解析ライブラリの開発方法

塚原大輔, 堀之内武

最終改訂: 2005 年 03 月 23 日

((:<hr>:))

== 目次

((:<ol class="contents">:))
<<< index.hindex.rd
((:</ol>:))

((:<hr>:))

== はじめに

本チュートリアルでは 
GPhys::EP_Flux 内部で用いられている微分演算モジュールを題材に
NArray および GPhys を利用した
Ruby による気象解析モジュールの開発の仕方を
順を追って紹介していきます.

各節の末尾に関連する小技(TIPS)へのリンクを提示します.
気象解析ライブラリを作る上で非常に便利な技などを
紹介していますので併せて参照ください.
TIPS は本チュートリアルの最後尾にまとめて掲載しています.

なお, 紹介しているコードは上記モジュールの一部を
チュートリアル用に簡略化したものです. 

== モジュールの仕様を決める

まずモジュールの仕様を決めます. 
ここでは以下の仕様のモジュールを作成することにします.

  * デカルト座標での微分演算モジュール.
  * NArray を対象
  * 作成するメソッド
    * cderiv(中央差分)
      * 引数: データ配列(NArray), 軸配列(NArray), 対象次元(Numeric)
      * 返り値: 微分したデータ配列(NArray)
    
モジュールの空間名は NumRu::Derivative とします. NumRu は
NUMericalRUby の略で, 数値計算や可視化関連のライブラリを
入れるモジュール空間です. 

== NArray 版の作成
=== メソッドを定義
  
# ===== メソッドを定義
#+ メソッドを定義

仕様にしたがってメソッドを定義します. 以下は NumRu::Derivative の 0 版です.
ユーザが呼び出すメソッドは cderiv です. 
与えられた配列の両端の境界を 1 グリッド拡張して値を線形補完し(b_expand_linear_ext),
ついでデータ配列, 軸配列の双方の中央差分を計算します(cdiff).
最後にそれらの商を返します.

((<derivative_ver0.rb|URL:derivative_ver0.rb>)):
((:<textarea cols="105" rows="20" wrap="hard" class="source">:))
01 require "narray"
02
03 module NumRu
04   module Derivative
05     module_function
06
07     #<<モジュール定数>>
08     LINEAR_EXT = 1                                 # 境界値の補完方法(線形補完)
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
19       # <<境界を拡張>>
20       case bc
21       when LINEAR_EXT
22         ze = b_expand_linear_ext(z,dim)            # データの dim 次元目を線形拡張.
23       else
24         raise ArgumentError,"unsupported boundary condition #{bc}."
25       end
26       xe = b_expand_linear_ext(x,0)                # 軸の境界を線形拡張.
27
28       # <<差分演算>>
29       dz = cdiff(ze,dim)                           # データの差分を計算
30       dx = cdiff(xe,0)                             # 軸の差分を計算
31       if dx.rank != dz.rank                        # 軸の配列のランクをデータ配列と揃える
32         dx = dx.reshape(*([1]*dim + [true] + [1]*(dz.rank-1-dim)))
33       end
34       dzdx = dz/dx                                 # 差分を計算
35       return dzdx
36     end
37
38     def b_expand_linear_ext(z,dim)
39      if z.shape[dim] < 2
40         raise ArgumentError,"the size of z's #{dim} th dimention (#{z.shape[dim]}) must be >= 2"
41       end
42       val0  = z[*([true]*dim +  [0] + [false])]    # 配列の先頭の値
43       val1  = z[*([true]*dim +  [1] + [false])]    #       先頭から 2 番目
44       valm1 = z[*([true]*dim + [-1] + [false])]    #       最後尾
45       valm2 = z[*([true]*dim + [-2] + [false])]    #       最後尾から 2 番目
46
47       # 境界拡張 & 線形補完
48       ze = z[*([true]*dim   + [[0,0..(z.shape[dim]-1),0]]  + [false])] # 両端を 1 グリッド拡張
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

((<例外処理 - 引数のチェック>)) /
((<配列の次元の指定方法>)) /
((<NArray 同士の 2 項演算>))

=== テスト
# ===== テスト
#+ テスト

上記で定義したメソッドをテストします. テストに際して以下の点をチェックすることにします.

* 演算精度
  * 解析解との差
  * 格子点数依存性
  * 不等間隔格子に対する演算結果
* 多次元配列への対応
* 引数チェック時の例外処理

以下にテストスクリプトを示します. 実際の製作過程では一項目ごとにテストを定義, 確認する
方が確実です. まとめて実行するとどこにバグが仕込まれているのかわからな
くなってしまいます.

  
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
09 # << 1 次元配列に対するテスト >>
10 def test1(x1)
11   f1 = sin(x1)
12   dfdx1 = Derivative::cderiv(f1, x1, 0)   # 計算結果
13   dfdx2 = cos(x1)                         # 解析解
14   p(dfdx1) if $VERBOSE                    # 冗長モード時のみ表示
15   diff = (dfdx1 - dfdx2)[1..-2].abs       # 解析解との差(境界以外)
16   err = diff.mean                         # 平均誤差
17   print "dfdx - analytic (except boundary): "
18   print "[mean] ", err, "\t", "[max] ", diff.max,"\n"
19   return err
20 end
21
22 # << 多次元配列に対するテスト >>
23 def test2
24   nx = 21
25   x = 2*PI*NArray.float(nx).indgen!/nx
26   f = sin(2*PI*NArray.float(nx,nx,nx).indgen!/nx)
27   dfdx1 = Derivative::cderiv(f, x, 0)     # 前方からカウント
28   dfdx2 = Derivative::cderiv(f, x, -3)    # 後方からカウント
29   p(dfdx1) if $VERBOSE
30   p diff = (dfdx1 - dfdx2).abs.max
31
32   # <<引数チェックテスト>>
33   begin                                   # 配列のランク外を指定した場合.
34     dfdx3 = Derivative::cderiv(f, x, -4)
35   rescue
36     print "\n"
37   end
38 end
39
40 #<< 配列生成 >>
41 def gen_x(nx)                             # 等間隔グリッド
42   2*PI*NArray.float(nx).indgen!/(nx-1)
43 end
44 def gen_x2(nx)                            # 不等間隔グリッド
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

上記スクリプトをコマンドラインから実行するとテスト結果が出力されます.
ただし, モジュールを定義したファイル(derivative_ver0.rb) がカレントディレクトリになくてはなりません.

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

上記の結果から, 格子点数が倍になると誤差は約 0.25 倍となり, 定義した微分メソッドは
基本的に 2 次精度であることがわかります. 多次元配列に対する処理も成功しており,
引数のチェックも正しくなされています.
なぜか不等間隔格子配列に対する結果の方が誤差は小さいですが,
たまたま与えた配列が相性が良かったと考えられます.

以上の結果からテストは成功です. 
新しいメソッドを付け加える場合はその都度テストを追加していきましょう. また既存のメソッドに手を加えた場合
あわせてテストも書き換えます. テストは頻繁に, かつ慎重に積み重ねることが重要です.

((<モジュールファイルにテストを埋め込む>)) / 
((<ユニットテストフレームワーク を用いたテスト>))

=== ドキュメント記述
# ===== ドキュメント記述
#+ ドキュメント記述

定義したメソッドのテストが成功したらドキュメントを書きましょう.
Ruby ではスクリプトファイルの中に埋め込む事を意図して定義
されたドキュメントフォーマット RD が存在します.
今回もスクリプトに RD でドキュメントを埋め込んでしまいます.
RD の書式については((<ドキュメンテーションリンク集|URL:http://www.gfd-dennou.org/arch/ruby/doc-link-j.htm>))
の RD の項のリンクを参照ください.

以下にドキュメントを埋め込んだソースを示します. ここでは英語で書いてますが, 日本語でも構いません.
(Ruby ユーザの間では英語のドキュメントを用意しておくというのが通例のようです.)

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
038        ((<z.rank ＋dim>)) must be > 0.                                
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
059        ((<z.rank ＋dim>)) must be > 0.                                
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
074        ((<z.rank ＋dim>)) must be > 0.                                
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
087      #<<モジュール定数>>                                              
088      LINEAR_EXT = 1                                 # 境界値の補完方法
089                                                                       
090      def cderiv(z, x, dim, bc=LINEAR_EXT)           # z: データ, x: 軸
091        dim += z.rank if dim<0                                         
092        if dim >= z.rank || dim<0                                      
093          raise ArgumentError,"dim value (#{dim}) must be smaller than 
094        end                                                            
095        if x.rank != 1                                                 
096          raise ArgumentError,"rank of x (#{x.rank}) must be 1"        
097        end                                                            
098                                                                       
099        # <<境界を拡張>>                                               
100       case bc                                                        
101       when LINEAR_EXT                                                
102         ze = b_expand_linear_ext(z,dim)            # データの dim 次 
103       else                                                           
104         raise ArgumentError,"unsupported boundary condition #{bc}."  
105       end                                                            
106       xe = b_expand_linear_ext(x,0)                # 軸の境界を線形拡
107                                                                      
108       # <<差分演算>>                                                 
109       dz = cdiff(ze,dim)                           # dz[i] = ze[n+1] 
110       dx = cdiff(xe,0)                             # dx[i] = xe[n+1] 
111       if dx.rank != dz.rank                        # 軸配列のランクを
112         dx = dx.reshape(*([1]*dim + [true] + [1]*(dz.rank-1-dim)))   
113       end                                                            
114       dzdx = dz/dx                                 # 差分を計算      
115       return dzdx                                                    
116     end                                                              
117                                                                      
118     def b_expand_linear_ext(z,dim)                                   
119      if z.shape[dim] < 2                                             
120         raise ArgumentError,"the size of z's #{dim} th dimention (#{z
121       end                                                            
122       val0  = z[*([true]*dim +  [0] + [false])]    # 配列の先頭の値  
123       val1  = z[*([true]*dim +  [1] + [false])]    #       先頭から 2
124       valm1 = z[*([true]*dim + [-1] + [false])]    #       最後尾    
125       valm2 = z[*([true]*dim + [-2] + [false])]    #       最後尾から
126                                                                      
127       # 境界拡張                                                     
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
154   # << 1 次元配列に対するテスト >>                                   
155   def test1(x1)                                                      
156     f1 = sin(x1)                                                     
157     dfdx1 = Derivative::cderiv(f1, x1, 0)   # 計算結果               
158     dfdx2 = cos(x1)                         # 解析解                 
159     p(dfdx1) if $VERBOSE                    # 冗長モード時のみ表示   
160     diff = (dfdx1 - dfdx2)[1..-2].abs       # 解析解との差(境界以外) 
161     err = diff.mean                         # 平均エラー             
162     print "dfdx - analytic (except boundary): "                      
163     print "[mean] ", err, "\t", "[max] ", diff.max,"\n"              
164     return err                                                       
165   end                                                                
166                                                                      
167   # << 多次元配列に対するテスト >>                                   
168   def test2                                                          
169     nx = 21                                                          
170     x = 2*PI*NArray.float(nx).indgen!/nx                             
171     f = sin(2*PI*NArray.float(nx,nx,nx).indgen!/nx)                  
172     dfdx1 = Derivative::cderiv(f, x, 0)     # 前方からカウント       
173     dfdx2 = Derivative::cderiv(f, x, -3)    # 後方からカウント       
174     p(dfdx1) if $VERBOSE                                             
175     p diff = (dfdx1 - dfdx2).abs.max                                 
176                                                                      
177     # <<引数チェックテスト>>                                         
178     begin                                   # 配列のランク外を指定し 
179       dfdx3 = Derivative::cderiv(f, x, -4)                           
180     rescue                                                           
181       print "test exception successful\n"                            
182     end                                                              
183   end                                                                
184                                                                      
185   #<< 配列生成 >>                                                    
186   def gen_x(nx)                             # 等間隔グリッド         
187     2*PI*NArray.float(nx).indgen!/(nx-1)                             
188   end                                                                
189   def gen_x2(nx)                            # 不等間隔グリッド       
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

コツは一つのメソッドが完成するごとに記述していくことです.
ちまちまと少しずつ付け足してます.
メソッドの仕様を変更するたびに更新していくのも重要です.
モジュール全体が完成してから書こうと思うとあとで大変な目に会います.
(更新内容を忘れてしまうという以上にモチベーションが上がりません.)

以上をもって NumRu::Derivative ver 1 の完成です. 今後は新しいメソッドを追加したリ,
既存のメソッドを改良したりしてモジュールのブラッシュアップを図ります.
( 製品版の微分演算モジュールでは
cderiv に加えて 3 点利用二次精度差分(threepoint_O2nd_deriv) が
定義されています. )
その都度, 上記の工程( メソッド定義/改良 --> テスト --> ドキュメント記述) を繰り返します.


== GPhys 版 の作成

以上で, 微分メソッドはできました. しかし, このままでは, 座標とデータは
一まとまりになっておらず, 単位も名前もありません. そこで GPhys 版を作
りましょう. GPhys オブジェクトを対象に微分を行い, 結果もGPhysとして返
します. 微分すると一般に単位も変わりますので, 正しく自動更新される
ようにしましょう. こうすることで, 自己記述的なデータから自己記述的な
計算結果を一行で得ることが出来るようになります. すると, あと 1-2 行で
図にすることも, ファイルに自己記述的な形で出力することもできますので,
とても便利です.

実現方法としては, (1) GPhys にメソッドを追加する, (2) モジュール
関数にする, の２つが考えられますが, ここでは後者を採用します.
前者のほうが自然なのですが, 無闇に自分でメソッド追加をすると, 
GPhys 備え付けのメソッドと自前で追加したメソッドの区別が難しく
なりがちという短所もあります. なお, これは GPhys 本家に取り込んで
貰いたいというのが出来ましたら, 積極的に作者まで連絡してください.
(連絡先は((<電脳 Ruby のメーリングリスト|URL:http://dennou-k.gfd-dennou.org/arch/ruby/home-j.htm#mailinglist>))へ)

=== メソッド定義

#+ GPhys の構造の復習

#* GPhys は データ配列(VArray)と格子クラス(Grid) からなる
#  * Grid クラスは軸クラス(Axis) を持つ
#  * Axis クラスは VArray + 軸情報(配置) を持つ

作成した NumRu::Derivative を利用して GPhys 版 Derivative を作成します.
微分演算のエンジンは NumRu::Derivative にお任せにして,
GPhys 版では物理量の単位と軸名などの属性に留意します.

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
12         # <<微分する次元を解析(名前 -> 数)>>
13         if (dim.is_a?(Numeric) && dim < 0)   # 後ろからカウント -> 前からカウント
14           dim += gp.rank
15         elsif dim.is_a?(String)              # 名前 -> 数
16           dim = gp.axnames.index(dim)
17         end
18
19         # <<gp の微分>>
20         v_data = gp.data;       v_x = gp.coord(dim)            # データと軸の VArray を取得
21         n_data = v_data.val;    n_x = v_x.val                  #      ::      NArray を取得
22         n_dgpdx = NumRu::Derivative::cderiv(n_data,n_x,dim,bc) # 微分結果(NArray) を取得
23         name = "d#{gp.name}_d#{v_x.name}"                      # データの名前 ( ex. "dT_dx" )
24         v_dgpdx = VArray.new(n_dgpdx, v_data, name)            # 微分結果の VArray を構成
25         g_dgpdx = GPhys.new( gp.grid_copy, v_dgpdx )                        GPhys  を構成
26
27         # <<属性設定>>
28         u_data = v_data.units;  u_x = v_x.units                # データと軸それぞれの単位を取得
29         g_dgpdx.units = u_data/u_x                             # 微分結果の単位( データ / 軸)
30         if v_data.get_att('long_name') && v_x.get_att('long_name') # long_name 属性設定
31           long_name = "d_#{v_data.get_att('long_name')}_d_#{v_x.get_att('long_name')}"
32         else
33           long_name = name
34         end
35         g_dgpdx.data.set_att("long_name",long_name)            # 微分した GPhys に long_name を設定
36         return g_dgpdx
37       end
38
39     end
40   end
41 end
</textarea>
=end html
=begin

微分演算部分は 22 行目だけです. 特に 27 行目以降はデータの単位と long_name 属性のケア
を行っています. 

((* TIPS *))

((<VArray/GPhys の作り方>))

=== テスト(解析解を用いて)

GPhys 版のテストでは以下の点に注意します.
計算誤差等のチェックは NArray 版の方で行っていますので
ここではしません.

* 属性, 単位
  * 微分前後でそれぞれ比較
* 次元の指定
  * String/Numeric 双方で指定できるか

以下にテストスクリプトを示します.
長いですが, 解析階も含め自己記述的な GPhys データをゼロから作り出して
るからです. 最初から自己記述的な NetCDF や GrADS ファイル中のデータ
を読んで微分するだけなら, 劇的に短くなります(下記の
((<4.3.3 節|URL:http://www.gfd-dennou.org/arch/ruby/products/gphys/tutorial/ep_flux/#4.3.3>))
参照) .


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


上記スクリプトをコマンドラインから実行するとテスト結果が出力されます.
derivative_ver0.rb, gphys_derivative_ver0.rb がカレントディレクトリになくてはなりません.
最初の行で警告が出ますが, gphys-0.3.5 に NumRu::Derivative および  GPhys::Derivative   
が既に取り込まれているためです.
  
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

=== テスト(実際のデータを用いて)

ここでは GPhys パッケージに付随するテストデータ(T.jan.nc)を用いて
テストをしましょう. このデータはNCEP 再解析による, 全球の気温の1月の気候値です.
テストデータの詳細は
((<GPhys のチュートリアル:本チュートリアルで用いるデータ|URL:http://www.gfd-dennou.org/arch/ruby/products/gphys/tutorial/body-j.html#h2:data>))
をご覧ください.

以下テストスクリプトです.

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
  
T.jan.nc 中の変数 "T" の経度微分を dtdx.jan.nc というファイルに保存しています.
微分がたった 1 行(05 行目)であることに注目です. これだけで
温度場の経度微分が計算できます. 上記のスクリプトを実行して生成された netCDF ファイルの中身を見てみましょう.

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
    
  
更に gphys 付属の gpview でコンター図を見てみましょう.

  % gpview dtdx.jan.nc@dT_dlon

((:<center><A HREF="./dtdx.jan.png"><IMG SRC="dtdx.jan.png" HEIGHT=300 WIDTH=400></A></center>:))

このように, 単位などの属性のケアも 1 行でできるようになりました.

=== ドキュメント記述

NArray 版同様にドキュメントをつけて完成です.

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
031       backward (((<dim>))<0), but ((<z.rank ＋dim>)) must be > 0.                    
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

== 完成

以上で微分演算モジュールが完成しました.
実際に GPhys::EP_Flux の中で用いられている微分演算モジュールは
上記の cderiv に加えて不等間隔格子に対して 2 次精度の差分を与える
メソッド(threepoint_O2_deriv) が加わっていますが, 基本的には上記で示したとおりです.

より複雑なライブラリの作成も基本的にはここで紹介した流れで開発を進めていきます.
意外と簡単だと思われたのではないでしょうか?
自分でもできそうだ, と感じた人はぜひ新たなライブラリを作成してみてください.
繰り返しになりますが, GPhys 本家に取り込んでもらいたいものができましたら
積極的に作者まで連絡してください.
また, 完成していなくてもこんなもの作ってます, という PR も大歓迎です.

((:<hr>:))

== (付録) TIPS

ここでは本文で紹介したライブラリコード中で登場する小技(TIPS)について
解説をします. これらの TIPS は気象解析ライブラリを作成知る上で非常に
便利ですので, ぜひとも参考にしてください.

=== 例外処理 - 引数のチェック

4.2.1 節で提示したコードの各メソッド定義部の先頭では引数のチェックが行われています.
これは想定外の引数が渡された場合は「例外」を上げるというものです.
Ruby では例外処理が言語仕様に組み込まれています.
組み込みメソッド raise は例外を発生させる命令で, 以下の書式にしたがって記述します.

  raise 例外クラス, 警告文(String)

ここで例外クラスには引数に関する例外クラス(ArgumentError)などを入れます.
ちなみに例外クラスは省略可能で

  raise 警告文(String)

のようにも書けます. この時は RuntimeError が上がります.

とある条件の時に例外を挙げる場合は

  if 条件式
  
    raise HogeError, "herohero"
  
  end

や
  
  raise HogeError, "herohero" if 条件式

のように書きます.
より詳しく知りたい方は((<こちら|URL:http://ruby.gfd-dennou.org/tutorial/rakuraku/exception.html>))
をご覧ください.

=== 配列の次元の指定方法
 
4.2.1 節で提示したコードでは以下のような配列の次元の指定の仕方が多用されています. 
これは配列 zの dim 次元目の先頭の値を取り出すものです.

  42       val0  = z[*([true]*dim +  [0] + [false])]      # 配列の先頭の値
  
  
これは以下の表記と同じです.

           val0  = z[true, tru, true, ..., 0, false]      
                     ^^^^^^^^^^^^^^^
                      true が dim 個
           
ここで true はその次元の配列要素全てを表しています. 
また false は任意個の true を表す「rubber(ゴム)次元」です. 
Array どうしの足し算は両辺をくっつけ, 掛け算は個数分の繰り返しであること,
そして最後の引数が * のついた配列である場合, その中身を展開することを利用してます.  
( [dennou-ruby:002026] 参照)

ここで Ruby の配列要素の指定の仕方について触れておきます. Ruby の配列は先頭から
0, 1, 2... と数えます. 一方後ろから数える場合は -1, -2, .. となります.

  ary = [2, 3, 5, 9, 10]
  p ary[0]  #=> 2
  p ary[-1] #=> 10  
  p ary[1..-2] #=> [3, 5, 9]

=== NArray 同士の 2 項演算


NArray 同士の 2 項演算では長さ1の次元は任意の長さに自動的に拡張されます.
具体例をお見せしましょう. 以下は [3,3] の配列と [3,1] の配列の和の
演算です. この場合 [3,1] の配列の 2 次元目は自動的に長さ 3 に拡張
されます.

 Narray[ [0,1,2],
         [3,4,5],     +   NArray[ [ 0,100,200 ] ]
         [6,7,8]]

 ---> # 内部で自動的に拡張
 
 Narray[ [0,1,2],         NArray[ [ 0,100,200 ]
         [3,4,5],     +           [ 0,100,200 ]
         [6,7,8]]                 [ 0,100,200 ] ]
 
 ---> # 結果
 Narray[ [0,101,202],     
         [3,104,205],     
         [6,107,208]]     

=== モジュールファイルにテストを埋め込む

4.2.2 節で示したテストスクリプトはモジュールを定義したファイルと独立にしましたが,
短いスクリプトならばモジュール本体のファイルに埋め込む方がよいでしょう.
無駄にファイルを増やさずに済みますし, モジュールの変更に応じてテストを書き
換えたり実行するのが簡単です.

テストを埋め込むにはテストコードを

  if $0 == __FILE__

  end

で挟みます. $0 は ruby インタプリタによって実行されている
ファイルを表す変数, __FILE__ は自身のスクリプトファイルを表す定数です.
こうするとモジュールファイル自身を実行した場合のみ, テストコードが実行されます. 
モジュールファイルを require してもテスト部分は無視されます.

=== ユニットテストフレームワーク を用いたテスト

Ruby にはユニットテストを支援するためのクラス RubyUnit(TEST::Unit) が存在します.
元々外部ライブラリだったのですが, Ruby 1.8 から標準添付ライブラリとして
TEST::Unit というクラスになりました.
個人的な感想として数値計算モジュールのテストにはあまり向かない
(ex. 精度の確認など) と思っているのですが, 興味のある方は利用して見てください.
そして数値計算向けのテストの方法を編み出した方, 積極的に申し出てください.


* ((<RubyUnit 公式ページ|URL:http://homepage1.nifty.com/markey/ruby/rubyunit/>))
  * Ruby 1.6 系を利用の方向け  
* ((<Ruby リファレンスマニュアル - TEST::Unit |URL:http://homepage1.nifty.com/markey/ruby/rubyunit/>))
  * (Ruby 1.8 系を利用の方向け)
* ((<Rubyを256倍使うための本 極道編|URL:http://www.amazon.co.jp/exec/obidos/ASIN/4756136877/ref=pd_rhf_f_2/249-5231327-8410745>))
  * RubyUnit 開発者が書いた解説本. 内容はやや古い.

=== VArray/GPhys の作り方

4.3.2 節で示したコードの 24-25 行目では微分結果の NArray を元に VArray, そして GPhys を構成しています.

  24         v_dgpdx = VArray.new(n_dgpdx, v_data, name)            # 微分結果の VArray を構成
  25         g_dgpdx = GPhys.new( gp.grid_copy, v_dgpdx )                        GPhys  を構成

VArray は基本的にデータ本体の NArray に, そのデータの名前と属性を加
えた配列です. VArray クラスのインスタンスを生成するには

  VArray.new( データ本体(NArray), 属性(Attribute, Hash, or VArray; 省略可, 変数名(String; 省略可) )

のようにします. 上記のように第二引数に VArray を与えると, その属
性がコピーされて, 新たに生成される VArray オブジェクト
に引き継がれます. 属性を陽に定義したいときは Hash を
使うと良いでしょう. なお, NumRu::Attributeはgphysのパッケージ内
で定義されています(解説は略). 第二, 第三引
数ともに省略可能で, 変数名は "noname" となります。nilを与えても
省略したことになります.
上記の例ではデータ本体を微分結果の NArray オブジェクト, 属性は微分前の属性を引き継ぎ新たに
生成した name を用いています.


GPhys はデータ配列(VArray) と格子情報(Grid) からなります. 
GPhys クラスのインスタンス生成するには

  GPhys.new( 格子情報(Grid), データ配列( VArray or VArrayComposit ) )

のようにします. 先の例(25 行目)では 24 行目で構成した VArray をデータ配列して
元の GPhys オブジェクトのグリッドをコピーして( gp.grid_copy )新たな GPhys
オブジェクトを生成しています. 

         
=end
