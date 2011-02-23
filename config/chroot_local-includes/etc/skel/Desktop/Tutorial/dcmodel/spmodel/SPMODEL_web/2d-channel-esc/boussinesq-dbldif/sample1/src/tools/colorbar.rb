=begin
表題  カラーバー作成 ruby ライブラリ
履歴  2004/05/27  小高正嗣

使い方

$cbar_conf = {
      "levels"     => nil,   # カラーバーのトーンレベル
      "colors"     => nil,   # カラーバーのトーンパターン
      "vx0"=>0.1,            # カラーバーの左下角の x 座標
      "vy0"=>0.04,           # カラーバーの左下角の y 座標
      "vxlength"=>0.31,      # カラーバーの 横の長さ
      "vylength"=>0.02,      # カラーバーの 縦の長さ
      "label_size"=>0.007,   # カラーバーのラベル文字の大きさ
      "tick1"      => 1,     # 目盛り(大)をつける間隔
      "tick2"      => 2,     # 目盛り(小)をつける間隔
      "log"        => false, # ログスケールにするか否か
      "eqlev"      => false, # 不等間隔のトーンレベルを指定したときに,
                             # 色分の間隔を等間隔にするかどうか 
      "nobound"    => false, # 両端を数字ではなく, ∞, -∞ とするかどうか.
                             # 0 , 1, 2 を入れる. それぞれどうなるかは...忘れました...
      "labels"     => nil    # ラベルをつけるか否か
}

=end 
module NumRu
  module DCL
    module Util

      Color_bar_opts = {
      "levels"     => nil,
      "colors"     => nil,
# set by YAMADA.YU at 2004-02-17 start
#      "vxlength"=>0.31,
#      "vylength"=>0.02, 
#      "label_size"=>0.007, 
#      "vx0"=>0.1,
#      "vy0"=>0.04,
      "vx0"=>0.05,
      "vy0"=>0.03,
      "label_size"=>0.01, 
      "vxlength"=>0.55, 
      "vylength"=>0.02, 
# set by YAMADA.YU end
#      "vx0"        => 0.8,
#      "vy0"        => 0.02,
#      "vxlength"   => 0.18,
#      "vylength"   => 0.02,
      "portrait"   => false,
      "tick1"      => 20,
      "tick2"      => 1,
#      "label_size" => 0.01,
      "log"        => false,
      # added by daktu32 at 2003-12-26
      "eqlev"      => false,
      "nobound"    => false,
      # added by daktu32 at 2004-01-14
      "labels"     => nil
      }

      def color_bar_options
	p Color_bar_opts
      end
      module_function :color_bar_options

      def color_bar(options={},default=Color_bar_opts)

	if options.type!=Hash then
	  print "options must be Hash\n"
	  exit
	end
	options.each_key{|key|
	  if !default.member?(key) then
	    print key," option is not exist\n"
	    exit
	  end
	}
	options = default.dup.update(options)

	levels = options['levels']
	colors = options['colors']

	if (levels.nil? && !colors.nil?) || (!levels.nil? && colors.nil?) then
	  print "levels and colors must set at same time\n"
	  exit
	end

	portrait  = options["portrait"]

	# added by daktu32 at 2004-01-14
	srclabel = options["labels"]
	if srclabel then
	  labels = Array.new
	  srclabel.to_a.each_index do |i| labels[i] = srclabel[i].to_s end
	end

	miss = DCL::glrget("rmiss")
	
	if !levels.nil? then
	  
	  nlevel = levels.length
	  if levels.type==Array then levels = NArray.to_na(levels) end
	  if colors.length != (nlevel-1) then
	    print "colors.length must be equal levels.length - 1\n"
	    exit
	  end
	  

	  #begin <--  make each int by daktu32 in 2003-12-26   --> 
	  if options["eqlev"] then
	    src_lev = levels.dup
            min = levels[levels.ne(miss).where].min
	    max = levels[levels.ne(miss).where].max
	    for i in 0..nlevel-1
	      levels[i] = min + (max-min).to_f/(nlevel-1)*i
	    end
	  end
	  #end <--  make each int by daktu32 in 2003-12-26   --> 
	  #	  DCL::ueitlv #make comment by daktu32 
	  DCL::uestln(levels,colors)
	  
	else
	  nton = DCL::ueqntl
	  if nton==0 then
	    print "no tone patern was set\n"
	    exit
	  end
	  lev1 = Array.new
	  lev2 = Array.new
	  for n in 0..nton-1
	    tlev1,tlev2,ipat = DCL::ueqtlv(n+1)
	    lev1.push(tlev1)
	    lev2.push(tlev2)
	  end

	  levels = lev1+lev2
	  levels = levels.uniq.sort
	  levels.delete(miss)
	  levels = NArray.to_na(levels)
	  if levels.ne(levels.sort).any? then
	    print "levels is not in order\n"
	    exit
	  end

	end


	vxmin = options["vx0"]
	vxlen = options["vxlength"]
	vymin = options["vy0"]
	vylen = options["vylength"]

	vxmax = vxmin + vxlen
	vymax = vymin + vylen

	tick1 = Array.new
	tick2 = Array.new

=begin # <--  make each int by daktu32 in 2003-12-26   --> 
	if options["eqlev"] then
	  for i in 0..src_lev.length-1
	    tick1.push(src_lev[i]) if i%options["tick1"]==0
	    tick2.push(src_lev[i]) if i%options["tick2"]==0
	  end	  
	else
	  for i in 0..levels.length-1
	    tick1.push(levels[i]) if i%options["tick1"]==0
	    tick2.push(levels[i]) if i%options["tick2"]==0
	  end
	end
#<--  make each int by daktu32 in 2003-12-26   --> 
=end 

	if srclabel then
	  for i in 0..srclabel.length-1
	    tick1.push(srclabel[i]) if i%options["tick1"]==0
	    tick2.push(srclabel[i]) if i%options["tick2"]==0
	  end
	else
	  for i in 0..levels.length-1
	    tick1.push(levels[i]) if i%options["tick1"]==0
	    tick2.push(levels[i]) if i%options["tick2"]==0
	  end
	end

	lsize = options["label_size"]

	min = levels[levels.ne(miss).where].min
	max = levels[levels.ne(miss).where].max

	nbar =100
	bar = NArray.float(nbar,2)
	for i in 0..nbar-1
	  bar[i,true] = min + (max-min).to_f/(nbar-1)*i
	end


	xb = DCL::uzlget("labelxb")
	yl = DCL::uzlget("labelyl")
	if portrait then
	  xmin = 0.0
	  xmax = 1.0
	  ymin = min
	  ymax = max
	  DCL::uzlset("labelxb",false)
	  DCL::uzlset("labelyl",true)
	  bar = bar.transpose(-1,0)
	  DCL::uwsgxa([0,1])
	  DCL::uwsgya(bar[0,true])
	else
	  xmin = min
	  xmax = max
	  ymin = 0.0
	  ymax = 1.0
	  DCL::uzlset("labelxb",true)
	  DCL::uzlset("labelyl",false)
	  DCL::uwsgxa(bar[true,0])
	  DCL::uwsgya([0,1])
	end

	type = 1
	if options["log"] then
	  type +=1
	  type +=1 if !portrait
	end

	DCL::grfig
	DCL::grsvpt(vxmin,vxmax,vymin,vymax)
	DCL::grswnd(xmin,xmax,ymin,ymax)
	DCL::grstrn(type)
	DCL::grstrf

	DCL::uetone(bar)

	rsizel = DCL::uzrget("rsizel1")
	DCL::uzrset("rsizel1",lsize)
	rsizet = DCL::uzrget("rsizet1")

	if portrait then
	  DCL::uzrset("rsizet1",vxlen)
	  DCL::uxaxdv("t",1,1)
	  DCL::uxaxdv("b",1,1)
	  DCL::uyaxnm("l",tick1,tick2)
	  DCL::uyaxnm("r",tick1,tick2)
	else
	  if srclabel then
	    p "a"
	    DCL::uxaxlb("b",tick1,tick2,labels,labels.length) 
	  else
	    #begin <--  make each int by daktu32 in 2003-12-26   --> 
	    if options["eqlev"] then
	      DCL::uzrset("rsizet1",vylen)
	      slabel = Array.new
##	      src_lev.each {|i| slabel.push(i.to_s)}
	      ## written by YAMADA.YU 2004.02.16
	      src_lev.each {|i| slabel.push( format("%#.3g", i).to_s)}
	      if options["nobound"] then
		infty = DCL::csgi(189)
		case options["nobound"] 
		when 0 
		  slabel[0] = "-"+infty; slabel[-1]=infty
		when 1 
		  slabel[0] = "-"+infty
		when 2 
		  slabel[-1]=infty
		end
	      end
	      DCL::uxaxlb("b",tick1,tick2,slabel,slabel.length) 
	    else
	      #end <--  make each int by daktu32 in 2003-12-26   --> 
	      DCL::uzrset("rsizet1",vylen)
	      #	  DCL::uxaxnm("t",tick1,tick2)
	      DCL::uxaxnm("b",tick1,tick2)
	      #	  DCL::uyaxdv("l",1,1)
	      #	  DCL::uyaxdv("r",1,1)
	    end
	  end
	end
	
	DCL::uzrset("rsizel1",rsizel)
	DCL::uzrset("rsizet1",rsizet)

	DCL::uzlset("labelxb",xb)
	DCL::uzlset("labelyl",yl)

      end
      module_function :color_bar
    end
  end
end

