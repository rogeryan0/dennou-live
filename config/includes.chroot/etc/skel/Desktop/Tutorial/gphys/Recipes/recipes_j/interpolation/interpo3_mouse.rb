require File.expand_path("~")+"/irbrc_ggraph.rb"
DCL.swpset("lwait",true)

temp = open("air.2010.nc/air").cut("time"=>Date.parse("2010-01-10"))
set_map "coast_world"=>true
set_fig "itr"=>30
tone temp                          # [important] no colorbar please
tcut,line = temp.mouse_cut(0,1)    # 0,1: plotted dims
next_fig "itr"=>2 ; tone tcut; color_bar

p tcut

tone temp
tcut,line = temp.mouse_cut(0,1,3)  # 0,1: plotted dims; 3: 3-point line
next_fig "itr"=>2 ; tone tcut; color_bar

DCL.grcls
