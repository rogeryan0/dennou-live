require File.expand_path("~/.irbrc_ggraph.rb")  # irb用のを流用
gphys = GPhys::IO.open('T.jan.nc', 'T')
GGraph.contour( gphys )
DCL.grcls
