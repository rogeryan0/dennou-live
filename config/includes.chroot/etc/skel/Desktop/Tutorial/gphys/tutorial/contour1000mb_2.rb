require File.expand_path("~/.irbrc_ggraph.rb")  # irb�ѤΤ�ή��
gphys = GPhys::IO.open('T.jan.nc', 'T')
GGraph.contour( gphys )
DCL.grcls
