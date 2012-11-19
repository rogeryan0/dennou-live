require "numru/ggraph"
include NumRu
begin
  print "\n** optoions of  GGraph.line **\n"
  GGraph.line( nil, true, 'help'=>true )
rescue
end
begin
  print "\n** optoions of  GGraph.contour **\n"
  GGraph.contour( nil, true, 'help'=>true )
rescue
end
