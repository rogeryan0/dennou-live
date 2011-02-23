class GPhys_cont
  require "numru/ggraph" ; include NumRu

  FILENAME = 'T.jan.nc'

  attr_reader   :var

  def initialize(var='T')
    @var  = var
  end
  def cont
    gphys = GPhys::IO.open(FILENAME, @var)
    DCL.gropn(1) ; DCL.sgpset('lcntl', false) ; DCL.uzfact(0.7)
    GGraph.contour( gphys)
    DCL.grcls
  end
end

if __FILE__ == $0
  gphys = GPhys_cont.new
  gphys.cont
end
