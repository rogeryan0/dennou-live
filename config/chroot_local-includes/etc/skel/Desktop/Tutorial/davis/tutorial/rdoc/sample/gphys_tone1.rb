require "gphys_cont1"
class GPhys_tone < GPhys_cont

  attr_accessor :draw_tone

  def initialize
    super
    @draw_tone = true
  end
  def tone(itr=1)
    gphys = GPhys::IO.open(FILENAME, @var)
    DCL.gropn(1) ; DCL.sgpset('lcntl', false) ; DCL.uzfact(0.7)
    GGraph.set_fig( 'itr'=>(itr == nil) ? 1 : itr.to_i)
    GGraph.tone( gphys ) if @draw_tone
    GGraph.contour( gphys, !@draw_tone )
    DCL.grcls
    return true
  end
end

if __FILE__ == $0
  gphys = GPhys_tone.new
  gphys.tone
end
