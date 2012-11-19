#
# GPhys を利用して等値線図を描画するクラスライブラリ
#
class GPhys_cont
  require "numru/ggraph" ; include NumRu

  # ファイル名 (固定)
  FILENAME = 'T.jan.nc'

  # 描画する変数
  attr_reader   :var

  #
  # 初期化処理用のメソッド. 引数 _var_ には描画する変数を
  # 与えます.
  #
  def initialize(var='T')
    @var  = var
  end

  #
  # ※ 空行を入れると, それより上の部分は無視されます.
  #

  #
  # 等値線図の描画を実行します.
  #
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
