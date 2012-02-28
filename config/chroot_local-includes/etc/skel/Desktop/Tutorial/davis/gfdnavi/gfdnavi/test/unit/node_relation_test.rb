require File.dirname(__FILE__) + '/../test_helper'

class NodeRelationTest < ActiveSupport::TestCase
  fixtures :nodes, :variables, :images, :node_relations

  # Replace this with your real tests.

  def setup
    @t = Node.find_by_path("/samples/reanalysis/ncep/T.jan.nc/T")
    @t_mean = Node.find_by_path("/samples/reanalysis/ncep/T.jan.zonal_mean.nc/T_mean")
    @t_image = Node.find_by_path("/samples/reanalysis/ncep/T.jan.100hPa.png")
  end

  def test_relation
    assert_equal [@t], @t_mean.references
    assert_equal [@t], @t_image.references
    assert_equal [@t_mean, @t_image].sort{|a,b| a.path<=>b.path}, @t.referenced_by.sort{|a,b| a.path<=>b.path}
  end

end
