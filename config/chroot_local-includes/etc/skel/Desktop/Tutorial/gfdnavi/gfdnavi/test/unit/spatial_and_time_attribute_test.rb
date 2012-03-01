require File.dirname(__FILE__) + '/../test_helper'

class SpatialAndTimeAttributeTest < ActiveSupport::TestCase
  fixtures :spatial_and_time_attributes, :nodes

  def setup
    @sa = SpatialAndTimeAttribute.find(1)
  end

  def test_create
    global = spatial_and_time_attributes(:ncep_jan_u_global)
    assert_kind_of SpatialAndTimeAttribute, @sa
    assert_equal global.node_id, @sa.node_id
    assert_equal global.longitude_lb, @sa.longitude_lb
    assert_equal global.latitude_lb, @sa.latitude_lb
    assert_equal global.longitude_rt, @sa.longitude_rt
    assert_equal global.latitude_rt, @sa.latitude_rt
    assert_equal global.starttime, @sa.starttime
    assert_equal global.endtime, @sa.endtime
  end

  def test_variable
    node = nodes(:ncep_uv_jan_u)
    assert_kind_of Node, @sa.node
    assert_kind_of Variable, @sa.node.entity
    assert_equal node, @sa.node
  end

end
