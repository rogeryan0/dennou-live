require File.dirname(__FILE__) + '/../test_helper'
require File.expand_path(File.dirname(__FILE__) + "/../../lib/narray_gfdnavi")

class VariableTest < ActiveSupport::TestCase
  fixtures :variables, :directories, :nodes, :users, :groups

  def setup
    @var = Variable.find(:first,:conditions=>["path=?","/samples/reanalysis/ncep/UV.jan.nc/U"])
    @root = User.find_by_login("root")
  end

  def test_db
    u = nodes(:ncep_uv_jan_u).entity
    assert_kind_of Variable, @var
    assert_equal u.id, @var.id
    assert_equal u.node_id, @var.node_id
    assert_kind_of Node, @var.node
    assert_equal u.parent, @var.parent
    assert_equal u.path, @var.path
    assert_equal u.name, @var.name
    assert_equal nodes(:ncep_uv_jan).entity, @var.parent.entity
  end

  def test_create
    var = Variable.new
    assert_equal Node::VARIABLE, var.node.node_type
    assert_raise(ActiveRecord::RecordNotSaved){ var.save! }
    var.name = "test"
    var.path = "/test"
    var.owner = @root
    assert var.save
  end


end
