require File.dirname(__FILE__) + '/../test_helper'
require File.expand_path(File.dirname(__FILE__) + "/../../lib/narray_gfdnavi")

class DirectoryTest < ActiveSupport::TestCase
  fixtures  :directories, :nodes, :users, :groups

  def setup
    @dir_reanalysis = Directory.find(3) # reanalysis
    @root = User.find_by_login("root")
  end

  def test_db
    reanalysis = directories(:reanalysis)
    assert_kind_of Directory, @dir_reanalysis
    assert_equal reanalysis.id, @dir_reanalysis.id
    assert_equal reanalysis.plain_file, @dir_reanalysis.plain_file
    assert_equal reanalysis.node, @dir_reanalysis.node
    assert_equal reanalysis.name, @dir_reanalysis.name
    assert_equal Node.find_by_path("/samples"), @dir_reanalysis.parent
  end

  def test_create
    dir = Directory.new
    assert_equal Node::DIRECTORY, dir.node.node_type
  end

  def test_update
  end

  def test_children
    ncep_jan = directories(:ncep_t_jan)
    children = @dir_reanalysis.children(false,:user=>@root)
    assert_equal 2, children.length
    assert_kind_of Node, children[1]
    assert_kind_of Directory, children[1].entity
    assert children[1].children.include?( ncep_jan.node )
    ncep = @dir_reanalysis.children.find(:first, :conditions => 'name = "ncep"')
    assert_kind_of Directory, ncep.entity
    assert_equal %w(T.jan.100hPa.png T.jan.nc T.jan.zonal_mean.nc UV.jan.nc).sort, ncep.children.collect{|n| n.name}.sort
  end

  def test_variables
    ncep = @dir_reanalysis.node.children.find(:first, :conditions => 'name = "ncep"')
    ncep_jan = ncep.children.find(:first, :conditions => 'name = "T.jan.nc"')
    assert_kind_of Directory, ncep_jan.entity
  end


end
