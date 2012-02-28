require File.dirname(__FILE__) + '/../test_helper'

class NodeTest < ActiveSupport::TestCase
  fixtures :nodes, :users, :groups
           #, :variables, :keyword_attributes,

  def setup
    @node_reanalysis = Node.find_by_path('/samples/reanalysis')
    @root = User.find_by_login("root")
  end

  def test_permission
    topnode = Node.find_by_path('/')
#    topnode.update # 2012-2-12 REMOVED BY OTSUKA
#    topnode.save!  # 2012-2-12 REMOVED BY OTSUKA
    #Node.find(:all, :user=>:all).each do |node| 
    #  printf("%016x %s\n",node.groups_readable,node.path)
    #end
    n1 = Node.find_by_path('/samples/reanalysis/era40')
    assert_equal n1.groups_readable, 32
    n2 = Node.find_by_path('/samples/reanalysis/era40/t.nc')
    assert_equal n2.groups_readable, 32
  end

  def test_db
    reanalysis = nodes(:reanalysis)
    assert_kind_of Node, @node_reanalysis
    assert_equal reanalysis.id, @node_reanalysis.id
    assert_equal reanalysis.name, @node_reanalysis.name
    assert_equal reanalysis.parent_id, @node_reanalysis.parent_id
    assert_equal reanalysis.node_type, @node_reanalysis.node_type
  end

  def test_tree
    assert_equal '/samples', @node_reanalysis.parent.path
    assert_equal ["ncep"], @node_reanalysis.children.collect{|node| node.name}
    assert_equal ["era40","ncep"], @node_reanalysis.children(false,:user=>@root).collect{|node| node.name}
  end

  def test_lineage
    # this is rather to test the consistency of the fixture
    upath = '/samples/reanalysis/ncep/UV.jan.nc/U'
    assert_kind_of Node, ( u = Node.find_by_path(upath) )
    path = u.file
    assert_equal( (pr=u.parent).path, path )
    while (path.sub!(/\/[^\/]*$/,'') !=  '')
      assert_equal( (pr=pr.parent).path, path )
    end
  end

  def test_group
    assert_equal Node.count(:user => @root), Node.find(:all, :user => @root).length

    bob = User.find_by_login("bob") # belongs to "era"
    path = "/samples/reanalysis/era40"
    assert_equal path, Node.find(:first, :conditions=>"path='#{path}'", :user=>bob).path
    path = "/samples/reanalysis/era40/t.nc"
    assert_equal path, Node.find(:first, :conditions=>"path='#{path}'", :user=>bob).path
    path = "/samples/reanalysis/era40/t.nc/t"
    assert_equal path, Node.find(:first, :conditions=>"path='#{path}'", :user=>bob).path
    assert_equal 3, Node.find(:all, :conditions=>"path like '%era%'", :user=>bob).length

    longbob = User.find_by_login("longbob") # "belongs to "bobs"
    path = "/usr/bob/hidden_image.png"
    assert_equal path, Node.find(:first, :conditions=>"path='#{path}'", :user=>@root).path
    assert_equal path, Node.find(:first, :conditions=>"path='#{path}'", :user=>longbob).path
    assert_equal 0, Node.find(:all, :conditions=>"path like '%era%'", :user=>longbob).length

    path = "/samples/reanalysis/ncep/T.jan.nc/T"
    assert_equal path, Node.find(:first, :conditions=>["path=?",path]).path
    assert_equal @node_reanalysis, Node.find(@node_reanalysis.id)
    assert_equal 0, Node.find(:all, :conditions=>"path like '%era%'").length
    path = "/usr/bob/hidden_image.png"
    assert_nil Node.find(:first, :conditions=>["path=?",path])
  end

  def test_update
    name = "changed_name"
    @node_reanalysis.name = name
    assert @node_reanalysis.save, @node_reanalysis.errors.full_messages.join("; ")
    @node_reanalysis.reload
    assert_equal name, @node_reanalysis.name
  end

end
