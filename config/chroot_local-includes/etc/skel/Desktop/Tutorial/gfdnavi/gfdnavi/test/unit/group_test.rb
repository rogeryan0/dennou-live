require File.dirname(__FILE__) + '/../test_helper'

class GroupTest < ActiveSupport::TestCase
  fixtures :users, :groups

  def setup
    @user_root = User.find_by_login("root")
  end

  def test_create_groups
    maxgid = Group.find_by_sql('select max(id) as id from groups').first.id
    tg = Group.new(:name => 'test')
    tg.owner = @user_root
    assert tg.save
    assert_equal @user_root.id, tg.owner_id
    tg2 = Group.new(:name => 'test2')
    tg2.owner = @user_root
    assert tg2.save
    assert_equal Group.find_by_name('test').id, maxgid+1
  end

  def test_members
    assert_equal ["root"], Group.find_by_name("root").members.collect{|gm| gm.login}
    assert_equal ["bob","longbob","root"], Group.find_by_name("all").members.collect{|gm| gm.login}.sort
    assert_equal ["bob"], Group.find_by_name("era").members.collect{|gm| gm.login}
    bobsgroup = Group.find_by_bit_flag(User.find_by_login("bob").groups).collect{|g| g.name}.sort
    assert_equal bobsgroup, ["all", "bob", "bobs", "era"] 
  end

  def test_add_del_members
    bobs = Group.find_by_name("bobs")
    mbs = ["bob","longbob"]
    bobs.add_members( mbs )
    assert_equal mbs, bobs.members.collect{|u| u.login}.sort
    bobs.del_members(["longbob"])
    assert_equal bobs.members.collect{|u| u.login}, ["bob"]
  end

  def test_destroy
    longbob = Group.find_by_name("longbob")
    longbob.add_members( ["longbob"] )
    lg1 = User.find_by_login("longbob").groups
    longbob.destroy
    lg2 = User.find_by_login("longbob").groups
    assert_equal  lg1 ^ lg2, longbob.bit_mask
  end

end
