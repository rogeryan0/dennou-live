require File.dirname(__FILE__) + '/../test_helper'

class KeywordAttributeTest < ActiveSupport::TestCase
  fixtures :keyword_attributes, :nodes, :directories, :variables

  def setup
    @ka = KeywordAttribute.find(1)
  end

  def test_db
    rd = keyword_attributes(:reanalysis_description)
    assert_kind_of KeywordAttribute, @ka
    assert_equal rd.node_id, @ka.node_id
    assert_equal rd.name, @ka.name
    assert_equal rd.value, @ka.value
    assert_equal rd.num_value, @ka.num_value
    assert_kind_of Node, rd.node
  end

  def test_stdname
    ka = KeywordAttribute.find_by_stdname('title')
    assert_equal 'long_name', ka.name
  end

  def test_node
    node = Node.find_by_path("/samples/reanalysis")
    ka = node.keyword_attributes
    assert_equal 1, ka.length
    assert_kind_of KeywordAttribute, ka[0]
    assert_kind_of KeywordAttribute, node.entity.keyword_attributes[0]
  end

end
