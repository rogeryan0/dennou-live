class NodeLineage < ActiveRecord::Base
   belongs_to :node_ancestor, :class_name => "Node", :foreign_key => "ancestor"
   belongs_to :node_descendant, :class_name => "Node", :foreign_key => "descendant"
#  belongs_to :ancestor, :class_name => "Node", :foreign_key => "ancestor"
#  belongs_to :descendant, :class_name => "Node", :foreign_key => "descendant"
end
