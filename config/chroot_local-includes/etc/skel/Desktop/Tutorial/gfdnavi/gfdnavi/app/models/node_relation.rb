class NodeRelation < ActiveRecord::Base
  belongs_to :reference, :class_name => "Node", :foreign_key => "reference"
  belongs_to :referenced_by, :class_name => "Node", :foreign_key => "referenced_by"
end
