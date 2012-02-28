class FunctionArgument < ActiveRecord::Base
  belongs_to :function
  belongs_to :value_type
  acts_as_list :scope => :function
  validates_presence_of :value_type_id

  after_update :update_yml
  after_save :update_yml

  def update_yml
    function.update_yml if function
  end
end
