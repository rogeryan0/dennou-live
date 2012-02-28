class FunctionOutput < ActiveRecord::Base
  belongs_to :function
  acts_as_list :scope => :function

  after_update :update_yml
  after_save :update_yml

  def update_yml
    function.update_yml if function
  end
end
