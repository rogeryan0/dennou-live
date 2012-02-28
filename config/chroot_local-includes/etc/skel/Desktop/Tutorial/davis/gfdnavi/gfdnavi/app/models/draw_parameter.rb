class DrawParameter < ActiveRecord::Base
  belongs_to :node
  validates_presence_of :name, :value

  def before_validation
    if value == false
      self.value = 0
    elsif value == true
      self.value = 1
    end
  end
end
