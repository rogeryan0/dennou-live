class DrawMethodOption < ActiveRecord::Base

  belongs_to :draw_method
  belongs_to :value_type

  after_save :update_yml
  after_update :update_yml

  validates_presence_of :name, :value_type_id

  def update_yml
    draw_method.update_yml if draw_method
  end

end
