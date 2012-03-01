class DrawMethodAttribute < ActiveRecord::Base

  belongs_to :draw_method
  belongs_to :value_type

  validates_presence_of :name, :value_type_id

end
