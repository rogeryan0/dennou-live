class SpatialAndTimeAttribute < ActiveRecord::Base
  belongs_to :node

  def longitude_lb=(v)
    super(v % 360)    # to ensure 0..360
  end
  def longitude_rt=(v)
    super(v % 360)    # to ensure 0..360
  end

end
