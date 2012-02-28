require "activerecord_gfdnavi"

class PointArray
 attr_accessor :lon,:lat, :cnt

def initialize(id)
  @id=id
  @lon=Array.new
  @lat=Array.new
  @cnt=Array.new 
end
end
