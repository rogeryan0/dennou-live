require "activerecord_gfdnavi"

class PartialArray
  attr_accessor :lat_lb,:lon_lb , :lat_rt,:lon_rt,:cnt

def initialize(id)
  
  @id=id
  @lat_lb=Array.new
  @lon_lb=Array.new
  @lat_rt=Array.new
  @lon_rt=Array.new
  @cnt=Array.new 
end
end
