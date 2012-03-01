require "activerecord_gfdnavi"

class NodeArray
 attr_accessor :name,:title, :desc,:path

def initialize(id)
  @id=id
  @name=Array.new
  @title=Array.new
  @desc=Array.new
  @path=Array.new
end
end
