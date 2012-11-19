require "activerecord_gfdnavi"

class CrossArray
 #attr_accessor :keynames 
 attr_accessor :keyname,:keyvalue , :cnt

def initialize(id)
    # @keynames = Array.new
    # @resulttree=PathNode.new
    # @resulttree.path="/"
    # @resultids = Array.new
    # @nodes=Array.new
    # @qsetid=qsetid
	#puts "aa"
  @id=id
  @keyname=Array.new

  @keyvalue=Array.new
  @cnt=Array.new 
end
end
