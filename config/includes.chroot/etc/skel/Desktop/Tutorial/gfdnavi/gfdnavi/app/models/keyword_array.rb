require "activerecord_gfdnavi"

class KeywordArray
 attr_accessor :keyname,:cnt 

def initialize(id)
  
  @id=id
  @keyname=Array.new
  @cnt=Array.new 
end
end
