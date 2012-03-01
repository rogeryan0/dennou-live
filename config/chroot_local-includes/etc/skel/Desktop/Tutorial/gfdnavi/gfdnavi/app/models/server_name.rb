require "activerecord_gfdnavi"

class ServerName 
 attr_accessor :id,:name,:point,:partial,:keyword,:node,:keyvalue


  def initialize(id)
    # server_name
    @name = Array.new
    # point_data
   @point = Array.new
  
    # partial_data
     @partial = Array.new
   
    # keyword_data
    @keyword = Array.new
  
    # node_data
    @node = Array.new
 
    # keyvalues_data
    @keyvalue=Array.new
    
  end

end
