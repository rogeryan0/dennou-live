require "activerecord_gfdnavi"

class ResultAtServer 
 attr_accessor :isInitial,:id,:name,:url,:check


  def initialize(name)
    #server_id
    @id ="" 
    
    # server_name
    @name = name
    
    #server_url
    @url = ""
    # @url = Array.new 
    #check
    @check=""  
        
  end
  
 

end
