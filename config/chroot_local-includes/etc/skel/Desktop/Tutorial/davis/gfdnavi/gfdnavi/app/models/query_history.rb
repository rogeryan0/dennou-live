class QueryHistory < ActiveRecord::Base

 def self.new_setid 
   res = self.find_by_sql("select max(queryset_id)+1 as ans from query_histories");
   if res.size>0 && res[0].ans then
     return res[0].ans
   else
     return 1
   end   
 end

end
