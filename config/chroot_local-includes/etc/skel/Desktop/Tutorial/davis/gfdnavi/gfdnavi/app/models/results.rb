require "activerecord_gfdnavi"

class Results
 attr_accessor :id, :keynames
  def initialize(id)
    @id=id
    @keynames = Array.new
  end
 
  def get_keyname_list
    if @id!=0 then
      keylist=KeywordAttribute.find(:all,:select=>"keyword_attributes.name as kname,count(*) as cnt",:joins=>"JOIN cache_#{@id} ON variable_id=cache_#{@id}.vid",:group=>"keyword_attributes.name",:order=>"cnt DESC")
    else
      keylist=KeywordAttribute.find(:all,:select=>"keyword_attributes.name as kname,count(*) as cnt",:joins=>"JOIN variables ON variable_id=variables.id",:group=>"keyword_attributes.name",:order=>"cnt DESC")
    end
    return keylist
  end
 
 def get_path_list
  if @id!=0 then
    tables="(select v.* from variables v,cache_#{@id} c where v.id=c.vid)"
    path=QueryHistory.find(@id).path
  else
    tables="variables"
    path="/"
  end
      sql1=<<-EOM
       select d2.path as dpath, count(v.id) as cnt
       from directories d1,directories d2,#{tables} v
       where d1.path = ?
         and d2.parent_id = d1.id
         and v.path like #{concat("d2.path","'%'")}
       group by d2.path
       order by cnt DESC
     EOM
     pathlist = Directory.find_by_sql [sql1,path]
   return pathlist
 end
 
 def get_keyvalue_list(keyname)
   if @id!=0 then
    valuelist=KeywordAttribute.find(:all,:joins=>"JOIN cache_#{@id} ON variable_id=cache_#{@id}.vid",:select=>"value,num_value,count(*) as cnt",:conditions=>["keyword_attributes.name=?",keyname],:group=>"value")
   else
    valuelist=KeywordAttribute.find(:all,:joins=>"JOIN variables ON variable_id=variables.id",:select=>"value,num_value,count(*) as cnt",:conditions=>["keyword_attributes.name=?",keyname],:group=>"value")   
   end
   return valuelist   
 end
end
