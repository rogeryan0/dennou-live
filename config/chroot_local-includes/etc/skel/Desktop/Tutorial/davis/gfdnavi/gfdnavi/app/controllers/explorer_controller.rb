#require "rdb_concat"

class ExplorerController < ApplicationController 
  layout "gfdnavi", :except => [:download_tar]
 
 
 def index 
   session[:previd]=nil
   @previd=nil
   @qargs=nil
   @results=ExplorerResult.new(0)
   @queryconditions=Array.new
   session[:results]=@results
    session[:result]=@results
   session[:qsetid]=1
   session[:queryconditions]=@queryconditions
   session[:qcond_selection]=Array.new
   session[:nodetype_selection]=Array.new
   session[:qcno]=0
 end
 
 def add_keyname
     @results=session[:results]
     @results.keynames.push(params[:keyname])
     @results.keynames.uniq!
     session[:results]=@results
     valuelist=@results.get_keyvalue_list(params[:keyname])
     if valuelist.size>0 then
       render :update do |page|
        valuelist.each{ |v|
          page.insert_html(:bottom,"kw_#{params[:keyname]}","<li><a href=\"/explorer/search_kw?keyname=#{params[:keyname]}&keyvalue=#{v.value}\">#{v.value}(#{v.cnt})</a></li>")
        }
       page.show("kw_#{params[:keyname]}_close")       
       end
     end     
 end
 
 def del_keyname
     @results=session[:results]
     @results.keynames.delete(params[:keyname])
     session[:results]=@results
     render :update do |page|
       page.replace_html("kw_#{params[:keyname]}","")
       page.hide("kw_#{params[:keyname]}_close")
     end
 end
 def get_keyvalue
      @results=ExplorerResult.new(0)
 
    # @results=session[:result]
     @results.keynames.push(params["keyname"])
    @results.keynames.uniq!
 
	session[:result]=@results
	@valuelist=@results.get_keyvalue_list(params["keyname"])
	

render :xml=>@valuelist
end

 def search_path
   desc="[P]#{params[:path]}"
   search("path",desc)
 end

 def search_freeword
   desc="[F]#{params[:freeword]}"
   search("freeword",desc)
 end

 
 def search_kw 
   desc="[K]#{params[:keyname]}=#{params[:keyvalue]}"
   search("keyword",desc)
 end
 
  
 def search_kname
   desc="[K]#{params[:keyname]}"
   search("keyname",desc) 
 end
 
  def search_space    
    desc = "[S](#{params[:start_lon]},#{params[:start_lat]})-(#{params[:end_lon]},#{params[:end_lat]})"
    search("space",desc)
 end

  def search_time
   desc="[T]#{params[:starttime]}~#{params[:endtime]}"
   search("time",desc)
 end
 
 def queryconditions_selection
   num=0
   for i in 0..session[:qcno].to_i-1
     if params["qcond_selection_#{i}"]=="1" then
       session[:qcond_selection][i]=1
       num=num+1
     else
       session[:qcond_selection][i]=0
     end
   end
   if num==0 then
     redirect_to :action=>"index"
   else
     search("qcond_selection",params[:queryset_id].to_i)
   end
 end
 
 def nodetype_selection
   num=0
   types=Array.new
   @nodetypes=Hash.new
   ntypes=["data","knowledge","function","draw_method"]
   ntypes.each{ |nt|
     value=params["nodetype_#{nt}"]
     if value then
       if params["nodetype_#{nt}"]=="1" then
         types.push(nt)
         num=num+1
         @nodetypes[nt]=1
       else
         @nodetypes[nt]=0
       end
     else
       @nodetypes[nt]=0
     end
   }
   if num==0 then
     redirect_to :action=>"index"
   else
     types_str=types.join(",")
     search("nodetype","[N]#{types_str}")
   end
 end
 
 def search(querytype,desc)
   if session[:queryconditions] then
     @queryconditions=session[:queryconditions]
     if querytype!="qcond_selection" then
       @queryconditions.push(desc)
     end
   end
   
   user = (login=session[:user]) && User.find_by_login(login)

   eq=ExplorerQuery.new
   qstr = eq.make_query(@queryconditions,user)
   if qstr!="" then
     @results=ExplorerResult.new(@qsetid)
     @results.put_results(Node.find_by_sql(qstr))
   end
   session[:results]=@results
   render :action=>"index.rhtml"
 end
 

 def categorized_search

   descriptions=Array.new
   queries=params[:query]
   queries.keys.each{|k|
    if queries[k]!=nil then
      descriptions.push(queries[k])
    end
   }
   options=Hash.new
   if params["show_kwfacets"] then
     options["show_kwfacets"]=params["show_kwfacets"]
   else
     options["show_kwfacets"]=0   
   end
   if params["show_kwvalues"] then
     options["show_kwvalues"]=params["show_kwvalues"]
   else
     options["show_kwvalue"]=0   
   end
   if params["show_spfacets"] then
     options["show_spfacets"]=params["show_spfacets"]
   else
     options["show_spfacets"]=0   
   end

   user = (login=session[:user]) && User.find_by_login(login)

   eq=ExplorerQuery.new
   qstr = eq.make_query(descriptions,user)
   rnodes=Node.find_by_sql(qstr)
   expres=ExplorerResult.new(-1)
   expres.put_results(rnodes)
   session[:result]=expres   
   results=eq.generate_results(rnodes,expres,options)
   render :xml => results
 end

 # == 複数のファイルを tar で固める
 # * 引数 paths はファイルのパスの配列
 # * システムの tar コマンドを呼び出して使う
 def download_tar
   filenames = params[:filenames]
   if (params[:tar_gz]["path"] == "")
     download_name = "gfdnavi"
   else
     download_name = params[:tar_gz]["path"]
   end
   
   response.headers['Content-Type'] = "application/x-tar"
   # response.headers['Content-Encoding'] = "x-gzip"
   response.headers['Content-Disposition'] = "attachment;filename=" + download_name + ".tar.gz"
   response.headers['Content-Transfer-Encoding'] = "binary"
   response.headers['Cache-Control'] = "private"
   render :text => Proc.new { |response,output|
     IO.popen("tar zcf - #{filenames}", "r") { |io|
       while buf = io.read(4096)
         output.write(buf)
       end
     }
   }
 end
 
end
