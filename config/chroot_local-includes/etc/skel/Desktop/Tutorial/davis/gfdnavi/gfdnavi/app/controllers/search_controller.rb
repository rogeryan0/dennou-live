class SearchController < ApplicationController
  layout "gfdnavi"

  def index
    mapsearch
    return
  end


  def msearch
    debug="**debug**"
    @query = Query.new(params[:query])
    if(query_points = @query.querystring("points"))
      @varpoints = Node.find_by_sql( [query_points.gsub(/%/,'%%')] )
      @results = true
    else
      @varpoints = nil
    end

    if @query.spatial_region
      lon_len = @query.start_lon - @query.end_lon if @query.start_lon >= @query.end_lon
      lon_len = @query.end_lon - @query.start_lon if @query.start_lon < @query.end_lon
      lat_len = @query.start_lat - @query.end_lat if @query.start_lat >= @query.end_lat
      lat_len = @query.end_lat - @query.start_lat if @query.start_lat < @query.end_lat
  
      #e = lon_len / 5.0 if lon_len >= lat_len
      #e = lat_len / 5.0 if lat_len > lon_len
      # 
      #if ((@query.zoomlevel).to_i) < ((@query.oldzoomlevel).to_i)
      #   e = e * 2.0*(((@query.oldzoomlevel).to_f) - ((@query.zoomlevel).to_f))
      #elsif ((@query.zoomlevel).to_i) > ((@query.oldzoomlevel).to_i)
      #   e = e / 2.0 / (((@query.zoomlevel).to_f)-((@query.oldzoomlevel).to_f))
      #end
      e = params[:query][:span].to_f/15
    else
      e = 30.0
    end

    
    if @varpoints 
      all_points_group=Hash.new
      pg_count=0
      @varpoints.each { |v|
	SpatialAndTimeAttribute.find(:all, :conditions=>{:node_id=>v.id}).each { |s| 
           if (all_points_group.keys.length==0)
             all_points_group["point_group0"]=Array.new
             all_points_group["point_group0"].push s
             pg_count=1
           else 
             i=0
             while i <= pg_count do
               points_group = all_points_group["point_group#{i}"]
               if (i==pg_count)
                  all_points_group["point_group#{pg_count}"]=Array.new
                  all_points_group["point_group#{pg_count}"].push s
                  pg_count=pg_count+1
                  break               
               elsif (points_group[0].latitude_lb+e >= s.latitude_lb && points_group[0].latitude_lb-e <= s.latitude_lb && points_group[0].longitude_lb+e >= s.longitude_lb && points_group[0].longitude_lb-e <= s.longitude_lb)
                  all_points_group["point_group#{i}"].push s
                  break
               end
               i=i+1
             end #while 
           end #if
         }#SpatialAttributes
       } #@verpoints.each
      @all_points_group=all_points_group
    end #@verpoints
    
    if(query_partial = @query.querystring("partial"))
      debug+=query_partial
      @varpartial = Node.find_by_sql( [query_partial.gsub(/%/,'%%')] )
      @results = true
    else
      @varpartial = nil
    end

    if @varpartial
      pag_count=0
      all_partial_group=Hash.new
      
      @varpartial.each do |v| 
        SpatialAndTimeAttribute.find(:all, :conditions=>{:node_id=>v.id}).each{ |s|
         if (all_partial_group.keys.length==0)
           all_partial_group["partial_group0"]=Array.new
           all_partial_group["partial_group0"].push s
           pag_count=1
         else  
            i=0 
            while i<=pag_count do
              if (i==pag_count)
                all_partial_group["partial_group#{pag_count}"]=Array.new
                all_partial_group["partial_group#{pag_count}"].push s
                pag_count=pag_count+1
                break
              else
                partial_group=all_partial_group["partial_group#{i}"]
                if(partial_group[0].latitude_lb+e >= s.latitude_lb && partial_group[0].latitude_lb-e <= s.latitude_lb && partial_group[0].longitude_lb+e >= s.longitude_lb && partial_group[0].longitude_lb-e <= s.longitude_lb)
                 all_partial_group["partial_group#{i}"].push s
                 break
                end
              end
              i=i+1
            end #while 
          end #if 
        }#SpatialAttribute
      end 
      @all_partial_group=all_partial_group
    end 
    
    if(query_global = @query.querystring("global"))
      debug+=query_partial
      @varglobal = Node.find_by_sql( [query_global.gsub(/%/,'%%')] )
      @results = true
    else
      @varglobal = nil
    end

    if (query_no_region = @query.querystring("no_region"))
      debug += query_no_region
      @varno_region = Node.find_by_sql( [query_no_region.gsub(/%/,'%%')] )
      @resutls = true
    else
      @varno_region = nil
    end

    res=Hash.new
    res["debug"]=debug
    res["points"]=@all_points_group
    res["partial"]=@all_partial_group
    res["global"] = @varglobal
    res["no_region"] = @varnoapatialattribute
    res["radius"]=e
    return res
  end

  def re_search
    res = msearch
    session[:query]=@query
    session[:all_partial_group]=@all_partial_group
    session[:all_points_group]=@all_points_group

    return res
  end

  def mapsearch
    @google_map_key = GFDNAVI_GOOGLE_MAP_KEY
    @page_title = 'search by google-map'

    if request.post?
      res = msearch
    else
      @query=Query.new();
    end

    session[:varglobal]=@varglobal
    session[:varpartial]=@varpartial
    session[:varpoints]=@varpoints
    session[:varno_region] = @varno_region
    session[:query]=@query
    session[:results]=@results
    session[:all_partial_group]=@all_partial_group
    session[:all_points_group]=@all_points_group

    @variables = Array.new
    [@varpoints, @varpartial, @varglobal, @varno_region].each{|ary|
      @variables += ary if Array === ary
    }
    render :action => "mapsearch"
  end

  def remapsearch
    r=re_search
    @objs = Array.new
    if @all_points_group!=nil then
      @all_points_group.keys.each{ |key|
        group = @all_points_group[key]
        size = group.length if group.length > 30
        size = 30 if group.length <= 30 
        if group.length!=0 then
          @objs.push( {:gid=>key, :radius=>r["radius"],:posx=>group[0].latitude_lb, :posy=>group[0].longitude_lb, :size=>size, :num=>group.length, :mode=>"points"} )
        end
      }
    end
    if @all_partial_group!=nil then
      @all_partial_group.keys.each{ |key|
        group = @all_partial_group[key]
        size = group.length if group.length > 30
        size = 30 if group.length <= 30 
        if group.length!=0 then
          @objs.push( {:gid=>key, :radius=>r["radius"], :posx=>group[0].latitude_lb, :posy=>group[0].longitude_lb, :size=>size, :num=>group.length, :mode=>"partial"} )
        end
      }
    end
    @debug = r["debug"]
    @queryregion = {:start_lon=>@query.start_lon,:start_lat=>@query.start_lat,:end_lon=>@query.end_lon,:end_lat=>@query.end_lat}
    @windowregion = {:window_start_lon=>@query.window_start_lon,:window_start_lat=>@query.window_start_lat,:window_end_lon=>@query.window_end_lon,:window_end_lat=>@query.window_end_lat}
  end

  def listvariables
    @all_group = session[:all_points_group]
    @mode = params[:mode]
    
    if @mode=="points"
      @all_group = session[:all_points_group]
    elsif @mode=="partial"
      @all_group = session[:all_partial_group]
    end
    @points_group = @all_group[params[:gid]]
    @gid=params[:gid]
    render(:partial=>"show_variablelist")
  end


  def change_results
    @varpoints = session[:varpoints] || []
    @varpartial = session[:varpartial] || []
    @varglobal = session[:varglobal] || []
    @varno_region = session[:varno_region] || []
    @mode = params[:show][:list]
    case @mode
    when "points"
      @no="p"
      @variables = @varpoints
    when "partial"
      @no="p"
      @variables = @varpartial
    when "global"
      @no="g"
      @variables = @varglobal
    when "no_region"
      @no=""
      @variables = @varno_region
    else
      @no=""
      @variables = @varpoints + @varpartial + @varglobal + @varno_region
    end
    render(:partial =>"show_results")
  end


  def change_points_listmode
    @varpoints=session[:varpoints]
    if params[:mode][:list]=="groupbyfiles"
       render(:partial =>"show_results_points_groupbyfile", :object => @varpoints)
    else
       render(:partial =>"show_results_points", :object => @varpoints)
    end
  end


  def change_partial_listmode
    @varpartial=session[:varpartial]
    if params[:mode][:list]=="groupbyfiles"
       render(:partial =>"show_results_partial_groupbyfile", :object => @varpartial)
    else
       render(:partial =>"show_results_partial", :object => @varpartial)
    end
  end

  def change_global_listmode
    @varglobal=session[:varglobal]
    if params[:mode][:list]=="groupbyfiles"
       render(:partial =>"show_results_global_groupbyfile", :object => @varglobal)
    else
       render(:partial =>"show_results_global", :object => @varglobal)
    end
  end



  private

  def open_dir(closes, parents, session, selected)
    dir_tree = session[:dir_tree]
    user = (login=session[:user]) && User.find_by_login(login)
    if parents[0]
      grand_parent = parents[0].parent
      if grand_parent && !dir_tree.include?(grand_parent.path)
        render :nothing => true
        return false
      end
    end
    render :update do |page|
      closes.each{|dir|
        page << "tree.dirChange('#{dir.path}');"
      }
      parents.each{|dir|
        path = dir.path
        @dirs = dir.directory_nodes(:user=>user)
        if !(dir_tree.include?(path)) && @dirs.length > 0
          @user = user
          @parent = dir
          session[:dir_tree].push path
          page.replace "dir#{path}", render(:partial => "children")
        end
      }
      if selected
        page << "tree.dirSelected('#{parents[-1].path}');"
      end
    end
    session[:dir_tree_selected] = parents[-1] if selected
  end

  def add_var_to_list(var)
    if Node === var
      var = var.entity
    elsif String === var
      user = (login=session[:user]) && User.find_by_login(user)
      var = Variable.find(:first, :conditions=>["path=?",path,], :user=>user )
    end
    if var
      session[:variables_list] ||= Array.new
      session[:variables_list].push var unless session[:variables_list].include?(var)
    end
  end

  def add_node_to_list(node)
    user = (login=session[:user]) && User.find_by_login(user)
    if String === node
      begin
        node = Node.find(:first, :conditions=>["path=?",node], :user=>user)
        return nil unless node
      end
    end

    if node.variable?
      add_var_to_list(node)
      return
    elsif node.directory?
      node.variable_nodes(:user=>user).each{|var|
        add_var_to_list(var)
      }
      node.directory_nodes(:user=>user).each{|dir|
        add_dir_to_list(dir)
      }
    end
  end


end
