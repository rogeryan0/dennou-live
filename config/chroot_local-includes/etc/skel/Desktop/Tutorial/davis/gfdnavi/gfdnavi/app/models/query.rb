class Query 
  attr_reader :spatial_region
  attr_reader :start_lon, :start_lat
  attr_reader :end_lon, :end_lat
  attr_reader :time_region
  attr_reader :starttime, :endtime
  attr_reader :keywords
  attr_reader :listmode
  attr_reader :zoomlevel, :oldzoomlevel
  attr_reader :center_x, :center_y
  attr_reader :window_start_lon, :window_start_lat
  attr_reader :window_end_lon, :window_end_lat


  def initialize(params=Hash.new)
    if params[:spatial_region] == "1"
      @spatial_region = true
      @start_lon = params[:start_lon].to_f
      @start_lat = params[:start_lat].to_f
      @end_lon = params[:end_lon].to_f
      @end_lat = params[:end_lat].to_f
      
      @start_lon=360+@start_lon if @start_lon < 0 
      @end_lon=360+@end_lon if @end_lon < 0 

      @start_lon, @end_lon = [@end_lon, @start_lon] if @start_lon > @end_lon
      @start_lat, @end_lat = [@end_lat, @start_lat] if @start_lat > @end_lat

      @window_start_lon = params[:window_start_lon].to_f
      @window_start_lat = params[:window_start_lat].to_f
      @window_end_lon = params[:window_end_lon].to_f
      @window_end_lat = params[:window_end_lat].to_f

      @window_start_lon=360+@window_start_lon if @window_start_lon < 0
      @window_end_lon=360+@window_end_lon if @window_end_lon < 0
      
      @window_start_lon, @window_end_lon = [0,360] if @window_start_lon > @window_end_lon
      @window_start_lat, @window_end_lat = [-90,90] if @window_start_lat > @window_end_lat

      
      @window_start_lon = @start_lon if @window_start_lon<@start_lon
      @window_start_lat = @start_lat if @window_start_lat<@start_lat
      @window_end_lon = @end_lon if @window_end_lon>@end_lon
      @window_end_lat = @end_lat if @window_end_lat>@end_lat
      
    end
    if params[:time_region] == "1"
      @starttime = Time.utc( params["starttime(1i)"], params["starttime(2i)"], params["starttime(3i)"], params["starttime(4i)"], params["starttime(5i)"] )
      @endtime = Time.utc( params["endtime(1i)"], params["endtime(2i)"], params["endtime(3i)"], params["endtime(4i)"], params["endtime(5i)"] )
      @time_region = true
    end
    
    if params[:zoomlevel]!=nil then
      @zoomlevel=params[:zoomlevel]
    else
      @zoomlevel=1
    end
    
    if params[:oldzoomlevel]!=nil then
      @oldzoomlevel=params[:oldzoomlevel]
    else
      @oldzoomlevel=1
    end
    
    if params[:center_x]!=nil then
      @center_x=params[:center_x]
    else
      @center_x=136.0
    end
    
    if params[:center_y]!=nil then
      @center_y=params[:center_y]
    else
      @center_y=37.0
    end
    
    
    @keywords = params[:keywords]
    @keywords = nil if @keywords == ""
  end

  def querystring(area)
    return nil unless keywords || spatial_region || time_region
    #tables = ["variables v"]
    tables = []
    conditions = Array.new
    sorts = Array.new
    
    sorts.push "n.path"

    if keywords
      kcond = Array.new
      keywords.split(" ").each{|key|
        if (v=key.to_i).to_s == key || (v=key.to_f).to_s == key
          kcond.push KeywordAttribute.format_value_search(v, :like, "k")
        else
          kcond.push "k.name like '%#{key}%' or k.value like '%#{key}%' "
        end
        kcond.push "v.path like '%#{key}%'"
      }
      kcond = kcond.join(" or ")
      path = Variable.connection.adapter_name=="MySQL" ? "concat(d.path,'%')" : "d.path||'%'"
      subquery =<<-EOF
        (select distinct v.*
           from nodes d, nodes v, keyword_attributes k
           where v.node_type=#{Node::VARIABLE} and d.node_type=#{Node::DIRECTORY} and
                (   (v.id=k.node_id and (#{kcond}))
                 or (v.path like #{path} and d.id=k.node_id and (#{kcond})))
        ) as n
      EOF
      tables.push subquery
    else 
      tables.push "nodes n"
    end

    conditions.push "n.node_type=#{Node::VARIABLE}"

    tables.push "spatial_and_time_attributes st"

    if time_region
      conditions.push "n.starttime<='#{endtime.strftime(format)}' and v.endtime>='#{starttime.strftime(format)}'"
    end

    format = "%Y-%m-%d %H:%M:%S"

    if area == "no_region"
      if spatial_region
        return "select * from nodes where null"
      end
      area_condition = "not exists (select * from spatial_and_time_attributes as st where st.node_id=n.id and (st.longitude_lb||st.longitude_rt||st.latitude_lb||st.latitude_rt))"
    else

      conditions.push "st.node_id=n.id"
      area_condition ="not(st.node_id IS NULL)";
    end

    if time_region
      area_condition += "and (st.starttime<='#{endtime.strftime(format)}' and st.endtime>='#{starttime.strftime(format)}')"
    end

    lon_width = 360*0.9
    lat_width = 180*0.9
    case area
    when "points"
      area_condition += " and (st.longitude_lb = st.longitude_rt)"
      area_condition += " and (st.latitude_lb = st.latitude_rt)"

    when "partial"
      area_condition += " and ((abs(st.longitude_lb-st.longitude_rt)<#{lon_width} and not(st.longitude_lb=0 and st.longitude_rt=0 ) and not(st.longitude_lb = st.longitude_rt and st.latitude_lb = st.latitude_rt))"
      area_condition += " or (abs(st.latitude_lb-st.latitude_rt)<#{lat_width} and not(st.latitude_lb=0 and st.latitude_rt=0 ) and not(st.longitude_lb = st.longitude_rt and st.latitude_lb = st.latitude_rt ))) "

    when "global"
      area_condition += " and abs(st.longitude_lb-st.longitude_rt)>=#{lon_width}"
      area_condition += " and abs(st.latitude_lb-st.latitude_rt)>=#{lat_width}"

    end

    if spatial_region
      slon=start_lon; slat=start_lat;
      elon=end_lon; elat=end_lat;
      slon=window_start_lon if start_lon<window_start_lon
      slat=window_start_lat if start_lon<window_start_lat
      elon=window_end_lon if end_lon>window_end_lon
      elat=window_end_lat if end_lon<window_end_lat

      area_condition+=" and st.longitude_lb <= #{elon} and st.longitude_rt >= #{slon}"
      area_condition+=" and st.latitude_lb <= #{elat} and st.latitude_rt >= #{slat}"
      #      s="pow(s.longitude_lb+s.longitude_rt+(#{slon}*-1)+(#{elon}*-1),2)+"
      #      s+="pow(s.latitude_lb+s.latitude_rt+(#{slat}*-1)+(#{elat}*-1),2)"
      s="(st.longitude_lb+st.longitude_rt+(#{slon}*-1)+(#{elon}*-1))*(st.longitude_lb+st.longitude_rt+(#{slon}*-1)+(#{elon}*-1))+"
      s+="(st.latitude_lb+st.latitude_rt+(#{slat}*-1)+(#{elat}*-1))*(st.latitude_lb+st.latitude_rt+(#{slat}*-1)+(#{elat}*-1))"
      sorts.push s
    end

    conditions.push area_condition

    
    if Variable.connection.adapter_name == "PostgreSQL"
      res = "select distinct on (n.path) n.* "
    else
      res = "select distinct n.* "
    end
    res += "from "
    res += tables.join(", ")
    res += " where "
    res += conditions.join(" and ")
    if sorts.size > 0
      res += " order by "
      res += sorts.join(",")
    end
    return res
  end
end
