# -*- coding: japanese-cp932 -*-
require "activerecord_gfdnavi"

class CrossResult
  attr_accessor :id, :keynames, :qsetid, :resulttree, :resultids, :nodes
  def initialize()
    @keynames = Array.new
    @resulttree = PathNode.new
    @resulttree.path = "/"
    @resultids = Array.new
    @nodes = Array.new
#    @qsetid=qsetid
  end
  def reset_resulttree
    @resulttree = PathNode.new
    @resulttree.path = "/"
  end 
  
  def put_results(nodes)
    @nodes = nodes
    @resultids = @nodes.map{|res| res.id}.uniq!  
  end
  
  def resultlist(p)
    reset_resulttree   
    dir = @nodes.dup.delete_if{|n| n.node_type != 0} # Directories
    res = @nodes.dup.delete_if{|n| n.node_type == 0} # Other items
    
    res.each{|r|
      flg = 0
      dir.each{|d|
        if r.path = ~/#{d.path}/ then
          flg = 1
          break
        end
      }
      if flg == 0 then
        @resulttree.addpath(r)
      end
    }
    return res
  end
 
  def count
    return @resultids.size
  end
  
  def all_covered(slat, slon, elat, elon)
    tablename = "nodes"
    
    #if @qsetid!=0 then
     # if $isInitial==1 then 
    if session[:isInitial] == 1 then
      nodelist = "(select distinct ancestor as id from node_lineages where descendant in (#{@resultids.join(',')})) n"
      return [] if @resultids.empty?
    else
      nodelist = "nodes n"
    end

    qstr = <<-EOM
      select latitude_lb,longitude_lb, latitude_rt, longitude_rt, 
             count(*) as cnt
      from spatial_and_time_attributes s join #{nodelist} on n.id = s.node_id
      where s.latitude_lb<#{slat} and s.latitude_rt>#{elat}
        and s.longitude_lb<#{slon} and s.longitude_rt>#{elon}
        group by s.latitude_lb,s.longitude_lb, s.latitude_rt, s.longitude_rt
    EOM
    return Node.find_by_sql(qstr)
  end

  def partial_covered(slat, slon, elat, elon)
    #if @qsetid!=0 then
   # if $isInitial==1 then
    if session[:isInitial] == 1 then
      nodelist = "(select distinct ancestor as id from node_lineages where descendant in (#{@resultids.join(',')})) n"
      return [] if @resultids.empty?
    else
      nodelist = "nodes n"
    end
    qstr = <<-EOM
      select latitude_lb,longitude_lb, latitude_rt, longitude_rt, 
             count(*) as cnt
      from spatial_and_time_attributes s join #{nodelist} on n.id = s.node_id
      where not (latitude_lb<#{slat} and latitude_rt>#{elat}
             and longitude_lb<#{slon} and longitude_rt>#{elon})
        and latitude_lb<>latitude_rt
        and longitude_lb<>longitude_rt
        group by latitude_lb,longitude_lb, latitude_rt, longitude_rt
    EOM
    return Node.find_by_sql(qstr)
  end

  def points(slat, slon, elat, elon)
#    if @qsetid!=0 then
   # if $isInitial==1 then
    if session[:isInitial] == 1 then
      nodelist = "(select distinct ancestor as id from node_lineages where descendant in (#{@resultids.join(',')})) n"
      return [] if @resultids.empty?
    else
      nodelist = "nodes n"
    end
    qstr = <<-EOM
      select latitude_lb,longitude_lb, latitude_rt, longitude_rt, 
             count(*) as cnt
      from spatial_and_time_attributes s join #{nodelist} on n.id = s.node_id
      where latitude_lb=latitude_rt
        and longitude_lb=longitude_rt
      group by latitude_lb,longitude_lb
    EOM
    return Node.find_by_sql(qstr)
  end

 
#------------------------------------------------------ 
# 一時テーブルを使う版
#------------------------------------------------------
  def CrossResult.get_keyname_and_keyvalue_list_fromtmptable(isInitial, tmptable, user)
    igr_attrs = (GFDNAVI_IGNORED_ATTRS + GFDNAVI_INVISIBLE_ATTRS).map{|att| "'#{att}'"}.join(", ")
    ignrattr = igr_attrs.empty? ? "" : "and not (k.name in (#{igr_attrs}))"
    
    if isInitial == 0 then
      qstr = <<-EOF
       select k.name as kname, k.value as kvalue, count(distinct l.descendant) as cnt
       from keyword_attributes k,
            node_lineages l,
            #{tmptable} n
       where l.ancestor=k.node_id 
         #{ignrattr} and k.value IS NOT NULL
         and l.descendant=n.id
       group by k.name,k.value
       order by k.name,k.value,cnt DESC
      EOF
    else
      if (access_conditions = Node.conditions_to_read(user))
        ac_cond = " and " + access_conditions + " "
      else
        ac_cond = ""
      end
      qstr = <<-EOF
         select k.name as kname, k.value as kvalue, count(distinct l.descendant) as cnt
         from keyword_attributes k,
            node_lineages l,
            nodes n
         where l.ancestor=k.node_id 
           and n.node_type!=0
           #{ignrattr} and k.value IS NOT NULL
           and l.descendant=n.id
           #{ac_cond}
         group by k.name,k.value
         order by k.name,k.value,cnt DESC
      EOF
    end
    return KeywordAttribute.find_by_sql(qstr)
  end

  def CrossResult.get_keyname_list_fromtmptable(isInitial, tmptable, user)
    igr_attrs = (GFDNAVI_IGNORED_ATTRS + GFDNAVI_INVISIBLE_ATTRS).map{|att| "'#{att}'"}.join(", ")
    ignrattr = igr_attrs.empty? ? "" : "and not (k.name in (#{igr_attrs}))"
    
    if isInitial == 0 then
      qstr = <<-EOF
       select k.name as kname, count(distinct l.descendant) as cnt
       from keyword_attributes k,
            node_lineages l,
            #{tmptable} n
       where l.ancestor=k.node_id 
         and not (k.name in (#{igr_attrs})) and k.value IS NOT NULL
         and l.descendant=n.id
       group by k.name
       order by k.name,cnt DESC
      EOF
    else
      if (access_conditions = Node.conditions_to_read(user))
        ac_cond = " and " + access_conditions + " "
      else
        ac_cond = ""
      end    
      qstr = <<-EOF
       select k.name as kname, count(distinct l.descendant) as cnt
       from keyword_attributes k,
            node_lineages l,
            nodes n
       where l.ancestor=k.node_id 
         and n.node_type!=0
         #{ignrattr}
         and k.value IS NOT NULL
         and l.descendant=n.id
         #{ac_cond}
       group by k.name
       order by k.name,cnt DESC
      EOF
    end
    return KeywordAttribute.find_by_sql(qstr)
  end

  def CrossResult.all_covered_fromtmptable(isInitial, tmptable, slat, slon, elat, elon, user)   
    if isInitial == 0 then
     qstr = <<-EOM
        select latitude_lb,longitude_lb, latitude_rt, longitude_rt, 
             count(distinct l.descendant) as cnt
        from spatial_and_time_attributes s,
           #{tmptable} n,
           node_lineages l
        where s.latitude_lb<#{slat} and s.latitude_rt>#{elat}
          and s.longitude_lb<#{slon} and s.longitude_rt>#{elon}
          and s.node_id=l.ancestor and l.descendant=n.id
          group by s.latitude_lb,s.longitude_lb, s.latitude_rt, s.longitude_rt
      EOM
    else
      if (access_conditions = Node.conditions_to_read(user))
        ac_cond = " and " + access_conditions
      else
        ac_cond = ""
      end
      qstr = <<-EOM
        select latitude_lb,longitude_lb, latitude_rt, longitude_rt, 
             count(distinct l.descendant) as cnt
        from spatial_and_time_attributes s,
           nodes n,
           node_lineages l
        where s.latitude_lb<#{slat} and s.latitude_rt>#{elat}
          and s.longitude_lb<#{slon} and s.longitude_rt>#{elon}
          and s.node_id=l.ancestor and l.descendant=n.id
          #{ac_cond} and n.node_type!=0
          group by s.latitude_lb,s.longitude_lb, s.latitude_rt, s.longitude_rt
      EOM
    end
    return Node.find_by_sql(qstr)
  end

  def CrossResult.partial_covered_fromtmptable(isInitial, tmptable, slat, slon, elat, elon, user)

    if isInitial == 0 then
      qstr = <<-EOM
      select latitude_lb,longitude_lb, latitude_rt, longitude_rt, 
             count(distinct l.descendant) as cnt
      from spatial_and_time_attributes s,
           #{tmptable} n,
           node_lineages l
      where not (latitude_lb<#{slat} and latitude_rt>#{elat}
             and longitude_lb<#{slon} and longitude_rt>#{elon})
        and latitude_lb<>latitude_rt
        and longitude_lb<>longitude_rt
        and s.node_id=l.ancestor and l.descendant=n.id        
        group by latitude_lb,longitude_lb, latitude_rt, longitude_rt
      EOM
    else
      if (access_conditions = Node.conditions_to_read(user))
        ac_cond = " and " + access_conditions
      else
        ac_cond = ""
      end
      qstr = <<-EOM
      select latitude_lb,longitude_lb, latitude_rt, longitude_rt, 
             count(distinct l.descendant) as cnt
      from spatial_and_time_attributes s,
           nodes n,
           node_lineages l
      where not (latitude_lb<#{slat} and latitude_rt>#{elat}
             and longitude_lb<#{slon} and longitude_rt>#{elon})
        and latitude_lb<>latitude_rt
        and longitude_lb<>longitude_rt
        and s.node_id=l.ancestor and l.descendant=n.id  
        #{ac_cond} and n.node_type!=0      
        group by latitude_lb,longitude_lb, latitude_rt, longitude_rt
      EOM
    end
    return Node.find_by_sql(qstr)
  end

  def CrossResult.points_fromtmptable(isInitial, tmptable, slat, slon, elat, elon, user)
    if isInitial == 0 then
      qstr = <<-EOM
        select latitude_lb,longitude_lb, latitude_rt, longitude_rt, 
             count(distinct l.descendant) as cnt
        from spatial_and_time_attributes s,
           #{tmptable} n,
           node_lineages l
        where latitude_lb=latitude_rt
          and longitude_lb=longitude_rt
          and s.node_id=l.ancestor and l.descendant=n.id        
        group by latitude_lb,longitude_lb
      EOM
    else
      if (access_conditions = Node.conditions_to_read(user))
        ac_cond = " and " + access_conditions
      else
        ac_cond = ""
      end
      qstr = <<-EOM
        select latitude_lb,longitude_lb, latitude_rt, longitude_rt, 
             count(distinct l.descendant) as cnt
        from spatial_and_time_attributes s,
           nodes n,
           node_lineages l
        where latitude_lb=latitude_rt
          and longitude_lb=longitude_rt
          and s.node_id=l.ancestor and l.descendant=n.id
          #{ac_cond} and n.node_type!=0
        group by latitude_lb,longitude_lb
      EOM
    end
    return Node.find_by_sql(qstr)
  end

=begin
  def CrossResult.get_keyvalue_list_fromtmptable(isInitial,tmptable,keyname)
    if isInitial==0 then
      nodelist=<<-EOM
        (select distinct ancestor as id
         from node_lineages l join #{tmptable} q on l.descendant=q.id) n
      EOM
    else
      nodelist = "nodes n"
    end
    qstr=<<-EOF
        select k.value as value, k.num_value as num_value, count(distinct n.id) as cnt
        from #{nodelist} join keyword_attributes k on k.node_id=n.id
        where k.name='#{keyname}'
        group by k.value
        order by cnt DESC
    EOF
    keylist=KeywordAttribute.find_by_sql(qstr)
    return keylist
 end
=end
=begin
  def get_keyname_list
#    if @qsetid!=0 then
   # if $isInitial==1 then
    if session[:isInitial]==1 then
        nodelist = "(select distinct ancestor as id from node_lineages where descendant in (#{@resultids.join(',')})) n"
        if @resultids.length ==0 then
           return []          
        end        
    else
      nodelist = "nodes n"
    end
    igr_attrs=""
    i=0
    GFDNAVI_IGNORED_ATTRS.each{ |iga|
      if i==0 then
        igr_attrs="'#{iga}'"
      else
        igr_attrs="#{igr_attrs},'#{iga}'"
      end
      i=i+1
    }
    
    GFDNAVI_INVISIBLE_ATTRS.each{ |iva|
      if i==0 then
       igr_attrs="'#{iva}'"
      else
       igr_attrs="#{igr_attrs}, '#{iva}'"
      end
      i=i+1
    }
    
    
    if GFDNAVI_IGNORED_ATTRS.size>0 then 
    qstr=<<-EOF
        select k.name as kname,count(distinct n.id) as cnt
        from  keyword_attributes k join #{nodelist} on k.node_id = n.id
        where not (k.name in (#{igr_attrs}))
        group by k.name
        order by cnt DESC
    EOF
    else
    qstr=<<-EOF
        select k.name as kname,count(distinct n.id) as cnt
        from  keyword_attributes k join #{nodelist} on k.node_id = n.id
        group by k.name
        order by cnt DESC
    EOF
    end
    keylist=KeywordAttribute.find_by_sql(qstr)
    return keylist
  end
  
 def get_keyname_and_keyvalue_list	 
#    if @qsetid!=0 then
   # if $isInitial==1 then 
   if session[:isInitial]==1 then
        nodelist = "(select distinct ancestor as id from node_lineages where descendant in (#{@resultids.join(',')})) n"
        if @resultids.length ==0 then
           return []          
        end        
    else
      nodelist = "nodes n"
    end
    igr_attrs=""
    i=0
    GFDNAVI_IGNORED_ATTRS.each{ |iga|
      if i==0 then
        igr_attrs="'#{iga}'"
      else
        igr_attrs="#{igr_attrs},'#{iga}'"
      end
      i=i+1
    }

    GFDNAVI_INVISIBLE_ATTRS.each{ |iva|
       if i==0 then
         igr_attrs="#{iga}'"
       else
         igr_attrs="#{igr_attrs}, '#{iva}'"
       end
       i=i+1
    }
    
    if GFDNAVI_IGNORED_ATTRS.size>0 then 
      qstr=<<-EOF
        select k.name as kname,k.value as kvalue, count(distinct n.id) as cnt
        from  keyword_attributes k join #{nodelist} on k.node_id = n.id
        where not (k.name in (#{igr_attrs})) and k.value IS NOT NULL
        group by k.name,k.value
        order by k.name,k.value,cnt DESC
      EOF
    else
      qstr=<<-EOF
        select k.name as kname,k.value as kvalue, count(distinct n.id) as cnt
        from  keyword_attributes k join #{nodelist} on k.node_id = n.id
        where k.value IS NOT NULL
        group by k.name,k.value
        order by k.name,k.value,cnt DESC
      EOF
    end
    keylist=KeywordAttribute.find_by_sql(qstr)
    return keylist
  end
=end

=begin
 def get_path
#  if @qsetid>0 then
    res=QueryHistory.find(:all,:conditions=>["queryset_id=? and querytype='path'",@qsetid])
    if res.size>0 then
      res[0].description=~/[P](.*)/
      return $1
    end
  end
  return "/"
 end
 
 def get_path_list
  if @qsetid>0 then
    QueryHistory.find(:all,:conditions=>["queryset_id=? and querytype='path'",@qsetid])  
    tables="(select v.* from nodes v, gfdnavi_querycache.queryset_#{@qsetid} c where v.id=c.vid)"
    path=get_path
  else
    tables="nodes"
    path="/"
  end
      sql1=<<-EOM
       select d2.path as dpath, count(distinct v.id) as cnt
       from nodes d1,nodes d2,#{tables} v
       where d1.path = ?
         and d2.parent_id = d1.id
         and v.path like #{concat("d2.path","'%'")}
       group by d2.path
       order by cnt DESC
     EOM
     pathlist = Node.find_by_sql([sql1,path])
   return pathlist
 end
 
 def get_keyvalue_list(keyname)
#    if @qsetid!=0 then
     # if $isInitial==1 then
   if session[:isInitial]==1 then
      nodelist = "(select distinct ancestor as id from node_lineages where descendant in (#{@resultids.join(',')})) n"
	else
      nodelist = "nodes n"
    end
    qstr=<<-EOF
        select k.value as value, k.num_value as num_value, count(distinct n.id) as cnt
        from #{nodelist} join keyword_attributes k on k.node_id=n.id
        where k.name='#{keyname}'
        group by k.value
        order by cnt DESC
    EOF
    keylist=KeywordAttribute.find_by_sql(qstr)
    return keylist
 end
 
 def get_querysetids(session_id)
   res=Array.new
   qhistories=QueryHistory.find_by_sql(["select distinct queryset_id from query_histories where session_id=? order by queryset_id;",session_id])
   qhistories.each{ |qh|
     res.push qh.queryset_id
   }
   return res
 end 
=end

end
