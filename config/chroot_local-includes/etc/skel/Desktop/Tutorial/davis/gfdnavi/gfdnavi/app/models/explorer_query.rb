require "activerecord_gfdnavi"

class ExplorerQuery
 
 def make_query(descriptions,user)
   results = Hash.new
   options=Array.new
   @spregion=[10,-80,350,80]
   descriptions.each{ |desc|
     if desc=~/\[(.*)\]/ then
       case $1
       when "K"
         options.push(get_condition4kw(desc))
       when "F"
         options.push(get_condition4fw(desc))
       when "S"
         o=get_condition4space(desc)
         options.push(o)
         @spregion=o[:spregion]
       when "T"
         options.push(get_condition4time(desc))
       when "P"
         options.push(get_condition4path(desc))
       when "N"
         options.push(get_condition4nodetype(desc))
       end
     end #if desc
   }
   if options then
     allqstr = generate_querystring(options,user)
   end
   #puts allqstr
   return allqstr
 end

 def generate_results(rnodes,expres,options)
   limit = 100
   if options[:limit]!=nil then
     limit = options[:limit].to_i
   end
   offset = 0
   if options[:offset]!=nil then
     offset = options[:offset].to_i
   end
     
   result_nodes=Array.new
   lcnt=0
   ocnt=0
   rnodes.each{|n|
     if ocnt>=offset then
       node=Hash.new
       node[:type] = Integer=== n.node_type ? Node::NODE_TYPES[n.node_type] : n.node_type
       node[:path] = n.path
       ttl = n.stdname("title")
       node[:title] = ttl ? ttl.value : n.name
       desc = n.stdname("description")
       node[:description] = desc ? desc.value : ''
       node[:name] = n.name
       result_nodes.push(node)      
       lcnt=lcnt+1
     end
     ocnt=ocnt+1
     if lcnt>=limit then
       break
     end
   }
   results = Hash.new     
   results["nodes"]=result_nodes
   #show keyword facet
   if options["show_kwfacets"].to_i==1 then
     if options["show_kwvalues"].to_i==1 then
       keyname="-"
       kcnt=-1
       keyvalues=Array.new
       keylist = expres.get_keyname_and_keyvalue_list
       if keylist!=nil then
         keywords = Array.new
         keylist.each{ |kl|
           if keyname.downcase!=kl.kname.downcase then
             if kcnt>-1 then
               keycount = Hash.new
               keycount[:keyname]=keyname
               keycount[:count]=kcnt
               keycount[:keyvalues]=keyvalues
               keywords.push(keycount)
             end
             keyvalues=Array.new
             if kl.cnt then
               kcnt=kl.cnt.to_i
             else
               kcnt=0
             end
             keyname=kl.kname
           else
             if kl.cnt then
               kcnt=kcnt+kl.cnt.to_i
             end
           end
           kvalue=Hash.new
           kvalue[:keyvalue]=kl.kvalue
           if kl.cnt then
             kvalue[:count]=kl.cnt.to_i
           else
             kvalue[:count]=0            
           end
           keyvalues.push(kvalue)
         }
         keycount = Hash.new
         keycount[:keyname]=keyname
         keycount[:count]=kcnt
         keycount[:keyvalues]=keyvalues
         keywords.push(keycount)
         keywords.reverse!{|a,b|
           a[:count].to_i<=>b[:count].to_i
         }
         results["keywords"]=keywords
       end      
     else
       keylist = expres.get_keyname_list
       if keylist!=nil then
         keywords = Array.new
         keylist.each{ |kl|
           keycount = Hash.new
           keycount[:keyname]=kl.kname
           keycount[:count]=kl.cnt
           keywords.push(keycount)
         }
         results["keywords"]=keywords
       end
     end
   end 
   #show spatial facet
   if options["show_spfacets"].to_i==1 then
     spfacets=Hash.new
     results[:spatial_attributes]=spfacets
     points=Array.new
     spfacets[:points]=points
     expres.points(@spregion[0],@spregion[1],@spregion[2],@spregion[3]).each{|p|
       pnt=Hash.new
       pnt[:latitude_lb]=p.latitude_lb
       pnt[:longitude_lb]=p.longitude_lb
       pnt[:latitude_rt]=p.latitude_rt
       pnt[:longitude_rt]=p.longitude_rt
       pnt[:count]=p.cnt
       points.push(pnt)
     }
     partial_covered_regions=Array.new
     spfacets[:partial_covered]=partial_covered_regions
     expres.partial_covered(@spregion[0],@spregion[1],@spregion[2],@spregion[3]).each{|p|
       rgn=Hash.new
       rgn[:latitude_lb]=p.latitude_lb
       rgn[:longitude_lb]=p.longitude_lb
       rgn[:latitude_rt]=p.latitude_rt
       rgn[:longitude_rt]=p.longitude_rt
       rgn[:count]=p.cnt
       partial_covered_regions.push(rgn)
     }
     all_covered_regions=Array.new
     spfacets[:all_covered]=all_covered_regions
     expres.all_covered(@spregion[0],@spregion[1],@spregion[2],@spregion[3]).each{|p|
       rgn=Hash.new
       rgn[:latitude_lb]=p.latitude_lb
       rgn[:longitude_lb]=p.longitude_lb
       rgn[:latitude_rt]=p.latitude_rt
       rgn[:longitude_rt]=p.longitude_rt
       rgn[:count]=p.cnt
       all_covered_regions.push(rgn)
     }
   end
   return results
  end

 def generate_querystring(options,user)
      cnt=0
      sptmky=Array.new
      sptm_qstr_conditions=Array.new
      node_qstr_conditions=Array.new
     
      options.each{ |qh|
        if qh[:querytype] == "keyword" || qh[:querytype]=="keyname" || qh[:querytype] == "freeword" then
          qstr=<<-EOM
            select r.descendant as nid, 'k#{cnt}' as qno, r.rel_depth as relevance
            from keyword_attributes k join node_lineages r on k.node_id = r.ancestor
            where #{qh[:cond]}
          EOM
          sptmky.push(qstr)
          if qh[:querytype] == "freeword" then
            qh[:cond] =~/name like (.*) or/            
            qstr=<<-EOM
              select r.descendant as nid, 'k#{cnt}' as qno, r.rel_depth as relevance
              from nodes n join node_lineages r on n.id = r.descendant
              where name like #{$1}
            EOM
            sptmky.push(qstr)
          end	
          cnt=cnt+1         
        elsif qh[:querytype]=="space" || qh[:querytype]== "time" then
     	  sptm_qstr_conditions.push(qh[:cond])
     	elsif qh[:querytype]=="path" || qh[:querytype]== "nodetype" then
     	  node_qstr_conditions.push(qh[:cond])     	
        end
      }
     #ノードタイプが画像であるものを検索対象から外す
      node_qstr_conditions.push("n.node_type!=2")
      
     #アクセス権の記述を加える
     access_conditions = Node.conditions_to_read(user)
     if access_conditions then
       node_qstr_conditions.push(access_conditions)
     end      
     
      if node_qstr_conditions.size>0 then
        node_qstr=<<-EOM
          select r.descendant as nid, 'n' as qno, r.rel_depth as relevance
          from nodes n join node_lineages r on n.id = r.descendant
          where #{node_qstr_conditions.join(" and ")}
        EOM
        cnt=cnt+1
        sptmky.push(node_qstr)
      end
          
     #spatial_and_time_attributesにおける検索部分を構成する
     if sptm_qstr_conditions.size>0 then
       sptm_qstr=<<-EOM
         select r.descendant as nid, 's' as qno, r.rel_depth as relevance
         from spatial_and_time_attributes s join node_lineages r on s.node_id = r.ancestor
         where #{sptm_qstr_conditions.join(" and ")}
       EOM
       cnt=cnt+1
       sptmky.push(sptm_qstr)
     end

     
     #上の二つを合わせる＆nodesでの条件（path）を合わせる
     allqstr =<<-EOM
         select n.*, sum(score) as score
         from 
         (select nid,qno,max(relevance) as score
          from (#{sptmky.join(' union all ')}) ar
          group by nid,qno) q join nodes n on q.nid = n.id
         group by nid
         having count(distinct qno)=#{cnt}
         order by sum(score) desc;
     EOM
     return allqstr 
 end

 def get_condition4kw(description)
   description=~/\[(.*)\](.*)=(.*)/
   qtype=$1
   qname=$2
   qvalue=$3
   options={
     :querytype=>"keyword",
     :cond=>"name = '#{$2}' and value = '#{$3}'",
     :description => description
   }
   return options
 end

 def get_condition4fw(description)
   description=~/\[(.*)\](.*)/
   qtype=$1
   qname=$2
   options={
     :querytype=>"freeword",
     :cond=>"name like '%#{$2}%' or value like '%#{$2}%'",
     :description => description
   }
   return options
 end
 
 def get_condition4time(description)
   description =~/\[(.*)\](.*)~(.*)/
   options={
     :querytype=>"time",
     :cond=>"starttime <= '#{$3}' or endtime >= '#{$2}'",
     :description => description
   }
 end
 
 def get_condition4path(description)
   description=~/\[(.*)\](.*)/
   qtype=$1
   qname=$2
   options={
     :querytype => "path",
     :cond => "n.path = '#{qname}'",
     :description => description     
   }   
   return options
 end
 
  def get_condition4nodetype(description)
   description=~/\[(.*)\](.*)/
   qtype=$1
   str=$2
   str.gsub!("data","directory,variable")
   qnames=str.split(/,/)
      
   nodetypes={"directory"=>0,
              "variable"=>1,
              "knowledge"=>3,
              "function"=>4,
              "draw_method"=>5}
              
   conds=Array.new
   qnames.each{ |qname|
     conds.push("n.node_type = #{nodetypes[qname]}")
   }             
   conds_str = conds.join(" or ")
   
   options={
     :querytype => "nodetype",
     :cond => "(#{conds_str})",
     :description => description     
   }   
   return options
 end
 
  def get_condition4space(description)
   description=~/\[(.*)\]\((.*),(.*)\)-\((.*),(.*)\)/
  
   start_lon = ( $2 != "" ? $2 : "0" )
   start_lat = ( $3 != "" ? $3 : "-90" )
   end_lon = ( $4 != "" ? $4 : "360" )
   end_lat = ( $5 != "" ? $5 : "90" )

   @dateline=0  # dummy (horinout 2008/07/28): Kept for a while to avoid to be nil, but should be removed in fufure
   
   if start_lon.to_f > end_lon.to_f then
     qcond=<<-EOM
        ( longitude_lb >= #{start_lon} or longitude_rt <= #{end_lon} )
        and latitude_lb <= #{end_lat} and latitude_rt >= #{start_lat} 
        and not(node_id IS NULL)
     EOM
   else
     qcond=<<-EOM
        longitude_lb <= #{end_lon} and longitude_rt >= #{start_lon} 
        and latitude_lb <= #{end_lat} and latitude_rt >= #{start_lat} 
        and not(node_id IS NULL)
     EOM
   end
   options={
     :querytype=>"space",
     :cond=>qcond,
     :description => description
   }
   options[:spregion]=[start_lon,start_lat,end_lon,end_lat]

   return options
 end 

end
