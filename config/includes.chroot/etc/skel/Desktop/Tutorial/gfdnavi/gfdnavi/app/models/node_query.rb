# -*- coding: cp932 -*-
require "activerecord_gfdnavi"

class NodeQuery
 
  def make_query(descriptions, user) #条件の場合分け
    flag = 0
    results = Hash.new
    options = Array.new(0)
    @spregion = [10, -80, 350, 80]
#debugger
    if descriptions.size == 1 && descriptions[0] == "all" then
      return "all"
    end

    descriptions.each{|desc|
=begin   
     if desc == "all"
       #allqstr = "select * from nodes;"
       allqstr="all"
       return allqstr
     end
=end     
      if desc =~ /(.*)=(.*)/ then
        
        case $1
        when "fw"
          options.push(get_condition4fw(desc))
        when "tm"
          options.push(get_condition4time(desc))
        when "path"
          options.push(get_condition4path(desc))
        when "datatype"
          options.push(get_condition4nodetype(desc))
        when "kw"
          options.push(get_condition4kwname(desc))
        when if $1 =~ /(.*)\./ then
               case $1      
               when "kw"
                 options.push(get_condition4kw(desc))
               else
                 options = Array.new(0)
                 break
               end
             end
        end
        
      elsif desc =~ /(.*)\[.*\]/ then
        case $1
        when "sp.overlap"
          
          o = get_condition4space(desc)
          options.push(o)
          @spregion = o[:spregion]
        else
          options = Array.new(0)
          break
        end
      end
    }

    if options && options.any?
      return generate_querystring(options, user)
    else
      return nil
    end
  end
  
  def generate_results(rnodes, expres, options)
    results = Hash.new     
    
    #show keyword facet
    if options["show_kwfacets"].to_i == 1 then
      if options["show_kwvalues"].to_i == 1 then
        keyname = "-"
        kcnt = -1
        keyvalues = Array.new
        keylist = expres.get_keyname_and_keyvalue_list
        unless keylist.nil?
          keywords = Array.new
          keylist.each{|kl|
            #if kl.kname == $qname then
            if keyname.downcase != kl.kname.downcase then 
              # if kl.kname == $qname then
              if kcnt > -1 then
                keywords.push({:keyname => keyname, :count => kcnt, :keyvalues => keyvalues})
              end
              # end
              keyvalues = Array.new
              kcnt = (kl.cnt || 0).to_i
              keyname = kl.kname
            else
              kcnt += kl.cnt.to_i if kl.cnt
            end
            
            keyvalues.push({:keyvalue => kl.kvalue, :count => (kl.cnt || 0).to_i})
            #end
          }
          
          keywords.push({:keyname => keyname, :count => kcnt, :keyvalues => keyvalues})
          keywords.reverse!{|a, b|
            a[:count].to_i <=> b[:count].to_i
          }
          results["keywords"] = keywords
        end
        
      else
        keylist = expres.get_keyname_list
        unless keylist.nil?
          keywords = Array.new
          keylist.each{|kl|
            keywords.push({:keyname => kl.kname, :count => kl.cnt})
          }
          results["keywords"] = keywords
        end
      end
    end 
    
    #show spatial facet
    if options["show_spfacets"].to_i == 1 then
      spfacets = Hash.new
      results[:spatial_attributes] = spfacets
      [:points, :partial_covered, :all_covered].each{|sptype|
        spfacets[sptype] = expres.send(sptype, *(@spregion)).map{|p|
          {:latitude_lb   => p.latitude_lb,
            :longitude_lb => p.longitude_lb,
            :latitude_rt  => p.latitude_rt,
            :longitude_rt => p.longitude_rt,
            :count        => p.cnt}
        }
      }
    end 
    if options["show_pathtree"].to_i == 1 then
      pt_ent = 15 #仮に15とする @debug 2010-07-21
      results["pathtree"] = generate_pathtree(rnodes, pt_ent)
    end
    
    return results
  end
  
  def generate_results_fromtmptable(isInitial, tmptable, options, user=nil)
    results = Hash.new
    @spregion = [10, -80, 350, 80]
    
    options ||= Hash.new
    
    #show keyword facet
    if options["show_kwfacets"].to_i == 1 then
      if options["show_kwvalues"].to_i == 1 then     
        keylist = CrossResult.get_keyname_list_fromtmptable(isInitial, tmptable, user)
        return if keylist.nil?
        keywords = Hash.new
        keylist.each{|kl|
          keywords[kl.kname] = {:keyname => kl.kname, :count => kl.cnt, :keyvalues => []}
        }
=begin       
       puts "----------------------------------"
       puts keywords.to_s
       puts "----------------------------------"
=end
        keylist = CrossResult.get_keyname_and_keyvalue_list_fromtmptable(isInitial, tmptable, user)
        unless keylist.nil?
          keylist.each{|kl|
            keywords[kl.kname][:keyvalues].push({:keyvalue => kl.kvalue, :count => (kl.cnt || 0).to_i})
          }
        end
        keywords_array = keywords.values  
        keywords_array.reverse!{|a, b|
          a[:count].to_i <=> b[:count].to_i
        }
        results[:keywords] = keywords_array
      else
        keylist = CrossResult.get_keyname_list_fromtmptable(isInitial, tmptable, user)
        unless keylist.nil?
          results["keywords"] = keylist.map{|kl|
            {:keyname => kl.kname, :count => kl.cnt}
          }
        end
      end
    end 
    
    #show spatial facet
    if options["show_spfacets"].to_i == 1 then
      
      spfacets = Hash.new
      results[:spatial_attributes] = spfacets
      [:points, :partial_covered, :all_covered].each{|sptype|
        spfacets[sptype] = CrossResult.send(sptype.to_s + "_fromtmptable", *([isInitial, tmptable] + @spregion + [user])).map{|p|
          {:latitude_lb   => p.latitude_lb,
            :longitude_lb => p.longitude_lb,
            :latitude_rt  => p.latitude_rt,
            :longitude_rt => p.longitude_rt,
            :count        => p.cnt}
        }
      }
    end 
    if options["show_pathtree"].to_i == 1 then
      pt_ent = 15 #仮に15とする @debug 2010-07-21
      res = generate_pathtree_fromtmptable(isInitial, tmptable, pt_ent, user) 
      results["pathtree"] = res unless res.nil?
    end   
    return results
  end
  
  
  def generate_pathtree(nodes, pt_ent, opts={})
    pathtree = {:name => "/", :count => 0, :path => "/"}
    nodes.each{|n|
      nd = {:path => n.path, :id => n.id, :node_type => n.node_type}
      # 検索結果がパラメータ cnt を持つかどうかでカウンターの動作を分ける
      nd[:count] = n.cnt.to_i if (has_cnt = n.respond_to?(:cnt))

      cur = pathtree
      path = ""
      path_elements = n.path.split(/\//)
      depth = path_elements.size
      dcount = 0
      path_elements.each{|name|
        dcount += 1
        if name == '' then
          cur[:count] += 1 unless has_cnt
          path = "/"
          next
        end
        parent = path # 親要素を記録
        path = "#{path}#{name}/"
        if dcount == depth then
          if has_cnt
            cur[:count] += nd[:count] unless cur[:already_count]
            dir = {:name => name, :path => path, :count => nd[:count], :parent => parent, :already_count => true}
            cur[:directories] ||= Array.new
            cur[:directories].push(dir)
          end
          break
        end
        cur[:directories] ||= Array.new
        if (idx = cur[:directories].map{|c| c[:name]}.index(name))
          cur[:directories][idx][:count] += 1 unless has_cnt
          cur = cur[:directories][idx] # 次の階層に入る
        else
          dir = {:name => name, :path => path, :count => has_cnt ? 0 : 1, :parent => parent}
          cur[:directories].push(dir)
          cur = dir
        end
      }
      unless opts[:ignore_objects] # 初期ツリーの時はオブジェクトを入れない（どうしてこうしないといけないのか？）
        cur[:objects] ||= Array.new
        cur[:objects].push(nd)
      end
    }

    # 子ディレクトリが1つの場合、親ディレクトリと合体する
    stack = [pathtree]
    while stack.size > 0 do
      cur = stack.pop
      # get the node depth
      #if cur.key?(:directories) && cur[:directories].size == 1 then
      if cur[:directories] && (cur[:directories].size == 1) && (!cur[:objects])
        dir = cur[:directories][0]
        #merge name
        cur[:name] = (cur[:name] + "/" + dir[:name]).gsub(/\/+/, "/")
        # 他の要素はそのまま子から親にコピー
        [:path, :count, :objects, :directories].each{|key|
          cur[key] = dir[key]
        }
        # さらに合体できないかどうか確認する
        stack.push(cur) if cur[:directories]
      elsif cur.key?(:directories)
        # 何もせずスタックに子ディレクトリを追加
        stack += cur[:directories]
      end
    end
    
    #返すノードのエントリ数をpt_entに限定する
    cnt = 0
    queue = Array.new
    queue.push(pathtree)
    while queue.size > 0 do
      cur = queue.shift
      #リーフノードかどうか
      cur[:leaf] = cur[:directories] ? 0 : 1
      #オブジェクトがあるかどうか
      cur[:hasobj] = cur[:objects] ? 1 : 0 
      tmp_cnt = cnt
      tmp_cnt += cur[:directories].size if cur[:directories]
      tmp_cnt += cur[:objects].size if cur[:objects]
      cur[:treeicon] = "plus" # ひとまず子ノードが無いと仮定
      if tmp_cnt > pt_ent && cur != pathtree
        #curの子ノードを加えるとpt_entを超えてしまう場合
        #curの子ノードを削除してしまう
        # （ただし、トップディレクトリなら子ノードはすべて展開）
        cur.delete(:directories)
        cur.delete(:objects)
      elsif (cur[:directories] && (cur[:directories].size > 0)) || ((cur[:objects]) && (cur[:objects].size > 0))
        # 子ノードがあるが、上限を超えない場合は、子ディレクトリを queue に追加
        cnt = tmp_cnt
        queue += cur[:directories] if cur[:directories]
        cur[:treeicon] = "minus" # 子ノードがあるので変更
      end
    end

    return pathtree
  end
  
  def generate_pathtree_fromtmptable(isInitial, tmptable, pt_ent, user)
    if isInitial == 0 then
      qstr = <<-EOM
        select n.id,n.path,n.node_type
        from nodes n join #{tmptable} t on n.id=t.id
      EOM
    else
      return generate_initial_pathtree(pt_ent, user)
    end
    if (access_conditions = Node.conditions_to_read(user))
      qstr += "where " + access_conditions + " and n.node_type!=0"
    else
      qstr += "where n.node_type!=0"
    end

    nodes = Node.find_by_sql(qstr)

    #結果が0だったらnullを返す
    return nil if nodes.size == 0
    # from here, the original code was exactly the same sa that in generate_pathtree
    return generate_pathtree(nodes, pt_ent)
  end
  
  def generate_initial_pathtree(pt_ent, user)
    if (access_conditions = Node.conditions_to_read(user, "n1."))
      n1_ac = "and " + access_conditions
      n2_ac = "and " + Node.conditions_to_read(user, "n2.")
    else
      n1_ac = ""
      n2_ac = ""
    end
    if (n3_ac = Node.conditions_to_read(user, "n3."))
      n3_ac = "and " + n3_ac
    else
      n3_ac = ""
    end

    qstr = <<-EOM
      select n2.id as id,n2.path as path,n2.node_type as node_type,count(distinct l2.id) as cnt
      from nodes n1, node_lineages l1,
           nodes n2, node_lineages l2,
           nodes n3
      where n1.id=l1.ancestor
        and n2.id=l1.descendant
        and n2.id=l2.ancestor
        and n3.id=l2.descendant
        and n2.node_type=0
        and n1.path="/"
        #{n1_ac} #{n2_ac} #{n3_ac}
        and l2.rel_depth>0
        and l1.rel_depth<4
        and n3.node_type!=0
        group by n2.id,n2.path,n2.node_type
        order by n2.path
    EOM
    
    nodes = Node.find_by_sql(qstr)   

    #結果が0だったらnullを返す
    return nil if nodes.size == 0
    # from here, the original code was similar to that in generate_pathtree
    return generate_pathtree(nodes, pt_ent, :ignore_objects => true)
  end


  def generate_querystring(options, user)
    cnt = 0
    sptmky = Array.new
    sptm_qstr_conditions = Array.new
    node_qstr_conditions = Array.new
    
    options.each{|qh|
      case qh[:querytype]
      when "keyword", "keyname", "freeword"
        qstr = <<-EOM
          select r.descendant as nid, 'k#{cnt}' as qno, r.rel_depth as relevance
          from keyword_attributes k join node_lineages r on k.node_id = r.ancestor
          where #{qh[:cond]}
        EOM
        sptmky.push(qstr)
        if qh[:querytype] == "freeword" then
          qh[:cond] =~ /name like (.*) or/            
          qstr = <<-EOM
            select r.descendant as nid, 'k#{cnt}' as qno, r.rel_depth as relevance
            from nodes n join node_lineages r on n.id = r.ancestor
            where name like #{$1}
          EOM
          sptmky.push(qstr)
        end
        cnt += 1         
      when "space", "time"
        sptm_qstr_conditions.push(qh[:cond])
      when "path"
        nodepath_qstr = <<-EOM
          select r.descendant as nid, 'np' as qno, r.rel_depth as relevance
          from nodes n join node_lineages r on n.id = r.ancestor
          where #{qh[:cond]}          
        EOM
        sptmky.push(nodepath_qstr)
        cnt += 1
      when "nodetype"
        nodetype_qstr = <<-EOM
          select n.id as nid, 'nt' as qno, 0 as relevance
          from nodes n 
          where #{qh[:cond]} and n.node_type!=2
        EOM
        sptmky.push(nodetype_qstr)
        cnt += 1
      end
    }
    #ノードタイプが画像であるものを検索対象から外す
    node_qstr_conditions.push("n.node_type!=2")
    # ノードタイプは「親ノードが該当したら子孫ノードは全部該当」ルールは適用しない     
    if node_qstr_conditions.size > 0 then
      node_qstr = <<-EOM
        select n.id as nid, 'n' as qno, 0 as relevance
        from nodes n 
        where #{node_qstr_conditions.join(" and ")}
      EOM
    end
          
    #spatial_and_time_attributesにおける検索部分を構成する
    if sptm_qstr_conditions.size > 0 then
      sptm_qstr = <<-EOM
        select r.descendant as nid, 's' as qno, r.rel_depth as relevance
        from spatial_and_time_attributes s join node_lineages r on s.node_id = r.ancestor
        where #{sptm_qstr_conditions.join(" and ")}
      EOM
      cnt += 1
      sptmky.push(sptm_qstr)
    end
    
    #アクセス権の記述を加える
    access_conditions = Node.conditions_to_read(user)
    unless access_conditions.nil?
      access_conditions = "where " + access_conditions
    else
      access_conditions = ""
    end
    
    #上の二つを合わせる＆nodesでの条件（path）を合わせる
    allqstr = <<-EOM
      select n.id as id, sum(score) as score
      from 
      (select nid,qno,max(relevance) as score
       from (#{sptmky.join(' union all ')}) ar
       group by nid,qno) q join nodes n on q.nid = n.id
      #{access_conditions}
      group by nid
      having count(distinct qno)=#{cnt}
      order by sum(score) desc;
    EOM
#     puts "++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
#     puts allqstr
#     puts "++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
#debugger
    return allqstr      
  end

  def get_condition4kw(description)
    description =~ /(.*)\.(.*)=(.*)/
    qtype = $1
    qname = $2
    qvalue = $3
    options = {
      :querytype   => "keyword",
      :cond        => "name = '#{$2}' and value = '#{$3}'",
      :description => description}
    #p options
    return options
  end
  
  def get_condition4kwname(description)
    description =~ /(.*)=(.*)/
    qtype = $1
    qname = $2
    $qname = $2
    options = {
      :querytype   => "keyname",
      :cond        => "name = '#{$2}'",
      :description => description}
    #s "kname"
    return options
  end
  
  def get_condition4fw(description)
    description =~ /(.*)=(.*)/
    qtype = $1
    qname = $2
    options = {
      :querytype   => "freeword",
      :cond        => "name like '%#{$2}%' or value like '%#{$2}%'",
      :description => description}
    return options
  end
  
  def get_condition4time(description)
    description =~ /(.*)=\[(.*),(.*)\]/
    #p $2,$3
    options = {
      :querytype   => "time",
      :cond        => "starttime <= '#{$3}' and endtime >= '#{$2}'",
      :description => description}
  end
  
  def get_condition4path(description)
    description =~ /(.*)=(.*)/
    qtype = $1
    qname = $2
    options = {
      :querytype => "path",
      :cond      => "n.path = '#{qname}'"}
    return options
  end
  
  def get_condition4nodetype(description)
    description =~ /(.*)=(.*)/
    qtype = $1
    str = $2
    str.gsub!("data", "directory,variable")
    qnames = str.split(/,/)
    
    nodetypes = {
      "directory"   => 0,
      "variable"    => 1,
      "knowledge"   => 3,
      "function"    => 4,
      "draw_method" => 5}
    
    conds_str = qnames.map{|qname| "n.node_type = #{nodetypes[qname]}"}.join(" or ")
    
    options = {
      :querytype   => "nodetype",
      :cond        => "(#{conds_str})",
      :description => description}
    return options
  end
  
  def get_condition4space(description)
    description =~ /(.*)\[(.*),(.*),(.*),(.*)\]/
    
    start_lon = ( $2 != "" ? $2 : "0" )
    start_lat = ( $3 != "" ? $3 : "-90" )
    end_lon = ( $4 != "" ? $4 : "360" )
    end_lat = ( $5 != "" ? $5 : "90" )
    
    @dateline = 0  # dummy (horinout 2008/07/28): Kept for a while to avoid to be nil, but should be removed in fufure
    
    if start_lon.to_f > end_lon.to_f then
      qcond = <<-EOM
        ( longitude_lb >= #{start_lon} or longitude_rt <= #{end_lon} )
        and latitude_lb <= #{end_lat} and latitude_rt >= #{start_lat} 
        and not(node_id IS NULL)
      EOM
    else
      qcond = <<-EOM
        longitude_lb <= #{end_lon} and longitude_rt >= #{start_lon} 
        and latitude_lb <= #{end_lat} and latitude_rt >= #{start_lat} 
        and not(node_id IS NULL)
      EOM
    end
    options = {
      :querytype   => "space",
      :cond        => qcond,
      :description => description,
      :spregion    => [start_lon, start_lat, end_lon, end_lat]}
    
    return options
  end 

end
