# -*- coding: cp932 -*-
class ResultInfo
  attr_accessor :server_name, :keyvalues,:pathtree, :point, :partial, :all_covered
  
  def initialize(server_name)
    @server_name = server_name
    @keyvalues = Hash.new
    @pathtree = Hash.new
    @point = Hash.new
    @partial = Hash.new
    @all_covered = Hash.new
  end
  
  def add_keyvalue_count(keyname, keyvalue, count)
    @keyvalues[keyname] ||= Hash.new
    @keyvalues[keyname][keyvalue] ||= 0
    @keyvalues[keyname][keyvalue] += count.to_i
  end
  
  
  def add_keyname_count(keyname, count)
    @keyvalues[keyname] ||= Hash.new
    @keyvalues[keyname][:all_values] ||= 0
    @keyvalues[keyname][:all_values] += count.to_i
  end
  
  def set_keyname_count(keyname, count)
    @keyvalues[keyname] ||= Hash.new
    @keyvalues[keyname][:all_values] = count
  end
  
  def get_keynames
    return @keyvalues.keys
  end
  
  def get_keynames_sorted
    sorted = @keyvalues.to_a.sort{|a, b|
      (b[1][:all_values] <=> a[1][:all_values])
    }
    sorted_keys = sorted.map{|srtd| srtd[0]}
    return sorted_keys
  end
  
  
  def get_keyvalues(keyname)
    return @keyvalues[keyname]
  end
  
  def get_keyvalues_sorted(keyname)
    sorted_keys = @keyvalues[keyname].to_a.sort{|a, b|
      (b[1] <=> a[1])
    }
    return sorted_keys
  end
  
  def get_keyvalue_count(keyname, keyvalue)
    return @keyvalues[keyname][keyvalue]
  end
  
  def get_keyname_count(keyname)
    return @keyvalues[keyname][:all_values]
  end
  
  def add_all_covered(added) #@added 2011.1.18 chiemi
    added.each_key{ |region|
      if @all_covered.key?(region) then
        @all_covered[region] = @all_covered[region] + added[region]
      else
        @all_covered[region] = added[region]
      end
    }
  end
  
  def add_partial(added) #@added 2011.1.18 chiemi
    added.each_key{ |region|
      if @partial.key?(region) then
        @partial[region] = @partial[region] + added[region]
      else
        @partial[region] = added[region]
      end
    }
  end
  
  def add_point(added)
    added.each_key{ |pnt|
      if @point.key?(pnt) then
        @point[pnt] = @point[pnt] + added[pnt]
      else
        @point[pnt] = added[pnt]
      end
    }
  end
  
  def get_count_point(pnt)
    return @point[pnt]
  end
  
  def get_count_partial(reg)
    return @partial[reg]
  end
  
  def get_count_all_covered(reg)
    return @all_covered[reg]
  end
  
  def ResultInfo.get_pathtree_viewinfo(pathtree, serverurl, urlroot, basedepth=0, restNodes=nil)
    viewInfoList = Array.new
    parents = Array.new
    
    return pathtree if pathtree.nil? || (pathtree == "no result")
    
    urlprefix = "http://" + serverurl
    urlprefix << urlroot if urlroot
    urlprefix.gsub!(/\/+\z/, "") # 末尾の "/" を取り除く

    ptStack = [pathtree]
    ptStackDepth = [basedepth]
    restNodes ||= []
    restNodes[basedepth] ||= 1 # まずは深さbasedepthのところに、最初の要素を追加
    while ptStack.size > 0 
      dir = ptStack.pop
      depth = ptStackDepth.pop
      
      if dir && dir.elements["name"]
        indents_tmp = ""
        #インデントを表示
        for d in 0...depth
          imgname = (restNodes[d] > 0) ? "tate.png" : "space.gif"
          indents_tmp << "<img src=#{urlroot}/images/tree/#{imgname} class=\"tree_element\">"
        end
        dir_last = (restNodes[depth] == 1)
        imgname = dir_last ? "last.png" : "t.png"
        indents = indents_tmp + "<img src=#{urlroot}/images/tree/#{imgname} class=\"tree_element\">"
        
        prevDepth = depth
        if dir.elements["path"] then
          input_path = dir.elements["path"].text
          input_path.strip!
          # @todo : how does it work when input_path = "/" ?
          # パスが "/" の場合には input_path も "/" のままにする
          input_path.gsub!(/\/\z/, "") unless input_path == "/"
          # nodepath に与える path の末尾の / はルートの場合でも除去
          nodepath = serverurl.gsub("/", "_") << "_" << input_path.gsub(/\/\z/, "").gsub("/", "_")

          # 次のディレクトリ以降に、親パスが分からない場合に備えて、現在のパスを保存
          parents.push(input_path.gsub(/\/\z/, ""))
          
          parent_nodepath = nil
          if dir.elements["parent"]
            # サーバ側が新バージョンなら親パスが分かる
            parent_path = dir.elements["parent"].text.strip.gsub(/\/\z/, "")
            parent_nodepath = serverurl.gsub("/", "_") << "_" << parent_path.gsub("/", "_")
          else 
            # 親パスが分からない場合、推定を試みる
            path_elem = input_path.split("/")
            unless path_elem.empty?
              tmp_parent_path = path_elem[0..-2].join("/")
              if parents.include?(tmp_parent_path)
                parent_nodepath = serverurl.gsub("/", "_") << "_" << tmp_parent_path.gsub("/", "_")
              end
            end
          end
          if dir.elements["treeicon"] then
            viewInfo = {
              :nodepath        => nodepath,
              :parent_nodepath => parent_nodepath,
              :indent          => indents,
              :display_path    => dir.elements["name"].text,
              :input_path      => input_path,
              :input_server    => serverurl,
              :restNodes       => restNodes.dup, # 複製すべし
              :depth           => depth,
              :count           => dir.elements["count"].text,
              :treeicon        => dir.elements["treeicon"].text,
              :imgsrc          => urlroot + "/images/tree/" + dir.elements["treeicon"].text + ".gif",
              :nodetype        => "directory"}
          end
          has_subdir = false
          cdirs = dir.elements["directories"] # 子ディレクトリの登録
          if cdirs && cdirs.class.to_s == "REXML::Element" then
            cdirs.each{|d|
              if d.class.to_s == "REXML::Element" then
                has_subdir = true
                ptStack.push(d) # キューに追加
                ptStackDepth.push(depth + 1)
                restNodes[depth + 1] ||= 0 # 無ければ初期化
                restNodes[depth + 1] += 1
              end
            }
          end
          # オブジェクトを表示
          cobjs = dir.elements["objects"]
          if cobjs && cobjs.class.to_s == "REXML::Element" then
            viewInfo[:objInfoList] = Array.new
            cobjs = cobjs.dup.delete_if{|o| o.class.to_s != "REXML::Element"}
            cobjs.each_with_index{|o, i|
              if o.class.to_s == "REXML::Element" then
                path      = o.elements["path"].text
                nodeid    = o.elements["id"].text
                node_type = o.elements["node-type"].text.to_i
                objname   = o.elements["path"].text.split("/").pop

                # インデントの追加
                imgname = dir_last ? "space.gif" : "tate.png"
                indents = indents_tmp + "<img src=#{urlroot}/images/tree/#{imgname} class=\"tree_element\">"
                # この深さにおける最後の要素かどうか
                imgname = ((i + 1 < cobjs.size) || has_subdir) ? "t.png" : "last.png"
                indents << "<img src=#{urlroot}/images/tree/#{imgname} class=\"tree_element\">"

                url = urlprefix + "/data#{path}.html"
                objInfo = {
                  :nodetype => "object",
                  :indent   => indents,
                  :imgsrc   => urlroot + "/images/tree/page.gif",
                  :objname  => objname,
                  :url      => url,
                  :path     => path}
                case node_type
                when Node::VARIABLE
                  objInfo[:analysis] = urlprefix + "/finder/add_to_list?path=#{path}"
                when Node::KNOWLEDGE
                  objInfo[:knowledge] = urlprefix + "/knowledge/show?path=#{path}"
                when Node::IMAGE
                  objInfo[:image] = url
                end
                viewInfo[:objInfoList].push(objInfo)
              end #if o.class.to_s=="REXML::Element" then
            }#cobjs.each{ |o|
          end #if cobjs && cobjs.class.to_s=="REXML::Element" then                           
        else #if dir.elements["path"] then
        end
      end # if dir && dir.elements["name"]
      restNodes[depth] = restNodes[depth] - 1 #表示したノード分の数を減らす
=begin
      if restNodes[depth] == 0 then
        dp = depth
        hasdescendant = 0            
        while dp < restNodes.size do
          if restNodes[dp] > 0 then
            hasdescendant = 1
          end
          dp += 1
        end
        if hasdescendant == 0 then
          closestr = "</div>"
          dp = depth - 1
          while dp > 0 do
            if restNodes[dp] == 0 then
              closestr << "</div>"
            end
            dp -= 1
          end
          viewInfo[:closestr] = closestr # たぶん使われていない
        end
      end
=end
      viewInfoList.push(viewInfo) if viewInfo && viewInfo.size > 0
    end # while ptStack.size>0
    return viewInfoList
  end
  
  
  
end
