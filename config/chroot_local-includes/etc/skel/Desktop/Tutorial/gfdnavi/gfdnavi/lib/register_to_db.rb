# -*- coding: cp932 -*-
require "kconv"
require 'opendapdir'
require 'localdir'

# とりあえず追加。
# あとでKnowledgeのsaveとrestoreを分離できたら消せるようになる。
require "numru/gfdnavi_data"
require "numru/gfdnavi_data/local"


#################################################
module NumRu

  # == To be merged into the original GPhys distribution
  # Geospatio-temporal coordinate handing
  class GPhys

    def lon_lat_t_ranges
      geocoords = lon_lat_t_coords
      georanges = Hash.new
      geocoords.each do |nm,crd|
        georanges[nm] = [crd.max, crd.min]
      end
      georanges
    end

    def lon_lat_t_coords
      geocoords = Hash.new   # will ba Hash of :lon, :lat, and :time

      #< from the coordinate variables >
      for dim in 0...self.rank
        crd = add_lon_lat_t_coords(self.coord(dim), geocoords)
      end

      #< from the "coordinates" attributes (CF convenction) >
      if geocoords.length!=3 and data.respond_to?(:file) and \
               (nms = self.get_att("coordinates"))
        nms.split.each do |nm|
          if data.file.var(nm)
            crd = GPhys::IO.open(data.file, nm).data
            geocoords = add_lon_lat_t_coords(crd, geocoords)
          end
        end
      end

      #< from scalar variables >
      if geocoords.length!=3 && data.respond_to?(:file)
        data.file.var_names.each do |nm|
          if /(lon|lat|time)/i =~ nm
            crd = GPhys::IO.open(data.file, nm).data
            if crd.length==1
              geocoords = add_lon_lat_t_coords(crd, geocoords) 
            end
          end
        end
      end

      geocoords.each do |nm,crd|
        if nm==:lon or nm==:lat
          un = crd.units
          if un and /(^degree|^\s*$)/i !~ un.to_s
            geocoords[nm] = un.convert2(crd,Units['degrees'])
          end
        end
      end

      geocoords
    end

    ########################
    private
    def add_lon_lat_t_coords(coord, geocoords)
      unt = coord.units.to_s
      if !geocoords[:lon] && /^degrees?_east$/i =~ unt
        geocoords[:lon] = coord
      elsif !geocoords[:lat] && /^degrees?_north$/i =~ unt
        geocoords[:lat] = coord
      elsif !geocoords[:time] &&  /since/ =~ unt
        geocoords[:time] = coord
      end
      nm = coord.name
      if !geocoords[:lon] && /^lon(gitude)?$/i =~ nm
        geocoords[:lon] = coord
      elsif !geocoords[:lat] && /^lat(itude)?$/i =~ nm
        geocoords[:lat] = coord
      end
      geocoords
    end

  end

  # == To be  merged into the original GPhys distribution 
  # for unumeric.rb
  class UNumeric
    def to_datetime(*args)
      if /(.*) *since *(.*)/ =~ units.to_s
        stun = $1
        since = DateTime.parse($2)
        tun = Units[stun]
        sec = tun.convert( val, Units['seconds'] ).round + 1e-1
               # ^ take "round" to avoid round error in sec
               # (Note that %S below takes floor of seconds).
        datetime = since + (sec/86400.0)
      else
        nil
      end
    end
  end
end
##################################################

# Hash with String / Regexp keys

class RegHash < Hash
  def []=(key,val)
    if key.is_a?(String) && /[\*%\?]/ =~ key  # wild cards
      key = Regexp.new( '^' + key.gsub(/[\*%]/,'.*').gsub(/\?/,'.') + '$' )
    end
    super(key,val)
  end
  def [](key)
    (v=super(key)) && (return(v))
    keys.each do |k|
      if key.is_a?(String) && k.is_a?(Regexp) && k =~ key
        return(super(k))
      else
        nil
      end
    end
    nil
  end
  def delete(key)
    (v=super(key)) && (return(v))
    keys.each do |k|
      if key.is_a?(String) && k.is_a?(Regexp) && k =~ key
        return(super(k))
      else
        nil
      end
    end
    nil
  end
end

module MSG
  module_function
  @@verbose_level = 0   # normal
  def to_quiet; @@verbose_level = 0; end
  def to_normal; @@verbose_level = 1; end
  def to_verbose; @@verbose_level = 2; end
  def verbose(msg, altmsg=nil)
    if @@verbose_level == 2
      print(msg) 
    elsif altmsg
      print(altmsg) 
    end
    STDOUT.flush
  end
  def normal(msg, altmsg=nil)
    if @@verbose_level >= 1
      print(msg) 
    elsif altmsg
      print(altmsg) 
    end
    STDOUT.flush
  end
end


Var_Meta_To_Register = RegHash.new
# === ノード同士の参照関係が、DB登録順が原因で壊れてしまうことのないようにするためのメソッド。
#     それまでDB未登録だったノードが新たに登録されたとき、
#     このノードを参照しているノードを再度登録しなおして参照関係を適切に復元する。
#     
#     register_image, register_knowledgeの最後にこれを呼ぶ。
#     前者は文書から画像への参照
#     後者は文書(コメント)から文書への参照
ToDo = Hash.new   #  path => proc
def after_save(obj)
  if proc = ToDo[obj.path]
    proc.call(obj.node)
    ToDo.delete(obj.path)
  end
end

def t_comp(a,b) # これの戻り値が真ならデータを更新するという仕様
  return(nil) if a.nil? or b.nil?
  #if a.class == b.class # 比較できないものを渡されたらエラーを吐くべきでは
    a > b # Time と ActiveSupport::TimeWithZone を比較する可能性
  #end
end

def parse_meta(path, node, force, attr, stremote)
  fname = path + ".yml"
  if File.exist?(fname)
    hash = YAML.load( File.read(fname) )
    if Hash === hash
      register_meta(hash,node, force, attr, stremote)
    else
      warn "yaml file must have hash data"
    end
  end

  fname = path + ".SIGEN"
  if File.exist?(fname)
    File.foreach(fname){|line|
      line = Kconv.kconv(line, @charset)
      k, v = line.chop.split(":")
      register_kattr(k, v.strip, node) if v
      
    }
  end
end

def register_gfdnavi_params(hash, node, force, attr, stremote)
  node = node.node unless Node === node
  hash.each do |key, val|
    case key
    when 'aggregate'
      raise('aggregate must have Hash') unless Hash === val
      val.each{|name, ha|
        dname = File.dirname(name)
        vname = File.basename(name)
        fname = File.join(node.path, dname)
        if ha && ali = ha['alias']
          path = File.expand_path(File.join(node.path, ali))
          path.sub!(/^[a-zA-Z]:/,"")
          parent = Node.find(:first, :conditions=>["path=?",File.dirname(path)])
          parent
          raise("alias is invalid") unless parent
        else
          path = fname
          parent = node
        end
        ka = {'remark' => 'This is a vertual united file'}
        parent, flag = register_dir(path, parent, nil, force, attr, nil, false, ka, stremote)
        register_var(fname, File.join(path,vname), parent, nil, force, attr, stremote) if flag
      }
    when 'owner','user'   # 'user' is for backward compatibility
      user = User.find_by_login(val)
      if user
        node.owner = user
      end
    when 'other_mode'
      node.other_mode = val == 4 ? 4 : 0
    when 'rgroups'
      raise("Array expected") if !val.is_a?(Array)
      node.set_rgroups(*val)
    when 'wgroups'
      raise("Array expected") if !val.is_a?(Array)
      node.set_wgroups(*val)
    when 'downloadable'
      if node.directory?
        node.entity.downloadable = val==true
      end
    when 'draw_parameters'
      val.each{|dpk, dpv|
        dp = DrawParameter.new
        dp.node = node
        dp.name = dpk
        dp.value = dpv
        node.draw_parameters.push(dp)
      }
    when 'vizshot'
      if node.image?
        node.entity.vizshot = val
      else
        raise "gfdinavi attribute of 'vizshot' is only for Image"
      end
    when "references"
      if Array === val
        val.each{|hash|
          if Hash === hash
            @references.push [hash["name"], hash["path"], node.path]
          else
            raise "gfdnavi attribute of references must be Array of Hash('name' and 'path')"
          end
        }
      else
        raise "gfdnavi attribute of references must be Array of Hash('name' and 'path')"
      end
    when 'category'
      node.entity.category = val
      register_kattr("category", val, node)
    when 'title'
      node.entity.title = val
      register_kattr("title", val, node)
    when 'creator'
      node.entity.creator = val
      register_kattr("creator", val, node)
    when 'textbody'
      node.entity.textbody = val
      register_kattr("textbody", val, node)
    when 'description'
      node.entity.description = val
      register_kattr("description", val, node)
    when 'horizontal_figures'
      node.entity.horizontal_figures = val
    when 'default_layout'
      node.entity.default_layout = val
    when 'figures_size_units'
      node.entity.figures_size_units = val
    when 'figures_size_height_or_width'
      node.entity.figures_size_height_or_width = val
    when 'figures_size_number'
      node.entity.figures_size_number = val
    when 'comment_on'
      if val
        if knowledge = Knowledge.find(:first, :conditions => ["path=?", val])
          node.entity.comment_on = knowledge.node_id
        else
          # このToDoが実行されるのはコメント対象の Knowledge の save 後
          ToDo[val] = Proc.new {|node|
            knowledge.comment_on = Knowledge.find(node.id).id
            knowledge.save("directory_scan")
          }
        end
      end
    when 'comment_number'
      node.entity.comment_number = val
    when 'knowledge_figures'
      knowledge_figures = Array.new
      val.each do |v|
        kf = KnowledgeFigure.new
        kf.caption = v["caption"] # ここにこれがあると、ToDoの場合にcaptionが入らないのでは????        
        if image = Image.find(:first, :conditions => ["path=?", v["image_path"]])
          # 画像が先にスキャンされた場合
          kf.knowledge = node.entity
          kf.image_path = image.path
        else 
          # 画像がまだスキャンされていない場合、ToDoリストに入れて後回し
          ToDo[v["image_path"]] = Proc.new {|imagenode|
            k = Knowledge.find(:first, :conditions => ["node_id=?", node.id])
            image = Image.find(:first, :conditions => ["node_id=?", imagenode.id])
            # このToDoが実行されるのは Image の save 後なので、ifで分岐させる必要はない。
            kf.knowledge = k
            kf.image_path = image.path
            kf.save
            k.knowledge_figures = (k.knowledge_figures || Array.new).dup.push(kf)
            k.save("directory_scan")
          }
        end
        knowledge_figures.push(kf)
      end
      node.entity.knowledge_figures = knowledge_figures
    when 'org_path'
      node.entity.org_path = val if val
    end
  end
end

def register_meta(meta, node, force, attr, stremote=false)
  meta.sort.each{|k,v| # sort to registe 'contains' firstly
    case k
    when "gfdnavi", "gfdnavi_knowledge"
      register_gfdnavi_params(v, node, force, attr, stremote)
    when "contains"
      v.each{ |name,meta|
        case name
        when /^[^\/]*$/   # variables directory under node
          Var_Meta_To_Register[File.join(node.path,name)] = meta
        end
      }
    else
      register_kattr(k, v, node)
    end
  }
end

def register_kattr(name, value, node)
  return if value.nil?
  value=NArray[value] if value.is_a?(Numeric)  # to rescue a inapropriate yaml
  ka = nil
  node.keyword_attributes.each{ |nka|   # node unsaved, so find explicitly
    if nka.name == name
      ka = nka
      break
    end
  }
  unless ka
    ka = KeywordAttribute.new
    ka.name = Kconv.kconv(name, @charset)
    ka.node = Node === node ? node : node.node
  end
  ka.value = String===value ? Kconv.kconv(value, @charset) : value
  node.keyword_attributes.push(ka)  # --> ka will be automtcly saved with node
end

def register_var(file, vname, parent, mtime, force, attr, stremote, ignore_errors=true)
  ## print "   registering  #{vname}\n"

  if String === file
    file = Dir[File.join(GFDNAVI_DATA_PATH,file)].sort
    rpath_var = vname
    vname = File.basename(vname)
  else
    rpath = file.path.sub(GFDNAVI_DATA_PATH,"").sub(GFDNAVI_USER_PATH,"/usr")
    rpath_var = File.join(rpath,vname)
    rpath_dir = rpath
    vname.split("/")[0..-2].each{|dname|
      rpath_dir = File.join(rpath_dir,dname)
      parent, = register_dir(rpath_dir, parent, mtime, force, attr, nil, false, nil, stremote)
    }
  end

  unless var = Variable.find(:first, :conditions=>["path=?",rpath_var])
    var = Variable.new
  end

  if Array === file
    file.each{|f|
      var.actual_files.push ActualFile.new(:path=>f.sub(GFDNAVI_DATA_PATH,""))
    }
  else
    var.file = rpath
  end

  var.name = File.basename(vname)
  var.mtime = mtime
  var.path = rpath_var
  var.parent = parent.node
  begin
    gphys = NumRu::GPhys::IO.open(file,vname)
  rescue
    puts "fail to register #{rpath_var}"
    raise $!
  end
  var.size = gphys.length
  node = var.node
  gphys.att_names.each{|aname|
    register_kattr(aname, gphys.get_att(aname), node)
  }
  if (meta = Var_Meta_To_Register[rpath_var])  # substitution, not ==
    register_meta(meta, node, force, attr, stremote)
  end
  if !node.remote? or stremote # too slow for remote
    georanges = gphys.lon_lat_t_ranges
    if georanges.length > 0
      sta = SpatialAndTimeAttribute.new
      sta.node = node
      if r=georanges[:lon]
        sta.longitude_lb, sta.longitude_rt = r.collect{|un| un.val}
      end
      if r=georanges[:lat]
        sta.latitude_lb, sta.latitude_rt = r.collect{|un| un.val}
      end
      if r=georanges[:time]
        sta.starttime,sta.endtime = r.collect{|un| un.to_datetime}
      end
      node.spatial_and_time_attributes.push(sta)
    end
  end
  unless var.save
    warn "failed to register variable: #{rpath_var}"
    if ignore_errors
      warn "  #{var.errors.full_messages}"
    else
      raise var.errors.full_messages
    end
  end
  MSG.verbose("registrated variable: #{rpath_var}\n")
end

def register_image(path, org_path, meta, mtime, size, parent, force, attr, ignore_errors=true)
  rpath = path.sub(GFDNAVI_DATA_PATH,"").sub(File.dirname(GFDNAVI_USER_PATH),"")
  rpath = '/' if rpath == ''   # local root directory
  node = Node.find(:first, :conditions=>["path=?",rpath])
  if node
    if force or t_comp(mtime, node.mtime) or !node.image?
      node.destroy   # for test
      node = nil     # for test
    else
      MSG.normal(".",".")
      return nil
    end
  end
  unless node
    img = Image.new
    img.name = File.basename(rpath)
    img.path = rpath
    img.org_path = org_path
    img.mtime = mtime
    img.size = size
    img.parent = parent.node
    img.owner = parent.owner
    register_meta(meta, img.node, force, attr) if meta
    unless img.save
      warn "failed to register an image file (#{rpath})"
      if ignore_errors
        warn "  #{img.errors.full_messages}"
      else
        raise img.errors.full_messages
      end
    end
    MSG.verbose("\nregister image: #{rpath}")
  end
  after_save(img)
  nil
end

def register_knowledge(path, meta, mtime, size, force, attr, ignore_errors=true)
  rpath = path.sub(GFDNAVI_DATA_PATH,"").sub(File.dirname(GFDNAVI_USER_PATH),"")
  rpath = '/' if rpath == ''   # local root directory
  node = Node.find(:first, :conditions=>["path=?",rpath])
  if node
    if force or t_comp(mtime, node.mtime) or !node.knowledge?
      node.destroy   # for test
      node = nil     # for test
    else
      MSG.normal(".",".")
      return nil
    end
  end

  unless node
    knowledge = Knowledge.new
    knowledge.presaved_node_relations = Array.new
    knowledge.ignore_save_knlge = true

    kf_hash_ary = meta["gfdnavi_knowledge"].delete("knowledge_figures")
    refer_hash_ary = meta["gfdnavi_knowledge"].delete("reference")
    comment_path = meta["gfdnavi_knowledge"].delete("comment_on")

    # KnowledgeFigure と NodeRelation は後で保存するようにする
    # (db/register_directory_tree.rb)
    if kf_hash_ary
      kf_hash_ary.each do |kf_hash|
        # knowledge_id, image_path, caption
        @knowledge_figures.push [rpath, kf_hash["image_path"], kf_hash["caption"]]
      end
    end
    if refer_hash_ary
      refer_hash_ary.each do |refer_hash|
        # name, reference, referenced_by
        @references.push [refer_hash["name"], refer_hash["path"], rpath]
      end
    end
    if comment_path
      # knowledge.path, parent_knowledge.path
      @comments.push [rpath, comment_path]
    end

    unless knowledge.restore(rpath, meta["gfdnavi_knowledge"], size)
     warn "failed to register a knowledge file (#{rpath})"
     warn "  #{knowledge.errors.full_messages}"
    end
    MSG.verbose("\nregister knowledge: #{rpath}")
  end
  after_save(knowledge)
  nil
=begin
  unless node
    knowledge = Knowledge.new
    knowledge.name = File.basename(rpath)
    knowledge.path = rpath
    knowledge.mtime = mtime
    knowledge.size = size
    register_meta(meta, knowledge.node, force, attr) if meta
    unless knowledge.save("directory_scan")
     warn "failed to register a knowledge file (#{rpath})"
     if ignore_errors
       warn "  #{knowledge.errors.full_messages}"
     else
       raise knowledge.errors.full_messages
     end
    end
    MSG.verbose("\nregister knowledge: #{rpath}")
  end
  after_save(knowledge)
  nil
=end
end

def register_dir(path, parent, mtime, force, attr, size=nil, plain_file=false, kas=nil, stremote=false)
  rpath = path.sub(GFDNAVI_DATA_PATH,"").sub(File.dirname(GFDNAVI_USER_PATH),"")
  rpath = '/' if rpath == ''   # local root directory
  dir = Directory.find(:first, :conditions=>["path=?",rpath], :user=>:all)
  if dir
    if force or t_comp(mtime, dir.mtime)
#      dir.destroy   # for test
#      dir = nil     # for test
    elsif attr
      parse_meta(path, dir.node, force, attr, stremote)
      dir.save
      MSG.normal(".",".")
      return [dir, false]
    else
      MSG.normal(".",".")
      return [dir, false]
    end
    MSG.normal("\nupdating  #{rpath}",".")
  else
    MSG.normal("\nregistering  #{rpath}",".")
  end
  KeywordAttribute.transaction do
    dir ||= Directory.new
    if parent
      dir.name = File.basename(rpath)
    else
      dir.name = rpath
    end
    dir.path = rpath
    dir.mtime = mtime
    dir.size = size if size
    dir.plain_file = plain_file
    if parent
      dir.parent = Node===parent ? parent : parent.node
      dir.owner = parent.owner
    else
      dir.owner = ROOT
    end
    parse_meta(path, dir.node, force, attr, stremote)
    kas.each{|k,v| register_kattr(k, v, dir.node)} if kas
    unless dir.save
      warn "failed to register directory(#{rpath})"
      warn "  #{dir.errors.full_messages}"
    end
    MSG.verbose("\nregister directory: #{rpath}")
  end
  return [dir, true]
end


def register_file(path, dir, rdir, mtime, size, klass, force, attr, ignore_errors, stremote)
  Directory.transaction do
    rd,flag = register_dir(path, rdir, mtime, force, attr, size, true, nil, stremote)
    if flag
      begin
        file = klass.open(path)
        begin
          NumRu::GPhys::IO.var_names_except_coordinates(file).each do |vname|
            register_var(file, vname, rd, mtime, force, attr, stremote, ignore_errors)
          end
        rescue
          if dir.is_a?( NumRu::OPeNDAPDir )
            # just to warn, because one cannot correct the problem in generae
            warn "*** Error occured while processing remote file #{file.path} : #{$!.to_s}" 
          else
            raise $!
          end
        ensure
          file.close
        end
      rescue
        unless ignore_errors
          raise $!
        end
      end
    end
  end
end
