#!/usr/bin/env ruby
# -*- coding: japanese-cp932 -*-

RAILS_ENV = 'production' if ARGV.delete('-p') or ARGV.delete('--production')

require File.dirname(__FILE__)+"/../config/environment"
require "optparse"
require "register_to_db"

class OptionParser
  # Changed to raise error. (Originally, invalid options
  # are just warned, which is easily overlooked.)
  def warn(mesg = $!)  
    raise mesg
  end
end


##################################################
include NumRu

MSG.to_normal

def open_dir(path)
  case path
  when /^http.*(\/dap\/|\/nph-dods\/)/
    dir = OPeNDAPDir.open(path)
  when /^\//, /^[a-zA-Z]:/
    dir = LocalDir.open(path)
  else
    raise ArgumentError, "Unsupported kind of path: #{path}"
  end
  dir
end

GFDNAVI_IGNORED_DIRS_PAT = Regexp.union( *GFDNAVI_IGNORED_DIRS.collect{|igd|
                           igd.is_a?(String) ? Regexp.new('^'+igd+'$') : igd} )
IMAGE_PAT = /(\.png$|\.jpg$|\.jpeg$|\.gif$|\.tiff$)/i
KNOWLEDGE_PAT = /\.knlge$/i

def parse_dir(dir, parent=nil, mtime=nil)
  #plain_file_paths = dir.plain_file_paths   # for yaml, png files etc

  rdir,flag = register_dir(dir.path, parent, mtime, OPTS[:force], OPTS[:attr], nil, false, nil, OPTS[:stremote])

  if rdir \
        && rdir.name != "functions" && rdir.name != "draw_methods" \
        && !(/\.nus$/ =~ rdir.name) # [開発MEMO] ここは消せるようにしたい (堀之内)
                                    # [開発MEMO] NuSDaS対応 (大塚)   
    d_chnames = dir.entries.collect{|e| e.sub(/\.yml$/,'')}
    r_childs = rdir.node.children(false, :user => :all)
    #r_chnames = r_childs.collect{|c| c.name} # [開発MEMO] ほんとはこれでいいんだけど
    r_chnames = r_childs.collect{|c| File.basename(c.path)}
    no_exist = r_chnames - d_chnames
    no_exist.delete("usr") if rdir.path=="/"   # [開発MEMO] ここは消せるようにしたい (堀之内)
    no_exist.each do |name|
      r_childs.each do |c|
        if c.name==name
          c.destroy
        end
      end
    end
  end

  unless OPTS[:dironly]

    dir.each_gphys_file do |path, mtime, size, klass|
      register_file(path, dir, rdir, mtime, size, klass, OPTS[:force], OPTS[:attr], OPTS[:ignore_errors], OPTS[:stremote])
    end

    Var_Meta_To_Register.clear

    dir.each_file(IMAGE_PAT) do |path, mtime, size|
      if File.exist?( yml = path+'.yml' )
        meta = YAML.load( File.read(yml) )
      else
        meta = nil
      end
      org_path=nil #とりあえずnil. あとで.ymlから復旧できるようにする
      register_image(path,org_path,meta,mtime,size,rdir, OPTS[:force], OPTS[:attr])
    end
  
    # knowledge
    dir.each_file(KNOWLEDGE_PAT) do |path, mtime, size|
      unless /\.\d+\.knlge$/  =~ path  # trueになるのはバックアップファイル
        if File.exist?(path)
          meta = YAML.load( File.read(path) )
        else
          meta = nil
        end
        register_knowledge(path,meta,mtime,size, OPTS[:force], OPTS[:attr])
      end
    end
    
  end

  unless /nus$/ =~ dir.path
    dir.each_dir do |subdir, mtime|
      if GFDNAVI_IGNORED_DIRS_PAT !~ subdir.name
        parse_dir(subdir, rdir, mtime)
      end
    end
  end

  rdir
end



##########################################
######         main part           #######
##########################################

@charset = Kconv::UTF8

#< interpret options >

opt = OptionParser.new
OPTS = {}
ARGV.options{|opt|
  opt.on( '-q', '--quiet', "Quiet mode" ){|v| OPTS[:quiet] = v}
  opt.on( '-v', '--verbose', "Verbose mode" ){|v| OPTS[:verbose] = v}
  opt.on( '-l', '--local-only', "Update only for local data" 
         ){|v| OPTS[:localonly] = v}
  opt.on( '-d=DIR', '--dir=DIR', "Process only under the directory"
         ){|v| OPTS[:dir] = v.sub(/^=/, '')}
  opt.on( '-f', '--force', "Force to (re-)register" ){|v| OPTS[:force] = v}
  opt.on( '-a', '--update-attributes', "Update attributes from YAML and SIGEN file" ){|v| OPTS[:attr] = v}
  opt.on( '--ignore-errors', "Ignore errors while opning files"){|v| OPTS[:ignore_errors] = v}
  opt.on( '--clear-tree', "Remove all nodes from DB. (Only to clear. No registering.)" ){|v| OPTS[:cleartree] = v}
  opt.on( '--st-remote', "Register space-time attributes of remote data as well as local data (Very slow with opendap. Be patient.)" ){|v| OPTS[:stremote] = v}
  ##See the top of this file##  opt.on( '-p', '--production', "Production DB" ){|v| OPTS[:production]=v}
  opt.on( '--dir-only', "Register not files but directories only" ){|v| OPTS[:dironly] = v}

  opt.on_tail('-h', '--help', "Show this help message"){|v| OPTS[:help] = v}
  opt.parse!
}

if OPTS[:help]
  print <<-"EOS"

  USAGE: ruby #{File.basename($0.to_s)} [options]

  OPTIONS: \n#{opt.to_a[1..-1].join("")}
  EOS
  exit
end

if OPTS[:quiet]
  MSG.to_quiet
elsif OPTS[:verbose]
  MSG.to_verbose
end

#< optional : remove all nodes from DB >

if OPTS[:cleartree]
  # Remove all root dirs. Children will be deleted automatically.
  rootdirs = Node.find(:all, :user=>:all, 
               :conditions=>'parent_id is NULL AND node_type = 0')
  print "Removing\n"
  rootdirs.each{|n| print("  ",n.path,"\n"); n.destroy}
  exit
end

#< register local data >

ROOT = User.find_by_login("root")
raise("Cannot find the user 'root'. Need to create it first.") if ROOT.nil?

begin

  @references = Array.new
  @knowledge_figures = Array.new
  @comments = Array.new

  if !OPTS[:dir]

    rootdir = parse_dir(open_dir(GFDNAVI_DATA_PATH), nil, 
                        File.mtime(GFDNAVI_DATA_PATH))

    parse_dir(open_dir(GFDNAVI_USER_PATH), rootdir, File.mtime(GFDNAVI_USER_PATH))

    #< register remote data >

    if !OPTS[:localonly] && GFDNAVI_REMOTE_DATA_PATHS
      GFDNAVI_REMOTE_DATA_PATHS.each do |path|
        opddir = open_dir(path)
        parse_dir(opddir)
      end
    end

  else

    dirname = OPTS[:dir]
    dir = Node.find_by_path(dirname)  or  raise("#{dirname} is not in DB")
    parent = dir.parent  or  raise("parent of #{dirname} is not in DB")
    parse_dir( open_dir(dir.fname), parent, File.mtime(dir.fname) )

  end


  # 他が登録された後に登録する
  print "\nregistering knowledge figures\n"
  # [knowledge_id, image_path, caption]
  if @knowledge_figures
    @knowledge_figures.each{|kf|
      knowledge_figure = KnowledgeFigure.new
      knowledge = Knowledge.find(:first,:conditions=>["path=?",kf[0]], :user=>:all)
      knowledge_figure.knowledge = knowledge
      unless Image.find(:first,:conditions=>["path=?",kf[1]], :user=>:all)
        print "Image file #{kf[1]} referred by document #{knowledge.path} is not found.\n"
        print "So their relation is not registered in database.\n"
      else
        knowledge_figure.image_path = kf[1]
        knowledge_figure.caption = NKF.nkf('-w', kf[2]) # UTF-8へ文字コード変換
        knowledge_figure.save!
      end
    }
  end

  # comments of documents
  # [knowledge.path, parent_knowledge.path]
  if @comments
    @comments.each do |comment|
      knowledge = Knowledge.find(:first,:conditions=>["path=?",comment[0]], :user=>:all)
      knowledge.presaved_node_relations = Array.new
      knowledge.ignore_save_knlge = true
      parent_knowledge = Node.find(:first,:conditions=>["path=?",comment[1]], :user=>:all)
      knowledge.comment_on = parent_knowledge.id
      knowledge.save!
      knowledge = Knowledge.find(:first,:conditions=>["path=?",comment[0]], :user=>:all)
    end
  end

rescue
  print "\n"
  raise $!
end


# node_relationsテーブルに関しては
print "\nregistering node relations\n"
# name, reference, referenced_by
if @references
  @references.each{|ref|
    begin
      nr = NodeRelation.new
      nr.name = ref[0]
      nr.reference = Node.find(:first,:conditions=>["path=?",ref[1]], :user=>:all)
      nr.referenced_by = Node.find(:first,:conditions=>["path=?",ref[2]], :user=>:all)
      nr.save!
    rescue
      print "Warning: a node relation failed to save.\n"
      print "#{ref[1]} is missing.\n" unless nr.reference
      print "#{ref[2]} is missing.\n" unless nr.referenced_by
      print "#{$!}\n"
    end
  }
end

#  # 「ファイルが無かったから登録できなかったもの一覧」を出力する
#  ToDo.each do |node, proc|
#    print "file \"#{node}\" not found, so is not registered.\n"
#  end


print "\n"
