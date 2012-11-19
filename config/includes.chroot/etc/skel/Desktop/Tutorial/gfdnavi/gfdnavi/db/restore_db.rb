#!/usr/bin/env ruby

# USAGE:
#
#   % ruby restore_db.rb [restore_file]
#
# If restore_file is not specified, use the newest one.

require File.join(File.dirname(__FILE__), "..", "config", "environment")

if ARGV[0]
  restore_file = ARGV[0]
else
  newest = nil
  Dir.foreach( File.dirname(__FILE__) ) do |dir_name|
    next unless /^database_save-(\d*)$/ =~ dir_name
    dt = DateTime.parse($1)
    if newest.nil? || dt > newest[0]
      newest = [dt, dir_name]
    end
  end
  if newest.nil?
    abort "No saved database data was found"
  end
  restore_file = File.join(File.dirname(__FILE__),newest[1])
end

print "restoring from #{restore_file}\n"
hash = YAML.load( File.read(restore_file) )

func = Hash.new
draw = Hash.new
hash.each do |table_name, ary|
  model_name = table_name.classify
  model_name += "e" if table_name == "diagram_caches"
  ar = ActiveRecord.class_eval(model_name)
  ar.delete_all if ar.table_exists?
  if model_name == "User"
    ary.each{|a|
      pw = a["password"]
      a["password"] = "temporary_password"
      a["password_confirmation"] = a["password"]
      if g=Group.find_by_name(a["login"])
        g.destroy
      end
      u = User.new( a )
      u.super_user = a['super_user']
      u.save!
      u.password = pw
      u.save!
    }
  else
    ary.each{|a|
      a1 = nil
      if table_name == "function_arguments"
        fp = a.delete("function_path")
        a1 = func[fp] || (func[fp]=Array.new)
      elsif table_name == "draw_method_options"
        dp = a.delete("draw_method_path")
        a1 = draw[dp] || (draw[dp]=Array.new)
      end
      rec = ar.new( a )
      if a1
        a1.push(rec)
      else
        rec.save!
      end
    }
  end
end

func.each do |fp,fas|
  f = Function.find(:first, :conditions=>["path=?",fp], :user=>:all)
  f.function_arguments << fas
end
draw.each do |dp,dos|
  d = DrawMethod.find(:first, :conditions=>["path=?",dp], :user=>:all)
  d.draw_method_options << dos
end

print "succeeded to restore database data\n"
