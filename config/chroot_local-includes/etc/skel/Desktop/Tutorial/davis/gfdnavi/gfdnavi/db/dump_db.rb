#!/usr/bin/env ruby

require File.join(File.dirname(__FILE__), "..", "config", "environment")

tables = %w( functions function_outputs value_types function_arguments 
             diagram_caches diagram_cache_data diagram_cache_sessions
             draw_methods draw_method_options
           )

hash = Hash.new

exist_tables = ActiveRecord::Base.connection.tables
tables.each do |table_name|

  model_name = table_name.classify
  model_name += "e" if table_name == "diagram_caches"
  if exist_tables.include?(table_name)
    ar = ActiveRecord.class_eval(model_name)
    ary = Array.new
    opts = {}
    if %(functions draw_methods).include?(table_name)
      opts[:user]=:all
    end
    ar.find(:all, opts).each{|row|
      h = row.attributes
      h.delete("node_id")
      h.delete("id")
      if row.is_a?(NodeEntityAbstract)
        h2 = row.node.attributes
        h2.delete("parent_id")
        h.update(h2)
      elsif table_name == "function_arguments"
        h.delete("function_id")
        h["function_path"] = (f=row.function) && f.path
      elsif table_name == "draw_method_options"
        h.delete("draw_method_id")
        h["draw_method_path"] = (d=row.draw_method) && d.path
      end
      ary.push h
    }
    hash[ar.table_name] = ary if ary.length > 0
  end

end

if hash.length == 0
  exit
end

dt = DateTime.now
fname = File.join(File.dirname(__FILE__),
                  "database_save-#{dt.year}#{"%02i"%dt.month}#{"%02i"%dt.day}#{"%02i"%dt.hour}#{"%02i"%dt.min}#{"%02i"%dt.sec}")

if File.exist?(fname)
  abort "file already exists: #{File.basename(fname)}"
end


File.open(fname, "w"){|file|
  file.write hash.to_yaml
}

print "sucessed to dump database data to #{fname}\n"
