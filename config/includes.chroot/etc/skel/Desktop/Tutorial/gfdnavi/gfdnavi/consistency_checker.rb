#!/usr/bin/env ruby

class DBT
  attr_reader :table_name, :columns
  def initialize(name)
    @table_name = name
    @columns = Hash.new
  end
  def column(name, type, *args)
    opts = {:type => type}
    unless args.length == 0
      opts.update(args[0])
    end
    @columns[name] = opts
  end
end
module ActiveRecord
  class Migration
  end
end
class InitialScheme < ActiveRecord::Migration
  @tables = Array.new
  def self.create_table(name,*opts)
    dbt = DBT.new(name)
    yield(dbt)
    @tables.push dbt
  end
  def self.get_tables
    @tables
  end
end



require "optparse"
OptionParser.new do |opts|
  opts.on("-c","--cvs", "set if you want to execute cvs update"){|v|
    @execute_cvs = v
  }
#  opts.on("-d","--database", "set if you want to update RDB tables"){|v|
#    @update_rdb = v
#  }
  opts.parse!(ARGV)
end



unless File.exist?("config/environment.rb")
  STDERR.print "This seems not to be top of gfdnavi tree.\n"
  abort
end


if @execute_cvs
  unless File.directory?("CVS")
    STDERR.print "Cannot find CVS directory\n"
    STDERR.print "You must update manually.\n"
    abort
  end

  unless system("cvs update")
    STDERR.print "faild to execute 'CVS update'\n"
    abort
  end
end

efname = "config/gfdnavi.yml.example"
cfname = "config/gfdnavi.yml"
if File.mtime(efname) > File.mtime(cfname)
  STDERR.print "'config/gfdnavi.yml.example' is newer than 'config/gfdnavi.yml'\n"
  STDERR.print "Please edit the 'gfdnavi.yml' with refering the 'gfdnavi.yml.example'.\n"
  abort
end



require "db/migrate/001_initial_scheme.rb"
InitialScheme.up
tables = InitialScheme.get_tables
require "config/environment.rb"

def table_has_changed
  if @update_rdb
  end
  abort
end

tables.each do |dbt|
  tname = dbt.table_name
  columns = dbt.columns
  class_name = tname.classify
  class_name += "e" if /Cach$/ =~ class_name
  ar = ActiveRecord.class_eval(class_name)
  unless ar.table_exists?
    STDERR.print "RDB does not have the table of '#{tname}'.\n"
    table_has_changed
  end
  columns_ar = ar.column_names
  columns.keys.each{|cname|
    unless columns_ar.include?(cname)
      STDERR.print "RDB table of '#{tname}' does not have the column of '#{cname}'\n"
      table_has_changed
    end
  }
  ar.columns.each{|col_ar|
    cname = col_ar.name
    next if cname == "id"
    unless (col_mig = columns[cname])
      STDERR.print "the column of '#{cname} in the table of '#{tname}' has deleted.\n"
      table_has_changed
    end
    unless col_ar.type == col_mig[:type]
      STDERR.print "Type of the column of '#{cname}' in the table of '#{tname}' has changed.\n"
      table_has_changed
    end
    if col_mig[:default] && col_mig[:default] != col_ar.default
      STDERR.print "Default value of the column of '#{cname}' in the table of '#{tname}' has changed.\n"
      table_has_changed
    end
    if col_mig[:null] && col_mig[:null] != col_ar.null
      STDERR.print "Null of the column of '#{cname}' in the table of '#{tname}' has changed.\n"
      table_has_changed
    end
  }
end



print "nothing to be inconsistent was found\n"
