desc "Setup all"

task :setup_noquestions do
  $noquestions = true
  Rake::Task["setup:tables"].invoke
  # UPDATE NODE INFO
  Rake::Task["setup:tree_dir"].invoke
  Rake::Task["setup:functions"].invoke
  Rake::Task["setup:draw_methods"].invoke
  Rake::Task["setup:tree_noclean"].invoke
end

task :setup => :environment do
  if exist_tables.include?("nodes") && Node.count > 0
    print "This will clear all the database tables and caches.\n"
    print "If you want just to rebuild database tables, use \"rake update\",\n"
    print "                 or update tree,             use \"rake update:tree\",\n"
    print "instead.\n"
    print "Are you sure to continue? (no/yes, default:no) "
    res = $stdin.gets.chomp!
    unless res == "yes"
      abort "execution was canceled"
    end
  end
  Rake::Task["clean"].invoke
  Rake::Task["setup:tables"].invoke
  # UPDATE NODE INFO
  Rake::Task["setup:tree_dir"].invoke
  Rake::Task["setup:functions"].invoke
  Rake::Task["setup:draw_methods"].invoke
  Rake::Task["setup:tree_noclean"].invoke
end

namespace :setup do
  desc "Setup database tables"
  task :tables => :environment do
    if exist_tables.any?
      command = 'ruby "' + File.join(File.expand_path(RAILS_ROOT), 'db', 'dump_db.rb') + '"'
      unless system( command )
        raise "failed to execute '#{command}'"
      end
    end
    fname_user = File.join(RAILS_ROOT,"db","user_#{RAILS_ENV}.yml")
    if File.exist?(fname_user)
      unless $noquestions
        print "Do you want to keep user information? (yes/no, default:yes): "
        res = $stdin.gets.chomp!
      else
        res = "yes"
      end
      if res == "no"
        suffix = Time.now.strftime("%Y%m%d%H%M%S")
        File.rename(fname_user, fname_user+"."+suffix)
        fname_group = File.join(RAILS_ROOT,"db","group_#{RAILS_ENV}.yml")
        File.rename(fname_group, fname_group+"."+suffix) if File.exist?(fname_group)
      end
    end

    connection = ActiveRecord::Base.connection
    connection.drop_table("schema_info") if connection.tables.include?("schema_info")
    connection.drop_table("schema_migrations") if connection.tables.include?("schema_migrations")
    connection.drop_table("sessions") if connection.tables.include?("sessions")
    Rake::Task["db:migrate"].invoke

    if File.exist?(fname_user)
      Rake::Task["setup:user"].invoke
    else
      Rake::Task["setup:root_password"].invoke
    end

  end

  desc "Set root password"
  task :root_password => :environment do
    unless /mswin(?!ce)|mingw|bccwin/ =~ RUBY_PLATFORM
      system("stty -echo > /dev/null 2>&1") #for unix-like systems
    end
    while true
      while true
        print "Input password for root (number of characters must be 5 to 40): "
        pw2 = $stdin.gets.chomp!
        if pw2.length < 5
          print "The password which you input is too short.\n"
        elsif pw2.length > 40
          print "The password which you input is too long.\n"
        else
          break
        end
      end
      print "\nRepeat password: "
      tmp = $stdin.gets.chomp!
      break if tmp == pw2
      print "\nNot correct!\n"
    end
    unless /mswin(?!ce)|mingw|bccwin/ =~ RUBY_PLATFORM
      system("stty echo > /dev/null 2>&1") #for unix-like systems
    end
    print("\n")
    if root = User.find_by_login("root")
      if root.change_password(pw2,tmp)
        print "changing was succeded\n"
      else
        print "changing was not succeded\n"
      end
    else
      root = User.new( :login => "root",
                       :full_name => "default super user",
                       :email_address => GFDNAVI_ADMIN_EMAIL,
                       :affiliation => "GFDNAVI",
                       :password => pw2,
                       :password_confirmation => pw2 )
      root.super_user = true
      root.internal_user = true
      root.save!
    end
  end

  desc "Create or restore User information"
  task :user => :environment do
    %w(user group).each{|table_name|
      if exist_tables.include?(table_name)
        model = ActiveRecord.class_eval(table_name.classify)
        model.delete_all
      end
    }
    fname = File.join(RAILS_ROOT,"db","user_#{RAILS_ENV}.yml")
    if File.exist?(fname)
      yml = YAML.load(File.read(fname))
      raise("user data is invalid") unless Hash===yml
      yml.each{|u,hash|
        pw = hash.delete("password")
	su = hash.delete("super_user")
        user = User.new(hash)
        pw_tmp = "temporary passowrd"
        user.password = pw_tmp
        user.password_confirmation = pw_tmp
	user.super_user = su
        User.transaction do
          user.password = pw
          user.instance_eval("create_without_callbacks") || raise(RecodeNotSaved)
        end
      }
    end
    fname = File.join(RAILS_ROOT,"db","group_#{RAILS_ENV}.yml")
    if File.exist?(fname)
      yml = YAML.load(File.read(fname))
      raise("group data is invalid") unless Hash===yml
      yml.each{|g,hash|
        group = Group.new(hash)
	group.instance_eval("create_without_callbacks") || raise(RecodeNotSaved)
      }
    end
  end

  desc "Setup tree of directories and variables"
  task :tree => :environment do
    Rake::Task["clean:diagram_cache"].invoke
    %w( draw_parameters
      keyword_attributes spatial_and_time_attributes
      node_relations node_lineages
      knowledge_figures knowledges
      actual_files images variables directories
      nodes).each{|table_name|
      if exist_tables.include?(table_name)
        model = ActiveRecord.class_eval(table_name.classify)
	model.delete_all
      end
    }
    command = 'ruby "' + File.join(File.expand_path(RAILS_ROOT), 'db', 'register_directory_tree.rb') + '"'
    unless system( command )
      raise "failed to execute '#{command}'"
    end
  end

  desc "Setup tree of directories and variables"
  task :tree_dir => :environment do
    Rake::Task["clean:diagram_cache"].invoke
    %w( draw_parameters
      keyword_attributes spatial_and_time_attributes
      node_relations node_lineages
      knowledge_figures knowledges
      actual_files images variables directories
      nodes).each{|table_name|
      if exist_tables.include?(table_name)
        model = ActiveRecord.class_eval(table_name.classify)
	model.delete_all
      end
    }
    command = 'ruby "' + File.join(File.expand_path(RAILS_ROOT), 'db', 'register_directory_tree.rb') + '" --dir-only'
    unless system( command )
      raise "failed to execute '#{command}'"
    end
  end

  desc "Setup tree of directories and variables"
  task :tree_noclean => :environment do
    command = 'ruby "' + File.join(File.expand_path(RAILS_ROOT), 'db', 'register_directory_tree.rb') + '"'
    unless system( command )
      raise "failed to execute '#{command}'"
    end
  end

  desc "Setup default functions database"
  task :functions => :environment do
    print "registeration functions\n"
    create_value_type_table
    unless Function.table_exists?
      raise "functions table does not exist"
    end
    unless FunctionOutput.table_exists?
      raise "function_outputs table does not exist"
    end
    unless FunctionArgument.table_exists?
      raise "function_arguments table does not exist"
    end
    FunctionArgument.delete_all
    FunctionOutput.delete_all if FunctionOutput.table_exists?
    Function.delete_all if Function.table_exists?
    Node.delete_all(['node_type = ?',Node::FUNCTION])
    dir = File.join(File.expand_path(RAILS_ROOT), 'db', 'functions')
    root = User.find_by_login("root")
    regist_functions(dir, root, true)
    Dir.foreach(GFDNAVI_USER_PATH){|dname|
      user = User.find_by_login(dname)
      dir = File.join(GFDNAVI_USER_PATH, dname, "functions")
      if user && File.exist?(dir)
        regist_functions(dir, user)
      end
    }
  end

  desc "Setup default draw methods database"
  task :draw_methods => :environment do
    print "registeration draw methods\n"
    create_value_type_table
    unless DrawMethod.table_exists?
      raise "draw_methods table does not exist"
    end
    unless DrawMethodOption.table_exists?
      raise "draw_method_options table does not exist"
    end
    DrawMethodOption.delete_all
    DrawMethod.delete_all 
    Node.delete_all(['node_type = ?',Node::DRAW_METHOD])
    dir = File.join(File.expand_path(RAILS_ROOT), 'db', 'draw_methods')
    root = User.find_by_login("root")
    regist_draw_methods(dir, root, true)
    Dir.foreach(GFDNAVI_USER_PATH){|dname|
      user = User.find_by_login(dname)
      dir = File.join(GFDNAVI_USER_PATH, dname, "draw_methods")
      if user && File.exist?(dir)
        regist_draw_methods(dir, user)
      end
    }
  end    

end


module Rails
  class Initializer
    alias :__load_environment__ :load_environment
    def load_environment
      __load_environment__
      configuration.cache_classes = false
    end
  end
end

def create_value_type_table
  if ValueType.table_exists?
    %w(int float string array_int array_float array_string boolean).each{|typ|
      ValueType.new(:name => typ).save! unless ValueType.find_by_name(typ)
    }
  end
end


def regist_functions(dir, user, default=false)
  path = "/usr/#{user.login}/functions"
  Node.make_user_directory(path, user, default ? 4 : 0)
  Dir.foreach(dir){|yml|
    next unless /\.yml$/=~yml
    fname = File.join(dir,yml)
    fixture = YAML.load( File.read(fname) )
    arguments = fixture.delete(:arguments)
    outputs = fixture.delete(:outputs)
    if outputs.nil?
      puts "A function must contain \":outputs\" in the YAML file; the function \"#{fixture[:name]}\" is skipped"
      next
    end
    func = Function.new
    fixture.each{|k,v|
      func.send(k.to_s+"=",v)
    }
    func.path = File.join(path, func.name)
    func.owner = user
    func.default = default
    func.other_mode = default ? 4 : 0
    Function.transaction{
      FunctionOutput.transaction{
        FunctionArgument.transaction{
          func.save!
          func.function_outputs.create(outputs)
          if arguments
            arguments.each{|arg|
              typ = arg.delete(:value_type)
              if /array/ =~ typ && arg[:default].is_a?(Array)
                arg[:default] = arg[:default].join(",")
              end
              fa = FunctionArgument.new(arg)
              fa.value_type = ValueType.find_by_name(typ)
              fa.function = func
              fa.save!
            }
          end
	  func.update_yml
        }
      }
    }
  }
end



def exist_tables
  ActiveRecord::Base.connection.tables
end


def regist_draw_methods(dir, user, default=false)
  path = "/usr/#{user.login}/draw_methods"
  Node.make_user_directory(path, user, default ? 4 : 0)
  Dir.foreach(dir){|yml|
    next unless /\.yml$/=~yml
    fname = File.join(dir,yml)
    fixture = YAML.load( File.read(fname) )
    opts = fixture.delete(:options)
    dm = DrawMethod.new
    fixture.each{|k,v|
      dm.send(k.to_s+"=",v)
    }
    dm.path = File.join(path, dm.name)
    dm.owner = user
    dm.default = default
    dm.other_mode = default ? 4 : 0
    DrawMethod.transaction{
      DrawMethodOption.transaction{
        dm.save!
        if opts
          opts.each{|opt|
            typ = opt.delete(:type)
            unless typ
              raise "type must be set for options"
            end
            dma = DrawMethodOption.new(opt)
            dma.value_type = ValueType.find_by_name(typ)
            dma.draw_method = dm
            dma.save!
          }
	  dm.update_yml
        end
      }
    }
  }
end
