desc "Update all"
task :update do
  while true
    print "Please select the following item\n"
    print " 1: rebuild database tables.\n"
    print " 2: update data tree.\n"
    print " x: quit.\n"
    print "select number: "
    res = $stdin.gets.chomp!
    unless ["1", "2", "x"].include?(res)
      print "Not correct\n\n"
      next
    end
    break
  end
  case res
  when "1"
    Rake::Task["update:tables"].invoke
  when "2"
    Rake::Task["update:tree"].invoke
  when "x"
    exit
  end
end

namespace :update do
  desc "Update database tables"
  task :tables => :environment do
    print "This will clear all the database tables and try to restore database.\n"
    print "This might fail.\n"
    print "Are you sure to continue? (no/yes, default:no) "
    res = $stdin.gets.chomp!
    unless res == "yes"
      abort "execution was canceled"
    end

    command = 'ruby "' + File.join(File.expand_path(RAILS_ROOT), 'db', 'dump_db.rb') + '"'
    unless system( command )
      raise "failed to execute #{command}"
    end

    Rake::Task["setup:tables"].invoke

    command = 'ruby "' + File.join(File.expand_path(RAILS_ROOT), 'db', 'restore_db.rb') + '"'
    unless system( command )
      raise "failed to execute #{command}"
    end
    command = 'ruby "' + File.join(File.expand_path(RAILS_ROOT), 'db', 'register_directory_tree.rb') + '"'
    unless system( command )
      raise "failed to execute #{command}"
    end
  end

  desc "Update tree of directories and variables"
  task :tree => :environment do
    command = 'ruby "' + File.join(File.expand_path(RAILS_ROOT), 'db', 'register_directory_tree.rb') + '" --verbose'
    unless system( command )
      raise "failed to execute #{command}"
    end
  end

end
