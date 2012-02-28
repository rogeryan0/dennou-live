desc "Clean all"
task :clean do
  Rake::Task["clean:diagram_cache"].invoke
  connection = ActiveRecord::Base.connection
  tables = connection.tables
  if tables.include?("sessions")
    Rake::Task["db:sessions:clear"].invoke
  end
  Rake::Task["clean:session_files"].invoke
  Rake::Task["clean:work_dirs"].invoke
end

namespace :clean do

  desc "Clean diagram cache"
  task :diagram_cache => :environment do
    path = GFDNAVI_DIAGRAM_CACHE_PATH + "/"
    Dir.foreach(path){|fname|
      fname = path + fname
      File.delete(fname) unless File.directory?(fname)
    }
    connection = ActiveRecord::Base.connection
    tables = connection.tables
    if tables.include?("diagram_cache_sessions")
      DiagramCacheSession.destroy_all unless DiagramCacheSession.count == 0
    end
    if tables.include?("diagram_cache_data")
      DiagramCacheDatum.destroy_all unless DiagramCacheDatum.count == 0
    end
    if tables.include?("diagram_caches")
      DiagramCache.destroy_all unless DiagramCache.count == 0
    end
  end

  desc "Clean session files"
  task :session_files do
    session_dir = "#{RAILS_ROOT}/tmp/sessions"
    Dir.foreach(session_dir){|fname|
      if fname =~ /^ruby_sess\./
        File.delete("#{session_dir}/#{fname}")
      end
    }
  end

  except_dirs = %w(. .. CVS)

  desc "Clean work directories"
  task :work_dirs => :environment do
    path = GFDNAVI_WORK_PATH + "/"
    Dir.foreach(path){|dname|
      next if except_dirs.include?(dname)
      dname = path + dname + "/"
      next unless File.directory?(dname)
      Dir.foreach(dname){|fname|
        begin
          File.delete(dname + fname) unless except_dirs.include?(fname)
        rescue
          # Suppose that fname is a directory
          Dir.foreach(dname + fname){|subfname|
             File.delete(dname + fname +'/' + subfname) \
                                 unless except_dirs.include?(subfname)
          }
          Dir.delete(dname + fname)
        end
      }
      Dir.delete(dname)
    }
  end

  task :clean_tmptables => :environment do
    connection = ActiveRecord::Base.connection
    tables = connection.tables
    tables.each{ |t|
       if t=~/tmptable/ then
         connection.drop_table(t)
       end
    }
    
  end

end
