#require "fileutils"
require "yaml"

def gfdnavi_configuration( key, allow_nil = false )
  val = @gfdnavi_configuration[key]
  if allow_nil || val
    return val
  else
    STDERR.print <<-EOS
*************************
 Error occuered while reading the gfdnavi configuration file (#{@gfdnavi_configuration_file}).
 Key "#{key}" was not found in your configuration file.
 See config/gfdnavi.yml.example and you need to update it.
*************************
    EOS
    abort
  end
end

@gfdnavi_configuration_file = File.join(File.dirname(__FILE__), 'gfdnavi.yml')
begin
  @gfdnavi_configuration = YAML::load( File.read(@gfdnavi_configuration_file) )
rescue IOError, SystemCallError
  $! = nil
  STDERR.print <<-EOS
*************************
 Error occuered while reading the gfdnavi configuration file (#{@gfdnavi_configuration_file}).
 You probably need to copy config/gfdnavi.yml.example (and to edit it if needed).
 *************************
  EOS
  abort
end

if (disp = gfdnavi_configuration('display',true))
  STDERR.print "'display' is obsolete; use 'DISPLAY' in 'env'"
  ENV['DISPLAY'] = disp
end

env = gfdnavi_configuration('env', true)
if Hash === env
  env.each{|key, val|
    ENV[key] = val unless ENV[key]
  }
elsif ! env.nil?
  raise "env must be a Hash"
end

RAILS_ENV.replace(ENV['RAILS_ENV']) if defined?(RAILS_ENV) && ENV['RAILS_ENV']
