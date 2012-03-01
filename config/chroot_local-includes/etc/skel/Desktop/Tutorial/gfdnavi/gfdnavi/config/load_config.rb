# load configuration for gfdnavi

require "fileutils"
require "yaml"

def gfdnavi_path( path )
  path = path.gsub( /RAILS_ROOT/, File.expand_path(RAILS_ROOT) )
  path.sub(/\/$/, "")
end

=begin
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
=end
require File.join(File.dirname(__FILE__), 'boot')


GFDNAVI_DATA_PATH = gfdnavi_path( gfdnavi_configuration('data_path') )
GFDNAVI_PUBLIC_PATH = File.join(File.expand_path(RAILS_ROOT), 'public')
if Regexp.new('^'+GFDNAVI_PUBLIC_PATH+'(.*)') =~ GFDNAVI_DATA_PATH
  # data are directly accessible
  GFDNAVI_DATA_URL_PREFIX = $1                   # path after '/public'
  #  GFDNAVI_DATA_URL_PREFIX.sub!(/([^\/])$/,'\1/')     # Add trailing '/'
else
  # data are NOT directly accessible
  GFDNAVI_DATA_URL_PREFIX = ""
end

GFDNAVI_REMOTE_DATA_PATHS = gfdnavi_configuration('remote_data_paths',true)
if GFDNAVI_REMOTE_DATA_PATHS and !GFDNAVI_REMOTE_DATA_PATHS.is_a?(Array)
  raise "gfdnavi config file error : remote_data_paths must be an array"
end

GFDNAVI_OTHER_GFDNAVI_SERVERS = gfdnavi_configuration("other_gfdnavi_servers",true)
if GFDNAVI_OTHER_GFDNAVI_SERVERS && !Array === GFDNAVI_OTHER_GFDNAVI_SERVERS
  raise "gfdnavi config file error: other_gfdnavi_servers must be an array"
end

GFDNAVI_USER_PATH = gfdnavi_path( gfdnavi_configuration('user_path') )
raise("Name of the user directory must be 'usr'") if File.basename(GFDNAVI_USER_PATH) != 'usr'

GFDNAVI_WORK_PATH = gfdnavi_path( gfdnavi_configuration('work_path') )


[GFDNAVI_DATA_PATH, GFDNAVI_USER_PATH, GFDNAVI_WORK_PATH].each{|path|
  FileUtils.makedirs(path) unless File.exist?(path)
}

GFDNAVI_DIAGRAM_CACHE_PATH = gfdnavi_path( gfdnavi_configuration('diagram_cache_path') )
FileUtils.makedirs(GFDNAVI_DIAGRAM_CACHE_PATH) unless File.exist?(GFDNAVI_DIAGRAM_CACHE_PATH)

GFDNAVI_DATA_CACHE_PATH = gfdnavi_path( gfdnavi_configuration('data_cache_path') )
FileUtils.makedirs(GFDNAVI_DATA_CACHE_PATH) unless File.exist?(GFDNAVI_DATA_CACHE_PATH)


GFDNAVI_IGNORED_DIRS = gfdnavi_configuration('ignored_dirs')

GFDNAVI_DEBUG = (gfdnavi_configuration("debug",true) == true)

GFDNAVI_BENCHMARK = (gfdnavi_configuration("benchmark",true) == true)

GFDNAVI_DISABLE_USER = (gfdnavi_configuration("disable_user",true) == true)

GFDNAVI_PROXY = gfdnavi_configuration('proxy',true)

GFDNAVI_USING_PROXY = (gfdnavi_configuration("using_proxy",true) == true)

GFDNAVI_ALLOW_ANY_OPENID = (gfdnavi_configuration("allow_any_openid",true) == true)

GFDNAVI_PAGINATE_PER = gfdnavi_configuration("paginate_per")

GFDNAVI_GOOGLE_MAP_KEY = gfdnavi_configuration('google_map_key')

GFDNAVI_CROSS_SEARCH_SERVER = gfdnavi_configuration('cross_search_center_server')

GFDNAVI_PASSPHRASE = gfdnavi_configuration('passphrase')

GFDNAVI_SECRET = gfdnavi_configuration('secret')

GFDNAVI_SESSION_DURATION = gfdnavi_configuration('session_duration')

GFDNAVI_IGNORED_ATTRS = gfdnavi_configuration('ignored_attributes')

GFDNAVI_INVISIBLE_ATTRS = gfdnavi_configuration('invisible_attributes')

admin = gfdnavi_configuration('admin')
GFDNAVI_ADMIN_NAME = admin['name']
GFDNAVI_ADMIN_EMAIL = admin['email']

GFDNAVI_ENABLE_EMAIL = (gfdnavi_configuration("enable_email",true) == true)

if GFDNAVI_ENABLE_EMAIL
  GFDNAVI_SMTP_CONFIG = gfdnavi_configuration('smtp')
end

GPHYS_READ_SIZE_LIMIT_1 = sl = 
    gfdnavi_configuration("soft_limit_array_size",true)
GPHYS_READ_SIZE_LIMIT_2 = hl = 
    gfdnavi_configuration("hard_limit_array_size",true)
if hl && sl && hl <= sl
  raise "hard limit must be greather than soft limit" 
end

# clean up
@gfdnavi_configuration_file = nil
@gfdnavi_configuration = nil
