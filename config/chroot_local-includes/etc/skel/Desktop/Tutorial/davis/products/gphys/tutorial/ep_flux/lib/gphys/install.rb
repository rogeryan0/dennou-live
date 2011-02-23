require 'rbconfig'
require 'find'
require 'ftools'

include Config

$version = CONFIG["MAJOR"]+"."+CONFIG["MINOR"]
$libdir = File.join(CONFIG["libdir"], "ruby", $version)
# $archdir = File.join($libdir, CONFIG["arch"])
$site_libdir = $:.find {|x| x =~ /site_ruby$/}
if !$site_libdir
  $site_libdir = File.join($libdir, "site_ruby")
elsif Regexp.compile($site_libdir) !~ Regexp.quote($version)
  $site_libdir = File.join($site_libdir, $version)
end

default_destdir = $site_libdir

def install_rb(srcdir, destdir)
  libdir = "lib"
  libdir = File.join(srcdir, libdir) if srcdir
  path = []
  dir = []
  Find.find(libdir) do |f|
    next unless FileTest.file?(f)
    next if (f = f[libdir.length+1..-1]) == nil
    next if (/CVS$/ =~ File.dirname(f))
    path.push f
    dir |= [File.dirname(f)]
  end
  for f in dir
    next if f == "."
    next if f == "CVS"
    File::makedirs(File.join(destdir, f))
  end
  for f in path
    next if (/\~$/ =~ f)
    next if (/^\./ =~ File.basename(f))
    File::install(File.join("lib", f), File.join(destdir, f), 0644, true)
  end
end

def ARGV.switch
  return nil if self.empty?
  arg = self.shift
  return nil if arg == '--'
  if arg =~ /^-(.)(.*)/
    return arg if $1 == '-'
    raise 'unknown switch "-"' if $2.index('-')
    self.unshift "-#{$2}" if $2.size > 0
    "-#{$1}"
  else
    self.unshift arg
    nil
  end
end

def ARGV.req_arg
  self.shift || raise('missing argument')
end

destdir = default_destdir

begin
  while switch = ARGV.switch
    case switch
    when '-d', '--destdir'
      destdir = ARGV.req_arg
#    when '-u', '--uninstall'
#      uninstall = true
    else
      raise "unknown switch #{switch.dump}"
    end
  end
rescue
  STDERR.puts $!.to_s
  STDERR.puts File.basename($0) + 
    " -d <destdir>"
  exit 1
end    

#if( defined?(uninstall) && uninstall )
#   uninstall_rb(nil, destdir)
#else
   install_rb(nil, destdir)
#end

