require 'rbconfig'
require 'ftools'

include Config
bindir = CONFIG["bindir"]

files = Dir.glob('*[^~]')
files.delete($0)
files.delete('CVS')

files.each{|f|
  File::install( f, File.join(bindir, f), 0755, true)
}
