#USAGE
# For itself:
# % ruby make_index.rb body-j.html
#
# For menu-j.html:
# % ruby make_index.rb -m body-j.html

if ARGV[0] == '-m'
  ARGV.shift
  h2 = 'body-j.html#h2'
  h3 = 'body-j.html#h3'
  href = 'target=page href='
  ull = "<ul><div>\n"
  ulr = "</div></ul>\n"
else
  h2 = '#h2'
  h3 = '#h3'
  href = 'href='
  ull = "<ul>\n"
  ulr = "</ul>\n"
end

while line = gets
  if /^\s*<li>/ !~ line
    case line
    when /"h2:/
      print ulr
      print "<li> " + line.sub(/name=/,href).sub(/h2/,h2).chomp
      print gets.gsub(/<.*?>/,'').chomp, "</a>\n"
      print ull
    when /"h3:/
      print "  <li> " + line.sub(/name=/,href).sub(/h3/,h3).chomp
      print gets.gsub(/<.*?>/,'').chomp, "</a>\n"
    end
  end
end

