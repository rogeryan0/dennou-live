#!/usr/bin/ruby -Ks

while (l=gets)
  if /^<<< *(.*) *$/ =~ l
    File.open($1){|f| 
      f.readlines.each{|s| 
        print s.sub(/^=== (.*)/,'* ((<\1>))').sub(/^==== (.*)/,'  ((:<span class=h4index>:))| ((<\1>))((:</span>:))')
      }
    }
  else
    print l
  end
end
