while(line=gets)
  lineno = ($.>=10) ? ($..to_s) : (' '+$..to_s)
  print lineno,': ',line
end
