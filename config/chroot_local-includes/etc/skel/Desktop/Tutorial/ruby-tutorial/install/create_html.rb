## Create html file
## for http://ruby.gfd-dennou.org/tutorial/install/
##
## Time-stamp: <2004-08-14 20:43:28 koshiro>
##

rdfile = ["index", "index-j"]
rdtitle = ["Dennou Ruby Products Installation Guide", "電脳Ruby謹製品 インストールガイド"]
rdfile.each_with_index do |rd, i|
  `rd2 -r rd/rd2html-lib.rb --with-part=html:include --with-css='http://ruby.gfd-dennou.org/tutorial/install/dennou-ruby-install.css' --html-title='#{rdtitle[i]}' #{rd}.rd > #{rd}.html`
end
