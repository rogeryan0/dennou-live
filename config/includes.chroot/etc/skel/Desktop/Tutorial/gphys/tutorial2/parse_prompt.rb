#!/usr/bin/env ruby -Ks
# 
# * RD に書かれたプロンプト ">" の行を, 太字にする
#   * コピペしやすいように
#

def decorate(str)
  "<span class=irb_input>" + str + "</span>"
end

def parse_html(path)
  # PRE の中（ソースコード表示）で irb のプロンプト irb(main):001:0> 
  # 等で始まる行はプロンプトを削除し decorate する（spanに入れる：上をみよ）．
  # プロンプトを残したい場合，RD 中で Irb(main):002:0> などと先頭を大文字にせよ
  # (すると irb に戻すだけとなる)．

  # input
  infile = File.open(path)
  ary = infile.read.split("\n")
  infile.close

  # output
  tmppath = "tmp"
  tmpfile = File.open(tmppath, 'w')
  in_pre = false

  ary.each{|str|
    in_pre = true if /^<pre>.*<\/pre>$/ !~ str and /^<pre>/ =~ str

    s = str
    if in_pre 
      if /^(.*)irb\(\S+\&gt; *(.*)$/ =~ str
        s = $1 + decorate($2)
      else  
        # 先頭を Irb に変えてある場合，irb に戻すだけでそのまま表示
        s = str.sub(/^(.*)Irb(\(\S+\&gt;.*)$/,'\1irb\2')
      end
    end
    tmpfile.puts s

    in_pre = false if /^<pre>.*<\/pre>$/ !~ str and /<\/pre>$/ =~ str

  }
  tmpfile.close

  `mv #{tmppath} #{path}`
end


htms = Dir::glob("*.htm")

htms.each{|htm|
  parse_html(htm)
}
