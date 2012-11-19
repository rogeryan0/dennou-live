#!/usr/bin/env ruby -Ks
# 
# * RD �ɏ����ꂽ�v�����v�g ">" �̍s��, �����ɂ���
#   * �R�s�y���₷���悤��
#

def decorate(str)
  "<span class=irb_input>" + str + "</span>"
end

def parse_html(path)
  # PRE �̒��i�\�[�X�R�[�h�\���j�� irb �̃v�����v�g irb(main):001:0> 
  # ���Ŏn�܂�s�̓v�����v�g���폜�� decorate ����ispan�ɓ����F����݂�j�D
  # �v�����v�g���c�������ꍇ�CRD ���� Irb(main):002:0> �ȂǂƐ擪��啶���ɂ���
  # (����� irb �ɖ߂������ƂȂ�)�D

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
        # �擪�� Irb �ɕς��Ă���ꍇ�Cirb �ɖ߂������ł��̂܂ܕ\��
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
