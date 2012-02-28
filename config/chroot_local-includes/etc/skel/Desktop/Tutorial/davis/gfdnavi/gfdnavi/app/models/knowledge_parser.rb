# -*- coding: japanese-cp932 -*-
# = Gfdnavi��knowledge�h�L�������g�����߂� RD �`���̕��͂��p�[�X����B
# == ���p��
#     rdp = RDParser.new( rdstring )
#     tree = rdp.make_tree     # parse����
#     tree.parse               # html�ɕϊ�����
#
# == knowledge�h�L�������g�̎d�l
# === ���o��(<h1>, <h2>, ... , <h6>)
# �s���� ==, ===, ====, +, ++�����Ă����
# ���ꂼ�� H2, H3, H4, H5, H6 �ƃ^�O�����B
# �Ȃ��A�^�C�g����H1�^�O���g���Ă��邽�߁A= �� H1 �^�O�͂��Ȃ��B
# === �Q��(�����N)
# URL�Ƀ����N�𒣂�ɂ͎��̂悤�ɂ���B
#   =begin
#     ((<URL:http://www.ruby-land.org>))      <- ���̌`���Œ��ɏ����ꂽURL�ւ̃����N��������B
#     ((<Ruby|URL:http://www.ruby-lang.org>)) <- "|"�̑O�ɕ�����������ƁA���̕����񂪕\�������B
#     ((<URL:/usr/root/knowledge/ex01.knlge>)) <- Gfdnavi����URL�ւ̃����N�͂��̋L�q��OK.�\���̂��߂�URL�֎����I�ɕϊ������B
#   =end
# === �������ɐ}��}������
#  (([1, 200]))�A(([2, 480]))�@�Ƃ����悤�ɐ}�̔ԍ��ƕ�(�s�N�Z��)���w�肷�邱�ƂŐ}��}���ł��܂��B
# === �ʏ�̕���
# �s�����當���������ƒʏ�̃e�L�X�g���o�͂����B<p>�^�O�ň͂܂��B
# ���s�͖�������A��s������Ɖ��i���ƂȂ�B
# === ���p���[�h
# �C���f���g���󂯂ĕ����������ƁA���p���[�h(Preformatted Text)�ƂȂ�B<pre>�^�O�ň͂܂��B
# �C���f���g�Ƃ��Ĉ�����͔̂��p�󔒂����ł���B
# === �ӏ�����
# �s���̕������A�X�^���X�N�̍s�͉ӏ������ɕϊ������B
# �C���f���g����Ɖӏ������̓���q�ɂȂ�B
# (����)�Ŏn�܂�s�͔ԍ��t�����X�g�ɂȂ�B
# === ����
# ((*����*))�̂悤�ɃA�X�^���X�N�ň͂ނƋ����\��(<em>�^�O)�ɂȂ�B
#
class RDParser
  # = knowledge�h�L�������g�Ɋ܂܂��v�f�͑S��RDElement�^�Ƃ���B
  #   RDElement�̒��ɕ�����RDElement������B
  class RDElement < Array
    attr_accessor :element_type

    def collect 
      ary = super
      ctn = self.class.new(ary)
      ctn.element_type = self.element_type
      ctn
    end
    
    def deep_each(&block)
      each do |x|
        if x.is_a?(RDElement)
          yield(x)
          x.deep_each(&block)
        else
          yield(x)
        end
      end
    end
    
    def deep_collect(&block)
      collect do |x|
        if x.is_a?(RDElement)
          yield(x)
          x.deep_collect(&block)
        else
          yield(x)
        end
      end
    end

    def insert_to_tree
      self[-2].push(self[-1])
      self.pop
      return self
    end
  end

  def initialize(text)
    @text = text
    @rd_top = RDElement.new
    @rd_top.element_type = ""
    @aname = Hash.new
    @parse_error = Array.new
    @mark_indent_stack = Array.new
    @text_indent_stack = Array.new
  end

  # �����͂ɂ��c���[�����
  def make_tree
    # initialize
    current_container = RDElement.new
    current_container.element_type = "top"
    current_container = @rd_top
    current_mode = String.new
    leaf = RDElement.new
    @mark_indent_stack = [-1]
    @text_indent_stack = [-1]

    @text.each_line {|line|
=begin
      print "===============================\n"
      print "line = "
      pp line
      print "@mark_indent_stack = "
      pp @mark_indent_stack
      print "@text_indent_stack = "
      pp @text_indent_stack
=end

      # "#" is comment.
      next if /^#/ =~ line

      # html_escape ({'&'=>'&amp;','"'=>'&quot;','>'=>'&gt;','<'=>'&lt;'})
      line.gsub!(/\&/, "&amp;")
      line.gsub!(/\"/, "&quot;")
      line.gsub!(/</, "&lt;")
      line.gsub!(/>/, "&gt;")
      
      # = �V���ȍs���O�̍s�̑������ǂ����𒲂ׂ�
      # == return String :     continue -> add String to current_container
      # == return nil    : not continue -> new_leaf = RDElement.new\ current_container.push(new_leaf)
      succeeding_line = succeession_check(current_mode, line)
      if succeeding_line
        current_container[-1][-1] += succeeding_line
      else
        case line
        when /^(\+|\=)+(.*?)\r?$/ # <h1>, <h2>, ... , <h6>
          new_leaf = RDElement.new([$2])
          new_leaf.element_type = case line
                                  when /^\+{2}/ then "h6"
                                  when /^\+{1}/ then "h5"
                                  when /^={4}/ then "h4"
                                  when /^={3}/ then "h3"
                                  when /^={2}/ then "h2"
                                  when /^={1}/ then "h1"
                                  end
          mark_indent = text_indent = 0
        when /^(\s*)\*(\s*)[ ]*(.*?)\r?$/ # itemize
          new_leaf = RDElement.new([$3])
          new_leaf.element_type = "item"
          container_type = "itemize"
          mark_indent = $1.length
          text_indent = mark_indent + 1 + $2.length
        when /^(\s*)\((\d)+\)([ ]*)(.*?)\r?$/ # enumerate
          new_leaf = RDElement.new([$4])
          new_leaf.element_type = "item"
          container_type = "enumerate"
          mark_indent = $1.length
          text_indent = mark_indent + 1 + $2.length + 1 + $3.length
        when /^(\s+)(.+?)\r?$/ # <pre> - </pre>
          new_leaf = RDElement.new([$2 + "\n"])
          new_leaf.element_type = "preformatted"
          container_type = "container"
          mark_indent = text_indent = $1.length
        else
          /^(.*?)(\r?)$/ =~ line
          new_leaf = RDElement.new([$1])
          new_leaf.element_type = "text"
          mark_indent = text_indent = 0
        end
=begin
        print "current_mode = "
        pp current_mode
        print "new_leaf.element_type = "
        pp new_leaf.element_type
=end
        
        # ���̃��[�h��item��preformatted�Ȃ�A�܂�1up����(�C���f���g����߂��Acurrent_container����O�ɕt���ւ���)
        # ������item����item�Ȃ炻��͂��Ȃ�
        if (current_mode == "item" && new_leaf.element_type != "item") || (current_mode == "preformatted")
          current_container = @rd_top
          (@mark_indent_stack.length - 1).times do
            current_container = current_container[-1]
          end
          @mark_indent_stack.pop
          @text_indent_stack.pop
        end
        
        case new_leaf.element_type
        when "item", "preformatted"
          case @mark_indent_stack[-1] <=> mark_indent
          when 1 # "up" �C���f���g����ʂɖ߂�ꍇ
            count = 0
            
            # �ǂ̈ʒu�܂Ŗ߂邩(���K�w�߂邩)���v�Z
            @mark_indent_stack.each {|m|
              if m >= mark_indent
                break
              else # m < mark_indent
                count += 1
              end
            }
            
            # item(itemize, enumerate)�Ȃ��Ōv�Z�����񐔕������̂ڂ���current_container����������B
            # ���ꂪ�A�z���[]����邱�ƂɂȂ�B
            if new_leaf.element_type == "item"
              current_container = @rd_top
              count.times do
                current_container = current_container[-1]
              end
            end
            @mark_indent_stack = @mark_indent_stack[0..count]
            @text_indent_stack = @text_indent_stack[0..count]
            
          when -1 # "down" �C���f���g������ɐ[���Ȃ�ꍇ
            # item(itemize, enumerate)�Ȃ�[���@��
            if new_leaf.element_type == "item"
              new_container = RDElement.new # for indent, make a new container.
              new_container.element_type = container_type
              current_container.push(new_container)
              current_container = current_container[-1]
            end
            @mark_indent_stack.push(mark_indent)
            @text_indent_stack.push(text_indent)
          end
        else # when "h1", "h2", ... , "h6", "text"
          current_container = @rd_top
          @mark_indent_stack = [-1]
          @text_indent_stack = [-1]
        end
        current_container.push(new_leaf)
        current_mode = new_leaf.element_type
      end
    }

=begin
    print "==========\n"
    print "==========\n"
    print "@rd_top = "
    pp @rd_top
=end

    return @rd_top
  end

  # �c���[�̗v�f�����āAelement_type�ɉ����ēK�؂�HTML������
  #def to_html(image_paths)
  def to_html(knowledge_figures, relative_url_root)
    # add the second argument (relative_url_root) by S Nishizawa (2012/02/21)
    @rd_top.deep_collect do |rd|
      if rd.class == RDParser::RDElement
        if rd[0].class == String
          # ���p���[�h�ȊO�ł͎Q�Ƃ�����
          unless rd.element_type == "preformatted"
            # ����<em>
            if /.*\(\(\*.+?\*\)\).*/ =~ rd[0]
              rd[0].gsub!(/\(\(\*/, "<em class=davis>")
              rd[0].gsub!(/\*\)\)/, "</em>")
            end
            
            # === �Q��(link)
            # ==== �܂�((< >))�̑g�ݍ��킹��������
            rd[0].gsub!(/\(\(&lt;(.+?)&gt;\)\)/){|x|
              # ====  | �̗L��
              inner = $1
              case inner
              when /(.+)\|(.+)/
                display = $1
                link = $2
              else
                display = inner
                link = inner
              end
              
              # ==== "URL:"�Ŏn�܂��Ă��邩�H
              case link
              when /^URL:(.+)/ # �O�������N
                link = $1
              when /^\/(.+)/ # �����p�X
                path = $1
                case path
                when /^(.+)\/(.+?).knlge$/ # �m���h�L�������g
                  link = "/knowledge/show?path=/#{$1}/#{$2}.knlge"
                when /^(.+)\/(.+?).png$/ # png�摜
                  link = "/image/show?path=/#{$1}/#{$2}.png"
                else # ���̑�(details��\��)
                  link = "/description/node?path=#{link}"
                  # ������̓f�B���N�g�����J����悤�ɂ���
                end
                link = relative_url_root + link
              else
                # �y�[�W�������N # not found �Ȃ�C�^���b�N�̂ŏo���悤�ɂ�����
                if /^figure\s*(\d+)$/ =~ link
                  link = "#figure_" + $1
                else
                  link = "#" + link
                end
              end
              "<a href=#{link}>#{display}</a>"
            }

            # === �G�̖��ߍ���
            # (([number. width]))
            rd[0].gsub!(/\(\(\[\s*(\d+)[\s,]+(.*?)\s*\]\)\)/){|x|
              kf = knowledge_figures[$1.to_i - 1]
              #if (image = Image.find(:first, :conditions => ['path=?', image_paths[$1.to_i - 1]]))
              if (kf && image = Image.find(:first, :conditions => ['path=?', kf.image_path]))
                html = "<a href=\"/data/#{kf.image_path}\">"
                html+= "<img src=\"/data/#{kf.image_path}\" alt=\"Figure #{$1}\" width=#{$2}/>\n"
                html+= "</a><br>\n"
                if image.org_path
                  html+= "<span title=\"then you can further explore the data.\">\n"
                  html+= "<a href=\"/knowledge/fig2analysis?org_path=#{image.org_path}\">&lt;Redraw this image&gt;</a>\n"
                  html+= "</span><br>\n"
                end
                html+= "<b>Fig.&nbsp;#{$1}.</b><br>\n"
                html+= "<p>\n"
                kf.caption.each_line do |line|
                  html+= html_escape("#{line}")
                end
                html+= "</p><br>\n"
                html
              end
            }
          end
        end

        case rd.element_type
        when /^h(\d)$/
          # <h1>�p��if��
          # <h1>�̓^�C�g���p�A�{���ɂ͎g�p�֎~
          if ($1.to_i == 1)
            rd[0] = "<h2 id=\"#{rd[0]}\" class=davis>" + rd[0] + "</h2>\n" + "&nbsp;&nbsp;<font color ='red'>= is disabled because &lt;h1&gt; tag is used for title.</font>"
          else
            rd[0] = "<h#{$1} id=\"#{rd[0]}\" class=davis>" + rd[0] + "</h#{$1}>\n"
          end
        when "preformatted"
          rd[0] = "<pre class=davis>" + rd[0] + "</pre>\n"
        when "itemize"
          ul1 = RDElement.new(["<ul class=davis>"])
          ul1.element_type = "ul1"
          ul2 = RDElement.new(["</ul>"])
          ul2.element_type = "ul2"
          rd.insert(0, ul1).push(ul2)
        when "enumerate"
          ol1 = RDElement.new(["<ol class=davis>"])
          ol1.element_type = "ol1"
          ol2 = RDElement.new(["</ol>"])
          ol2.element_type = "ol2"
          rd.insert(0, ol1).push(ol2)
        when "item" then rd[0] = "<li class=davis>" + rd[0].to_s + "</li>\n"
        when "text" then rd[0] = "<p class=davis>" + rd[0].to_s + "</p>\n" unless rd[0] == ""
        end
      end
    end
    return @rd_top
  end
 
private
  # check that line is the succeeding phrase as previous line or not.
  ## return String :     continue -> add String to current_container
  ## return nil    : not continue -> new_leaf = RDElement.new\ current_container.push(new_leaf)
  def succeession_check(current_mode, line)
    case current_mode
    when "text" # �s����+,=,*, ,(1)�A�������͋�s�Ȃ�p�����Ȃ�
      return case line
      when /^(=|\+|\*|\s|\(\d+\))/ then nil
      when /^(.*?)\r?$/            then $1
      else                              line
      end
    when "item" 
      # �擪����󔒂������A
      # ���͕����̃C���f���g�̐�����v����Όp��
      # h1, h2, ... , itemize, enumerate �ɂȂ�Ȃ�p�����Ȃ�
      unless ((/^(\+|\=)/ =~ line) || (/^[ ]*(\*|\(\d+\))/ =~ line))
        if (/^([ ]*)(.*?)([\r]*)$/ =~ line) 
          spaces = $1.length
          linebody = $2
          return linebody if (spaces == @text_indent_stack[-1])
        end
      end
    when "preformatted"
      case line
      when /^([ ]*?)[\r]*$/ # ��s�̏ꍇ�͋�s�����̂܂܏o��
        return "\n"
      when /^([ ]+)(.*?)[\r]*$/ # �s�����󔒂Ȃ�
        diff = $1.length - @text_indent_stack[-1] # indent = $1.length
        return case
               when diff > 0
                 " " * diff + $2 + "\n" # �C���f���g���������炻�̕��̋󔒂�����
               when diff < 0
                 nil                    # �C���f���g���߂�Όp�����Ȃ�
               else
                 $2 + "\n"
               end
      else # �s�����󔒂Ŗ�����Όp�����Ȃ�
        return nil
      end
    else # current_type��nil�̂Ƃ��A�܂���<h1>-<h6>�̂Ƃ��͌p�����Ȃ�
      return nil
    end
  end

private  
  # views �Ŏg����html_escape���\�b�h��models�Ŏg�����߂ɂ��������ōĒ�`
  def html_escape(html)
    html_escapes = {  '&' => '&amp;', '"' => '&quot;', '>' => '&gt;', '<' => '&lt;' }
    html.to_s.gsub(/[&\"><]/) { |special| html_escapes[special] }
  end
end
