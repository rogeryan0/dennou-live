# -*- coding: japanese-cp932 -*-
# = Gfdnaviのknowledgeドキュメントを収める RD 形式の文章をパースする。
# == 利用例
#     rdp = RDParser.new( rdstring )
#     tree = rdp.make_tree     # parseする
#     tree.parse               # htmlに変換する
#
# == knowledgeドキュメントの仕様
# === 見出し(<h1>, <h2>, ... , <h6>)
# 行頭に ==, ===, ====, +, ++がついていると
# それぞれ H2, H3, H4, H5, H6 とタグがつく。
# なお、タイトルにH1タグを使っているため、= で H1 タグはつかない。
# === 参照(リンク)
# URLにリンクを張るには次のようにする。
#   =begin
#     ((<URL:http://www.ruby-land.org>))      <- この形式で中に書かれたURLへのリンクが張られる。
#     ((<Ruby|URL:http://www.ruby-lang.org>)) <- "|"の前に文字列を書くと、その文字列が表示される。
#     ((<URL:/usr/root/knowledge/ex01.knlge>)) <- Gfdnavi内のURLへのリンクはこの記述でOK.表示のためのURLへ自動的に変換される。
#   =end
# === 文書内に図を挿入する
#  (([1, 200]))、(([2, 480]))　というように図の番号と幅(ピクセル)を指定することで図を挿入できます。
# === 通常の文章
# 行頭から文字を書くと通常のテキストが出力される。<p>タグで囲まれる。
# 改行は無視され、空行があると改段落となる。
# === 引用モード
# インデントを空けて文字を書くと、引用モード(Preformatted Text)となる。<pre>タグで囲まれる。
# インデントとして扱われるのは半角空白だけである。
# === 箇条書き
# 行頭の文字がアスタリスクの行は箇条書きに変換される。
# インデントすると箇条書きの入れ子になる。
# (数字)で始まる行は番号付きリストになる。
# === 強調
# ((*強調*))のようにアスタリスクで囲むと強調表示(<em>タグ)になる。
#
class RDParser
  # = knowledgeドキュメントに含まれる要素は全てRDElement型とする。
  #   RDElementの中に複数のRDElementが入る。
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

  # 字句解析によりツリーを作る
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
      
      # = 新たな行が前の行の続きかどうかを調べる
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
        
        # 今のモードがitemかpreformattedなら、まず1upする(インデントを一つ戻し、current_containerを一つ前に付け替える)
        # ただしitemからitemならそれはやらない
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
          when 1 # "up" インデントが上位に戻る場合
            count = 0
            
            # どの位置まで戻るか(何階層戻るか)を計算
            @mark_indent_stack.each {|m|
              if m >= mark_indent
                break
              else # m < mark_indent
                count += 1
              end
            }
            
            # item(itemize, enumerate)なら上で計算した回数分さかのぼってcurrent_containerをつけかえる。
            # これが、配列の[]を閉じることになる。
            if new_leaf.element_type == "item"
              current_container = @rd_top
              count.times do
                current_container = current_container[-1]
              end
            end
            @mark_indent_stack = @mark_indent_stack[0..count]
            @text_indent_stack = @text_indent_stack[0..count]
            
          when -1 # "down" インデントがさらに深くなる場合
            # item(itemize, enumerate)なら深く掘る
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

  # ツリーの要素を見て、element_typeに応じて適切にHTML化する
  #def to_html(image_paths)
  def to_html(knowledge_figures, relative_url_root)
    # add the second argument (relative_url_root) by S Nishizawa (2012/02/21)
    @rd_top.deep_collect do |rd|
      if rd.class == RDParser::RDElement
        if rd[0].class == String
          # 引用モード以外では参照を扱う
          unless rd.element_type == "preformatted"
            # 強調<em>
            if /.*\(\(\*.+?\*\)\).*/ =~ rd[0]
              rd[0].gsub!(/\(\(\*/, "<em class=davis>")
              rd[0].gsub!(/\*\)\)/, "</em>")
            end
            
            # === 参照(link)
            # ==== まず((< >))の組み合わせを見つける
            rd[0].gsub!(/\(\(&lt;(.+?)&gt;\)\)/){|x|
              # ====  | の有無
              inner = $1
              case inner
              when /(.+)\|(.+)/
                display = $1
                link = $2
              else
                display = inner
                link = inner
              end
              
              # ==== "URL:"で始まっているか？
              case link
              when /^URL:(.+)/ # 外部リンク
                link = $1
              when /^\/(.+)/ # 内部パス
                path = $1
                case path
                when /^(.+)\/(.+?).knlge$/ # 知見ドキュメント
                  link = "/knowledge/show?path=/#{$1}/#{$2}.knlge"
                when /^(.+)\/(.+?).png$/ # png画像
                  link = "/image/show?path=/#{$1}/#{$2}.png"
                else # その他(detailsを表示)
                  link = "/description/node?path=#{link}"
                  # いずれはディレクトリも開けるようにする
                end
                link = relative_url_root + link
              else
                # ページ内リンク # not found ならイタリック体で出すようにしたい
                if /^figure\s*(\d+)$/ =~ link
                  link = "#figure_" + $1
                else
                  link = "#" + link
                end
              end
              "<a href=#{link}>#{display}</a>"
            }

            # === 絵の埋め込み
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
          # <h1>用のif文
          # <h1>はタイトル用、本文には使用禁止
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
    when "text" # 行頭が+,=,*, ,(1)、もしくは空行なら継続しない
      return case line
      when /^(=|\+|\*|\s|\(\d+\))/ then nil
      when /^(.*?)\r?$/            then $1
      else                              line
      end
    when "item" 
      # 先頭から空白が続き、
      # 文章部分のインデントの数が一致すれば継続
      # h1, h2, ... , itemize, enumerate になるなら継続しない
      unless ((/^(\+|\=)/ =~ line) || (/^[ ]*(\*|\(\d+\))/ =~ line))
        if (/^([ ]*)(.*?)([\r]*)$/ =~ line) 
          spaces = $1.length
          linebody = $2
          return linebody if (spaces == @text_indent_stack[-1])
        end
      end
    when "preformatted"
      case line
      when /^([ ]*?)[\r]*$/ # 空行の場合は空行をそのまま出力
        return "\n"
      when /^([ ]+)(.*?)[\r]*$/ # 行頭が空白なら
        diff = $1.length - @text_indent_stack[-1] # indent = $1.length
        return case
               when diff > 0
                 " " * diff + $2 + "\n" # インデントが増えたらその分の空白を入れる
               when diff < 0
                 nil                    # インデントが戻れば継続しない
               else
                 $2 + "\n"
               end
      else # 行頭が空白で無ければ継続しない
        return nil
      end
    else # current_typeがnilのとき、または<h1>-<h6>のときは継続しない
      return nil
    end
  end

private  
  # views で使えるhtml_escapeメソッドをmodelsで使うためにここだけで再定義
  def html_escape(html)
    html_escapes = {  '&' => '&amp;', '"' => '&quot;', '>' => '&gt;', '<' => '&lt;' }
    html.to_s.gsub(/[&\"><]/) { |special| html_escapes[special] }
  end
end
