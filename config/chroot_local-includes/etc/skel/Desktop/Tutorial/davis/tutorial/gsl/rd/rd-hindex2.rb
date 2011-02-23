#!/usr/bin/env ruby
=begin

= RDのHeadLineの目次を作る
<<< rd-hindex2.hindex.rd

== 概要

RDからHeadLineの目次RDを作成する。
長文RDを扱うときは便利。

元々は ((<rubikitch さん|URL:http://www.rubyist.net/~rubikitch/>))
が、((<URL:http://www.rubyist.net/~rubikitch/computer/myruby/rd-hindex/>))
で公開していた rd-hindex.rd を勝手に改造したもの。

+ レベル 5 のヘッダ
と
++ レベル 6 のヘッダ
も目次として取り込む仕様になっている。

== 使い方

まずこのスクリプトで目次を作成し、もとのRDに「Include」で取り込む。

(1) 目次作成
      $ rd-hindex2.rb input.rd > input.hindex.rd

(2) もとのRDにIncludeで取り込む。((<実行結果>))参照。

=== 例

例えばこの rd-hindex2.rb でやってみると以下のような感じ

(1) 自身を取り込めないので、まずはテスト用にコピー
    その際 「Include」行をコメントアウト。

      $ sed s/^'<<<'/'#<<<'/ < rd-hindex2.rd  > input.rd

(2) 目次ファイル生成。
      $ ruby rd-hindex2.rb input.rd > rd-hindex2.hindex.rd

(3) 取り込んで HTML 作成。
      $ rd2 rd-hindex2.rb > rd-hindex2.html


=end

puts "=begin"
puts readlines.collect {|line|
  next if line =~ /^=(begin|end)$/
  if line =~ /^(=+|\++)\s*(.+)$/
    level = $1.length
    content = $2
    if $1 =~ /^\++$/
      level = level + 4
    end
    #pp %w[level content], binding
    next if level == 1
    %Q[#{'  ' * (level - 2)}* ((<"#{content}">))]
  end
}.compact
puts "=end"
