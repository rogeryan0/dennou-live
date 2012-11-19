#!/usr/bin/env ruby
=begin
= RDのHeadLineの目次を作る
<<< rd-hindex.hindex.rd
== 概要
RDからHeadLineの目次RDを作成する。
長文RDを扱うときは便利。

== 使い方
まずこのスクリプトで目次を作成し、もとのRDに「Include」で取り込む。

(1) 目次作成
      $ rd-hindex.rb input.rd > input.hindex.rd
(2) もとのRDにIncludeで取り込む。((<実行結果>))参照。

=end

puts "=begin"
puts readlines.collect {|line|
  next if line =~ /^=(begin|end)$/
  if line =~ /^(=+)\s*(.+)$/
    level = $1.length
    content = $2
    #pp %w[level content], binding
    next if level == 1
    %Q[#{'  ' * (level - 2)}* ((<"#{content}">))]
  end
}.compact
puts "=end"

