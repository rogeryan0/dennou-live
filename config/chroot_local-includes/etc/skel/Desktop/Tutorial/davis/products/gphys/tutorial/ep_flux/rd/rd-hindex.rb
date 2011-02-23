#!/usr/bin/env ruby
=begin
= RD��HeadLine���ܼ�����
<<< rd-hindex.hindex.rd
== ����
RD����HeadLine���ܼ�RD��������롣
ĹʸRD�򰷤��Ȥ���������

== �Ȥ���
�ޤ����Υ�����ץȤ��ܼ������������Ȥ�RD�ˡ�Include�פǼ����ࡣ

(1) �ܼ�����
      $ rd-hindex.rb input.rd > input.hindex.rd
(2) ��Ȥ�RD��Include�Ǽ����ࡣ((<�¹Է��>))���ȡ�

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

