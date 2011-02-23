#!/usr/bin/env ruby
=begin

= RD��HeadLine���ܼ�����
<<< rd-hindex2.hindex.rd

== ����

RD����HeadLine���ܼ�RD��������롣
ĹʸRD�򰷤��Ȥ���������

������ ((<rubikitch ����|URL:http://www.rubyist.net/~rubikitch/>))
����((<URL:http://www.rubyist.net/~rubikitch/computer/myruby/rd-hindex/>))
�Ǹ������Ƥ��� rd-hindex.rd �򾡼�˲�¤������Ρ�

+ ��٥� 5 �Υإå�
��
++ ��٥� 6 �Υإå�
���ܼ��Ȥ��Ƽ�������ͤˤʤäƤ��롣

== �Ȥ���

�ޤ����Υ�����ץȤ��ܼ������������Ȥ�RD�ˡ�Include�פǼ����ࡣ

(1) �ܼ�����
      $ rd-hindex2.rb input.rd > input.hindex.rd

(2) ��Ȥ�RD��Include�Ǽ����ࡣ((<�¹Է��>))���ȡ�

=== ��

�㤨�Ф��� rd-hindex2.rb �Ǥ�äƤߤ�Ȱʲ��Τ褦�ʴ���

(1) ���Ȥ������ʤ��Τǡ��ޤ��ϥƥ����Ѥ˥��ԡ�
    ���κ� ��Include�׹Ԥ򥳥��ȥ����ȡ�

      $ sed s/^'<<<'/'#<<<'/ < rd-hindex2.rd  > input.rd

(2) �ܼ��ե�����������
      $ ruby rd-hindex2.rb input.rd > rd-hindex2.hindex.rd

(3) ������� HTML ������
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
