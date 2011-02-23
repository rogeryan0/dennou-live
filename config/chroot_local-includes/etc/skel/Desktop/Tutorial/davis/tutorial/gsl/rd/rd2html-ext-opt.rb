#!/usr/bin/env ruby
require 'rd/rd2html-opt'

q = ARGV.options

q.on_tail("--ref-extension") do
  $Visitor.opt_ref_extension = true
end

q.on_tail("--headline-title") do
  $Visitor.opt_headline_title = true
end

q.on_tail("--headline-secno") do
  $Visitor.opt_headline_secno = true
end

q.on_tail("--enable-br") do
  $Visitor.opt_enable_br = true
end

q.on_tail("--native-inline") do
  $Visitor.opt_native_inline = true
end

q.on_tail("--head-element") do
  $Visitor.opt_head_element = true
end
