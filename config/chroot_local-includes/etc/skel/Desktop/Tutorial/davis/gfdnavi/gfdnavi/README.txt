= Gfdnavi - Geophysical fluid data navigator

Copyright 2007-2012 (C) GFD Dennou Club (http://www.gfd-dennou.org/)
All rights reserved.

== Brief introduction

Gfdnavi is a tool to archive, share, distribute, analyze, and
visualize geophysical fluid data and knowledge. With Gfdnavi, one can
create a web-accessible database of his/her data. The data will then
be served for searching, analysis, and visualization. Registered users
can also save their results and write&store interactive "knowledge"
documents.

Gfdnavi is developed under the web-application framework RubyOnRails
(http://rubyonrails.org/), and it heavily relies on the GFD Dennou
Ruby products such as GPhys
(http://ruby.gfd-dennou.org/products/gphys/).

== Requirement

The current version of Gfdnavi requires Ruby 1.8 (ruby 1.8.7 is 
recommended), and RubyOnRails 2. It also requires rake and GPhys
1.2.0 or later. 

== Installation

To install Gfdnavi, move to the top directory of Gfdnavi, and execute
the following:

  ruby install.rb

It will interactively install a Gfdnavi server along with sample data
while asking some questions to you (which includes the directory under
which you want to install your Gfdnavi server).

To incorporate your data in the database, cd to the "data" directory
right under the top directory of the installed Gfdnavi server. Copy
your data there or make symbolic links to your data. In any case, you
can make subdirectories as deep as you like. Then at the top directly,
execute the following command:

  rake update

or more specifically,

  rake update:tree

A detailed instruction is found on the Gfdnavi homepage
http://www.gfd-dennou.org/arch/davis/gfdnavi/.

== License

Gfdnavi is released under the conditions in LICENSE.txt located 
in this directory, except for the third party products, which are

  All files under the directories vendor/rails and vendor/plugins;
  controls.js, dragdrop.js, effects.js, and prototype.js in
  the directory public/javascripts.

Their licenses are either directly written in each file or expressed in
some files (such as README, MIT-LICENSE, and LICENSE) under these
directories. Here, vendor/plugins contains RubyOnRails plugins, and
vendor/rails includes a copy of some RubyOnRails packages to avoid
version conflicts.


