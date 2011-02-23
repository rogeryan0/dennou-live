=begin

= NumRu Installer -- the auto install script of Dennou Ruby products

Last modified: <2007-04-27 06:08:01 koshiro>

This is a Ruby script for auto-installation of Dennou Ruby products.
With this installer, you can install Dennou Ruby products much more easily than install manually.
Now all packages of Dennou Ruby products related GAVE can be installed.
(Ruby-SSL2 and Ruby/Msise90 are not supported.)

=end
=begin html
<font color="red">*** This software is not maintained now! ***</font>
=end
=begin

== Download

((<numru-install.rb|URL:http://ruby.gfd-dennou.org/products/installer/numru-install.rb>))
(Ver. 3.4.7)

== Required environment

Ruby 1.6.7 or later, which includes URI module as a standard library, is required.

This installer should work on any UNIX OS(Linux, FreeBSD, Solaris, Cygwin on Windows, etc.), but GNU gzip and GNU patch are required.

We have already checked that this installer works correctly on the following environment:

* Vine Linux 2.6 + Ruby 1.6.7
* Vine Linux 3.1 + Ruby 1.8.2
* Debian GNU/Linux 3.1 + Ruby 1.8.2
* Cygwin(on Windows XP Professional) + Ruby 1.8.4

== How to use

Type the following command:

((%$ ruby numru-install.rb%))

And answer several quiestions.

If your Internet connection is via a firewall or proxy, you must set
the environment variable "http_proxy" with the following syntax:

(sh, bash) ((%$ export http_proxy=http://your.proxy.server:8080/%))

(csh, tcsh) ((%% setenv http_proxy http://your.proxy.server:8080/%))

And if your have to use passive mode when you connect FTP server through
the firewall, you must set the environment variable "FTP_PASSIVE_MODE":

(sh, bash) ((%$ export FTP_PASSIVE_MODE=YES%))

(csh, tcsh) ((%% setenv FTP_PASSIVE_MODE YES%))

After installation, make a point of checking your environment variable "PATH" and "RUBYLIB".

== Related information

=== Patches for Cygwin

For some products, installation is failed on Cygwin environment.
NumRu Installer "numru-install.rb" uses patches to solve this problem:

* DCL C version (5.3.x) -- ((<dcl53c_cygwin.patch.gz|URL:http://ruby.gfd-dennou.org/products/installer/patches/dcl53c_cygwin.patch.gz>)) (Thanks to Endo-san)
  * Change first lines of csh script "dclcc": ((%#!/bin/csh -f%)) -> ((%#!/bin/tcsh -f%)) (Cygwin package of csh is not provided)

* NetCDF (3.6.1) -- ((<netcdf361_cygwin.patch.gz|URL:http://ruby.gfd-dennou.org/products/installer/patches/netcdf361_cygwin.patch.gz>))
  * Comment out the code to create a `whatis' file. (make failure after makewhatis returns status 255)

== Change log
:Feb 13, 2006 (3.4.7)
  * apply netcdf361_cygwin.patch.gz if needed.

:Feb 8, 2006 (3.4.6)
  * change URL of narray.
  * change installation check method "installed?" for ruby-fftw3.

:Jun 27, 2005 (3.4.5)
  * add installation check by method "installed?" after installation.
  * change module NumRu::Fetch::http_get_list
  * stop using getopts.
  * use an external Ruby process to load ruby library for installation check.

:May 24, 2005 (3.4.4)
  * remove rubydcl150_cygwin.patch.gz.

:Mar 17, 2005 (3.4.3)
  * change download site of ruby-gnome, ruby-gnome2: voxel.net -> JAIST

:Mar 3, 2005 (3.4.2)
  * deal with installation failure of the import library of narray on Cygwin + Ruby 1.8.2.

:Mar 3, 2005 (3.4.1)
  * use 3.6.1 beta release on Cygwin if the latest version of netcdf is 3.6.0-p?

:Mar 3, 2005 (3.4.0)
  * connect only one time to FTP server when checking version of Dennou Ruby Products.
  * change URL of fftw: Japan mirror site -> original site

:Mar 1, 2005 (3.3.3)
  * deal with changing the installation script of narray_miss.
  * change regular expression for checking latest version of packages.
  * change URL of fftw: original site -> Japan mirror site

:Jan 15, 2005 (3.3.2)
  * deal with changing the installation script of gphys.

:Jan 6, 2005 (3.3.1)
  * apply rubydcl150_cygwin.patch.gz if needed.
  * change a configure option on Linux and a pacth file on Cygwin for netcdf.
  * use 3.6.1 beta release on Cygwin if the latest version of netcdf is 3.6.0.

:Dec 22, 2004 (3.3.0)
  * deal with changing of the tarball name and URL of dcl-C.
  * deal with changing the installation script of gave.

:Aug 12, 2004 (3.2.1)
  * bug fix.

:Aug 12, 2004 (3.2.0)
  * change class RbLib: deal with changing installation script of narray_miss.

:Aug 11, 2004 (3.1.0)
  * change class RubySoft: deal with changing installation script of gave.
  * name of package is changed: misc -> numru-misc
  * bug fix.

:Jul 15, 2004 (3.0.0)
  * add packages: fftw, ruby-fftw3, multibitnums, gpv, numru-units, ruby-{gnome|gnome2}-all, gave
  * change module NumRu::Fetch::ftp_fetch, NumRu::Fetch::ftp_get_list: add check environment variable 'FTP_PASSIVE_MODE'
  * revise module NumRu::Install
    * add checking of GTK version
    * add class Installer: add self-version-check and self-download function
    * add method 'install' to class RubySoft
    * add class RubyLib, which is superclass of RbExtLib and RbLib
    * change method 'current_ver'(Software, RubySoft): version + GTK version
    * add method 'current_ver_disp' for class Software
    * revise the scheme of checking package dependencies:add method 'get_dep_check_pkgs' and revise method 'check_depend'
    * add module function 'upgrade'
  * change package dependencies:
    * rubydcl requires ruby-{gnome|gnome2}-all
    * gphys requires ruby-fftw3 and numru-units
  * 'gpatch' is used, if found, instead of 'patch'.
  * bug fix.

:Dec 18, 2003 (2.0.1)
  * bug fix. Thanks to Nishizawa-san.
    * change uncompress command: 'zcat' -> 'gzip -dc'
    * rescue exceptions in Net::HTTP#get (for Ruby 1.6.7)
  * apply netcdf350_cxx_gcc3.patch if needed

:Dec 10, 2003 (2.0.0)
  * add packages: misc, met, gphys
  * revise module NumRu::Install
    * select download directory
    * select installation prefix and Ruby libraries directory
    * check file and version dependencies
    * apply patches
    * extra processing for Cygwin

:Jun 27, 2003 (1.0.0)
  * first release.
  * Thanks to Gotoken-san: contributed by coding module NumRu::Fetch

=end
=begin html
<br>
<hr>
<address>
Copyright (C) 2003-2006 GFD Dennou Club. All rights reserved.
</address>
=end
