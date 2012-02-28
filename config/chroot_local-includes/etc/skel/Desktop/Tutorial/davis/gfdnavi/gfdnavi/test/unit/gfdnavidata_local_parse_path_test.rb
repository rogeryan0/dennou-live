# -*- coding: cp932 -*-
require File.dirname(__FILE__) + '/../test_helper'
require "numru/path_parse.tab"
require "numru/gfdnavi_data"

class UserTest < ActiveSupport::TestCase
  fixtures :nodes, :directories, :variables, :draw_methods, :value_types, :functions, :function_arguments, :function_outputs, :images

  def test_parse_bad_slash_slash
    path_bad_slash_slash = "//abc"
    assert_raise(RuntimeError) { NumRu::GfdnaviData::Local.parse_path(path_bad_slash_slash) }
  end

  def test_parse_bad_comma
    path_bad_comma = "/abc,/add"
    assert_raise(Racc::ParseError) { NumRu::GfdnaviData::Local.parse_path(path_bad_comma) }
  end

  def test_parse_bad_method
    path_bad_method = "/abc,/plot"
    assert_raise(Racc::ParseError) { NumRu::GfdnaviData::Local.parse_path(path_bad_method) }
  end

  def test_parse_root
    path_root = "/"
    assert_nothing_raised{NumRu::GfdnaviData::Local.parse_path(path_root)}
  end

  def test_parse_single
    path_single = "/samples"
    assert_nothing_raised{NumRu::GfdnaviData::Local.parse_path(path_single)}
  end

  def test_parse_multidir
    path_multidir = "/samples/reanalysis/ncep"
    assert_nothing_raised{NumRu::GfdnaviData::Local.parse_path(path_multidir)}
  end

  #def test_parse_strange
  #  path_strange = "/My Document/#abc/a[/b@c/!/.+-_^=;/<a>/v~"
  #  assert_nothing_raised{NumRu::GfdnaviData::Local.parse_path(path_strange)}
  #end

  def test_parse_simple
    path_simple = "/samples/reanalysis/ncep/T.jan.nc/T/cut(level=>1000.0)/plot(tone_contour;anim=0,color_bar=1,coloring=0,colormap=1,contour=1,log=0,map=0,map_axis= , , ,map_fit=1,map_window= , , , ,pileup=0,projection=1,size=400,400,tonc=0,tone=1,viewport=0.2,0.8,0.2,0.8,window= , , , ,x_axis=lon,y_axis=lat,z_axis=level)[0]"
    assert_nothing_raised{NumRu::GfdnaviData::Local.parse_path(path_simple)}
  end

  def test_parse_tree
    path_tree = "/[/[/samples/reanalysis/ncep/UV.jan.nc/U/cut(lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000..10),/samples/reanalysis/ncep/UV.jan.nc/U/cut(lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000..10)/analysis(mean;lon)[0]/cut(lat=>81.25..-77.5,level=>1000..10)]/analysis(addition,bob)[0]/cut(lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000..10),/samples/reanalysis/ncep/UV.jan.nc/V/cut(lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000..10)]/analysis(addition,bob)[0]/cut(lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000)/plot(tone_contour;projection=1,map_fit=1,map_axis=0,0,0,map_radius=,map_window=0,0,0,0,size=400,400,viewport=0.2,0.8,0.2,0.8,colormap=1,x_axis=lon,y_axis=lat,window= , , , ,tone=1,contour=1,min=,max=,nlev=,levels=,interval=,color_bar=1,log=0,coloring=0,tonc=0)[0]"
    assert_nothing_raised{NumRu::GfdnaviData::Local.parse_path(path_tree)}
  end

  def test_parse_bracketpath
    path_bracketpath = "/[/samples/jmadata/MSM-P/2006/010[1-3].nc/temp,/samples/jmadata/MSM-P/2006/010[1-3].nc/temp]/cut(lon=>120..147.5,lat=>47.5999984741211..25.6000003814697,p=>975,time=>0)/plot(vector;projection=1,map_fit=1,map_axis=,map_radius=,map_window=,size=400,400,viewport=0.2,0.8,0.2,0.8,colormap=1,x_axis=lon,y_axis=lat,window= , , , ,xintv=1,yintv=1,unit_vect=0)[0]"
    assert_nothing_raised{NumRu::GfdnaviData::Local.parse_path(path_bracketpath)}
  end

  def test_parse_overlay
    path_overlay = "/[/[/[/samples/reanalysis/ncep/UV.jan.nc/U/cut(lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000..10),/samples/reanalysis/ncep/UV.jan.nc/U/cut(lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000..10)/analysis(mean;lon)[0]/cut(lat=>81.25..-77.5,level=>1000..10)]/analysis(addition,bob)[0]/cut(lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000..10),/samples/reanalysis/ncep/UV.jan.nc/V/cut(lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000..10)]/analysis(addition,bob)[0]/cut(lon=>108.75..148.75,lat=>41.25..21.25,level=>1000)/plot(tone_contour;projection=1,map_fit=1,map_axis=0,0,0,map_radius=,map_window=0,0,0,0,size=400,400,viewport=0.2,0.8,0.2,0.8,colormap=1,x_axis=lon,y_axis=lat,window= , , , ,tone=1,contour=1,min=,max=,nlev=,levels=,interval=,color_bar=1,log=0,coloring=0,tonc=0)[0],/[/samples/reanalysis/ncep/UV.jan.nc/U,/samples/reanalysis/ncep/UV.jan.nc/V]/cut(lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000..10)/plot(vector;projection=1,map_fit=1,map_axis=0,0,0,map_radius=,map_window=0,0,0,0,size=400,400,viewport=0.2,0.8,0.2,0.8,colormap=1,x_axis=lon,y_axis=lat,window=,xintv=1,yintv=1,unit_vect=0)[0]]/overlay()"
    assert_nothing_raised{NumRu::GfdnaviData::Local.parse_path(path_overlay)}
  end
end
