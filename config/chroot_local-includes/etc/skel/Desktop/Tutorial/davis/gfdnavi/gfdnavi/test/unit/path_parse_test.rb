#require "test/unit"
require File.dirname(__FILE__) + '/../test_helper'
require "numru/path_parse.tab"


=begin
class PathParse
  def parse(arg)
    nil
  end
end
=end


class PathTest < Test::Unit::TestCase

  def setup

    @path_bad_slash_slash = "//abc"

    @path_bad_comma = "/abc,/add"

    @path_bad_method = "/abc,/plot"


    @path_root = "/"
    @result_root = ["/"]

    @path_single = "/samples"
    @result_single = [@path_single]

    @path_multidir = "/samples/reanalysis/ncep"
    @result_multidir = [@path_multidir]

    @path_strange = "/My Document/#abc/a[/b@c/!/.+-_^=;/<a>/v~"
    @result_strange = [@path_strange]

    @path_array = "/[/samples/reanalysis/ncep/UV.jan.nc/U,/samples/reanalysis/ncep/UV.jan.nc/V]"
    @result_array = [
                     "/samples/reanalysis/ncep/UV.jan.nc/U",
                     "/samples/reanalysis/ncep/UV.jan.nc/V"
                    ]

    @path_array_slice = "/[/samples/reanalysis/ncep/UV.jan.nc/U,/samples/reanalysis/ncep/UV.jan.nc/V][0]"
    @result_array_slice = [["/samples/reanalysis/ncep/UV.jan.nc/U",
                            "/samples/reanalysis/ncep/UV.jan.nc/V"],
                           [:slice, [0]]
                          ]
    @path_array_nested = "/[/samples/reanalysis/ncep/UV.jan.nc/U,/[/samples/reanalysis/ncep/UV.jan.nc/U,/samples/reanalysis/ncep/UV.jan.nc/V]]"
    @result_array_nested = ["/samples/reanalysis/ncep/UV.jan.nc/U",
                            ["/samples/reanalysis/ncep/UV.jan.nc/U",
                             "/samples/reanalysis/ncep/UV.jan.nc/V"]]

    
    @path_simple = "/samples/reanalysis/ncep/T.jan.nc/T/cut(level=>1000.0)/plot(tone_contour;anim=0,color_bar=1,coloring=0,colormap=1,contour=1,log=0,map=0,map_axis= , , ,map_fit=1,map_window= , , , ,pileup=0,projection=1,size=400,400,tonc=0,tone=1,viewport=0.2,0.8,0.2,0.8,window= , , , ,x_axis=lon,y_axis=lat,z_axis=level)[0]"
    @result_simple = ["/samples/reanalysis/ncep/T.jan.nc/T",
                      [:cut,"level=>1000.0"],
                      [:plot,"tone_contour;anim=0,color_bar=1,coloring=0,colormap=1,contour=1,log=0,map=0,map_axis= , , ,map_fit=1,map_window= , , , ,pileup=0,projection=1,size=400,400,tonc=0,tone=1,viewport=0.2,0.8,0.2,0.8,window= , , , ,x_axis=lon,y_axis=lat,z_axis=level"],
                      [:slice,[0]]
                     ]

    @path_tree = "/[/[/samples/reanalysis/ncep/UV.jan.nc/U/cut(lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000..10),/samples/reanalysis/ncep/UV.jan.nc/U/cut(lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000..10)/analysis(mean;lon)[0]/cut(lat=>81.25..-77.5,level=>1000..10)]/analysis(subtraction)[0]/cut(lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000..10),/samples/reanalysis/ncep/UV.jan.nc/V/cut(lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000..10)]/analysis(multiplication)[0]/cut(lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000)/plot(tone_contour;projection=1,map_fit=1,map_axis=0,0,0,map_radius=,map_window=0,0,0,0,size=400,400,viewport=0.2,0.8,0.2,0.8,colormap=1,x_axis=lon,y_axis=lat,window= , , , ,tone=1,contour=1,min=,max=,nlev=,levels=,interval=,color_bar=1,log=0,coloring=0,tonc=0)[0]"
    @result_tree = [
                    [
                     [
                      [
                       ["/samples/reanalysis/ncep/UV.jan.nc/U",
                        [:cut,"lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000..10"]
                       ],
                       ["/samples/reanalysis/ncep/UV.jan.nc/U",
                        [:cut,"lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000..10"],
                        [:analysis,"mean;lon"],
                        [:slice,[0]],
                        [:cut,"lat=>81.25..-77.5,level=>1000..10"]
                       ]
                      ],
                      [:analysis,"subtraction"],
                      [:slice,[0]],
                      [:cut,"lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000..10"]
                     ],
                     ["/samples/reanalysis/ncep/UV.jan.nc/V",
                      [:cut,"lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000..10"]
                     ]
                    ],
                    [:analysis,"multiplication"],
                    [:slice,[0]],
                    [:cut,"lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000"],
                    [:plot,"tone_contour;projection=1,map_fit=1,map_axis=0,0,0,map_radius=,map_window=0,0,0,0,size=400,400,viewport=0.2,0.8,0.2,0.8,colormap=1,x_axis=lon,y_axis=lat,window= , , , ,tone=1,contour=1,min=,max=,nlev=,levels=,interval=,color_bar=1,log=0,coloring=0,tonc=0"],
                    [:slice,[0]]
                   ]


    @path_bracketpath = "/[/samples/jmadata/MSM-P/2006/010[1-3].nc/temp,/samples/jmadata/MSM-P/2006/010[1-3].nc/temp]/cut(lon=>120..147.5,lat=>47.5999984741211..25.6000003814697,p=>975,time=>0)/plot(vector;projection=1,map_fit=1,map_axis=,map_radius=,map_window=,size=400,400,viewport=0.2,0.8,0.2,0.8,colormap=1,x_axis=lon,y_axis=lat,window= , , , ,xintv=1,yintv=1,unit_vect=0)[0]"
    @result_bracketpath = [
                           ["/samples/jmadata/MSM-P/2006/010[1-3].nc/temp",
                            "/samples/jmadata/MSM-P/2006/010[1-3].nc/temp"
                           ],
                           [:cut,"lon=>120..147.5,lat=>47.5999984741211..25.6000003814697,p=>975,time=>0"],
                           [:plot,"vector;projection=1,map_fit=1,map_axis=,map_radius=,map_window=,size=400,400,viewport=0.2,0.8,0.2,0.8,colormap=1,x_axis=lon,y_axis=lat,window= , , , ,xintv=1,yintv=1,unit_vect=0"],
                           [:slice,[0]]
                          ]
                             
    @path_overlay = "/[/[/[/samples/reanalysis/ncep/UV.jan.nc/U/cut(lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000..10),/samples/reanalysis/ncep/UV.jan.nc/U/cut(lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000..10)/analysis(mean;lon)[0]/cut(lat=>81.25..-77.5,level=>1000..10)]/analysis(subtraction)[0]/cut(lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000..10),/samples/reanalysis/ncep/UV.jan.nc/V/cut(lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000..10)]/analysis(multiplication)[0]/cut(lon=>108.75..148.75,lat=>41.25..21.25,level=>1000)/plot(tone_contour;projection=1,map_fit=1,map_axis=0,0,0,map_radius=,map_window=0,0,0,0,size=400,400,viewport=0.2,0.8,0.2,0.8,colormap=1,x_axis=lon,y_axis=lat,window= , , , ,tone=1,contour=1,min=,max=,nlev=,levels=,interval=,color_bar=1,log=0,coloring=0,tonc=0)[0],/[/samples/jmadata/MSM-P/2006/010[1-3].nc/temp,/samples/jmadata/MSM-P/2006/010[1-3].nc/temp]/cut(lon=>120..147.5,lat=>47.5999984741211..25.6000003814697,p=>975,time=>0)/plot(vector;projection=1,map_fit=1,map_axis=0,0,0,map_radius=,map_window=0,0,0,0,size=400,400,viewport=0.2,0.8,0.2,0.8,colormap=1,x_axis=lon,y_axis=lat,window=,xintv=1,yintv=1,unit_vect=0)[0]]/overlay()"
    @result_overlay = [
                       [
                        [
                         [
                          [
                           [
                            ["/samples/reanalysis/ncep/UV.jan.nc/U",
                             [:cut,"lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000..10"]
                            ],
                            ["/samples/reanalysis/ncep/UV.jan.nc/U",
                             [:cut,"lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000..10"],
                             [:analysis,"mean;lon"],
                             [:slice,[0]],
                             [:cut,"lat=>81.25..-77.5,level=>1000..10"]
                            ]
                           ],
                           [:analysis,"subtraction"],
                           [:slice,[0]],
                           [:cut,"lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000..10"]
                          ],
                          ["/samples/reanalysis/ncep/UV.jan.nc/V",
                           [:cut,"lon=>8.75..348.75,lat=>81.25..-77.5,level=>1000..10"]
                          ]
                         ],
                         [:analysis,"multiplication"],
                         [:slice,[0]],
                         [:cut,"lon=>108.75..148.75,lat=>41.25..21.25,level=>1000"],
                         [:plot,"tone_contour;projection=1,map_fit=1,map_axis=0,0,0,map_radius=,map_window=0,0,0,0,size=400,400,viewport=0.2,0.8,0.2,0.8,colormap=1,x_axis=lon,y_axis=lat,window= , , , ,tone=1,contour=1,min=,max=,nlev=,levels=,interval=,color_bar=1,log=0,coloring=0,tonc=0"],
                         [:slice,[0]]
                        ],
                        [
                         ["/samples/jmadata/MSM-P/2006/010[1-3].nc/temp",
                          "/samples/jmadata/MSM-P/2006/010[1-3].nc/temp"
                         ],
                         [:cut,"lon=>120..147.5,lat=>47.5999984741211..25.6000003814697,p=>975,time=>0"],
                         [:plot,"vector;projection=1,map_fit=1,map_axis=0,0,0,map_radius=,map_window=0,0,0,0,size=400,400,viewport=0.2,0.8,0.2,0.8,colormap=1,x_axis=lon,y_axis=lat,window=,xintv=1,yintv=1,unit_vect=0"],
                         [:slice,[0]]
                        ]
                       ],
                       [:overlay,nil]
                      ]
    @path_find = "/samples/find(all)"
    @result_find = ["/samples", [:find, "all"]]

    @parser = PathParser.new
  end

  def test_parse_bad_slash_slash
    assert_raise(RuntimeError) { @parser.parse(@path_bad_slash_slash) }
  end

  def test_parse_bad_comma
    assert_raise(Racc::ParseError) { @parser.parse(@path_bad_comma) }
  end

  def test_parse_bad_method
    assert_raise(Racc::ParseError) { @parser.parse(@path_bad_method) }
  end


  def test_parse_root
    assert_equal @result_root, @parser.parse(@path_root)
  end

  def test_parse_single
    assert_equal @result_single, @parser.parse(@path_single)
  end

  def test_parse_multidir
    assert_equal @result_multidir, @parser.parse(@path_multidir)
  end

  def test_parse_strange
    assert_equal @result_strange, @parser.parse(@path_strange)
  end

  def test_parse_array
    assert_equal @result_array, @parser.parse(@path_array)
  end

  def test_parse_array_slice
    assert_equal @result_array_slice, @parser.parse(@path_array_slice)
  end

  def test_parse_array_nested
    assert_equal @result_array_nested, @parser.parse(@path_array_nested)
  end

  def test_parse_simple
    assert_equal @result_simple, @parser.parse(@path_simple)
  end
  def test_parse_tree
    assert_equal @result_tree, @parser.parse(@path_tree)
  end
  def test_parse_bracketpath
    assert_equal @result_bracketpath, @parser.parse(@path_bracketpath)
  end
  def test_parse_overlay
    assert_equal @result_overlay, @parser.parse(@path_overlay)
  end

  def test_parse_find
    assert_equal @result_find, @parser.parse(@path_find)
  end
end
