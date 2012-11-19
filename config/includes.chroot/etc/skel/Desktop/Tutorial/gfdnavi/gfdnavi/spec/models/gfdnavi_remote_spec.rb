# -*- coding: cp932 -*-
require File.expand_path(File.dirname(__FILE__) + '/../spec_helper')
require "numru/gfdnavi_data"
require 'gphys_gfdnavi'

prefix = "http://localhost:3000/data"

describe "NumRu::GfdnaviData::Remote.parse_url" do
  fixtures :nodes, :variables, :images
  fixtures :functions, :function_arguments, :function_outputs
  fixtures :draw_methods, :draw_method_options, :value_types
  fixtures :users
  fixtures :diagram_caches
  it "should return NumRu::GfdnaviData::RemoteDirectory" do
    path = "/samples"
    gl = NumRu::GfdnaviData::Remote.parse_url(prefix+path)
    gl.should be_instance_of(NumRu::GfdnaviData::DirectoryRemote)
  end

  it "should return NumRu::GfdnaviData::RemoteDirectory" do
    #path = "/samples/reanalysis/ncep/T.jan.nc"
    path = "/samples/reanalysis"
    gl = NumRu::GfdnaviData::Remote.parse_url(prefix+path)
    gl.should be_instance_of(NumRu::GfdnaviData::DirectoryRemote)
  end

  it "should return NumRu::GfdnaviData::RemoteVariable" do
    path = "/samples/reanalysis/ncep/T.jan.nc/T"
    gl = NumRu::GfdnaviData::Remote.parse_url(prefix+path)
    gl.should be_instance_of(NumRu::GfdnaviData::VariableRemote)
  end
end

describe "NumRu::GfdnaviData::Base#children", "when it correspond to directory node" do
  fixtures :nodes, :variables, :images
  fixtures :functions, :function_arguments, :function_outputs
  fixtures :draw_methods, :draw_method_options, :value_types
  fixtures :users
  fixtures :diagram_caches
  it "should return its children nodes" do
    path = "/samples/reanalysis/ncep"
    gl = NumRu::GfdnaviData::Remote.parse_url(prefix+path)
    child = gl.children
    child.should be_instance_of(NumRu::GfdnaviData::ArrayRemote)
    gl.should have(4).children
    node = child[0]
    node.url.should == prefix+path+"/T.jan.100hPa.png"
    node.should be_instance_of(NumRu::GfdnaviData::ImageRemote)
  end

  it "should return its children depending on user" do
    path = "/samples/reanalysis/ncep"
    gl = NumRu::GfdnaviData::Remote.parse_url(prefix+path,"root")
    gl.should have(5).children
  end
end

describe "NumRu::GfdnaviData::RemoteData#variables", "when it correspond to directory node" do
  fixtures :nodes, :variables, :images
  fixtures :functions, :function_arguments, :function_outputs
  fixtures :draw_methods, :draw_method_options, :value_types
  fixtures :users
  fixtures :diagram_caches
  it "should return variable nodes which are its children" do
    path = "/samples/reanalysis/ncep/T.jan.nc"
    gl = NumRu::GfdnaviData::Remote.parse_url(prefix+path)
    vars = gl.variables
    vars.should be_instance_of(NumRu::GfdnaviData::ArrayRemote)
    gl.should have(1).variables
    var = vars[0]
    var.should be_instance_of(NumRu::GfdnaviData::VariableRemote)
    var.path.should == "/samples/reanalysis/ncep/T.jan.nc/T"
  end
end

describe "NumRu::GfdnaviData::RemoteData#parent" do
  fixtures :nodes, :variables, :images
  fixtures :functions, :function_arguments, :function_outputs
  fixtures :draw_methods, :draw_method_options, :value_types
  fixtures :users
  fixtures :diagram_caches
  it "should return its parent node" do
    path = "/samples/reanalysis"
    gl = NumRu::GfdnaviData::Remote.parse_url(prefix+path)
    parent = gl.parent
    parent.should be_instance_of(NumRu::GfdnaviData::DirectoryRemote)
    parent.path.should == "/samples"
  end
end

describe "NumRu::GfdnaviData::RemoteData#analysis" do
  fixtures :nodes, :variables, :images
  fixtures :functions, :function_arguments, :function_outputs
  fixtures :draw_methods, :draw_method_options, :value_types
  fixtures :users
  fixtures :diagram_caches

  before do
    @path = "/samples/reanalysis/ncep/T.jan.nc/T"
  end

  it "should return NumRu::GfdnaviData::ArrayRemote" do
    gl = NumRu::GfdnaviData::Remote.parse_url(prefix+@path)
    ga = gl.analysis("mean", "lon")
    ga.should be_instance_of(NumRu::GfdnaviData::ArrayRemote)
    ga.length.should == 1
    gd = ga[0]
    gd.should be_instance_of(NumRu::GfdnaviData::VariableRemote)
    gd.path.should == @path+"/analysis(mean;lon)[0]"
  end
  
  it "should give NumRu::GfdnaviData::RemoteData which gives NumRu::GPhys with #to_gphys" do
    gl = NumRu::GfdnaviData::Remote.parse_url(prefix+@path)
    gd = gl.analysis("mean", "lon")[0]
    gphys = gd.to_gphys
    gphys.should be_instance_of(NumRu::GPhys)
    gphys.rank.should == 2
    gl.to_gphys.rank.should == 3
    gd2 = gd.cut("lat"=>0..90)
    gd2.should be_instance_of(NumRu::GfdnaviData::VariableRemote)
    gd2.to_gphys.coord("lat").val.should == gphys.cut("lat"=>0..90).coord("lat").val
  end

end

describe "NumRu::GfdnaviData::RemoteData#plot" do
  fixtures :nodes, :variables, :images
  fixtures :functions, :function_arguments, :function_outputs
  fixtures :draw_methods, :draw_method_options, :value_types
  fixtures :users
  fixtures :diagram_caches

  it "should plot the data" do
    path = "/samples/reanalysis/ncep/T.jan.nc/T"
    gl = NumRu::GfdnaviData::Remote.parse_url(prefix+path)
    images = gl.plot("tone")
    images.should be_instance_of(NumRu::GfdnaviData::ArrayRemote)
    images.length.should == 1
    image = images[0]
    image.should be_instance_of(NumRu::GfdnaviData::ImageRemote)
    # the following test may fail because URL with plot will be modified by VirtualData to make up missing options
    image.path.should == path + "/plot(tone;color_bar=1,coloring=0,contour=1,log=0,tonc=0,tone=1)[0]"
    png = image.to_png
    png.should be_instance_of(String)
    png.should match(/\A.PNG/)
  end
end

describe "NumRu::GfdnaviData::RemoteData#plot", "when called for a result of NumRu::GfdnaviData::RemoteData#analysis" do
  fixtures :nodes, :variables, :images
  fixtures :functions, :function_arguments, :function_outputs
  fixtures :draw_methods, :draw_method_options, :value_types
  fixtures :users
  fixtures :diagram_caches

  it "should draw the data which is analyized" do
    path = "/samples/reanalysis/ncep/T.jan.nc/T"
    gl = NumRu::GfdnaviData::Remote.parse_url(prefix+path)
    ga = gl.analysis("mean","lon")

    images = ga.plot("tone")
    # the following test may fail because URL with plot will be modified by VirtualData to make up missing options
    images.path.should == "/samples/reanalysis/ncep/T.jan.nc/T/analysis(mean;lon)/plot(tone;color_bar=1,coloring=0,contour=1,log=0,tonc=0,tone=1)"

    images = ga[0].plot("tone")
    # the following test may fail because URL with plot will be modified by VirtualData to make up missing options
    images[0].path.should == "/samples/reanalysis/ncep/T.jan.nc/T/analysis(mean;lon)[0]/plot(tone;color_bar=1,coloring=0,contour=1,log=0,tonc=0,tone=1)[0]"
  end
end

describe "GfdnaviArray.[]", "when elements are NumRu::GfdnaviData::RemoteData" do
  fixtures :nodes, :variables, :images
  fixtures :functions, :function_arguments, :function_outputs
  fixtures :draw_methods, :draw_method_options, :value_types
  fixtures :users
  fixtures :diagram_caches

  it "should generate NumRu::GfdnaviData::ArrayRemote" do
    path = "/samples/reanalysis/ncep/UV.jan.nc/U"
    u = NumRu::GfdnaviData::Remote.parse_url(prefix+path)
    path = "/samples/reanalysis/ncep/UV.jan.nc/V"
    v = NumRu::GfdnaviData::Remote.parse_url(prefix+path)
    ary = NumRu::GfdnaviData::Array[u, v]
    uv = ary.analysis("addition,bob")[0]
    guv = u.to_gphys + v.to_gphys
    (uv.to_gphys.val - guv.val).abs.max.should == 0.0
    fig = ary.plot("vector")
    fig.should be_instance_of(NumRu::GfdnaviData::ArrayRemote)
    png = fig.to_png
    png.should match(/\A.PNG/)
  end
end

describe "NumRu::GfdnaviData::RemoteData.parse_url", "when url =~ /\\/[path1,path2]/" do
  fixtures :nodes, :variables, :images
  fixtures :functions, :function_arguments, :function_outputs
  fixtures :draw_methods, :draw_method_options, :value_types
  fixtures :users
  fixtures :diagram_caches

  it "should return NumRu::GfdnaviData::ArrayRemote and the same as that created with NumRu::GfdnaviData[]" do
    path = "/samples/reanalysis/ncep/UV.jan.nc/U"
    u = NumRu::GfdnaviData::Remote.parse_url(prefix+path)
    path = "/samples/reanalysis/ncep/UV.jan.nc/V"
    v = NumRu::GfdnaviData::Remote.parse_url(prefix+path)
    ary1 = NumRu::GfdnaviData::Array[u, v]
    path = "/[/samples/reanalysis/ncep/UV.jan.nc/U,/samples/reanalysis/ncep/UV.jan.nc/V]"
    ary2 = NumRu::GfdnaviData::Remote.parse_url(prefix+path)
    (ary2[0].to_gphys - ary1[0].to_gphys).val.abs.max.should == 0
    (ary2[1].to_gphys - ary1[1].to_gphys).val.abs.max.should == 0
  end
end

describe NumRu::GfdnaviData::ArrayRemote, "when its length is one" do
  fixtures :nodes, :variables, :images
  fixtures :functions, :function_arguments, :function_outputs
  fixtures :draw_methods, :draw_method_options, :value_types
  fixtures :users
  fixtures :diagram_caches

  it "should forward #to_(gphys|png) to self[0]" do
    path = "/samples/reanalysis/ncep/T.jan.nc/T"
    gl = NumRu::GfdnaviData::Remote.parse_url(prefix+path)
    ga = gl.analysis("mean","lon")
    gphys = ga.to_gphys
    gphys.should be_instance_of(NumRu::GPhys)
    gp = ga.plot("tone")
    png = gp.to_png
    png.should match(/\A.PNG/)
  end
end

describe NumRu::GfdnaviData::Remote do
  fixtures :nodes, :variables, :images
  fixtures :functions, :function_arguments, :function_outputs
  fixtures :draw_methods, :draw_method_options, :value_types
  fixtures :users
  fixtures :diagram_caches

  it "should give Ruby script with #to_rb" do
    path = "/samples/reanalysis/ncep/T.jan.nc/T"

    gd = NumRu::GfdnaviData::Remote.parse_url(prefix+path)
    rb = gd.to_rb
    gd2 = eval(rb, TOPLEVEL_BINDING) # Note: "include NumRu" in the script "rb" may have side effects
    gd2.object_id.should_not == gd.object_id
    gd2.url.should == gd.url

    gd = NumRu::GfdnaviData::Remote.parse_url(prefix+path).analysis("mean","lon")
    rb = gd.to_rb
    gd2 = eval(rb, TOPLEVEL_BINDING) # Note: "include NumRu" in the script "rb" may have side effects
    gd2.object_id.should_not == gd.object_id
    gd2.url.should == gd.url

    gd = NumRu::GfdnaviData::Remote.parse_url(prefix+path).analysis("mean","lon").plot("tone")
    rb = gd.to_rb
    gd2 = eval(rb, TOPLEVEL_BINDING) # Note: "include NumRu" in the script "rb" may have side effects
    gd2.object_id.should_not == gd.object_id
#    gd2.url.should == gd.url # URL with plot will be modified by VirtualData to make up missing options

    path = "/samples/reanalysis/ncep/UV.jan.nc/"

    u = NumRu::GfdnaviData::Remote.parse_url(prefix+path+"U").analysis("mean","lon")
    v = NumRu::GfdnaviData::Remote.parse_url(prefix+path+"V").analysis("mean","lon")
    gd = NumRu::GfdnaviData::Array[u,v].plot("vector")
    rb = gd.to_rb
    gd2 = eval(rb, TOPLEVEL_BINDING) # Note: "include NumRu" in the script "rb" may have side effects
    gd2.object_id.should_not == gd.object_id
    gd2.url.should == gd.url
  end
end

describe NumRu::GfdnaviData::Remote, "#save_as" do
  fixtures :nodes, :variables, :images
  fixtures :functions, :function_arguments, :function_outputs
  fixtures :draw_methods, :draw_method_options, :value_types
  fixtures :users
  fixtures :diagram_caches

  it "NumRu::GfdnaviData::VariableRemote#save_as" do
    t = NumRu::GfdnaviData::Remote.parse_url(prefix+"/samples/reanalysis/ncep/T.jan.nc/T")
    t = t.analysis("mean", "lon")[0]
    fname = "savetest.nc"
    t.save_as(fname, "root")
    t2 = NumRu::GfdnaviData::Remote.parse_url(prefix+"/usr/root/"+fname, "root")
    begin
      t2.should be_instance_of(NumRu::GfdnaviData::DirectoryRemote)
    ensure
      t2.delete
    end
  end

  it "NumRu::GfdnaviData::ImageRemote#save_as" do
    t = NumRu::GfdnaviData::Remote.parse_url(prefix+"/samples/reanalysis/ncep/T.jan.nc/T")
    t = t.plot("tone")[0]
    fname = "savetest.png"
    t.save_as(fname, "root")
    t2 = NumRu::GfdnaviData::Remote.parse_url(prefix+"/usr/root/"+fname, "root")
    begin
      t2.should be_instance_of(NumRu::GfdnaviData::ImageRemote)
    ensure
      t2.delete
    end
  end
end

describe NumRu::GfdnaviData::Remote, "#save" do
  fixtures :nodes, :variables, :images
  fixtures :functions, :function_arguments, :function_outputs
  fixtures :draw_methods, :draw_method_options, :value_types
  fixtures :users
  fixtures :diagram_caches

  it "NumRu::GfdnaviData::VariableRemote#saves" do
    t = NumRu::GfdnaviData::Remote.parse_url(prefix+"/samples/reanalysis/ncep/T.jan.nc/T")
    gphys = t.analysis("mean", "lon")[0].to_gphys
    user = "root"
    fname = "savetest.nc"
    vname = gphys.name
    url = File.join(prefix, "usr", user, fname)
    gd = NumRu::GfdnaviData::VariableRemote.new
    gd.user = user
    gd.url = File.join(url, vname)
    gd.gphys = gphys
    title = "test"
    gd.title = title
    gd.save
    gd2 = NumRu::GfdnaviData::Remote.parse_url(url, user)
    begin
      gd2.should be_instance_of(NumRu::GfdnaviData::DirectoryRemote)
      gd3 = gd2.variable_nodes[0]
      gd3.should be_instance_of(NumRu::GfdnaviData::VariableRemote)
      gd3.title.should == title
    ensure
      gd2.delete
    end
  end

  it "NumRu::GfdnaviData::ImageRemote#saves" do
    t = NumRu::GfdnaviData::Remote.parse_url(prefix+"/samples/reanalysis/ncep/T.jan.nc/T")
    png = t.plot("tone")[0].to_png
    user = "root"
    fname = "test.png"
    url = File.join(prefix, "usr", user, fname)
    gd = NumRu::GfdnaviData::ImageRemote.new
    gd.user = user
    gd.url = url
    gd.png = png
    gd.save
    gd2 = NumRu::GfdnaviData::Remote.parse_url(url, user)
    begin
      gd2.should be_instance_of(NumRu::GfdnaviData::ImageRemote)
    ensure
      gd2.delete
    end
  end
end


=begin
# for knowledge test
# path に関して、相対パスを期待するものと、絶対パスを期待するものの2つのテストを書くこと
# * 絶対パス用のテスト
describe NumRu::GfdnaviData::RemoteData, "in createing" do
  fixtures :nodes, :variables, :images # using table
  fixtures :knowledges, :knowledge_figures
  fixtures :users

  # before test
  before do
    @path = prefix + "/usr/root/knowledge/creating_test/test.knlge"
    @user = "root"
    @hash = {"title"=>"Title_test.", "creator"=>"a Gfdnavi developer."}
  end

  # after test
  # (delete created knowledge document.)
  # DB is deleted automatically.
  after do
  end

  # Creating Knowledge
  it "should be done successfully with #save document" do
    new_knowledge = NumRu::GfdnaviData.new(@path, @user, "knlge", @hash)
    new_knowledge.name = File.basename(@path)
    new_knowledge.owner = @user
    new_knowledge.mtime = Time.new
    new_knowledge.other_mode = 4
    new_knowledge.other_readable = 1 # true
    new_knowledge.groups_readable = -1
    new_knowledge.title = "Title_test."
    new_knowledge.textbody = "koreha textbody no!\ntest da yo!\nhogehoge!!!!!!!\n"
    new_knowledge.category = "test"
    new_knowledge.creator = "Gfdnavi Developer."
    new_knowledge.description = "description"
    new_knowledge.default_layout = 0
    new_knowledge.horizontal_figures = 0
    new_knowledge.figures_size_height_or_width = 0
    new_knowledge.figures_size_units = 0
    new_knowledge.figures_size_number = 80
# debugger
    new_knowledge.save
    # searching from knowledge table
    k = NumRu::GfdnaviData.parse(@path, @user)
debugger
    k.should be_instance_of(NumRu::GfdnaviData)
    k.url.should == @path  # k.path.should == @path ？
  end

end

=end
