require File.expand_path(File.dirname(__FILE__) + '/../spec_helper')
require "numru/gfdnavi_data"
require "numru/gphys"

describe VirtualData, "in creation" do
  fixtures :nodes, :variables

  it "should be created from Node successfully" do
    path = "/samples/reanalysis/ncep/T.jan.nc/T"
    node = Node.find(:first, :conditions =>  ["path=?", path])
    vd = VirtualData.new(node)
    vd.should be_instance_of(VirtualData)
  end

  it "should be created from Variable successfully" do
    path = "/samples/reanalysis/ncep/T.jan.nc/T"
    var = Variable.find(:first, :conditions =>  ["path=?", path])
    vd = VirtualData.new(var)
    vd.should be_instance_of(VirtualData)
  end

  it "should be created from NumRu::GfdnaviData::Local successfully" do
    path = "/samples/reanalysis/ncep/T.jan.nc/T"
    ld = NumRu::GfdnaviData::Local.parse_path(path)
    vd = VirtualData.new(ld)
    vd.should be_instance_of(VirtualData)
  end

end

describe VirtualData, "in analysis and plot" do
  fixtures :nodes, :variables
  fixtures :functions, :function_arguments, :function_outputs
  fixtures :draw_methods, :draw_method_options, :value_types

  before do
    path = "/samples/reanalysis/ncep/T.jan.nc/T"
    node = Node.find(:first, :conditions =>  ["path=?", path])
    @vd1 = VirtualData.new(node)
    @var = Variable.find(:first, :conditions =>  ["path=?", path])
    @vd2 = VirtualData.new(@var)
    ld = NumRu::GfdnaviData::Local.parse_path(path)
    @vd3 = VirtualData.new(ld)
    @func = Function.find(:first, :conditions => ["name=?","mean"])
    @args = ["lon"]
    @dm = DrawMethod.find(:first, :conditions => ["name=?","tone"])
  end

  it "should be done successfully with #analysis" do
    v1 = @vd1.analysis(@func,*@args)[0]
    v2 = @vd2.analysis(@func,*@args)[0]
    v3 = @vd3.analysis(@func,*@args)[0]
    gphys = NumRu::GPhys::IO.open(@var.fname, @var.vname).mean("lon")
    nary = gphys.val
    (v1.to_gphys.val - nary).abs.min.should == 0.0
    (v2.to_gphys.val - nary).abs.min.should == 0.0
    (v3.to_gphys.val - nary).abs.min.should == 0.0
  end

  it "should be done successfully with #plot" do
    d1 = @vd1.plot(@dm)[0]
    d2 = @vd2.plot(@dm)[0]
    d3 = @vd3.plot(@dm)[0]
    d1.to_png.should match(/\A.PNG/)
    d2.to_png.should match(/\A.PNG/)
    d3.to_png.should match(/\A.PNG/)
  end

  it "should be done successfully with #analysis and #plot" do
    d1 = @vd1.analysis(@func,*@args).plot(@dm)[0]
    d2 = @vd2.analysis(@func,*@args).plot(@dm)[0]
    d3 = @vd3.analysis(@func,*@args).plot(@dm)[0]
    d1.to_png.should match(/\A.PNG/)
    d2.to_png.should match(/\A.PNG/)
    d3.to_png.should match(/\A.PNG/)
  end

  it "should be done successfully with #dimensions" do
    @vd1.dimensions.length.should == 3
    @vd2.dimensions.length.should == 3
    @vd3.dimensions.length.should == 3
    d1 = @vd1.analysis(@func,*@args)
    d1.dimensions.length.should == 2
    d2 = @vd2.analysis(@func,*@args)
    d2.dimensions.length.should == 2
    d3 = @vd3.analysis(@func,*@args)
    d3.dimensions.length.should == 2
  end

end

describe VirtualData do
  fixtures :nodes, :variables
  fixtures :functions, :function_arguments, :function_outputs
  fixtures :draw_methods, :draw_method_options, :value_types

  before do
    path = "/samples/reanalysis/ncep/T.jan.nc/T"
    node = Node.find(:first, :conditions =>  ["path=?", path])
    @vd = VirtualData.new(node)
    @func = Function.find(:first, :conditions => ["name=?","mean"])
    @args = ["lon"]
    @dm = DrawMethod.find(:first, :conditions => ["name=?","tone"])
  end

  it "should give ruby code with #to_rb" do
    an1 = @vd.analysis(@func,*@args)[0]
    rb = an1.to_rb
    an2 = eval(rb, TOPLEVEL_BINDING) # Note: "include NumRu" in the script "rb" may have side effects
    (an2.to_gphys.val - an1.to_gphys.val).abs.max.should == 0.0

    pl1 = @vd.plot(@dm)[0]
    rb = pl1.to_rb
    pl2 = eval(rb, TOPLEVEL_BINDING) # Note: "include NumRu" in the script "rb" may have side effects
    pl2.to_png.should == pl1.to_png
  end
end


describe VirtualData, "in saving" do
  fixtures :nodes, :variables, :images
  fixtures :functions, :function_arguments, :function_outputs
  fixtures :draw_methods, :draw_method_options, :value_types
  fixtures :users

  before do
    path = "/samples/reanalysis/ncep/T.jan.nc/T"
    node = Node.find(:first, :conditions =>  ["path=?", path])
    vd1 = VirtualData.new(node)
    func = Function.find(:first, :conditions => ["name=?","mean"])
    args = ["lon"]
    dm = DrawMethod.find(:first, :conditions => ["name=?","tone"])
    @v1 = vd1.analysis(func,*args)[0]
    @d1 = vd1.plot(dm)[0]
    @user = User.find(:first, :conditions => ["login=?", "root"])
    @path_nc = "__test_t_xm.nc"
    @path_png = "__test_t.png"
  end

  after do
    [@path_nc, @path_png].each do |path|
      fname = File.join(GFDNAVI_USER_PATH, @user.login, path)
      FileUtils.rm_f(fname)
    end
  end

  it "should be done successfully with #save for analyized data" do
    @v1.save(@path_nc, @user)
    path = File.join("/usr", @user.login, @path_nc)
    node = Node.find(:first, :conditions => ["path=?", path])
    node.should be_instance_of(Node)
    node.path.should == path
    vs = node.variables
    vs.length.should == 1
  end

  it "should be done successfully with #save for diagram" do
    path = "t.png"
    @d1.save(@path_png, @user)
    path = File.join("/usr", @user.login, @path_png)
    v = Image.find(:first, :conditions => ["path=?", path])
    v.should be_instance_of(Image)
    v.path.should == path
  end
end
