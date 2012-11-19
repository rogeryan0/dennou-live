# -*- coding: cp932 -*-
require File.expand_path(File.dirname(__FILE__) + '/../spec_helper')
require 'gphys_gfdnavi'
require "numru/gfdnavi_data"


describe NumRu::GfdnaviData::Local, "#parse(path)" do
  fixtures :nodes

  it "should return NumRu::GfdnaviData::DirectoryLocal" do
    path = "/samples"
    gl = NumRu::GfdnaviData::Local.parse_path(path)
    gl.should be_instance_of(NumRu::GfdnaviData::DirectoryLocal)
  end

  it "should return NumRu::GfdnaviData::VariableLocal" do
    path = "/samples/reanalysis/ncep/T.jan.nc/T"
    gl = NumRu::GfdnaviData::Local.parse_path(path)
    gl.should be_instance_of(NumRu::GfdnaviData::VariableLocal)
  end
end

describe NumRu::GfdnaviData::Local do
  fixtures :nodes, :directories, :variables, :images, :users

  it "should return NumRu::GfdnaviData::ArrayLocal object by #children" do
    path = "/samples/reanalysis"
    gl = NumRu::GfdnaviData::Local.parse_path(path)
    gl.should have(1).children
    child = gl.children
    child.should be_instance_of(NumRu::GfdnaviData::ArrayLocal)
    node = child[0]
    node.should be_instance_of(NumRu::GfdnaviData::DirectoryLocal)
    node.path.should == "/samples/reanalysis/ncep"

    gl = NumRu::GfdnaviData::Local.parse_path(path,"root")
    gl.should have(2).children
  end

  it "should return NumRu::GfdnaviData::ArrayLocal object by #variables" do
    path = "/samples/reanalysis/ncep/T.jan.nc"
    gl = NumRu::GfdnaviData::Local.parse_path(path)
    gl.should have(1).variables
    vars = gl.variables
    vars.should be_instance_of(NumRu::GfdnaviData::ArrayLocal)
    var = vars[0]
    var.should be_instance_of(NumRu::GfdnaviData::VariableLocal)
    var.path.should == "/samples/reanalysis/ncep/T.jan.nc/T"
  end

  it "should return NumRu::GfdnaviData::ArrayLocal object by #images" do
    path = "/usr/bob"
    gl = NumRu::GfdnaviData::Local.parse_path(path)
    gl.should have(1).images
    img = gl.images[0]
    img.should be_instance_of(NumRu::GfdnaviData::ImageLocal)
    img.path.should == "/usr/bob/image.png"
  end

  it "should return NumRu::GfdnaviData::DirectoryLocal object by #parent" do
    path = "/samples/reanalysis"
    gl = NumRu::GfdnaviData::Local.parse_path(path)
    parent = gl.parent
    parent.should be_instance_of(NumRu::GfdnaviData::DirectoryLocal)
    parent.path.should == "/samples"
  end

end

describe NumRu::GfdnaviData::Local, "when #analysis method is called" do
  fixtures :nodes, :variables
  fixtures :functions, :function_arguments, :function_outputs

  it "should return NumRu::GfdnaviData::ArrayLocal" do
    path = "/samples/reanalysis/ncep/T.jan.nc/T"
    gl = NumRu::GfdnaviData::Local.parse_path(path)
    ga = gl.analysis("mean,bob", "lon")
    ga.should be_instance_of(NumRu::GfdnaviData::ArrayLocal)
    ga.length.should == 1
    ga[0..0].should  be_instance_of(NumRu::GfdnaviData::ArrayLocal)
    gd = ga[0]
    gd.should be_instance_of(NumRu::GfdnaviData::VariableLocal)
    gd.path.should == path+"/analysis(mean,bob;lon)[0]"
    gphys = gd.to_gphys
    gphys.should be_instance_of(NumRu::GPhys)
    gphys.rank.should == 2
    gphys2 = gl.to_gphys
    gphys2.should be_instance_of(NumRu::GPhys)
    gphys2.rank.should == 3
    gd2 = gd.cut("lat"=>0..90)
    gd2.should be_instance_of(NumRu::GfdnaviData::VariableLocal)
    gd2.to_gphys.coord("lat").val.should == gphys.cut("lat"=>0..90).coord("lat").val

    ga3 = gl.analysis("mean", "lon")
    ga3.should be_instance_of(NumRu::GfdnaviData::ArrayLocal)

    gl2 = NumRu::GfdnaviData::Local.parse_path(ga.path)
    gl2.should be_instance_of(NumRu::GfdnaviData::ArrayLocal)

    gl3 = NumRu::GfdnaviData::Local.parse_path(gd.path)
    gl3.should be_instance_of(NumRu::GfdnaviData::VariableLocal)
  end

end

describe NumRu::GfdnaviData::Local, "when #plot method is called" do
  fixtures :nodes, :variables
  fixtures :draw_methods, :draw_method_options, :value_types

  it "should return NumRu::GfdnaviData::ArrayLocal" do
    path = "/samples/reanalysis/ncep/T.jan.nc/T"
    gl = NumRu::GfdnaviData::Local.parse_path(path)
    images = gl.plot("tone,bob")
    images.should be_instance_of(NumRu::GfdnaviData::ArrayLocal)
    images.length.should == 1
    image = images[0]
    image.should be_instance_of(NumRu::GfdnaviData::ImageLocal)
    image.path.should == path+"/plot(tone,bob;contour=1,tone=1)[0]"
    png = image.to_png
    png.should be_instance_of(String)
    png.should match(/\A.PNG/)
  end
end

describe NumRu::GfdnaviData::ArrayLocal, "when #analysis and #plot method is called" do
  fixtures :nodes, :variables
  fixtures :functions, :function_arguments, :function_outputs
  fixtures :draw_methods, :draw_method_options, :value_types

  it "should return NumRu::GfdnaviData::ArrayLocal" do
    path = "/samples/reanalysis/ncep/T.jan.nc/T"
    gl = NumRu::GfdnaviData::Local.parse_path(path)
    ga = gl.analysis("mean,bob","lon")

    images = ga.plot("tone,bob")
    images.path.should == "/samples/reanalysis/ncep/T.jan.nc/T/analysis(mean,bob;lon)/plot(tone,bob;contour=1,tone=1)"
    NumRu::GfdnaviData::Local.parse_path(images.path).should be_instance_of(NumRu::GfdnaviData::ArrayLocal)

    images = ga[0].plot("tone,bob")
    images[0].path.should == "/samples/reanalysis/ncep/T.jan.nc/T/analysis(mean,bob;lon)[0]/plot(tone,bob;contour=1,tone=1)[0]"
    NumRu::GfdnaviData::Local.parse_path(images[0].path).should be_instance_of(NumRu::GfdnaviData::ImageLocal)
  end

end

describe NumRu::GfdnaviData::ArrayLocal, "when create with NumRu::GfdnaviData::Array.[]" do
  fixtures :nodes, :variables
  fixtures :functions, :function_arguments, :function_outputs
  fixtures :draw_methods, :draw_method_options, :value_types

  it "should return NumRu::GfdnaviData::Array" do
    u = NumRu::GfdnaviData::Local.parse_path("/samples/reanalysis/ncep/UV.jan.nc/U")
    v = NumRu::GfdnaviData::Local.parse_path("/samples/reanalysis/ncep/UV.jan.nc/V")
    ary = NumRu::GfdnaviData::Array[u, v]
    uv = ary.analysis("addition,bob")[0]
    guv = u.to_gphys + v.to_gphys
    (uv.to_gphys.val - guv.val).abs.max.should == 0.0
  end
end

describe NumRu::GfdnaviData::ArrayLocal, "when create path like [path1,path2]" do
  fixtures :nodes, :variables, :draw_methods, :draw_method_options, :value_types

  it "should return NumRu::GfdnaviData::Array and the same as that created with NumRu::GfdnaviData[]" do
    u = NumRu::GfdnaviData::Local.parse_path("/samples/reanalysis/ncep/UV.jan.nc/U")
    v = NumRu::GfdnaviData::Local.parse_path("/samples/reanalysis/ncep/UV.jan.nc/V")
    ary1 = NumRu::GfdnaviData::Array[u, v]
    ary2 = NumRu::GfdnaviData::Local.parse_path("/[/samples/reanalysis/ncep/UV.jan.nc/U,/samples/reanalysis/ncep/UV.jan.nc/V]")
  
    (ary2[0].to_gphys - ary1[0].to_gphys).val.abs.max.should == 0
    (ary2[1].to_gphys - ary1[1].to_gphys).val.abs.max.should == 0

    fig = ary1.plot("vector")
    fig.should be_instance_of(NumRu::GfdnaviData::ArrayLocal)
    png = fig.to_png
    png.should match(/\A.PNG/)
  end
end

describe NumRu::GfdnaviData::ArrayLocal, "when length is one" do
  fixtures :nodes, :variables
  fixtures :functions, :function_arguments, :function_outputs
  fixtures :draw_methods, :draw_method_options, :value_types
  it "should forward #to_(gphys|png) to self[0]" do
    path = "/samples/reanalysis/ncep/T.jan.nc/T"
    gl = NumRu::GfdnaviData::Local.parse_path(path)
    ga = gl.analysis("mean","lon")
    gphys = ga.to_gphys
    gphys.should be_instance_of(NumRu::GPhys)
    gp = ga.plot("tone")
    png = gp.to_png
    png.should match(/\A.PNG/)
  end
end

describe NumRu::GfdnaviData::Local do
  fixtures :nodes, :variables
  fixtures :functions, :function_arguments, :function_outputs
  fixtures :draw_methods, :draw_method_options, :value_types
  it "should give ruby code with #to_rb" do
    path = "/samples/reanalysis/ncep/T.jan.nc/T"

    gd = NumRu::GfdnaviData::Local.parse_path(path)
    rb = gd.to_rb
    gd2 = eval(rb, TOPLEVEL_BINDING) # Note: "include NumRu" in the script "rb" may have side effects
#    gd2.object_id.should_not == gd.object_id
    gd2.path.should == gd.path

    gd = NumRu::GfdnaviData::Local.parse_path(path).analysis("mean","lon")
    rb = gd.to_rb
    gd2 = eval(rb, TOPLEVEL_BINDING) # Note: "include NumRu" in the script "rb" may have side effects
    gd2.object_id.should_not == gd.object_id
    gd2.path.should == gd.path

    gd = NumRu::GfdnaviData::Local.parse_path(path).analysis("mean","lon").plot("tone")
    rb = gd.to_rb
    gd2 = eval(rb, TOPLEVEL_BINDING) # Note: "include NumRu" in the script "rb" may have side effects
    gd2.object_id.should_not == gd.object_id
    gd2.path.should == gd.path

    path = "/samples/reanalysis/ncep/UV.jan.nc/"

    u = NumRu::GfdnaviData::Local.parse_path(path+"U").analysis("mean","lon")
    v = NumRu::GfdnaviData::Local.parse_path(path+"V").analysis("mean","lon")
    gd = NumRu::GfdnaviData::Array[u,v].plot("vector")
    rb = gd.to_rb
    gd2 = eval(rb, TOPLEVEL_BINDING) # Note: "include NumRu" in the script "rb" may have side effects
    gd2.object_id.should_not == gd.object_id
    gd2.path.should == gd.path
  end
end


describe NumRu::GfdnaviData::Base, "save_as" do
  fixtures :nodes, :variables, :images
  fixtures :functions, :function_arguments, :function_outputs
  fixtures :draw_methods, :draw_method_options, :value_types
  fixtures :users
  fixtures :diagram_caches

  before do
    path = "/samples/reanalysis/ncep/T.jan.nc/T"
    gl = NumRu::GfdnaviData::Local.parse_path(path)
    @v1 = gl.analysis("mean", "lon")[0]
    @d1 = gl.plot("tone")[0]
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

  # Variable
  it "should be done successfully with #save_as for analyized data" do
    @v1.save_as(@path_nc, @user)
    path = File.join("/usr", @user.login, @path_nc)
    node = Node.find(:first, :conditions => ["path=?", path])
    node.should be_instance_of(Node)
    node.path.should == path
    vs = node.variables 
    vs.length.should == 1
  end

  # Image
  it "should be done successfully with #save_as for diagram" do
    path = "t.png"
    @d1.save_as(@path_png, @user)
    path = File.join("/usr", @user.login, @path_png)
    v = Image.find(:first, :conditions => ["path=?", path])
    v.should be_instance_of(Image)
    v.path.should == path
  end
end

describe NumRu::GfdnaviData::Base, "#save" do
  fixtures :nodes, :variables, :images
  fixtures :functions, :function_arguments, :function_outputs
  fixtures :draw_methods, :draw_method_options, :value_types
  fixtures :users
  fixtures :diagram_caches

  before do
    path = "/samples/reanalysis/ncep/T.jan.nc/T"
    gl = NumRu::GfdnaviData::Local.parse_path(path)
    @gphys = gl.analysis("mean", "lon")[0].to_gphys
    @png = gl.plot("tone")[0].to_png
    @user = User.find(:first, :conditions => ["login=?", "root"])
    @path_nc = "__test.nc"
    @path_png = "__test.png"
  end

  after do
    [@path_nc, @path_png].each do |path|
      fname = File.join(GFDNAVI_USER_PATH, @user.login, path)
      FileUtils.rm_f(fname)
    end
  end

  # Variable
  it "gphys should be dave successfully with #save" do
    vname = @gphys.name
    path = File.join("/usr", @user.login, @path_nc, vname)
    gd = NumRu::GfdnaviData::VariableLocal.new
    gd.path = path
    gd.name = vname
    gd.owner = @user
    gd.gphys = @gphys
    gd.save
    gd2 = NumRu::GfdnaviData::Local.parse_path(path, @user)
    gd2.should be_instance_of(NumRu::GfdnaviData::VariableLocal)
  end

  # Image
  it "png should be dave successfully with #save" do
    path = File.join("/usr", @user.login, @path_png)
    gd = NumRu::GfdnaviData::ImageLocal.new
    gd.path = path
    gd.name = @path_png
    gd.owner = @user
    gd.png = @png
    gd.save
    gd2 = NumRu::GfdnaviData::Local.parse_path(path, @user)
    gd2.should be_instance_of(NumRu::GfdnaviData::ImageLocal)
  end

end


=begin
# * create knowledge document
#   * path���΃p�X�Ŏw��, title��creator��new�̂Ƃ���hash�Ŏw��
#   * ���ł� to_knlge ���\�b�h���e�X�g
describe NumRu::GfdnaviData::KnowledgeLocal, "in createing knowledge document" do
  fixtures :nodes, :variables, :images
  fixtures :knowledges, :knowledge_figures
  fixtures :knowledge_backups, :knowledge_figure_backups
  fixtures :users
  fixtures :diagram_caches

  # before test
  before do
    # ��΃p�X
    @path1 = "/usr/root/knowledge/creating_test/test01.knlge"
    @path2 = "/usr/root/knowledge/creating_test/test02.knlge"
    # ���΃p�X
    # "/usr/bob/knowledge/creating_test/test.knlge"
    @path3 = "creating_test/test.knlge"

    # super user �̏ꍇ
    @user1 = User.find(:first, :conditions => ["login=?", "root"])
    # super user �łȂ��ꍇ
    @user2 = User.find(:first, :conditions => ["login=?", "bob"])

    # �����̉摜�̃p�X
    @img_path1 = "/samples/reanalysis/ncep/T.jan.100hPa.png"
    # �V���ɍ��摜�̃p�X(����͑S�Đ�΃p�X�Ƃ��Ĉ�����)
    @img_path2 = "_test_t2.png"
    # �m�������ɐ}�Ƃ��ē����Ƃ��́A��΃p�X�Ŏw�肵�Ȃ���΂Ȃ�Ȃ�
    @img_path3 = File.join("/usr", @user2.login, "_test_t2.png")

    @hash = {"title"=>"Test of inputting title by Hash.", "creator"=>"Bob Sup"}
  end

  # after test
  # test DB is deleted automatically. (development DB is not.)
  # file is not deleted automatically.
  after do
  end

  # Creating Knowledge Document
  it "should be done successfully with #save document" do
    # * �p�X�A�쐬�҂̃��O�C�����A�g���q���w�肵�� new ����B
    #   �쐬�҂��X�[�p�[���[�U�̏ꍇ�A�C�ӂ̃p�X���w�肷�邱�Ƃ��ł���B
    #   �K����΃p�X�Ŏw�肷��B
    new_knowledge = NumRu::GfdnaviData::KnowledgeLocal.new(@path1, @user1.login)
    # * ���g��������
    #   name, other_mode, other_readable, groups_readable,
    #   default_layout, horizontal_figures, 
    #   figures_size_units, figures_size_height_or_width, 
    #   figures_size_number ��
    #   �w�肵�Ȃ���Ύ����ݒ肳���
    new_knowledge.title = "Temperature data from ncep."
    new_knowledge.textbody = "The most heated area in Figure 1 is a point of 130 degrees of east longitude, 20 degrees of south latitude.\nThat is, it's Australia."
    new_knowledge.category = "memo"
    new_knowledge.creator = "Davis Taro"
    new_knowledge.description = "Australia is hot."
    # * �}�̑}���� insert_figures ���\�b�h��p����B
    #   �摜�̃p�X�A�L���v�V�������n�b�V���ɂ��A�쐬�҂̃��O�C�����Ƌ��ɔz��ɂ��ēn���B
    #   �����̉摜����x�ɑ}�����邱�Ƃ��ł���B
    # * �����̉摜��}�Ƃ��Ďg�p����B
    new_knowledge.insert_figures = {"image"=>@img_path1, "caption"=>"Earth temperature\nlevelist=1000mb"}, {"image"=>@img_path1, "caption"=>"copy of figure 1."}, @user1.login
    # * save ���邱�Ƃɂ��A�͂��߂�DB�ɕۑ������(.knlge�t�@�C�����f�B�X�N���ɍ쐬�����)
    new_knowledge.save

    # * �����ƕۑ�����Ă��邩�`�F�b�N
    k = Knowledge.find(:first, :conditions => ["path=?", @path1])
    k.should be_instance_of(Knowledge)
    k.path.should == @path1

    # * to_knlge ���\�b�h��p���āA�m�������̓��e���n�b�V���œ���
    gl = NumRu::GfdnaviData::Local.parse_path(@path1)
    k_hash = gl.to_knlge

    # * swap_figures ���\�b�h��p���Đ}�̏��Ԃ����ւ���B
    #   �}�̔ԍ�2�ƍ쐬�҂̃��O�C�����������Ƃ��ēn���B
    #   ���̃��\�b�h�����s����Ǝ����� save ���s����B
    #   ���̃��\�b�h�́A�ۑ��O�̒m�������ɂ͎g���Ȃ��B
    gl.swap_figures(1,2,"root")

    # * delete_figure ���\�b�h��p���Đ}���폜����B
    #   �}�̔ԍ��ƍ쐬�҂̃��O�C�����������Ƃ��ēn���B
    #   ���s���Ă������� save �͍s���Ȃ��B��� save ���邱�Ƃł͂��߂�DB�ƃf�B�X�N�ɔ��f�����B
    #   ���̃��\�b�h�́A�ۑ��O�̒m�������ɂ͎g���Ȃ��B
    gl.delete_figure(1, "root")
    gl.save_as(@path2, @user1.login)

    # * �摜�̕ۑ�
    gl_k = NumRu::GfdnaviData::Local.parse_path("/samples/reanalysis/ncep/T.jan.nc/T")
    @d1_k = gl_k.plot("tone")[0]
    @d1_k.save_as(@img_path2, @user2)


    ########################
    # * �����̒��g���n�b�V���ɂ��Ďw�肵�� new ���邱�Ƃ��ł���
    # * �X�[�p�[���[�U�Ŗ����ꍇ�� "/usr/���[�U��/knowledge/"�ȉ��ɂ����ۑ��ł��Ȃ�
    #   �����Ƃ��ēn���ꂽ path �� "/usr/���[�U��/knowledge/�c" �̌`�ɂȂ��Ă��Ȃ����
    #   �����I�� "/usr/���[�U��/knowledge/" �ȉ��ɕۑ������
    #   (����������ƁA"/usr/���[�U��/knowledge/" �̕������ȗ����Ă��\��Ȃ�)
    new_knowledge = NumRu::GfdnaviData::KnowledgeLocal.new(@path3, @user2.login, @hash)
    # * Gfdnavi���ō쐬�����摜��}�Ƃ��ėp����
    new_knowledge.insert_figures = {"image"=>@img_path3, "caption"=>"01\nfirst figure."}, {"image"=>@img_path3, "caption"=>"02\nsecond figure!"}, @user2.login
    new_knowledge.category = "test"
    new_knowledge.description = "Test of creating a knowledge document."
    new_knowledge.save

    # * �����ƕۑ�����Ă��邩�`�F�b�N
    path = "/usr/"+ @user2.login + "/knowledge/" + @path3
    k = Knowledge.find(:first, :conditions => ["path=?", path])
    k.should be_instance_of(Knowledge)
    k.path.should == path

    # * �ҏW����Ƃ��́A�܂� parse ����B
    #   ���̌�A���g�������邱�ƂŊ����̂��̂Ɠ���ւ��B
    #   save ���邱�ƂőS�Ă�DB�ƃf�B�X�N�ɔ��f�����B
    knowledge = NumRu::GfdnaviData.parse(path)
    knowledge.title = "Title is overwritten."
    knowledge.textbody = "teisei simasita yo.\n"
    knowledge.insert_figures = {"image"=>@img_path1, "caption"=>"03."}, {"image"=>@img_path1, "caption"=>"04"}, {"image"=>@img_path1, "caption"=>"05"}, @user2.login
    knowledge.save

    # * �R�����g������ɂ� make_new_comment ���\�b�h��p����B
    #   �����Ƃ��āA���[�U�̃��O�C������n���B
    #   ���̌�A�ʏ�̒m�������Ɠ��l�ɒ��g�������Asave ����B
    k = Knowledge.find(:first, :conditions => ["path=?", path])
    nc = k.make_new_comment("bob")
    nc.textbody = "This is a comment.\n"
    nc.save
    # * �����ЂƂR�����g������B
    nc2 = k.make_new_comment("root")
    nc2.textbody = "this is also a comment.\n\n...."
    nc2.save

    # * comments ���\�b�h��p���邱�ƂŁA�����ɂ���ꂽ�R�����g�̃��X�g���擾�ł���B
    #   �����Ƃ��ă��[�U�̃��O�C������n���B
    comments = k.comments("bob")

    # * �R�����g�ɃR�����g�����邱�Ƃ��ł���B
    nc3 = comments[0].make_new_comment("bob")
    nc3.textbody = "this is a comment to a comment.\n"
    nc3.save

    # * �m���������폜����ɂ� delete ���\�b�h��p����B
    #   save �̕K�v�͂Ȃ��B
    knowledge = NumRu::GfdnaviData.parse(path)
    knowledge.delete(@user2.login)
    
    # * relational_images ���\�b�h��p���Ċ֌W�̂���摜�� node_id �̃��X�g���擾�ł���B
    #   �����Ƃ��ă��[�U�̃��O�C������n���B
    k.relational_images("bob")

    # relational_variables ���\�b�h��p���Ċ֌W�̂���ϐ��� node_id �̃��X�g���擾�ł���B
    #   �����Ƃ��ă��[�U�̃��O�C������n���B
    k.relational_variables("bob")

  end
end
=end
