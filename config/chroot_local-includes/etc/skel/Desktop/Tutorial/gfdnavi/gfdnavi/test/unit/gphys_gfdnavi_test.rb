# -*- coding: cp932 -*-
#require "test/unit"
# $LOAD_PATH << "../../lib"
require File.dirname(__FILE__) + '/../test_helper'
require "gphys_gfdnavi"

class TC_GPhys_Gfdnavi < Test::Unit::TestCase
  def setup
    @msm_temp_dir = File.join(File.dirname(__FILE__), "../../data/samples/jmadata/MSM-P/2006")
  end

  def teardown
    NumRu::GPhys.class_eval <<EOC
      @@read_size_limit_1 = nil
      @@read_size_limit_2 = nil
EOC
  end

  def test_gphys_read_size_limit
    ix   = 200
    lim1 = 100
    lim2 =  50 # lim2 < lim1 < ix
    na   = NArray.sfloat(ix).random
    va   = NumRu::VArray.new(na, {}, "test")
    na_x = NArray.sfloat(ix).indgen!
    va_x = NumRu::VArray.new(na_x, {}, "x")
    gp   = NumRu::GPhys.new(NumRu::Grid.new(NumRu::Axis.new.set_pos(va_x)), va)

    NumRu::GPhys.read_size_limit_1 = lim1
    assert_raise(NumRu::GPhys::Read_Size_Limit_1_Exceeded){gp.val}
    assert_nothing_raised{gp[0...lim1].val}

    NumRu::GPhys.read_size_limit_2 = lim2
    assert_raise(NumRu::GPhys::Read_Size_Limit_2_Exceeded){gp[0...lim1].val}
    assert_nothing_raised{gp[0...lim2].val}
  end

  def test_gphys_marshal
    # TEST FOR MARSHAL DUMP/LOAD OF PLANE GPhys
    org_g = NumRu::GPhys::IO.open(File.join(@msm_temp_dir, "0101.nc"), "temp").copy
    new_g = Marshal.load(Marshal.dump(org_g))
    assert_equal(NumRu::GPhys, new_g.class)
    assert_equal(org_g.val, new_g.val)
    assert_equal(org_g.axis(0).pos.val, new_g.axis(0).pos.val)

    # TEST FOR MARSHAL DUMP/LOAD OF single NetCDF
    org_g = NumRu::GPhys::IO.open(File.join(@msm_temp_dir, "0101.nc"), "temp")
    new_g = Marshal.load(Marshal.dump(org_g))
    assert_equal(NumRu::GPhys, new_g.class)
    assert_equal(org_g.val, new_g.val)
    assert_equal(org_g.axis(0).pos.val, new_g.axis(0).pos.val)

    # TEST FOR MARSHAL DUMP/LOAD OF composite NetCDF
    org_g = NumRu::GPhys::IO.open(Dir.glob(File.join(@msm_temp_dir, "010[1-3].nc")).sort, "temp")
    new_g = Marshal.load(Marshal.dump(org_g))
    assert_equal(NumRu::GPhys, new_g.class)
    assert_equal(org_g.axis(0).pos.val, new_g.axis(0).pos.val)
    assert_equal(org_g.val, new_g.val)
  end

  def test_varray_marshal
    # ONE-DIMENSIONAL PLANE VArray
    ix     = 10
    na     = NArray.sfloat(ix).random
    org_va = NumRu::VArray.new(na, {"units" => "m", "long_name" => "test data"}, "test")
    new_va = Marshal.load(Marshal.dump(org_va))
    assert_equal(NumRu::VArray, new_va.class)
    assert_equal(org_va.name, new_va.name)
    assert_equal(org_va.long_name, new_va.long_name)
    assert_equal(org_va.units, new_va.units)
    assert_equal(org_va.val, new_va.val)

    # TWO-DIMENSIONAL PLANE VArray
    iy     = 20
    na     = NArray.sfloat(ix, iy).random
    org_va = NumRu::VArray.new(na, {"units" => "m", "long_name" => "test data"}, "test")
    new_va = Marshal.load(Marshal.dump(org_va))
    assert_equal(NumRu::VArray, new_va.class)
    assert_equal(org_va.shape, new_va.shape)
    assert_equal(org_va.name, new_va.name)
    assert_equal(org_va.long_name, new_va.long_name)
    assert_equal(org_va.units, new_va.units)
    assert_equal(org_va.val, new_va.val)
  end

  def test_varray_composite_marshal
    # VArrayComposite OF VArrayNetCDF
    g = NumRu::GPhys::IO.open(Dir.glob(File.join(@msm_temp_dir, "010[1-3].nc")).sort, "temp")
    org_va = g.data
    new_va = Marshal.load(Marshal.dump(org_va))
    assert_equal(NumRu::VArrayComposite, new_va.class)
    assert_equal(org_va.val, new_va.val)
  end

  def test_gphys_dummy
    org_g = NumRu::GPhys::IO.open(File.join(@msm_temp_dir, "0101.nc"), "temp").copy
    new_g = NumRu::GPhysDummy.new(org_g.grid_copy, NumRu::VArrayDummy.new)
    assert_equal(NumRu::GPhysDummy, new_g.class)
    assert_equal(org_g.rank, new_g.rank)
    assert_equal(org_g.shape, new_g.shape)
    assert_equal(org_g[false, 0].shape, new_g[false, 0].shape)

    dmp_g = Marshal.load(Marshal.dump(new_g))
    assert_equal(NumRu::GPhysDummy, dmp_g.class)
    assert_equal(new_g.rank, dmp_g.rank)
    assert_equal(new_g.shape, dmp_g.shape)
  end

=begin
  def test_varray_dummy
  end
=end
end
