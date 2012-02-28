# Extension of GPhys class for Gfdnavi

require "numru/gphys"
require "narray_gfdnavi"
require "netcdf_gfdnavi"
require "nusdas_gfdnavi"

module NumRu
  class GPhys

    # == Parameters @@read_size_limit_1 and @@read_size_limit_2
    # 
    # By default, both are nil, so no restriction is made
    # on the size of data to read in onto run-time memory.
    # To limit it, set these parameters explicitly by calling 
    # the class methods read_size_limit_1= and/or read_size_limit_2=.
    # 
    # It's up to the programmer how to use the
    # two limits. It's OK to use only one of them.
    # If both are set, it is recommended to set as
    # @@read_size_limit_2 > @@read_size_limit_1, because
    # of the order of judgements (the former is evaluated first).

    @@read_size_limit_1 = nil   # e.g., 2000000
    @@read_size_limit_2 = nil   # e.g., 10000000


    class Read_Size_Limit_2_Exceeded < StandardError
    end

    class Read_Size_Limit_1_Exceeded < StandardError
    end

    # class method definitions:
    class << self
      unless defined?(__new__)
        alias :__new__ :new

      # Limit the size of data to read in with GPhys#val.
      # This is implemented by defining singletone method
      # for the VArray that represent the main data array.
      # (Note that no restriction is made for coordinate varaibles,
      # which is practically unnecessary.)
        def new(grid,data)
          if VArrayDummy === data
            if NumRu::GPhysDummy == self
              return __new__(grid,data)
            else
              return GPhysDummy.new(grid,data)
            end
          end
          gp = __new__(grid,data)
          data = gp.data
          data = GPhys::add_size_limit(data)
=begin
          def data.val
            if @@read_size_limit_2 && length > @@read_size_limit_2
              raise Read_Size_Limit_2_Exceeded, "Exceeded the maximum array size to read in (length=#{@@read_size_limit_2}). Reduce the size by using the 'axes' window, and try again. (Or perhaps you may get around the error by programming with GPhys::each_along_dims.)"
            elsif @@read_size_limit_1 && length > @@read_size_limit_1
              raise Read_Size_Limit_1_Exceeded, "Exceeded the maximum array size to read in (length=#{@@read_size_limit_1}). Reduce the size by using the 'axes' window, and try again. (Or perhaps you may get around the error by programming with GPhys::each_along_dims.)"
	  end
            super
          end
=end
          gp
        end
      end

      # Set internal parameter @@read_size_limit_1
      def read_size_limit_1= (limit)
	return if limit.nil?
	raise(ArgumentError,"Expect an integer") if !limit.is_a?(Integer)
	@@read_size_limit_1 = limit
      end

      # Set internal parameter @@read_size_limit_2 
      def read_size_limit_2= (limit)
	return if limit.nil?
	raise(ArgumentError,"Expect an integer") if !limit.is_a?(Integer)
	@@read_size_limit_2 = limit
      end

      def read_size_limit_1
	@@read_size_limit_1
      end

      def read_size_limit_2
	@@read_size_limit_2
      end

      def add_size_limit(data)
        raise ArgumentError unless data.is_a?(VArray)
        def data.val
          if @@read_size_limit_2 && length > @@read_size_limit_2
            raise Read_Size_Limit_2_Exceeded, "Exceeded the maximum array size to read in (length=#{@@read_size_limit_2}). Reduce the size by using the 'axes' window, and try again. (Or perhaps you may get around the error by programming with GPhys::each_along_dims.)"
          elsif @@read_size_limit_1 && length > @@read_size_limit_1
            raise Read_Size_Limit_1_Exceeded, "Exceeded the maximum array size to read in (length=#{@@read_size_limit_1}). Reduce the size by using the 'axes' window, and try again. (Or perhaps you may get around the error by programming with GPhys::each_along_dims.)"
          end
          super
        end
        data
      end
    end

    def size
      NArray.new(typecode,1).to_s.size * total
    end


# --  Redefined --
    def marshal_dump
      # Remove singleton methods from @data
      if @data.class == VArrayNetCDF || @data.class == VArrayNuSDaS
        data = @data.class.new(@data.instance_variable_get("@ary"))
      elsif @data.class == VArray || @data.class == VArrayComposite
        data = @data
      else
        data = @data.copy # Read from file
      end
      [data, @grid]
    end

    def marshal_load(ary)
      @data = GPhys::add_size_limit(ary[0])
      @grid = ary[1]
    end
# ------------------

  end

  class VArray
    def marshal_dump
      [@name, @mapping, @varray, @ary, @attr]
    end

    def marshal_load(ary)
      @name, @mapping, @varray, @ary, @attr = *ary
    end
  end

  class VArrayComposite
    def marshal_dump
      # NArray which contains Objects cannot be dumped
      varrays_a = @varrays.to_a
      [@attr, @bound_idx, @first_vary, @length, @name, @rank, @shape, varrays_a]
    end

    def marshal_load(ary)
      @attr, @bound_idx, @first_vary, @length, @name, @rank, @shape, varrays_a = *ary
      @varrays = NArray.to_na(varrays_a)
    end
  end

  class GPhysDummy < GPhys
    def initialize(grid,data)
      @data = data
      @grid = grid
    end
    def rank
      @grid.rank
    end
    def shape_current
      @grid.shape
    end
    alias shape shape_current
  end
  class VArrayDummy < VArray
    def inspect
      "VArrayDummy"
    end
    def [](*arg)
      self
    end
    def copy
      self
    end
    def reshape!(*arg)
      self
    end
    for f in Math_funcs + Binary_operators.collect{|name| name.sub(/^\./,"")} + Binary_operatorsL.collect{|name| name.sub(/^\./,"")} + Unary_operators + NArray_type1_methods + NArray_type2_methods + NArray_type3_methods
      eval <<-EOS
        def #{f}(*arg)
          self
        end
      EOS
    end
  end

end

######################################################
## < test >
if $0 == __FILE__

  # SAME TEST IS LOCATED AT test/unit/gphys_gfdnavi_test.rb

  include NumRu

  # TEST FOR MARSHAL DUMP/LOAD OF PLANE GPhys
  org_g = GPhys::IO.open("../data/samples/jmadata/MSM-P/2006/0101.nc", "temp").copy
  p new_g = Marshal.load(Marshal.dump(org_g))
  p new_g.class
  p new_g.val
  p new_g.axis(0).pos.val
  
  # TEST FOR MARSHAL DUMP/LOAD OF single NetCDF
  org_g = GPhys::IO.open("../data/samples/jmadata/MSM-P/2006/0101.nc", "temp")
  p new_g = Marshal.load(Marshal.dump(org_g))
  p new_g.class
  p new_g.val
  p new_g.axis(0).pos.val

  # TEST FOR MARSHAL DUMP/LOAD OF composite NetCDF
  org_g = GPhys::IO.open(Dir.glob("../data/samples/jmadata/MSM-P/2006/010[1-3].nc").sort, "temp")
  p new_g = Marshal.load(Marshal.dump(org_g))
  p new_g.class
  p new_g.val
  p new_g.axis(0).pos.val
end
