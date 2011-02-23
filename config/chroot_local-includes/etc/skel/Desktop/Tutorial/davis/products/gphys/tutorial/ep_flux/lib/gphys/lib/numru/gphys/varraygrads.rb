=begin
=ToDo list
* Support a temporay attribute (@tmp_attr = Attribute.new)
=end

#require "numru/netcdf"
require "numru/gphys/grads_gridded"
require "numru/gphys/varray"
#require "numru/gphys/attributenetcdf"


module NumRu

#   class VArrayNetCDF < VArray
   class VArrayGrADS < VArray

      ## < initialization redefined > ##

#      def initialize(aNetCDFVar)
      def initialize(aGrADSVar)
#         @name = aNetCDFVar.name
         @name = aGrADSVar.name
         @mapping = nil
         @varray = nil
#         raise ArgumentError,"Not a NetCDVAr" if ! aNetCDFVar.is_a?(NetCDFVar)
         raise ArgumentError,"Not a GrADSVar" if ! aGrADSVar.is_a?(GrADSVar)
#         @ary = aNetCDFVar
         @ary = aGrADSVar
#         @ary.extend(NetCDFVarDeferred)
#         @ary.file.extend(NetCDFDeferred)
#         @attr = AttributeNetCDF.new(aNetCDFVar)
#         @attr = Attribute.new
         @attr = aGrADSVar.attr
      end

      def inspect
#         "<'#{@name}' in '#{@ary.file.path}'  #{@ary.ntype}#{shape_current.inspect}>"
         "<'#{@name}'  #{shape_current.inspect}>"
      end

      class << self
         ## < redefined class methods > ##

         def new2(file, name, ntype, dimensions, attr=nil)
            va = new( file.def_var(name, ntype, dimensions) )
            if attr; attr.each{|key,val| va.attr[key]=val}; end 
            va
         end

         ## < additional class methods > ##

         def write_control(file, gphys)
           raise ArgumentError, "1st arg: not a GrADS_Gridded" if !file.is_a?(GrADS_Gridded)
           rank = gphys.data.rank
           raise ArgumentError, "only 4D data is supported" if rank != 4

           for i in 0..2
             c = ""
             gphys.axis(i).pos.val.each{|j| c += (j.to_s + " ")}
             file.dimensions[i] = {
               :len=>gphys.axis(i).pos.length, 
               :flag=> "LEVELS", 
               :spec=>c
             }
           end

           i = 3 # time
             c = Date.jd(gphys.axis(i).pos.val[0]).strftime("%d%b%Y") + " <increment>"
             file.dimensions[i] = {
               :len=>gphys.axis(i).pos.length, 
               :flag=> "LINEAR", 
               :spec=>c
             }
           # end

           file.def_var(gphys.name, gphys.data.get_att("nlev"), 
                        99, gphys.data.get_att("long_name") )

           ctlfile = File.open(file.path,"w")
           ctlfile << file.to_ctl
           ctlfile.close

         end


         def write_binary(file, vary, rename=nil, dimnames=nil)
            raise ArgumentError, "1st arg: not a GrADS_Gridded" if !file.is_a?(GrADS_Gridded)
            raise ArgumentError, "2nd arg: not a VArray" if !vary.is_a?(VArray)
            rank=vary.rank
            raise ArgumentError, "only 4D data is supported" if rank != 4

            file.put(vary.val)
         end
      end

      ## < redefined instance methods > ##

      def val
#         mode_switched = @ary.file.enddef
         v = @ary.get
#         if mode_switched; @ary.file.redef; end
#         v
      end

      def val=(narray)
         @ary.put( __check_ary_class2(narray) )
         narray
      end

      # It is safer not to have the method "shape" to avoid misconfusion of 
      # shape_ul0 and shape_current:
      def shape
         raise "The shape method is not available. Use shape_ul0 or shape_current instead."
      end

      def name=(nm)
         @ary.name = nm
         @name = nm
      end
      def rename!(nm)
         @ary.name = nm
         @name = nm
         self
      end
      def rename_no_file_change(nm)
         @name = nm
         self
      end
      def rename(nm)
        self.dup.rename_no_file_change(nm)
      end

      def ntype
        @ary.ntype
      end

      def total
         len = 1
         @ary.shape_current.each{|i| len *= i}
         len
      end
      alias length total

      def rank
        shape_current.length
      end

#      def reshape!( *args )
#        raise "cannot reshape a #{class}. Use copy first to make it a VArray with NArray"
#      end
      undef reshape!


      ## < additional instance methods > ##

      def dim_names
         @ary.dim_names
      end
      def shape_ul0
         @ary.shape_ul0
      end
      def shape_current
         @ary.shape_current
      end
      def file
         @ary.file
      end

   end
end

###########################################################
### < test >

if $0 == __FILE__
 #  $DEBUG = true
   include NumRu

   begin
     grvar = GrADSVar.new("../../testdata/T.jan.ctl","T")
   rescue
     grvar = GrADSVar.new("../../../testdata/T.jan.ctl","T")
   end
   va = VArrayGrADS.new(grvar)

   p va.dim_names
   p va.shape_ul0
   p va.val

   p va.att_names
   p va.get_att("long_name")

   va2 = va[3..9,5..15,0,0]
   p va2.shape
   p va2.val

   p va2.get_att("long_name")
#   va2.get_att("units")
#   va2[3,true,true,true] = 5.0
#   va2.val

   va3 = (va2*3).val
   p va3

end
