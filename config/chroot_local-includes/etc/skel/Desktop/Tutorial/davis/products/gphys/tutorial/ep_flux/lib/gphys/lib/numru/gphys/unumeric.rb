require "numru/units"
require "rational"    # for UNumeric#sqrt

## require "numru/gphys/gphys"   # --> in the test program

=begin
=class NumRu::UNumeric

Class of Numeric with Units.

Dependent on 
((<NumRu::Units|URL:http://ruby.gfd-dennou.org/products/numru=units>))
and Rational, a standard library.

==Class Methods

---new(val, uni)

    Constractor.

    ARGUMENTS
    * ((|val|)) (Numeric)
    * ((|units|)) (NumRu::Units or String):  if string, internally converted to
      a NumRu::Units

==Methods

---val

    RETURN VALUE
    * the value (Numric)

---units

    RETURN VALUE
    * the units (NumRu::Units)

---inspect

    RETURN VALUE
    * a String (e.g., '1 m')

---to_s

    aliasesed to ((<inspect>)).

---to_f
    RETURN VALUE
    * val.to_f

---to_i
    RETURN VALUE
    * val.to_i

---convert(to_units)

    Convert to ((|to_units|)).

    RETURN VALUE
    * a UNumeric

    EXCEPTION
    * when the current units is incompatible with to_units.

---convert2

    Same as ((<convert>)), but returns ((|self|)) if the units are
    incompatible (Warned).

    EXCEPTION
    * none

    WARING MADE

    Warning is made to $stderr if the following
    condition is satisfied.

    * the units of ((|self|)) and ((|to_units|)) are incompatible.

---coerce(other)

    As you know. Can handle Numeric, Array, NArray.
    NOTE: VArray and GPhys know UNumeric.

--- *(other)

     Multiplication. Knows Numeric, UNumeric, VArray, and GPhys.
     The units are multipled too. (if other is Numeric, it is assumed 
     to be non-dimension)

     RETURN VALUE
     * a UNumeric, VArray, or GPhys

--- /(other)

     Division. See ((<*>)).

--- +(other)

     Addition. Knows Numeric, UNumeric, VArray, and GPhys.
     The return value will have the units of ((|self|)).

     SPECIAL REMARK!

     If ((|other|)) has units within a factor and/or offset
     of difference, It is CONVERTED before addition (by using ((<convert2>)))!
 
     RETURN VALUE
     * a UNumeric, VArray, or GPhys

     WARING MADE 

     Warning is made to $stderr if the following
     condition is satisfied.

     * the units of ((|self|)) and ((|to_units|)) are incompatible.
     * ((|other|)) is Numeric.

--- -(other)

     Subtraction. See ((<+>)).

=end

module NumRu

  class UNumeric
    
    def initialize(val, uni)
      raise TypeError unless Numeric === val
      uni = Units.new(uni) if String === uni
      raise TypeError unless Units === uni
      @val, @uni = val, uni
    end
      
    def self::[](val, uni)
      new(val, uni)
    end

    def val; @val; end

    def units; @uni; end

    def inspect
      val.to_s + ' ' +units.to_s
    end

    alias to_s inspect

    def to_f; @val.to_f; end
    def to_i; @val.to_i; end

    def convert(to_units)
      if ( units == to_units )
	self
      else
	UNumeric[ units.convert(val, to_units), to_units ]
      end
    end

    def convert2(to_units)
      # returns self if the units are incompatible
      begin
	convert(to_units)
      rescue
	#if $VERBOSE
	$stderr.print(
                   "WARNING: incompatible units: #{units.to_s} - #{to_units.to_s}\n")
	#end   # warn in Ruby 1.8
	self
      end
    end

    def coerce(other)
      case
      when Numeric
	c_other = UNumeric.new( other, Units.new("1") )
      when Array
	c_other = VArray.new( NArray.to_na(other) )
      when NArray
	c_other = VArray.new( other )
      else
	raise "#{self.class}: cannot coerce #{other.class}"
      end
      [ c_other, self ]
    end

    def *(other)
      case other
      when UNumeric
	UNumeric.new( val * other.val , units * other.units )
      when Numeric
	# assumed to be non-dimensional
	UNumeric.new( val * other, units )
      when VArray, GPhys
	result = other * val
	result.units = units * other.units
	result
      else
	s, o = other.coerce( self )
	s * o
      end
    end

    def +(other)
      case other
      when UNumeric
	v = val + other.convert2( units ).val
	UNumeric.new( v , units )
      when Numeric
	v = val + other
	$stderr.print("WARNING: raw Numeric added to #{inspect}\n") #if $VERBOSE
	UNumeric.new( v, units )
      when VArray, GPhys
	ans = other.units.convert2(other, units) + val
	ans.units = units     # units are taken from the lhs
	ans
      else
	s, o = other.coerce( self )
	s + o
      end
    end
    
    def **(other)
      UNumeric.new( val**other, units**other )
    end

    def abs
      UNumeric.new( val.abs, units )
    end

    def -@
      UNumeric.new( -val, units )
    end

    def +@
      self
    end

    def -(other)
      self + (-other)   # not efficient  --> Rewrite later!
    end

    def /(other)
      self * (other**(-1))   # not efficient  --> Rewrite later!
    end

    LogicalOps = [">",">=","<","<=","==","==="]
    LogicalOps.each { |op|
      eval <<-EOS, nil, __FILE__, __LINE__+1
        def #{op}(other)
          case other
	  when UNumeric
	    val #{op} other.convert2( units ).val
	  when Numeric
	    $stderr.print("WARNING: raw Numeric added to #{inspect}\n") #\
                # if $VERBOSE   # warn in Ruby 1.8
	    val #{op} other
	  when VArray, GPhys
	    val #{op} other.units.convert2(other, units)
	  else
	    s, o = other.coerce( self )
	    s #{op} o
	  end
	end
      EOS
    }

    Math_funcs_nondim = ["exp","log","log10","log2","sin","cos","tan",
	    "sinh","cosh","tanh","asinh","acosh",
	    "atanh","csc","sec","cot","csch","sech","coth",
	    "acsch","asech","acoth"]
    Math_funcs_nondim.each{ |f|
      eval <<-EOS, nil, __FILE__, __LINE__+1
        def #{f}
          UNumeric.new( Math.#{f}(val), Units.new('1') )
        end
      EOS
    }
    Math_funcs_radian = ["asin","acos","atan","acsc","asec","acot"]
    Math_funcs_radian.each{ |f|
      eval <<-EOS, nil, __FILE__, __LINE__+1
        def #{f}
          UNumeric.new( Math.#{f}(val), Units.new('rad') )
        end
      EOS
    }

    def atan2(other)
      case other
      when Numeric
	UNumeric.new( Math.atan2(val, other), Units.new('rad') )
      when UNumeric
	UNumeric.new( Math.atan2(val, other.val), Units.new('rad') )
      else
	c_me, c_other = other.coerce(self)
	c_me.atan2(c_other)
      end
    end

    def sqrt
      UNumeric.new( Math.sqrt(val), units**Rational(1,2) )
    end

  end   # class UNumeric
end   # module NumRu

######################################
if $0 == __FILE__

  require "narray"

  include NumRu
  a = UNumeric[ 10.0, Units['m/s'] ]
  b = UNumeric[ 2.0, Units['m/s'] ]
  c = UNumeric[ 5.0, Units['m'] ]

  print "\n** Section 1 **\n"
  p a
  p a*b
  p a+b
  p a+c
  p a+7
  p a*7
  p -a
  p a-b, a-1000, a/100
  p a.log, a.sin
  p UNumeric[1.0,Units['1']].atan2( UNumeric[1.0,Units['1']] )

  print "\n** Section 2 **\n"
  p a > 1
  p 1 > a

  print "\n** Section 3 **\n"

  require "numru/gphys/varray"

  na = NArray.float(4).indgen
  va = VArray.new( na )
  vb = a + va
  p vb, vb.units, vb.att_names

end
