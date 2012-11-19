require "yaml"
require "narray"

$__yaml_new__ =  Class.method_defined?("yaml_as")
$__yaml_domain__ = "ruby.yaml.org,2002"
$__yaml_type__ = "narray"


class NArray

# --- To be removed; marged to GPhys ------

  def self.endian
    NArray[1].to_s[1] == 1 ? :little : :big
  end

  def _dump(limit)
    Marshal.dump([typecode, shape, NArray.endian, to_s])
  end

  def self._load(str)
    ary = Marshal.load(str)
    typecode, shape, endian, str = ary
    na = NArray.to_na(str, typecode, *shape)
    na = na.swap_byte unless endian == NArray.endian
    return na
  end

# ------------

  if $__yaml_new__

    yaml_as "tag:#{$__yaml_domain__}:#{$__yaml_type__}"
    def NArray.yaml_new( klass, tag, val )
      na = NArray.new(val["typecode"], *val["shape"])
      na[true] = val["ary"]
      na
    end
    def to_yaml(opts={})
      YAML::quick_emit( object_id, opts ) do |out|
        out.map( taguri, to_yaml_style ) do |map|
          map.add( "typecode", typecode )
          map.add( "shape", shape )
          map.add( "ary", to_a.flatten )
        end
      end
    end

  else

    def is_complex_yaml?
      true
    end
    def to_yaml_type
      "!#{$__yaml_domain__}/#{$__yaml_type__}"
    end
    def to_yaml( opts = {} )
      YAML::quick_emit( object_id, opts ) { |out|
        out.map( to_yaml_type ) { |map|
          map.add( "typecode", typecode )
          map.add( "shape", shape )
          map.add( "ary", to_a.flatten )
        }
      }
    end

  end
end

unless $__yaml_new__
  YAML.add_domain_type( $__yaml_domain__, /^#{$__yaml_type__}/ ) { |type, val|
    na = NArray.new(val["typecode"], *val["shape"])
    na[true] = val["ary"]
    na
  }
end

$__yaml_new__ = nil
