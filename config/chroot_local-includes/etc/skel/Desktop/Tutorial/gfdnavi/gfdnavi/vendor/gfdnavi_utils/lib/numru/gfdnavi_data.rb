require "numru/gfdnavi_data/remote"

module NumRu::GfdnaviData

  module_function

  def open(url, *args)
    if /\Ahttps?:\/\// =~ url
      NumRu::GfdnaviData::Remote.parse_url(url, *args)
    else
      NumRu::GfdnaviData::Local.parse_path(url, *args)
    end
  end
  alias :parse :open

  def hash_to_str(hash)
    ary = ::Array.new
    hash.sort.each{|k,v|
      ary += param_to_str(k, v) unless v.nil? || v=="nil"
    }
    ary.join(",")
  end


  def param_to_str(name, val)
    ary = ::Array.new
    case val
    when String
      ary.push "#{name}=#{val}"
    when Numeric
      val = val.to_i if Float === val && val.to_i == val
      ary.push "#{name}=#{val}"
    when TrueClass
      ary.push "#{name}=1"
    when FalseClass
      ary.push "#{name}=0"
    when ::Array
      val = val.map{|v| v.nil? ? " " : v}
      ary.push "#{name}=#{val.join(',')}"
    when Hash
      val = val.sort{|a,b|
        if String===a
          if a=="min"
            return -1
          elsif a=="max"
            return 1
          end
        end
        a <=> b
      }
      val.each{|k,v|
        ary += param_to_str("#{name}[#{k}]", v)
      }
    else
      ary.push "#{name}=#{val.inspect}"
    end
    return ary
  end


end
