require "narray"

class KeywordAttribute < ActiveRecord::Base
  belongs_to :node
  validates_presence_of :name
  after_create :set_title_in_node
  after_update :set_title_in_node

  ### // standard name space -->

  StdName = {
    'title' => ['title','subject','long_name','standard_name','name'],
    'description' => ['description'],
    'who' => ['creator','maintainer'],
    'information_url' => [{:conditions => "name = 'information_url' OR ( (name = 'reference' OR name = 'references' ) AND value LIKE 'http://%' ) OR name = 'url'", :order => 'name'}]  # order: information_url > reference% > url
  }

  def self.find_by_stdname(name)
    if aliases=StdName[name]        # substitution, not ==
      strs = Array.new
      has = Array.new
      aliases.each do |expr|
        case expr
        when String
          strs.push expr
        when Hash
          has.push expr
        else
          raise "[BUG] Unsupported expression type #{expr.class}"
        end
      end
      if strs.length > 0
        if ar = find(:first, :conditions => strs.collect{|st| "name='#{st}'"}.join(" OR "))
          return ar
        end
      end
      if has.length > 0
        has.each{|ha|
          if ar = find(:first, ha)
            return ar
          end
        }
      end
    end
    nil
  end

  def self.find_by_stdname_or_name(name)
    find_by_stdname(name) || find(:first,:conditions=>["name=?",name])
  end

  ### <-- standard name space //

  ### // support numerical (NArray) attribute values with num_value -->

  def value= (v)
    case v
    when String
      super(v)
    when NArray
      raise "Supported typecodes are 1 digit" if v.typecode>=10
            # Currently (narray-0.5), it never happens since typecode<= 8
      self.num_value= v.typecode.to_s + v.hton.to_s
    else
      raise ArgumentError, "Unsupported class for a keyword value: #{v.class} (name=#{name})"
    end
    v
  end

  def value
    if (buf=num_value) && buf != "NULL"               # substitution into buf, not ==
      v=NArray.to_na(buf[1..-1],buf[0..0].to_i).ntoh  # typecode is one-digit
    else
      v=super
    end
    v
  end

  def self.format_value_search(val, mode=:equal, name="")
    name += "." unless name==""
    allowed = [:equal, :like]
    if !allowed.include?(mode)
      raise ArgumentError,"2nd arg must be one of #{allowed.inspect}"
    end
    case val
    when String
      case mode
      when :like
        "#{name}value LIKE '%#{val}%'"
      when :equal
        "#{name}value = '#{val}'"
      else
        raise "Unsupported mode  #{mode.inspect}"
      end
    when Numeric
      if val.is_a?(Integer)
        tcrange = 1..5    # check all integer- and float-types
      elsif val.is_a?(Float)
        tcrange = 4..5    # check float-types
      else
        raise "Unsupported Numeric type  #{val.class}"
      end
      case mode
      when :like
        exprs = []
        for tc in tcrange
          str = NArray.new(tc,1).fill!(val).hton.to_s
          if connection.adapter_name=="SQLite"
            str.gsub!(/\000/,'%00')
            wild_chr = "*"
            operator = "GLOB"
          else
            str = str.gsub(/_/,'\_').gsub(/%/,'\%')
            wild_chr = "%"
            operator = "LIKE BINARY"
          end
          exprs.push( "#{name}num_value #{operator} '#{tc.to_s}#{wild_chr}#{str}#{wild_chr}'")
        end
        exprs.join(' OR ')
      when :equal
        exprs = []
        for tc in tcrange
          str = NArray.new(tc,1).fill!(val).hton.to_s

          str.gsub!(/\000/,'%00') if connection.adapter_name=="SQLite"
          exprs.push( "#{name}num_value = '" + tc.to_s + str  + "'")

        end
        exprs.join(' OR ')
      else
        raise "Unsupported mode  #{mode.inspect}"
      end
    when NArray
      # :like are treated as :equal here
      str = val.hton.to_s
      str.gsub!(/\000/,'%00') if connection.adapter_name=="SQLite"
      "#{name}num_value = '" + val.typecode.to_s + str + "'"
    end
  end

  ### <-- support numerical (NArray) attribute values with num_value //


  alias _node node
  def node(reload=false, hash={})
    if u = hash[:user]
      return Node.find(node_id, :user=>u)
    else
      return _node(reload)
    end
  end


  def to_xml(opt)
    opt = opt.dup.update(:except => [:id, :node_id, :value, :num_value])
    super(opt) do |xml|
      xml.value self.value
    end
  end


  protected
  def set_title_in_node
    nod = self.node(false,:user=>:all)
    if StdName["title"].include? self.name
      unless nod.title && nod.title != "NULL"
        nod.title = self.value
        nod.save!
      end
    elsif StdName["description"].include? self.name
      unless nod.description && nod.description != "NULL"
        nod.description = self.value
        nod.save!
      end
    end
  end


end
