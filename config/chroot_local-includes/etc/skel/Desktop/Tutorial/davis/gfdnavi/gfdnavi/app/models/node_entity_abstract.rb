require "forwardable"

class NodeEntityAbstract < ActiveRecord::Base
  extend Forwardable

  self.abstract_class = true

  before_save :save_node
  before_destroy :set_destroy_flag
  after_destroy :destroy_node

  attr_reader :to_be_destroyed

  class << self
    alias :_find :find
    def find(*args)
      nodes = Node.send("find_#{table_name}",*args)
      case nodes
      when Node
        return nodes.entity
      when Array
        return nodes.collect{|n| n.entity}
      else
        return nodes
      end
    end
  end

  def initialize(attrs=nil)
    super
    self.attributes = attrs
  end

  def attributes=(attrs)
    attrs_node = Hash.new
    attrs_org = attrs
    if attrs
      attrs = attrs.dup
      Node.column_names.each{|cname|
        if ( v = (attrs.delete(cname) || attrs.delete(cname.to_sym)) )
          attrs_node[cname] = v
        end
      }
    end
    super(attrs)
    if n = node
      n.attributes = attrs_node
    else
      n = Node.new(attrs_node)
    end
    n.node_type = Node.const_get(self.class.table_name.singularize.upcase)
    self.node = n
    n.entity = self
    return attrs_org
  end

  def node
    return @node if @node
    if node_id
      @node = Node.find(node_id, :user => :all)
      @node.entity = self
      return @node
    end
    return nil
  end

  def node=(node)
    @node = node
  end

  if Node.table_exists?
    meths = Node.column_names.dup
    meths.delete("id")
    meths.delete("node_type")
    meths.collect!{|me| /\A(.+)_id\z/ =~ me ? $1 : me}
    meths += meths.collect{|me| "#{me}="}
  else
    meths = Array.new
  end
  meths += %w(
     keyword_attributes spatial_and_time_attributes
     draw_parameters
     references referenced_by
     fname
     set_rgroups set_wgroups
     add_prefix
     node_type
     stdname
     opendap?
     visibility
  )
  meths.each do |me|
    def_delegator :node, me
  end

  def destroy_all
    yml = fname+".yml"
    if File.exist?(yml)
      FileUtils.remove(yml)
    end
    destroy
  end
  
  def to_xml(opts = {}, &block)
    opts = opts.dup
    user = opts.delete(:user)
    num_dirs = opts.delete(:num_dirs)
    uri_prefix = opts.delete(:uri_prefix)
    serializer = NodeXmlSerializer.new(self, opts)
    serializer.to_s do |xml|
      if node
        xml.node_type Node::NODE_TYPES[node.node_type]
	xml.keyword_attributes({"uri" => File.join(uri_prefix,"data/")+File.join(path,"keyword_attributes.xml")}) if uri_prefix
	xml.parent({"uri"=>File.join(uri_prefix,"data/")+File.dirname(path)+".xml"}) if uri_prefix
#	xml.parent{|xml| xml.uri File.join(uri_prefix,"data/")+File.dirname(path)+".xml"} if uri_prefix
	if user && (user.super_user? || owner == user)
	  xml.owner owner.login
	  xml.other_mode other_mode
	  xml.rgroups Group.find_by_bit_flag(rgroups)
	  xml.wgroups Group.find_by_bit_flat(wgroups)
	end
	if node.directory?
	  xml.num_dirs node.count_directory_nodes(:user=>user) if num_dirs
	  xml.children(children.collect{|chld|{"uri"=>File.join(uri_prefix,"data",child.path+".xml")}}) if uri_prefix
	  xml.dl_url File.join(uri_prefix,"data",path) if downloadable? && uri_prefix
	end
        if node.variable? && opts[:dimensions]
          gphys = node.entity.gphys
          xml.dimensions do |xml|
            gphys.rank.times{|i|
              coord = gphys.coord(i)
              xml.dimension do |xml|
                xml.name coord.name
                xml.max coord.max.val
                xml.min coord.min.val
                xml.units coord.get_att("units")
              end
            }
          end
        end
        if uri_prefix
	  xml.dl_url File.join(uri_prefix,"data",path) if node.directory? && downloadable?
          xml.img_src File.join(uri_prefix, "data", path) if node.image?
          xml.gfdnavi_root uri_prefix
	end
      end
      block.call(xml) if block_given?
    end
  end

  def to_hash(opts={}, &b)
    opts = opts.dup
    user = opts.delete(:user)
    uri_prefix = opts.delete(:uri_prefix)
    hash = {
      "path" => path,
      "type" => Node::NODE_TYPES[node.node_type],
    }
    %w(size title description).each do |name|
      hash[name] = node.send(name)
    end
    if parent
      hash["parent"] = {
        "url" => File.join(uri_prefix, "data", parent.path),
        "type" => "directory"
      }
    end
    return hash
  end

  def save_as(path, owner)
    if owner.super_user? && /\A\// =~ path
      path = File.join(GFDNAVI_DATA_PATH, path[i])
    else
      path = File.join(prefix, path[i])
    end
    entity = self.copy(path)
    entity.owner = owner
    entity.save
  end

  protected
  def save_node
    if node.save
      self.node_id = node.id
      return true
    else
      node.errors.each{|k,v| errors.add(k,v) }
      return false
    end
  end

  def set_destroy_flag
    @to_be_destroyed = true
  end

  def destroy_node
    node.destroy
  end

=begin
  def method_missing(method_id, *args)
    begin
      super(method_id, *args)
    rescue NoMethodError
      error = $!
      begin
        node.send(method_id, *args)
      rescue NoMethodError
        raise error
      end
    end
  end
=end

  class NodeXmlSerializer < ActiveRecord::XmlSerializer
    def serializable_attribute_names
      attribute_names = @record.attribute_names
      attribute_names += @record.node.attribute_names if @record.node
      attribute_names.uniq!
      %w(id node_id node_type owner_id parent_id guest_owner_id other_mode rgroups wgroups file other_readable groups_readable).each{|n|
        attribute_names.delete(n)
      }
      attribute_names.collect!{|n| /^(.+)_id$/=~n ? $1 : n}
      if options[:only]
        options.delete(:except)
	attribute_names = attribute_names & Array(options[:only]).collect{|n| n.to_s }
      else
        options[:except] = Array(options[:except]) | ( Array(@record.class.inheritance_column) + Array(@record.node && Node.inheritance_column) )
	options[:except].uniq!
	attribute_names = attribute_names - options[:except].collect {|n| n.to_s }
      end
    end
  end
end

module ActiveRecord
  class  XmlSerializer
    class Attribute
      protected
      def compute_type
        column = @record.class.serialized_attributes.has_key?(name) ? :yaml : @record.class.columns_hash[name]
	column ||= Node.serialized_attributes.has_key?(name) ? :yaml : Node.columns_hash[name]
	type = column.type
	case type
	when :text
	  :string
	when :time
	  :datetime
	else
	  type
	end
      end
    end
  end

end


