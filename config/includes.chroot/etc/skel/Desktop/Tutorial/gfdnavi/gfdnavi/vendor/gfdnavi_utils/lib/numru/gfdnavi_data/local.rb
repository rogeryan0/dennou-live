# -*- coding: cp932 -*-
require "virtual_data"
require "narray"

require "numru/gfdnavi_data/local_cache"

require "numru/gfdnavi_data/array_local"
require "numru/gfdnavi_data/directory_local"
require "numru/gfdnavi_data/variable_local"
require "numru/gfdnavi_data/image_local"
require "numru/gfdnavi_data/knowledge_local"
require "numru/gfdnavi_data/function_local"
require "numru/gfdnavi_data/draw_method_local"

require "numru/path_parse.tab"

require "forwardable"


module NumRu::GfdnaviData
  module Local
    extend Forwardable

    @@local_cache_path = LocalCache.new
    @@local_cache_gphys = LocalCache.new
    @@alias = Hash.new
    @@path_parser = PathParser.new

    # module functions

    def self.parse_path(path, user=nil)
      # global variables for "find" (maybe)
      $descriptions = ::Array.new
      $i = 1
      # parse path
      ary = @@path_parser.parse(path)
      ary = ary[0] if ary.length == 1 && ary[0].is_a?(String)
      # get object
      return path_array_to_object(ary, user, path)
    end

    def self.path_array_to_object(ary, user=nil, path_org=nil)
      no_cache = false
      no_description = false

      path_org ||= path_array_to_path(ary) # reconstruct original path
      path_org = @@alias[path_org] || path_org
      if lc = @@local_cache_path.get([path_org, user])
        return lc # return cached object
      end

      unless ary.is_a?(::Array) || ary.is_a?(String)
        raise ArgumentError, "Argument should be an Array or a String, but was #{ary.class}: #{ary}"
      end
      if ary.is_a?(String) # not array, but a signle path
        # get node object
        obj = Node.find(:first, :conditions => ["path=?", ary], :user => user)
        raise "path is not found: #{ary}, user=#{user.inspect}" unless obj
        if obj.entity.is_a?(::Variable)
          obj = VirtualData.new(obj) # wrap object to improve performance using cache
        end
        gd = NumRu::GfdnaviData::Local.create(user, obj)
      elsif ary[-1].is_a?(::Array) && ary[-1][0].kind_of?(Symbol) # a path with operations
        no_description = true
        gd = path_array_to_object(ary.shift, user, nil) # recursive call 
        # apply operations
        ary.each{|func| # func[0]: method (Symbol), func[1]: options (String)
          func = func.dup
          unless gd.is_a?(Array)
            path_new = path_array_to_path([gd.path, func])
            path_new = @@alias[path_new] || path_new
          end
          if path_new && lc = @@local_cache_path.get([path_new, user])
            gd = lc # use cached object
          else
            func[1] = func[1].split(";") unless (func[0] == :slice) || (func[0] == :find) || func[1].nil?
            gd = gd.send(*func.flatten.compact)
            no_cache = true if func[0] == :find
            unless no_cache || gd.is_a?(Array)
              @@alias[path_new] = gd.path if path_new && !(@@alias.has_key?(path_new))
              @@local_cache_path.push([gd.path, user], gd)
            end
          end
        }
      else # an array of paths
        ary_gd = ary.map{|item|
          path_array_to_object(item, user, nil) # recursive call
        }
        gd = NumRu::GfdnaviData::Local.create(user, VirtualData.new(ary_gd))
      end
      $descriptions.push("path=" << gd.path) unless no_description || gd.is_a?(Array) || (gd.path == "/")
      unless no_cache || gd.is_a?(Array)
        @@alias[path_org] = gd.path if path_org && !(@@alias.has_key?(path_org))
        @@local_cache_path.push([gd.path, user], gd)
      end
      return gd
    end

    def self.path_array_to_path(ary)
      unless ary.is_a?(::Array) || ary.is_a?(String)
        raise ArgumentError, "Argument should be an Array or a String, but was #{ary.class}: #{ary}"
      end
      ary = ary.dup
      ary = ary[0] if ary.kind_of?(Array) && ary.length == 1
      if ary.is_a?(String) # not array, but a signle path
        return ary # do nothing
      elsif ary[-1].is_a?(::Array) && ary[-1][0].kind_of?(Symbol) # a path with operations
        str = path_array_to_path(ary.shift)
        ary.each{|func| # func[0]: method (Symbol), func[1]: options (String)
          case func[0]
          when :slice
            str << "[" << func[1].join(",") << "]"
          else
            str == "" if str == "/"
            str << "/" << func[0].to_s << "(" << func[1].to_s << ")"
          end
        }
        return str
      else # an array of paths
        return "/[" << ary.map{|a| path_array_to_path(a)}.join(",") << "]"
      end
    end

    class << self
      private :path_array_to_object, :path_array_to_path
    end

    def self.create(user, object)
      object = object.entity if Node === object
      hash = {"user"=>user, "object"=>object}
      case object
      when NodeEntityAbstract
        obj = NumRu::GfdnaviData.const_get(Node::NODE_TYPES[object.node_type].camelcase+"Local").new(hash)
      when VirtualData
        if object.array?
          obj = NumRu::GfdnaviData::ArrayLocal.new(hash)
        else
          if object.type == "draw"
            obj = NumRu::GfdnaviData::ImageLocal.new(hash)
          else
            obj = NumRu::GfdnaviData::VariableLocal.new(hash)
          end
        end
      when ::Array, ActiveRecord::Associations::AssociationCollection
        obj = NumRu::GfdnaviData::ArrayLocal.new(hash)
      else
        raise "type (#{object.class}) is invalid"
      end
      return obj
    end

    def self.get_variable_nodes(obj)
      if obj.respond_to?(:virtual_nodes)
        return obj.virtual_nodes
      end
      variables = ::Array.new
      if obj.is_a?(::Variable)
        variables << obj
      elsif obj.respond_to?(:each)
        obj.each do |v|
          if v.is_a?(::Variable)
            variables << v
          elsif v.is_a?(Node) && v.variable?
            variables << v.entity
          elsif v.respond_to?(:virtual_nodes)
            variables += v.virtual_nodes
          else
            raise "#{v.class.to_s} is not supported"
          end
        end
      else
        raise "#{obj.class.to_s} is not supported"
      end
      return variables.uniq
    end


    # instance methods

    def init(hash)
      @user = User.find(:first, :conditions => ["login=?",user]) if String === @user
      unless @object
      
        @object = eval("::#{self.class.const_get(:OBJECT_CLASS).name}").new
        @new_data = true
      else
        @new_data = false
      end
    end

    def local?
      true
    end

    def remote?
      false
    end

    def get_object
      return @object
    end

    def to_hash(opts={})
      @object.to_hash(opts)
    end

    def to_rb(opts={})
      if @object.respond_to?(:to_rb)
        @object.to_rb(opts)
      else
        uri_prefix = opts[:uri_prefix]
        minimal = opts[:minimal]
        pa = @object.path
        pa = File.join(uri_prefix, "data", pa) if uri_prefix
        code = ""
        unless minimal
          code = <<-EOF
require "numru/gfdnavi_data"
include NumRu

        EOF
        end
        code << <<-EOF
data = GfdnaviData.open("#{pa}")
        EOF
      end
    end

    def user=(user)
      @user = user
    end

    def virtual_nodes
      NumRu::GfdnaviData::Local.get_variable_nodes(@object)
    end

    %w(name path mtime owner other_mode rgroups wgroups size guest_owner file other_readable groups_readable title description visibility).each do |me|
      def_delegator :@object, me+"="
      def_delegator :@object, me
    end

    def save_as(path, user=nil)
      unless NumRu::GfdnaviData::Variable === self || NumRu::GfdnaviData::Image === self || NumRu::GfdnaviData::Knowledge === self
        raise "saving is not supported for array"
      end
      user ||= @user
      unless user
        raise "user must be specified"
      end
      @object.save_as(path, user)
    end

    def delete
      unless @user
        raise "user must be specified"
      end
      unless NodeEntityAbstract === @object && !(::Variable  === @object)
        raise "cannot delete this data: #{@object.class}"
      end
      if @user.super_user? || @object.owner == @user
        @object.destroy_all
      else
        raise "cannot delete: Permission denied"
      end
    end

    private
    def str_to_hash(hash, key, val)
      if /\A([^\[]+)\[(.+)\]\z/ =~ key
        key = $1
        keys = $2.split("][")
        keys.unshift key
        keys[0..-2].each do |k|
          hash = (hash[k] ||= Hash.new)
        end
        hash[keys[-1]] = val
      else
        hash[key] = val
      end
    end

    def str_to_options(str)
      opts = Hash.new
      while /\A([^=]+)=(.*)\z/ =~ str
        key = $1
        val = $2
        if /\A([^=]*),([^,=]+=.*)\z/ =~ val
          val = $1
          str = $2
        else
          str = ""
        end
        val = val.split(",") if /,/ =~ val
        str_to_hash(opts, key, val)
      end
      return opts
    end

    def get_attribute(name)
      #    get_object(path, @user)
      case name
      when /\A(?:variable|image)_nodes\z/
        @object.send(name, :user => @user)
      when "children"
        @object.send(name, false, :user => @user)
      else
        @object.send(name)
      end
    end

    def create(obj)
      NumRu::GfdnaviData::Local.create(@user, obj)
    end

  end
end
