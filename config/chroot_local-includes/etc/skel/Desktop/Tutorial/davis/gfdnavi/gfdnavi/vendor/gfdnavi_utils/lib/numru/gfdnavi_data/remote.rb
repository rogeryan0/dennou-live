require "net/http"
require "cgi"
require "yaml"

require "numru/gfdnavi_data/array_remote"
require "numru/gfdnavi_data/directory_remote"
require "numru/gfdnavi_data/variable_remote"
require "numru/gfdnavi_data/image_remote"
require "numru/gfdnavi_data/knowledge_remote"


module NumRu::GfdnaviData

  module Remote

    @@object_cache = Hash.new
    @@http_cache = Hash.new

    @@node_methods = %w(owner mtime other_mode rgroups wgroups size other_readable groups_readable title description visibility)


    # module functions

    def self.parse_url(url, user=nil, password=nil)
      (password ||= NumRu::GfdnaviData::Remote.get_password(url, user)) if user
      obj = self.get_object(url, user, password)
      dtype = obj["type"]
      dtype = "array" if /array/ =~ dtype
      create(url, user, password, obj, dtype)
    end

    def self.create(url, user, password, obj, dtype)
      hash = {"url"=>url, "user"=>user, "object"=>obj}
      case dtype
      when "array"
        gr = NumRu::GfdnaviData::ArrayRemote.new(hash)
      when "directory"
        gr = NumRu::GfdnaviData::DirectoryRemote.new(hash)
      when "variable"
        gr = NumRu::GfdnaviData::VariableRemote.new(hash)
      when "image"
        gr = NumRu::GfdnaviData::ImageRemote.new(hash)
      when "knowledge"
        gr = NumRu::GfdnaviData::KnowledgeRemote.new(hash)
      else
        raise "data type (#{dtype}) is invalid"
      end
      gr.password = password
      return gr
    end

    def self.http_request(klass, url, user, passwd, suffix="yml", data=nil)
      /\A(https?:\/\/[^\/]+)(\/.*)\z/ =~ url
      path = $2
      u = URI.parse($1)
      protocol = u.scheme
      host = u.host
      port = u.port
      key_http = host+port.to_s
      http = @@http_cache[key_http] || (@@http_cache[key_http] = Net::HTTP.new(host,port) )
      path = CGI.escape(path).gsub(/\%2F/,"/")

      unless /\.#{suffix}\z/ =~ path
        path = path + "." + suffix
      end
      req = klass.new(path)
      req.basic_auth(user, passwd) if user

      if data
        data = data.map{|k,v| "#{k}=#{CGI.escape(v ? v.to_s : "")}"}.join("&")
      end
      res = http.request(req, data)
      case res.code
      when "200"
        obj = res.body
        case suffix
        when "yml", "knlge"
          obj = YAML.load(obj)
          unless Hash === obj
            raise "object is invalid"
          end
        end
        return obj
      when "302"
        url = res.header["Location"]
        return NumRu::GfdnaviData::Remote.get_object(url, user, passwd, suffix)
      else
        #raise "http failed: code=#{res.code}"
        raise "http failed: code=#{res.code}, body='#{res.body}'"
      end
    end

    def self.get_object(url, user, passwd, suffix="yml")
      cache_key = [url, user, suffix]
      if obj = @@object_cache[cache_key]
        return obj
      end
      obj = NumRu::GfdnaviData::Remote.http_request(Net::HTTP::Get, url, user, passwd, suffix)
      @@object_cache[cache_key] = obj
      obj
    end


    def self.save_object(url, user, passwd, hash)
      NumRu::GfdnaviData::Remote.http_request(Net::HTTP::Put, url, user, passwd, "yml", hash)
    end

    def self.delete_object(url, user, passwd)
      NumRu::GfdnaviData::Remote.http_request(Net::HTTP::Delete, url, user, passwd)
    end

    def self.get_password(url, user)
      if user
        unless /mswin(?!ce)|mingw|bccwin/ =~ RUBY_PLATFORM
          system("stty -echo > /dev/null 2>&1")
        end
        $stderr.print "\ninput password of #{user} for #{url} : "
        passwd = $stdin.gets
        passwd.chomp! if passwd
        unless /mswin(?!ce)|mingw|bccwin/ =~ RUBY_PLATFORM
          system("stty echo > /dev/null 2>&1")
        end
        $stderr.print "\n"
        return passwd
      end
      return nil
    end


    # instance methods

    def init(hash)
      @url = hash.delete("url")
      if @url || @object
        @new_data = false
      else
        @new_data = true
        @object = Hash.new
      end
      @representation = Hash.new
    end

    def local?
      false
    end

    def remote?
      true
    end

    def password=(passwd)
      @password = passwd
    end

    def password
      @password
    end

    def get_object(suffix=nil)
      if @new_data
        return @object
      else
        @password ||= NumRu::GfdnaviData::Remote.get_password(@url, @user) if @user
        if suffix && suffix != "yml"
          unless (obj = @representation[suffix])
            obj = NumRu::GfdnaviData::Remote.get_object(@url, @user, @password, suffix)
            @representation[suffix] = obj
          end
          obj
        else
          @object ||= NumRu::GfdnaviData::Remote.get_object(@url, @user, @password)
        end
      end
    end

    def save_as_object(new_path, user)
      if /\A\// !~ new_path
        new_path = File.join("/usr", user, new_path)
      end
      new_url = url.sub(/#{Regexp.escape(path)}/, new_path)
      hash = Hash.new
      hash["orig_path"] = path
      hash["owner"] = user
      hash["save_as"] = "1"
      @password ||= NumRu::GfdnaviData::Remote.get_password(new_url, user)
      NumRu::GfdnaviData::Remote.save_object(new_url, user, @password, hash)
    end

    def save_object
      hash = Hash.new
      hash["save"] = "1"
      hash["class"] = self.class.name.sub(/Remote/,"")
      @@node_methods.each do |mn|
        if (v = @object[mn])
          hash[mn] = v
        end
      end
      update_save_data(hash)
      @password ||= NumRu::GfdnaviData::Remote.get_password(url, user)
      res = NumRu::GfdnaviData::Remote.save_object(url, user, @password, hash)
      @data_new = false
      res
    end

    def delete_object
      @password ||= NumRu::GfdnaviData::Remote.get_password(url, @user)
      NumRu::GfdnaviData::Remote.delete_object(url, @user, @password)
    end

    def url
      return @url if @url
      get_object unless @object
      return @object["url"]
    end
    def url=(url)
      @url = url
    end

    def location
      return @location ? @location : nil
    end
    def location=(location)
      @location = location
    end

    def user=(user)
      @user = user
    end

    def to_rb
      get_object("rb")
    end

    @@node_methods.each do |me|
      eval <<-EOL, binding, __FILE__, __LINE__+1
    def #{me}
      @object["#{me}"]
    end
    def #{me}=(arg)
      @object["#{me}"] = arg
    end
      EOL
    end

    def save_as(path, user)
      unless NumRu::GfdnaviData::VariableRemote === self || NumRu::GfdnaviData::ImageRemote === self
        raise "not supported"
      end
      save_as_object(path, user)
    end

    def save
      unless NumRu::GfdnaviData::DirectoryRemote === self || NumRu::GfdnaviData::VariableRemote === self || NumRu::GfdnaviData::ImageRemote === self || NumRu::GfdnaviData::KnowledgeRemote === self
        raise "not supported"
      end
      save_object
    end

    def delete
      unless NumRu::GfdnaviData::DirectoryRemote === self || NumRu::GfdnaviData::VariableRemote === self || NumRu::GfdnaviData::ImageRemote === self || NumRu::GfdnaviData::KnowledgeRemote === self
        raise "not supported"
      end
      delete_object
    end
    alias :destroy :delete



    private
    def get_attribute(name)
      get_object
      @object[name]
    end

    def create(obj)
      obj = obj.dup
      url = obj.delete("url")
      dtype = obj["type"]
      dtype = "array" if /array/ =~ dtype
      raise("object does not have type: #{obj.inspect}") unless dtype
      obj = nil if url
      NumRu::GfdnaviData::Remote.create(url, @user, @password, obj, dtype)
    end

  end
end
