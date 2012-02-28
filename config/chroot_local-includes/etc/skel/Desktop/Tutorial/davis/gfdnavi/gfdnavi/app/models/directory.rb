require "activerecord_gfdnavi"
require "fileutils"

class Directory < NodeEntityAbstract

  def initialize(*arg)
    super(*arg)
    self.downloadable = true
  end

  def downloadable?
    return false unless plain_file
    dir = self
    while (dir)
      return false unless dir.downloadable
      dir = dir.parent
      dir = dir && dir.entity
    end
    return true
  end

  def destroy_all
    FileUtils.remove(fname) if plain_file
    super
  end

  def to_hash(opts={}, &b)
    hash = super(opts, &b)
    opts = opts.dup
    uri_prefix = opts.delete(:uri_prefix)
    user = opts.delete(:user)
    ha = Hash.new
    i = 0
    children(false, :user => user).each{|no|
      ha[i] = {"url" => File.join(uri_prefix, "data", no.path)}
      i += 1
    }
    ha["length"] = i
    ha["type"] = "array"
    hash["children"] = ha
    %w(variable_nodes image_nodes).each{|nname|
      ha = Hash.new
      i = 0
      self.send(nname, :user => user).each{|no|
        ha[i] = {"url" => File.join(uri_prefix, "data", no.path)}
        i += 1
        ha["length"] = i
        ha["type"] = "array"
      }
      hash[nname] = ha
    }
    return hash
  end

  %w(children directory_nodes variable_nodes image_nodes knowledge_nodes).each do |name|
    eval <<EOF
  def #{name}(*args)
    node.#{name}(*args)
  end
EOF
  end

end
