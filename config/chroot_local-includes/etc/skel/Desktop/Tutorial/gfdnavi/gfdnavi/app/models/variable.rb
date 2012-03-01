require "narray_gfdnavi"
require "numru/gphys"
require "numru/netcdf"

class Variable < NodeEntityAbstract

  has_many :diagram_caches, :through => :diagram_cache_data, :source => :diagram_cache
  has_many :diagram_cache_data, :dependent => :destroy
  has_many :actual_files, :dependent => :destroy
  has_many :knowledge_figures

  after_create :save_references


  def vname
    if vn = node.file
      return node.path.sub(/#{vn}\//,"")
    else
      return name
    end
  end

  def fname
    if node.file
      node.add_prefix(node.file)
    elsif (acs = actual_files).length > 0
      acs.collect{|ac| node.add_prefix(ac.path) }
    else
      raise "[BUG]"
    end
  end

  def to_gphys(dummy=false)
    gp = NumRu::GPhys::IO.open(fname, vname)
    if dummy
      gp = NumRu::GPhysDummy.new(gp.instance_variable_get(:@grid), 
                                 NumRu::VArrayDummy.new)
    end
    gp
  end

  def references_tmp
    unless @new_record
      raise "Cannot #call references_tmp for saved data. Use references instead"
    end
    @references_tmp ||= Array.new
  end

  def downloadable?
    if parent
      return parent.entity.downloadable?
    else
      da = true
      references_tmp.each{|v|
        unless v.downloadable?
          da = false
          break
        end
      }
      return da
    end
  end

  def to_hash(opts={}, &b)
    hash = super(opts, &b)
    opts = opts.dup
    uri_prefix = opts.delete(:uri_prefix)
    hash["plot"] = {"url" => File.join(uri_prefix, "data", path, "plot({draw_method_name};{draw_method_options})"), "draw_method_name" => "{draw_method_name}", "draw_method_options" => "{draw_method_options}"}
    hash["analysis"] = {"url" => File.join(uri_prefix, "data", path, "analysis({function_name};{function_arguments})"), "function_name" => "{function_name}", "function_arguments" => "{function_arguments}"}
    hash["cut"] = {"url" => File.join(uri_prefix, "data", path, "cut({arguments})"), "arguments" => "{arguments}"}
    return hash
  end


  protected
  def save_references
    if @references_tmp
      @references_tmp.each{|v|
        v = v.node unless Node === v
        NodeRelation.new(:name=>"analysis", :reference=>v, :referenced_by=>self.node).save!
      }
    end
  end

end
