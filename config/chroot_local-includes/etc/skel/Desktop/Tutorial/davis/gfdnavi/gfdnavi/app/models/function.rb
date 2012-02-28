class Function < NodeEntityAbstract
  has_many :function_outputs, :order => :position, :dependent => :destroy
  has_many :function_arguments, :order => :position, :dependent => :destroy

  validates_presence_of :nvars, :script

  before_save :check_name
  after_update :update_yml
  after_save :update_yml

  def get_script
    result = ""
    function_arguments.length.times{|i|
      arg = function_arguments[i]
      result << " arg#{i} = arg#{i}.#{to_type(arg.name)}\n"
    }
    result << script
    return result
  end

  def update_yml
    return if default
    h = Hash.new
    h[:name] = self.name
    h[:description] = self.description
    h[:nvars] = self.nvars
    h[:script] = self.script
    h[:setting_html] = self.setting_html
    ary = Array.new
    function_arguments.each{|fa|
      h2 = Hash.new
      h2[:description] = fa.description
      h2[:value_type] = (vt=fa.value_type) && vt.name
      h2[:default] = fa.default
      h2[:position] = fa.position
      ary.push h2
    }
    h[:arguments] = ary
    ary = Array.new
    function_outputs.each{|fo|
      h2 = Hash.new
      h2[:name] = fo.name
      h2[:subscript] = fo.subscript
      h2[:description] = fo.description
      ary.push h2
    }
    h[:outputs] = ary
    yml = add_prefix(self.path)+".yml"
    File.open(yml,"w"){|file|
      file.print h.to_yaml
    }
  end

  protected

  def check_name
    func = Function.find(:first, :conditions=>["name=?",node.name],:user=>:all)
    if func && func != self
      errors.add("name", "must be uniq")
      return false
    end
  end

  def to_type(type)
    case type.value_type
    when "int"
      "to_i"
    when "float"
      "to_f"
    when "string"
      "to_s"
    else
      raise "argument type of function is invalid"
    end
  end

end
