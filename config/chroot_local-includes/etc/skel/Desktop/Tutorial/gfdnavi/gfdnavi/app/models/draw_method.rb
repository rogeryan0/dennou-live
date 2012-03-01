class DrawMethod < NodeEntityAbstract

  has_many :draw_method_options, :dependent => :destroy

  before_save :check_name
  after_save :update_yml
  after_update :update_yml


  def to_xml(opts={})
    super(opts.merge(:include => :draw_method_options))
  end

  def update_yml
    return if default
    h = Hash.new
    h[:name] = self.name
    h[:description] = self.description
    h[:ndims] = self.ndims
    h[:nvars] = self.nvars
    h[:vizshot_method] = self.vizshot_method
    h[:script] = self.script if self.script
    h[:ggraph] = self.ggraph if self.ggraph
    h[:setting_html] = self.setting_html if self.setting_html
    ary = Array.new
    draw_method_options.each{|da|
      h2 = Hash.new
      h2[:name] = da.name
      h2[:value_type] = da.value_type.name
      h2[:default] = da.default
      h2[:optional] = da.optional
      h2[:parser] = da.parser
      ary.push h2
    }
    h[:options] = ary
    yml = add_prefix(self.path)+".yml"
    File.open(yml,"w"){|file|
      file.print h.to_yaml
    }
  end

  protected

  def check_name
    dm = DrawMethod.find(:first, :conditions=>["name=?",node.name], :user=>:all)
    if dm && dm != self
      errors.add("name", "must be uniq")
      return false
    end
  end


end
