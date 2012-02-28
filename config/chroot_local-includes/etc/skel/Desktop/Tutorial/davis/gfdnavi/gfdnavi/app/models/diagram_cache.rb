class DiagramCache < ActiveRecord::Base
  belongs_to :variable
  has_many :variables, :through => :diagram_cache_data
  has_many :diagram_cache_data, :dependent => :destroy
#  has_many :diagram_cache_sessions, :dependent => :destroy

  def self.find_for_session(id, session_id)
    dc = DiagramCache.find_by_id(id)
    if dc && dc.diagram_cache_sessions.find(:first, :conditions => ["session=?",session_id])
      return dc
    else
      return nil
    end
  end

  def files
    yml = files_yaml
    yml && YAML.load(yml)
  end

  def files=(ary)
    if Array === ary
      self.files_yaml = ary.to_yaml
    end
  end

end
