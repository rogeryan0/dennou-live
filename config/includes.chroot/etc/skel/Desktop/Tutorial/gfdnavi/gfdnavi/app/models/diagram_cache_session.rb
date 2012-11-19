class DiagramCacheSession < ActiveRecord::Base
  belongs_to :diagram_cache

  def initialize(*arg)
    super(*arg)
    self.share = true
  end
end
