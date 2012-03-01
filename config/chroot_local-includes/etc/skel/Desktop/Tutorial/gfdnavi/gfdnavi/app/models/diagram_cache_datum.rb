class DiagramCacheDatum < ActiveRecord::Base
  belongs_to :diagram_cache
  belongs_to :variable
end
