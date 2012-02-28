class KnowledgeFigureBackup < ActiveRecord::Base
  belongs_to :knowledge_backup
  belongs_to :image, :class_name=>"Image", :foreign_key=>'image_id'

  validates_length_of :caption, :within => 0..5000

  alias :__image__ :image
  def image
    image = __image__
    raise("#{path} is a GPhys, not a figure.") if image.gphysizable?
    image
  end

end


