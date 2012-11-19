class KnowledgeFigure < ActiveRecord::Base
  belongs_to :knowledge
  belongs_to :image, :class_name=>"Image", :foreign_key=>'image_id'

  validates_length_of :caption, :within => 0..5000

  alias :__image__ :image
  def image
    image = __image__
p "knowledge_figure.rb / image: #{image}"
    raise("#{path} is a GPhys, not a figure.") if image.respond_to?("gphysizable") && image.gphysizable?
    image
  end

  def link_of_figure
    self.image_path
  end

end


