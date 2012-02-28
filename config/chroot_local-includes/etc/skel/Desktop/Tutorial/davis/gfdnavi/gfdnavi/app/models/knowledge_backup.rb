class KnowledgeBackup < ActiveRecord::Base

  has_many :knowledge_figure_backups, :dependent => :destroy

  validates_length_of :title, :within => 0..400
  validates_length_of :textbody, :within => 0..100000
  
  #<< class methods >>

  class << self
    def copy_from_knowledge(knowledge, type, user, groups, version_number)
      backup = KnowledgeBackup.new
      backup.attributes = {
        "title" => knowledge.title,
        "creator" => knowledge.creator,
        "textbody" => knowledge.textbody,
        "description" => knowledge.description,      
        "default_layout" => knowledge.default_layout || 0,
        "horizontal_figures" => knowledge.horizontal_figures,
        "figures_size_height_or_width" => knowledge.figures_size_height_or_width,
        "figures_size_units" => knowledge.figures_size_units,
        "figures_size_number" => knowledge.figures_size_number,
        "comment_on" => knowledge.comment_on,
        "comment_number" => knowledge.comment_number,
        "backup_on" => knowledge.path || "temporarily_backup",
        "mtime" => knowledge.mtime,
        "backup_on" => knowledge.path,
        "version_number" => version_number,
        "temporary" => (type == "temporary_save"),
        "owner_id" => user.id,
        "rgroups" => groups,
        "other_mode" => knowledge.other_mode
      }
      return backup
    end

    def new_comment_number_from_parent_document(parent_knowledge_id, user)
      comments = Knowledge.find(:all, :conditions => ["comment_on = ? AND comment_number IS NOT NULL", parent_knowledge_id], :user => user)
      if comments.length > 0
        comments.collect! {|comment|
          comment = comment.comment_number
        }
        comment_number = comments.max + 1
      else
        comment_number = 1
      end
    end

    # copy of ./node.rb
    def set_rgroups(*groups)
      groups = groups[0] if groups.length==1 and groups[0].is_a?(Array)
      self.rgroups = Group.bit_mask_for(*groups)
    end
  end
end



