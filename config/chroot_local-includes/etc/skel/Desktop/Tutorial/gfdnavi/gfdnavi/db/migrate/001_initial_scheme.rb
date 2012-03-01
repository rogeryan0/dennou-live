# -*- coding: cp932 -*-
class InitialScheme < ActiveRecord::Migration

  def self.up
    create_table "nodes", :force => true do |t|
      t.column "parent_id", :integer
      t.column "name", :string, :null => false
      t.column "path", :string, :null => false
      t.column "node_type", :integer, :null => false
               # 0: directory
               # 1: variable, 2: image
               # 3: knowledge, 4: function, 5: draw_method
      t.column "mtime", :datetime, :null => false
      t.column "owner_id", :integer, :null => false
      t.column "other_mode", :integer, :default => 4, :null => false
      t.column "rgroups", :integer, :length => 12, :default => 0  # 64bit
      t.column "wgroups", :integer, :length => 12, :default => 0  # 64bit
      t.column "size", :integer, :length => 12                    # 64bit
      t.column "guest_owner_id", :integer
      t.column "file", :string
      t.column "other_readable", :boolean, :null => false
      t.column "groups_readable", :integer, :length=>12, :null=>false #64bit
      t.string "title", :null => true
      t.text   "description", :null => true
      t.integer "directory_num", :default => 0
      t.integer "variable_num", :default => 0
      t.integer "image_num", :default => 0
      t.integer "knowledge_num", :default => 0
      t.integer "function_num", :default => 0
      t.integer "draw_method_num", :default => 0
      t.timestamps
    end
    add_index :nodes, :path, :unique => true

    create_table "guest_owners", :force => true do |t|
      t.column "name", :string, :null => false
      t.column "email", :string
      t.column "identifier", :string, :null => false
    end

    create_table "directories", :force => true do |t|
      t.column "node_id", :integer, :null => false
      t.column "plain_file", :boolean, :default => false, :null => false
      t.column "downloadable", :boolean, :default => true, :null => false
    end

    create_table "actual_files", :force => true do |t|
      t.column "variable_id", :integer, :null => false
      t.column "path", :string, :null => false
    end

    create_table "variables", :force => true do |t|
      t.column "node_id", :integer, :null => false
    end

    create_table "images", :force => true do |t|
      t.column "node_id", :integer, :null => false
      t.column "org_path", :text
    end

    create_table "knowledges", :force => true do |t|
      t.column "node_id", :integer, :null => false
      t.column "category", :string, :null => false
      t.column "creator", :string
      t.column "textbody", :text, :null => false
      t.column "default_layout", :integer, :null => false
      t.column "horizontal_figures", :integer, :null => false
      t.column "figures_size_height_or_width", :integer, :null => false
               # 0: height 1: width
      t.column "figures_size_units", :integer, :null => false
               # 0: %      1: px
      t.column "figures_size_number", :integer, :null => false
      t.column "comment_on", :integer # コメント元になったKnowledgeのnode_idを格納
      t.column "comment_number", :integer # 何番目のコメントか
    end

    create_table "knowledge_figures", :force => true do |t|
      t.column "knowledge_id", :integer, :null => false # Knowledge テーブルの id
#      t.column "knowledge_node_id", :integer # Knowledge テーブルの node_id
      t.column "image_path", :text, :null => false
      t.column "caption", :text, :null => false
    end

    create_table "knowledge_backups", :force => true do |t|
      t.column "category", :string
      t.column "creator", :string
      t.column "textbody", :text, :null => false
      t.column "default_layout", :integer, :null => false
      t.column "horizontal_figures", :integer, :null => false
      t.column "figures_size_height_or_width", :integer, :null => false
               # 0: height 1: width
      t.column "figures_size_units", :integer, :null => false
               # 0: %      1: px
      t.column "figures_size_number", :integer, :null => false
      t.column "comment_on", :integer
      t.column "comment_number", :integer
      t.column "backup_on", :string, :null => false  # Knowledge の path
      t.column "version_number", :integer, :null => false
      t.column "temporary", :boolean, :null => false
      t.column "title", :string, :null => false
      t.column "description", :text
      t.column "mtime", :datetime, :null => false
      t.column "owner_id", :integer, :null => false
      t.column "rgroups", :integer, :length => 12, :default => 0  # 64bit
      t.column "other_mode", :integer, :default => 4, :null => false
    end
        
    create_table "knowledge_figure_backups", :force => true do |t|
      t.column "knowledge_backup_id", :integer, :null => false # KnowledgeBackup の id
      t.column "image_path", :text
      t.column "caption", :text, :null => false
    end

    create_table "node_relations", :force => true do |t|
      t.column "name", :text, :null => false
      t.column "reference", :integer, :null => false
      t.column "referenced_by", :integer, :null => false
    end
    add_index :node_relations, :reference
    add_index :node_relations, :referenced_by

    create_table "keyword_attributes", :force => true do |t|
      t.column "node_id", :integer, :null => false
      t.column "name", :string, :null => false
      t.column "value", :text
      t.column "num_value", :binary
      t.column "lang", :string
    end

    create_table "spatial_and_time_attributes", :force => true do |t|
      t.column "node_id", :integer, :null => false
      t.column "longitude_lb", :float, :default => nil 
      t.column "latitude_lb", :float, :default => nil
      t.column "longitude_rt", :float, :default => nil
      t.column "latitude_rt", :float, :default => nil
      t.column "starttime", :datetime, :default => nil
      t.column "endtime", :datetime, :default => nil
    end

    create_table "value_types", :force => true do |t|
      t.column "name", :string, :null => false
    end

    create_table "functions", :force => true do |t|
      t.column "node_id", :integer, :null => false
      t.column "description", :text
      t.column "nvars", :integer, :null => false
      t.column "script", :text, :null => false
      t.column "setting_html", :text
      t.column "default", :boolean, :default => false
    end

    create_table "function_outputs", :force => true do |t|
      t.column "function_id", :integer, :null => false
      t.column "name", :string, :null => false
      t.column "description", :text
      t.column "subscript", :string, :null => false
      t.column "position", :integer
    end

    create_table "function_arguments", :force => true do |t|
      t.column "function_id", :integer, :null => false
      t.column "description", :text
      t.column "value_type_id", :integer, :null => false
      t.column "default", :string
      t.column "position", :integer
    end

    create_table "draw_methods", :force => true do |t|
      t.column "node_id", :integer, :null => false
      t.column "description", :text
      t.column "ndims", :integer, :null => false
      t.column "nvars", :integer, :null => false
      t.column "vizshot_method", :string, :null => false
      t.column "script", :text
      t.column "ggraph", :text
      t.column "setting_html", :text
      t.column "default", :boolean, :default => false
    end

    create_table "draw_method_options", :force => true do |t|
      t.column "draw_method_id", :integer, :null => false
      t.column "name", :string, :null => false
      t.column "value_type_id", :integer, :null => false
      t.column "default", :string
      t.column "min", :string
      t.column "max", :string
      t.column "optional", :boolean, :default => false, :null => false
      t.column "parser", :string
      t.column "description", :text
    end

    create_table "users", :force => true do |t|
      t.column "login", :string, :null => false
      t.column "password", :string, :null => false
      t.column "full_name", :string, :null => false
      t.column "email_address", :string, :null => false
      t.column "affiliation", :string, :null => false
      t.column "super_user", :boolean, :default => false, :null => false
      t.column "groups", :integer, :length => 12, :default => 0
      t.column "openid", :boolean
      t.column "internal_user", :boolean, :default => false, :null => false
    end

    create_table "sign_up_users", :force => true do |t|
      t.column "login", :string, :null => false
      t.column "password", :string, :null => false
      t.column "full_name", :string, :null => false
      t.column "email_address", :string, :null => false
      t.column "affiliation", :string, :null => false
      t.column "openid", :boolean
    end

    create_table "groups", :force => true do |t|
      t.column "name", :string, :null => false
      t.column "owner_id", :integer, :null => false
      t.column "description", :string, :null => true
    end

##    create_table "group_members", :force => true do |t|
##      t.column "group_id", :integer, :null => false
##      t.column "user_id", :integer, :null => false
##    end

    create_table "diagram_caches", :force => true do |t|
      t.column "path", :string, :null => false
      t.column "files_yaml", :text
      t.column "label", :string
    end

    create_table "diagram_cache_data", :force => true do |t|
      t.column "diagram_cache_id", :integer, :null => false
      t.column "variable_id", :integer, :null => false
    end

=begin
    create_table "diagram_cache_sessions", :force => true do |t|
      t.column "diagram_cache_id", :integer, :null => false
      t.column "session", :string, :null => false
      t.column "share", :boolean, :default => true, :null => false
    end
=end

    create_table "draw_parameters", :force => true do |t|
      t.column "node_id", :integer, :null => false
      t.column "name", :string, :null => false
      t.column "value", :string, :null => false
    end

    create_table "query_histories", :force => true do |t|
      t.column "args",         :string
      t.column "querytype",    :string
      t.column "user_id",      :integer
      t.column "time",         :datetime
      t.column "description",  :string
      t.column "queryset_id",  :int
      t.column "conditions",  :text
      t.column "count",        :integer
      t.column "session_id",		:string
    end

    create_table "node_lineages", :force => true do |t|
      t.column "ancestor",	:integer, :null=>false
      t.column "descendant", :integer, :null=>false
      t.column "rel_depth",	:integer, :default=>1, :null=>false
    end
    add_index :node_lineages, :ancestor
    add_index :node_lineages, :descendant

  end

  def self.down

    drop_table :nodes rescue nil
    drop_table :guest_owners rescue nil
    drop_table :directories rescue nil
    drop_table :actual_files rescue nil
    drop_table :variables rescue nil
    drop_table :images rescue nil
    drop_table :knowledges rescue nil
    drop_table :knowledge_figures rescue nil
    drop_table :node_relations rescue nil
    drop_table :keyword_attributes rescue nil
    drop_table :spatial_and_time_attributes rescue nil
    drop_table :draw_method_options rescue nil
    drop_table :draw_methods rescue nil
    drop_table :function_outputs rescue nil
    drop_table :function_arguments rescue nil
    drop_table :functions rescue nil
    drop_table :value_types rescue nil
    drop_table :users rescue nil
    drop_table :sign_up_users rescue nil
    drop_table :groups rescue nil
##    drop_table :group_members rescue nil
    drop_table :diagram_caches rescue nil
    drop_table :diagram_cache_data rescue nil
#    drop_table :diagram_cache_sessions rescue nil
    drop_table :draw_parameters rescue nil
    drop_table :query_histories rescue nil
    drop_table :node_lineages rescue nil

  end
end
