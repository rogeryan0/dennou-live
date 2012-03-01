# This file is auto-generated from the current state of the database. Instead of editing this file, 
# please use the migrations feature of Active Record to incrementally modify your database, and
# then regenerate this schema definition.
#
# Note that this schema.rb definition is the authoritative source for your database schema. If you need
# to create the application database on another system, you should be using db:schema:load, not running
# all the migrations from scratch. The latter is a flawed and unsustainable approach (the more migrations
# you'll amass, the slower it'll run and the greater likelihood for issues).
#
# It's strongly recommended to check this file into your version control system.

ActiveRecord::Schema.define(:version => 3) do

  create_table "actual_files", :force => true do |t|
    t.integer "variable_id", :null => false
    t.string  "path",        :null => false
  end

  create_table "diagram_cache_data", :force => true do |t|
    t.integer "diagram_cache_id", :null => false
    t.integer "variable_id",      :null => false
  end

  create_table "diagram_caches", :force => true do |t|
    t.string "path",       :null => false
    t.text   "files_yaml"
    t.string "label"
  end

  create_table "directories", :force => true do |t|
    t.integer "node_id",                         :null => false
    t.boolean "plain_file",   :default => false, :null => false
    t.boolean "downloadable", :default => true,  :null => false
  end

  create_table "draw_method_options", :force => true do |t|
    t.integer "draw_method_id",                    :null => false
    t.string  "name",                              :null => false
    t.integer "value_type_id",                     :null => false
    t.string  "default"
    t.string  "min"
    t.string  "max"
    t.boolean "optional",       :default => false, :null => false
    t.string  "parser"
    t.text    "description"
  end

  create_table "draw_methods", :force => true do |t|
    t.integer "node_id",                           :null => false
    t.text    "description"
    t.integer "ndims",                             :null => false
    t.integer "nvars",                             :null => false
    t.string  "vizshot_method",                    :null => false
    t.text    "script"
    t.text    "ggraph"
    t.text    "setting_html"
    t.boolean "default",        :default => false
  end

  create_table "draw_parameters", :force => true do |t|
    t.integer "node_id", :null => false
    t.string  "name",    :null => false
    t.string  "value",   :null => false
  end

  create_table "function_arguments", :force => true do |t|
    t.integer "function_id",   :null => false
    t.text    "description"
    t.integer "value_type_id", :null => false
    t.string  "default"
    t.integer "position"
  end

  create_table "function_outputs", :force => true do |t|
    t.integer "function_id", :null => false
    t.string  "name",        :null => false
    t.text    "description"
    t.string  "subscript",   :null => false
    t.integer "position"
  end

  create_table "functions", :force => true do |t|
    t.integer "node_id",                         :null => false
    t.text    "description"
    t.integer "nvars",                           :null => false
    t.text    "script",                          :null => false
    t.text    "setting_html"
    t.boolean "default",      :default => false
  end

  create_table "groups", :force => true do |t|
    t.string  "name",        :null => false
    t.integer "owner_id",    :null => false
    t.string  "description"
  end

  create_table "guest_owners", :force => true do |t|
    t.string "name",       :null => false
    t.string "email"
    t.string "identifier", :null => false
  end

  create_table "images", :force => true do |t|
    t.integer "node_id",  :null => false
    t.text    "org_path"
  end

  create_table "keyword_attributes", :force => true do |t|
    t.integer "node_id",   :null => false
    t.string  "name",      :null => false
    t.text    "value"
    t.binary  "num_value"
    t.string  "lang"
  end

  create_table "knowledge_backups", :force => true do |t|
    t.string   "category"
    t.string   "creator"
    t.text     "textbody",                                    :null => false
    t.integer  "default_layout",                              :null => false
    t.integer  "horizontal_figures",                          :null => false
    t.integer  "figures_size_height_or_width",                :null => false
    t.integer  "figures_size_units",                          :null => false
    t.integer  "figures_size_number",                         :null => false
    t.integer  "comment_on"
    t.integer  "comment_number"
    t.string   "backup_on",                                   :null => false
    t.integer  "version_number",                              :null => false
    t.boolean  "temporary",                                   :null => false
    t.string   "title",                                       :null => false
    t.text     "description"
    t.datetime "mtime",                                       :null => false
    t.integer  "owner_id",                                    :null => false
    t.integer  "rgroups",                      :default => 0
    t.integer  "other_mode",                   :default => 4, :null => false
  end

  create_table "knowledge_figure_backups", :force => true do |t|
    t.integer "knowledge_backup_id", :null => false
    t.text    "image_path"
    t.text    "caption",             :null => false
  end

  create_table "knowledge_figures", :force => true do |t|
    t.integer "knowledge_id", :null => false
    t.text    "image_path",   :null => false
    t.text    "caption",      :null => false
  end

  create_table "knowledges", :force => true do |t|
    t.integer "node_id",                      :null => false
    t.string  "category",                     :null => false
    t.string  "creator"
    t.text    "textbody",                     :null => false
    t.integer "default_layout",               :null => false
    t.integer "horizontal_figures",           :null => false
    t.integer "figures_size_height_or_width", :null => false
    t.integer "figures_size_units",           :null => false
    t.integer "figures_size_number",          :null => false
    t.integer "comment_on"
    t.integer "comment_number"
  end

  create_table "node_lineages", :force => true do |t|
    t.integer "ancestor",                  :null => false
    t.integer "descendant",                :null => false
    t.integer "rel_depth",  :default => 1, :null => false
  end

  add_index "node_lineages", ["ancestor"], :name => "index_node_lineages_on_ancestor"
  add_index "node_lineages", ["descendant"], :name => "index_node_lineages_on_descendant"

  create_table "node_relations", :force => true do |t|
    t.text    "name",          :null => false
    t.integer "reference",     :null => false
    t.integer "referenced_by", :null => false
  end

  add_index "node_relations", ["reference"], :name => "index_node_relations_on_reference"
  add_index "node_relations", ["referenced_by"], :name => "index_node_relations_on_referenced_by"

  create_table "nodes", :force => true do |t|
    t.integer  "parent_id"
    t.string   "name",                           :null => false
    t.string   "path",                           :null => false
    t.integer  "node_type",                      :null => false
    t.datetime "mtime",                          :null => false
    t.integer  "owner_id",                       :null => false
    t.integer  "other_mode",      :default => 4, :null => false
    t.integer  "rgroups",         :default => 0
    t.integer  "wgroups",         :default => 0
    t.integer  "size"
    t.integer  "guest_owner_id"
    t.string   "file"
    t.boolean  "other_readable",                 :null => false
    t.integer  "groups_readable",                :null => false
    t.string   "title"
    t.text     "description"
    t.integer  "directory_num",   :default => 0
    t.integer  "variable_num",    :default => 0
    t.integer  "image_num",       :default => 0
    t.integer  "knowledge_num",   :default => 0
    t.integer  "function_num",    :default => 0
    t.integer  "draw_method_num", :default => 0
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  add_index "nodes", ["path"], :name => "index_nodes_on_path", :unique => true

  create_table "open_id_authentication_associations", :force => true do |t|
    t.integer "issued"
    t.integer "lifetime"
    t.string  "handle"
    t.string  "assoc_type"
    t.binary  "server_url"
    t.binary  "secret"
  end

  create_table "open_id_authentication_nonces", :force => true do |t|
    t.integer "timestamp",  :null => false
    t.string  "server_url"
    t.string  "salt",       :null => false
  end

  create_table "query_histories", :force => true do |t|
    t.string   "args"
    t.string   "querytype"
    t.integer  "user_id"
    t.datetime "time"
    t.string   "description"
    t.integer  "queryset_id"
    t.text     "conditions"
    t.integer  "count"
    t.string   "session_id"
  end

  create_table "sessions", :force => true do |t|
    t.string   "session_id",                     :null => false
    t.text     "data",       :limit => 16777215
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  add_index "sessions", ["session_id"], :name => "index_sessions_on_session_id"
  add_index "sessions", ["updated_at"], :name => "index_sessions_on_updated_at"

  create_table "sign_up_users", :force => true do |t|
    t.string  "login",         :null => false
    t.string  "password",      :null => false
    t.string  "full_name",     :null => false
    t.string  "email_address", :null => false
    t.string  "affiliation",   :null => false
    t.boolean "openid"
  end

  create_table "spatial_and_time_attributes", :force => true do |t|
    t.integer  "node_id",      :null => false
    t.float    "longitude_lb"
    t.float    "latitude_lb"
    t.float    "longitude_rt"
    t.float    "latitude_rt"
    t.datetime "starttime"
    t.datetime "endtime"
  end

  create_table "users", :force => true do |t|
    t.string  "login",                            :null => false
    t.string  "password",                         :null => false
    t.string  "full_name",                        :null => false
    t.string  "email_address",                    :null => false
    t.string  "affiliation",                      :null => false
    t.boolean "super_user",    :default => false, :null => false
    t.integer "groups",        :default => 0
    t.boolean "openid"
    t.boolean "internal_user", :default => false, :null => false
  end

  create_table "value_types", :force => true do |t|
    t.string "name", :null => false
  end

  create_table "variables", :force => true do |t|
    t.integer "node_id", :null => false
  end

end
