class CreateSessions < ActiveRecord::Migration
  def self.up
    create_table :sessions, :force => true do |t|
      t.string :session_id, :null => false
      t.text :data, :limit => 16777215
      t.timestamps
    end

    add_index :sessions, :session_id
    add_index :sessions, :updated_at
  end

  def self.down
    drop_table :sessions
  end
end
