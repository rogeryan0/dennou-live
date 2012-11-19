# Be sure to restart your server when you modify this file

# Specifies gem version of Rails to use when vendor/rails is not present
#RAILS_GEM_VERSION = '2.3.8' unless defined? RAILS_GEM_VERSION

# load configurations of gfdnavi in gfdnavi.yml
require File.join(File.dirname(__FILE__), 'load_config')

# Bootstrap the Rails environment, frameworks, and default configuration
require File.join(File.dirname(__FILE__), 'boot')


# $:.unshift File.join(RAILS_ROOT, "vendor/gfdnavi_utils/lib")

Rails::Initializer.run do |config|
  # Settings in config/environments/* take precedence over those specified here.
  # Application configuration should go into files in config/initializers
  # -- all .rb files in that directory are automatically loaded.

  # Add additional load paths for your own custom dirs
  # config.load_paths += %W( #{RAILS_ROOT}/extras )
  if config.respond_to?(:autoload_paths)
    config.autoload_paths += %W( #{RAILS_ROOT}/vendor/gfdnavi_utils/lib )
  else
    config.load_paths += %W( #{RAILS_ROOT}/vendor/gfdnavi_utils/lib )
  end

  # Specify gems that this application depends on and have them installed with rake gems:install
  # config.gem "bj"
  # config.gem "hpricot", :version => '0.6', :source => "http://code.whytheluckystiff.net"
  # config.gem "sqlite3-ruby", :lib => "sqlite3"
  # config.gem "aws-s3", :lib => "aws/s3"
  config.gem "rack"
  config.gem "json"

  # Only load the plugins named here, in the order given (default is alphabetical).
  # :all can be used as a placeholder for all plugins not explicitly named
  # config.plugins = [ :exception_notification, :ssl_requirement, :all ]

  # Skip frameworks you're not going to use (only works if using vendor/rails).
  # To use Rails without a database, you must remove the Active Record framework
  # config.frameworks -= [ :active_record, :active_resource, :action_mailer ]

  # Set Time.zone default to the specified zone and make Active Record auto-convert to this zone.
  # Run "rake -D time" for a list of tasks for finding time zone names.
  config.time_zone = 'UTC'

  # The default locale is :en and all translations from config/locales/*.rb,yml are auto loaded.
  # config.i18n.load_path += Dir[Rails.root.join('my', 'locales', '*.{rb,yml}')]
  # config.i18n.default_locale = :de

  # Your secret key for verifying cookie session data integrity.
  # If you change this key, all old sessions will become invalid!
  # Make sure the secret is at least 30 characters and all random, 
  # no regular words or you'll be exposed to dictionary attacks.
  if config.action_controller.respond_to?("session")
    config.action_controller.session = {
      :session_key => '_test_session',
      :secret      => GFDNAVI_SECRET
    }
  end

  # Use the database for sessions instead of the cookie-based default,
  # which shouldn't be used to store highly confidential information
  # (create the session table with 'rake db:sessions:create')
  config.action_controller.session_store = :active_record_store
  #config.action_controller.session_store = :p_store

  ENV['http_proxy'] = GFDNAVI_PROXY

  if GFDNAVI_ENABLE_EMAIL
    config.action_mailer.delivery_method = :smtp
    config.action_mailer.smtp_settings = GFDNAVI_SMTP_CONFIG
  end
  
end

Mime::Type.register "application/x-netcdf", :nc
Mime::Type.register "image/png", :png
Mime::Type.register "text/yaml", :yml
Mime::Type.register "application/octet-stream", :gphys
Mime::Type.register "text/plain", :rb
Mime::Type.register "text/yaml", :knlge
