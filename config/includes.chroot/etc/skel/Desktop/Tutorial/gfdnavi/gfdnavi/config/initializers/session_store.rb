# Be sure to restart your server when you modify this file.

# Your secret key for verifying cookie session data integrity.
# If you change this key, all old sessions will become invalid!
# Make sure the secret is at least 30 characters and all random, 
# no regular words or you'll be exposed to dictionary attacks.
ActionController::Base.session = {
  :key         => '_gfdnavi_session',
  :secret      => 'c0a35dfd4afc0a62731d95094d3949f4f284e06fbc5c37e02233b13641b43625eba1172ced702bc40df995ad55b807c28776596d980ed6487cef3059d237429e'
}

# Use the database for sessions instead of the cookie-based default,
# which shouldn't be used to store highly confidential information
# (create the session table with "rake db:sessions:create")
# ActionController::Base.session_store = :active_record_store
