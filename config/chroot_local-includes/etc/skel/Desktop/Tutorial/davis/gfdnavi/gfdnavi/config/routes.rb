ActionController::Routing::Routes.draw do |map|
=begin
  map.resources( :nodes,
  :collection => {:top_directory_nodes=>:get},
  :member =>{:parent=>:get},
  :has_many => [:keyword_attributes]) do |node|
    node.resources :children, :controller => :nodes
  end
=end

  # for OpenID
  map.openid "user", :controller => "user", :requirements => {:method => :get}

  # get top node
  map.connect '/data', :controller => "nodes", :action => "index", :conditions => {:method => :get}
  map.connect '/data.:format', :controller => "nodes", :action => "index", :conditions => {:method => :get}
  # get keyword attributes
  map.data_keyword_attributes '/data/*path/keyword_attributes.:format', :controller => "keyword_attributes", :action => "index", :conditions => {:method => :get}
  map.data_keyword_attributes '/data/keyword_attributes.:format', :controller => "keyword_attributes", :action => "index", :conditions => {:method => :get}
  map.connect '/data/*path/keyword_attributes', :controller => "keyword_attributes", :action => "index", :conditions => {:method => :get}
  map.connect '/data/keyword_attributes.:format', :controller => "keyword_attributes", :action => "index", :conditions => {:method => :get}
  map.connect '/data/keyword_attributes', :controller => "keyword_attributes", :action => "index", :conditions => {:method => :get}
  # get node's edit page
  map.connect '/data/*path/edit', :controller => "nodes", :action => "edit", :conditions => {:method => :get}
  # put (update/save) node's information
  map.connect '/data/*path.:format', :controller => "nodes", :action => "update", :conditions => {:method => :put}, :requirements => {:format => /(xml|html|yml)/}
  map.connect '/data/*path', :controller => "nodes", :action => "update", :conditions => {:method => :put}
  # delete node
  map.connect '/data/*path.:format', :controller => "nodes", :action => "delete", :conditions => {:method => :delete}, :requirements => {:format => /(xml|html|yml)/}
  map.connect '/data/*path', :controller => "nodes", :action => "delete", :conditions => {:method => :delete}
  # get node
  map.data '/data/*path.:format', :controller => "nodes", :action => "path", :conditions => {:method => :get}, :requirements => {:format => /(xml|html|yml|gphys|rb|knlge)/}
  map.data_dl '/data/*path', :controller => "nodes", :action => "path", :conditions => {:method => :get}


# draw methods
  map.draw_methods '/draw_methods.:format', :controller => "draw_methods", :action => "index", :conditions => {:method => :get}
  map.connect '/draw_methods', :controller => "draw_methods", :action => "index", :conditions => {:method => :get}
  
  map.connect '/draw_methods.:format', :controller => "draw_methods", :action => "create", :conditions => {:method => :post}
  map.connect '/draw_methods', :controller => "draw_methods", :action => "create", :conditions => {:method => :post}


# functions
  map.functions '/functions.:format', :controller => "functions", :action => "index", :conditions => {:method => :get}
  map.connect '/functions', :controller => "functions", :action => "index", :conditions => {:method => :get}
  
  map.connect '/functions.:format', :controller => "functions", :action => "create", :conditions => {:method => :post}
  map.connect '/functions', :controller => "functions", :action => "create", :conditions => {:method => :post}


  map.connect ':controller/:action/:id'
  map.connect ':controller/:action/:id.:format'

end
