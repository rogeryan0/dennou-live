# -*- coding: japanese-cp932 -*-
require "numru/gfdnavi_data"
require "numru/gfdnavi_data/local"

class NodesController < ApplicationController
  layout "gfdnavi"
#  session :off

#  before_filter :login_required, :only => [:change_mode, :edit]
  before_filter :basic_authorization_required, :only => [:change_mode, :edit] 
  before_filter :basic_authorization, :except => [:change_mode, :edit]


  def index
    user = get_user
    nodes = Node.top_directory_nodes
    list(nodes,user)
  end

  def path
    $options=Hash.new
  
    $options["show_kwfacets"] = params["show_kwfacets"]
    $options["show_spfacets"] = params["show_spfacets"]
    $options["show_kwvalues"] = params["show_kwvalues"]
    $options["show_pathtree"] = params["show_pathtree"]
    $options["no_result"]     = params["no_result"]
    
    path = File.join("",params[:path])
    user = get_user

    unless params[:format]
      if /\A(.+)\.(\w+)\z/ =~ path
        path = $1
        params[:format] = $2
      else
        params[:format] = "html"
#        params[:format] = "yml"
      end
    end

    case params[:format]
    when "knlge"
      path << ".knlge"
    when "png"
      if /\A.+plot\([^)]+\)(?:\[[\d,]+\])?\z/ !~ path && /\/overlay\(/ !~ path
        path << ".png"
      end
    when "nc"
      if /\A.+analysis\([^)]+\)(?:\[[\d,]+\])?\z/ !~ path
        path << ".nc"
      end
    when "xml"
      if /\A(.+find\(.+)\.xml\z/ =~ path
        path = $1
        params[:format] = "xml"
      end
    end

    gl = NumRu::GfdnaviData::Local.parse_path(path, user)
    if  params[:format]&& ($options["show_kwfacets"] || $options["show_spfacets"] || ($options["show_kwfacets"] && $options["show_kwvalues"]) ||  $options["show_pathtree"]  || $options["no_result"])
         
      respond_to  do |format|
        format.xml {
          render :xml => gl
        }
      end
    else
      node = gl.get_object
      case node
      when NodeEntityAbstract, VirtualData
        if params[:format].nil? && download(node,user)
          return
        end
        respond_to do |format|
          format.html {
            case node
            when NodeEntityAbstract
              description(node,user)
            when VirtualData
              if node.type == "draw"
                draw(gl)
              elsif node.type == "node" && (on = node.instance_variable_get(:@original_nodes)).length == 1
                description(on[0], user)
              else
                description_methods(node,user)
              end
            end
          }
          format.xml {
            render :xml => gl.to_hash(:user=>user, :uri_prefix=>uri_prefix, :dimensions=>params[:dimensions]).to_yaml
          }
          format.yml {
            render :text => gl.to_hash(:user=>user, :uri_prefix=>uri_prefix).to_yaml
          }
          format.gphys {
            render :text => Marshal.dump(gl.to_gphys.copy)
          }
          format.nc {
            if (file = gl.to_nc)
              render :text => File.read(file)
            else
              response.headers['Content-Type'] = 'text/plain'
              render :status => :bad_request, :text => "cannot find"
            end
          }
          format.png {
            if (((NumRu::GfdnaviData::Array === gl && gl.length == 1 && gl = gl[0]) || NumRu::GfdnaviData::Base === gl)\
                && ((image = gl.to_png).is_a?(String) || (image.is_a?(Array) && (image.length == 1) && (image = image[0]))))
              if image.respond_to?(:force_encoding) # FOR RUBY 1.9
                image = image.force_encoding('ASCII-8BIT')
              end
              render :text => image, :content_type => 'image/png'
            else
              response.headers['Content-Type'] = 'text/plain'
              render :status => :bad_request, :text => "There are multiple image files. Try to change suffix from 'png' to 'zip'"
            end
          }
          format.rb {
            render :text => gl.to_rb(:uri_prefix => uri_prefix)
          }
          format.knlge {
            render :text => gl.to_knlge.to_yaml
          }
        end
      when Array
        if  params[:format]
          respond_to  do |format|
          format.xml {
            render :xml => node
          }
          end
        else
          list(node,user)
        end
      else
        render_error(:not_found)
      end
    end
  end

  def edit
    flag = false
    if (user = get_user)
      path = File.join("",params[:path])
      node = Node.find(:first, :conditions => ["path=?",path], :user => user)
      if node
        if user.super_user? || node.owner == user
          @node = node
          if user.super_user?
            @groups = Group.find(:all)
            @users = User.find(:all)
          else
            @groups = user.belonging_groups
          end
          return
        end
      end
    end
    render_error(:forbidden)
  end

  def update
    flag = false
    if user = get_user
      path = File.join("",params[:path])
      path.sub!(/\.(xml|html|yml)\z/, "")
      if params[:save_as]
        gd = NumRu::GfdnaviData::Local.parse_path(params[:orig_path])
        if gd.save_as(path, user)
          response.headers["Location"] = data_url(:path => path, :format => params[:format])
          redirect_to
        else
          render_error(:bad_request, gd.errors)
        end
        return
      elsif params[:save]
        gd = eval("#{params[:class]}Local").new
        gd.user = user
        gd.path = path
        gd.name = File.basename(path)
        gd.owner = user
        params.each do |k,v|
          next if ["path", "save", "controller", "action", "class", "format"].include?(k)
          gd.send("#{k}=", v)
        end
        if gd.save
          response.headers["Location"] = data_url(:path => path, :format => params[:format])
          redirect_to
        else
          render_error(:bad_request, gd.errors)
        end
        return
      else
        if node = Node.find(:first, :conditions => ["path=?",path], :user => user)
          if user.super_user? || node.owner == user
            flag = true
          end
        end
      end
    end
    unless flag
      render_error(:forbidden)
      return
    end

    if user.super_user?
      owner = User.find(:first, :conditions=>["login=?",params[:owner]])
      node.owner = owner if owner
      #gowner = User.find(:first, :conditions=>["login=?",params[:guest_owner_id]])
      #node.guest_owner = gowner if gowner
    end
    if Array === (rgs=params[:rgroups])
      if rgs[0] == 'everyone'
        node.other_mode = 4
        rgs.shift
      else
        node.other_mode = 0
      end
      if rgs.length > 0
        node.set_rgroups(rgs)
      else
        node.rgroups = 0
      end
    else
      node.other_mode = 0
      node.rgroups = 0
    end
    if Array === (wgs=params[:wgroups]) && wgs.length > 0
      node.set_wgroups(wgs)
    else
      node.wgroups = 0
    end
    if node.save
      response.headers["Location"] = data_url(:path => node.path, :format => params[:format])
      #render :text => "update sucessed", :status => :ok
      redirect_to 
    else
      render :text => "update failed<br/>\n"+node.errors.full_messages.join("<br/>\n"), :status => :bad_request
    end
  end

  def delete
    if user = get_user
      path = File.join("",params[:path])
      if /\A(.+)\.(xml|html|yml)\z/ =~ path
        path = $1
        params[:format] = $2
      end
      gd = NumRu::GfdnaviData::Local.parse_path(path, user)
      if gd.delete
        respond_to do |format|
          format.html {
            flash[:notice] = "#{path} was deleted"
            redirect_to :action => :index, :params => {:path=>["/"]}
          }
          format.xml { render :xml => {"status" => "OK"}.to_xml }
          format.yml { render :text => {"status" => "OK"}.to_yaml }
        end
        return
      end
    end
    render_error(:forbidden)
  end



  protected


  def download(node,user)
    if node
      if node.kind_of?(Node)
        if node.directory? 
          dir = node.entity
          if dir.plain_file
            if dir.downloadable?
              send_file dir.fname, :filename => dir.name, :type => "application/x-netcdf"
              return true
            else
              render_error(:forbidden)
              return true
            end
          end
        elsif node.image?
          img = node.entity
          response.headers['Content-Type'] = img.content_type
          File.open(img.fname,"rb"){|file|
            render :text => file.read
          }
          return true
        end
      end
    end
    return false
  end


  def list(nodes,user)
    if nodes.length > 0
      @nodes = nodes
      respond_to do |format|
        format.html { render :action => :index }
        format.xml { render :xml => nodes.to_xml(:user=>user, :num_dirs=>params[:num_dirs], :uri_prefix=>uri_prefix) }
#        format.yml { render :text => nodes.collect{|node| node.to_hash(:user=>user, :uri_prefix=>uri_prefix)}.to_yaml }
      end
    else
      render_error(:not_found)
    end
  end

  def description(node,user)
    @node = node
    @name = node.name
    @path = node.path
    @type = Node::NODE_TYPES[node.node_type]
    ttl = node.title
    @title = (ttl && ttl!="NULL") ?  ttl : ""
    if user && (user.super_user? || node.owner==user)
      mode = {
        :owner => node.owner.login,
        :other_mode => node.other_mode,
        :rgroups => Group.find_by_bit_flag(node.rgroups).collect{|g| g.name}.join(", "),
        :wgroups => Group.find_by_bit_flag(node.wgroups).collect{|g| g.name}.join(", ")
      }
      @mode = mode
    end
    @ancestors = get_descriptions(node)
    if node.kind_of?(Directory)
      @directories = node.directory_nodes(:user=>user)
      @variables = node.variable_nodes(:user=>user)
      @images = node.image_nodes(:user=>user)
    end
    @references = node.references(:user=>user).collect{|ref| {"path"=>ref.path}}
    ref_by = node.referenced_by(:user=>user)
    @referenced_by = ref_by.collect{|ref| {"path"=>ref.path}}
        
    # Knowledge <--> Variable , Knowledge <--> Image の関係は
    # node_relations テーブルに入っていないので、別途 find する
    # (Variable <--> Image は大丈夫)
    @link_variables = Array.new
    @link_images = Array.new
    @linked_knowledges = Array.new
    case node.node_type
    when Node::IMAGE # Image -> Knowledge
      node = node.entity if node.is_a?(Node)
      image_path = Image.find(node.id, :user => user).path
      KnowledgeFigure.find(:all, :conditions => ["image_path = ?", image_path]).each do |kf|
        knowledge = Knowledge.find(kf.knowledge_id, :user => user)
        @linked_knowledges.push({"title"=>knowledge.title, "path"=>knowledge.path})
      end
      @linked_knowledges.uniq!
    when Node::KNOWLEDGE # Knowledge -> Image, Knowledge -> Variable
      Knowledge.find(node.node.entity.id, :user => user).knowledge_figures.each do |kf|
        #image = Image.find_by_id(kf.image_id)
        image = Image.find(:first, :conditions => ["path = ?", kf.image_path], :user => user)
        @link_images.push({"path"=>image.path})
        image.references.each do |v|
          @link_variables.push({"path"=>v.path}) if v
        end
      end
    end
        
=begin
        klg = Array.new
        node.knowledges(:user=>user).each{|kn|
          hash = {"title"=>kn.title, "path"=>kn.path}
          klg.push(hash) unless klg.include?(hash)
        }
        ref_by.each{|rb|
          rb.knowledges(:user=>user).each{|kn|
            hash = {"title"=>kn.title, "path"=>kn.path}
            klg.push(hash) unless klg.include?(hash)
          }
        }
        @knowledges = klg
=end

    if node.kind_of?(Directory)
      @plain_file = node.plain_file
      if node.downloadable?
        if node.opendap?
          @dl_url = node.path + '.html'
        else
          @dl_url = data_dl_url(:path => node.path.sub(/^\//,""))
        end
      end
    elsif node.kind_of?(Image)
      @dl_url = data_dl_url(:path => node.path.sub(/^\//,""))
    end
    render :action => "description", :layout => !request.xhr?
  end

  def description_methods(node,user)
    @node = node
    render :action => :description_methods
  end

  def draw(gla)
    @imgs = Array.new
    case gla
    when NumRu::GfdnaviData::ArrayLocal
      gla.each{|gl|
        @imgs.push gl.path
      }
    when NumRu::GfdnaviData::ImageLocal
      @imgs.push gla.path
    else
      raise "BUG: invalide class (#{gla.class.to_s})"
    end
    render :action => "draw", :layout => !request.xhr?
  end

  def get_descriptions(node)
    descs = Array.new
    loop do
      h = Hash.new
      h[:type] = node.node_type
      h[:path] = node.path
      ttl = node.title
      h[:title] = (ttl && ttl!="NULL") ? ttl : node.name
      kwattrs = node.keyword_attributes
      desc = node.description
      h[:description] = (desc && desc!="NULL") ? desc : ''
      h[:name] = node.name
      h[:keyword_attributes] = get_kwattrs(kwattrs, desc)
      info_url = node.stdname("information_url")
      h[:infomation_url] = info_url.value if info_url
      descs.unshift( h )
      node = node.parent
      break unless node
    end
    return descs
  end

  def get_kwattrs(kwattrs, desc)
    ary = Array.new
    kwattrs.each do |att| 
      if att != desc
        v=att.value
        if v.is_a?(NArray)
          if v.length == 1
            v = v[0].to_s
          else
            v = v.to_a.inspect
          end
        end
        ary.push( {:name=>att.name, :value=>v} )
      end
    end
    return ary
  end

  def get_user
    if login = (session[:user] || basic_login)
      User.find_by_login(login)
    else
      nil
    end
  end

  def render_error(status, message=nil)
    mes = Array.new
    mes.push message if message
    mes += @message if @message
    respond_to do |format|
      format.html {
        text = status.to_s
        text += "<br/>\n" + mes.join("<br/>\n")
        render :text => text, :status => status
      }
      format.xml{ render :xml => {:messages => mes.join(", "), :status => status.to_s}.to_xml}
    end
  end


  def get_diagram_cache(path)
    if dc = DiagramCache.find_by_path(path)
      return dc.files
    else
      return nil
    end
  end

  def put_diagram_cache(path,files)
    dc = DiagramCache.new(:path => path)
    dc.files = files
    dc.save!
  end

end
