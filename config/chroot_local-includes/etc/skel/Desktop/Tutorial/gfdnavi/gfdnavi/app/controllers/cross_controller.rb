#!/usr/bin/ruby
# -*- coding: japanese-cp932 -*-
#require "rdb_concat"
require 'rubygems'
require 'open-uri'
#require 'hpricot'
require "rexml/document"
require 'kconv'
require 'net/http'
require "socket"
require 'uri'
Net::HTTP.version_1_2 

class CrossController < ApplicationController
  layout "gfdnavi"
  
  def index
    # @queryconditions�͑S�����Aqcno�͑S�����̐�
    # params["query#{}"]�̓`�F�b�N�������Ă�������̂݊i�[
    # nofquery�̓`�F�b�N�������Ă�������̐�

    # qcno �� queryconditions ���疈��v�Z���邱�Ƃɂ��āA�폜 by Otsuka
    # params["query#{}"] �� nofquery �� queryconditions �� qcond_selection ���疈��v�Z���邱�Ƃɂ��āA�폜 by Otsuka

    # session[:previd], session[:selection], session[:qsetid], @qargs �͎g���Ă��Ȃ��悤�Ȃ̂ō폜 by Otsuka

    #�⍇���������̈����p���i�Ȃ���Ώ������j
    @queryconditions = session[:queryconditions] || Array.new
    @nodetypes = session[:nodetype] || Hash.new
    session[:qcond_selection] ||= Array.new # ����܂Ŏg�����������������ݑI������Ă��邩�ǂ���
 
    #�T�[�o���X�g�̈����p���i�Ȃ���Ώ������j
    if session[:serverlist] then
      @serverlist = session[:serverlist]
    else
      @serverlist = Hash.new(nil)
      result_at_server = ResultAtServer.new("localhost")
      result_at_server.id = 0
      result_at_server.url = request.raw_host_with_port
      result_at_server.check = 1      
      @serverlist[:localhost] = result_at_server
    end
    session[:server_selection] ||= Array.new
    session[:nodetype] ||= Hash.new
    session[:isInitial] ||= 1 # �l���Z�b�g����Ă��Ȃ���Ώ��񌟍�

   #���̃Z�b�V�������Ȃǂ͏��������Ă���
    #�ϐ��̏�����
    # @result_at_server=ResultAtServer.new(0) #�e�T�[�o�ł̌������� # @??? @modified 2011.1.18 chiemi
    @results = CrossResult.new()
    @pathtree = Array.new() #pathtree��name�i�[

    #�Z�b�V�������̏�����(���ꂼ��{���Ɏg���Ă��邩���m�F����)
    #�Z�b�V���������g���Ė⍇��������
    query_join(nil)
    cross_search()
    unless @pathtrees
      @pathtree_html = nil
    else
      @pathtree_html = Hash.new
    end
    @pathtrees ||= Hash.new # ������Ώ�����
    @pathtrees.keys.each{|pt|    
      servername = pt
      @serverlist = session[:serverlist]         
      @serverlist.each_key{|key|
        if @serverlist[key].url == pt then ########## ���ϐ����폜�����֌W�ŕύX
          servername = key
        end	     
      }
      @pathtree_html[servername] = ResultInfo.get_pathtree_viewinfo(@pathtrees[pt], pt, relative_url_root)
    }
  end

  def get_selected_queries
    # params["query#{}"] �� nofquery ���������̂� queryconditions �� qcond_selection ����v�Z
    selected_queries = []
    @queryconditions.each_with_index{|q, i|
      selected_queries.push(q) if session[:qcond_selection][i] == 1
    }
    return selected_queries
  end
    
  def check_box
    # �T�[�o�I����ʂ� [CLOSE] ���������Ƃ�
    @resultInfoList = {:all_servers => ResultInfo.new("all_servers")}
    #�T�[�o�̑I�� 
    @serverlist = session[:serverlist]

    old_status = session[:server_selection].dup

    num_checked = 0
    @serverlist.each{|key, obj|
      checked = (params["server_selection_#{obj.id}"] == "1") # �`�F�b�N����Ă��邩�ǂ���
      session[:server_selection][obj.id] = checked ? 1 : 0
      @serverlist[key].check = checked ? 1 : 0
      num_checked += 1 if checked
    }

    if session[:server_selection] == old_status # �����ς���Ă��Ȃ����
      render :nothing => true
      return
    end

    if num_checked == 0 # �`�F�b�N���S���O����Ă��܂�����
      # ���ł������̂őI�����Ă����Ȃ��ƃG���[�ɂȂ�
      srv = @serverlist[@serverlist.keys[0]] # �Ƃ肠�����擪�̗v�f
      params["server_selection_#{srv.id}"] = "1"
      session[:server_selection][srv.id] = 1
      srv.check = 1
    end

    session[:serverlist] = @serverlist 
    @queryconditions = session[:queryconditions]###
    unless (!@queryconditions) || @queryconditions.empty? ### ���܂Ō��������N�G��������ꍇ
      query_join(nil)###
      cross_search()###
      show_result ### 
    else ###�@"all"�̏ꍇ
       session[:queryconditions] = Array.new
       @queryconditions = Array.new(0)###
       session[:qcond_selection] = Array.new
       get_xml
    end###
    #get_xml
    
  end

  def reset
    session[:queryconditions]  = nil
    session[:qcond_selection]  = nil
    session[:serverlist]       = nil
    session[:server_selection] = nil
    session[:nodetype]         = nil 
    session[:isInitial]        = nil
    redirect_to :action => "index"
  end

  def get_xml ##�S�t�@�Z�b�g���擾���鎞�̏���
    unless @serverlist == nil
      @serverlist.each_key{|key|
        if @serverlist[key].check == 1 then 
          if @serverlist[key].url == request.raw_host_with_port  then
            url = @serverlist[key].url
            check = @serverlist[key].check
            params["show_facets"]   = 1
            params["show_kwvalues"] = 1
            params["show_spfacets"] = 1
            params["no_result"]     = 1
            params["show_pathtree"] = 1
            
            xmlString = categorized_search()
            doc = xmlString.to_xml
            get_results(doc, key)
            @pathtrees ||= Hash.new
            @pathtrees[url] = @pathtree || "no result"       
            @serverlist[key].url = url
            @serverlist[key].check = check
          else 
            url = @serverlist[key].url
            check = @serverlist[key].check
            queryurl = "http://#{url}data/find(all).xml?show_kwfacets=1&show_kwvalues=1&show_spfacets=1&no_result=1"             
            begin
              ## �ڑ����Ԃ�ݒ肷��ꍇ
              uu = URI.parse("http://#{url}")
              # puts uu.host,uu.port,uu.path
              Net::HTTP.start(uu.host, uu.port){|http|
		http.read_timeout = 240
                doc = http.get("#{uu.path}data/find(all).xml?show_kwfacets=1&show_kwvalues=1&show_spfacets=1&no_result=1&show_pathtree=1").body
              }
              ##
              ## open-uri�ŊJ���ꍇ
              #doc = open(queryurl).read.toutf8 
              get_results(doc, key) 
              @pathtrees ||= Hash.new
              @pathtrees[url] = @pathtree || "no result"
            rescue        
            end
            @serverlist[key].url = url
            @serverlist[key].check = check
          end
        end
      }   
    end
    session[:serverlist] = @serverlist
    show_result
  end
  
#==begin �L�[���[�h���N���b�N�����Ƃ��C���łɏ�����Ă���keyvalues��\�����邾���ɂ��� (written by chiemi)
  def show_keyvalues
    keyname = params[:keyname]
    render :update do |page|
      page.show("kw_#{keyname}")
      page.show("kw_#{keyname}_close")
    end
  end
  
  def hide_keyvalues
    keyname = params[:keyname]
    render :update do |page|
      page.hide("kw_#{keyname}")
      page.hide("kw_#{keyname}_close")
    end  
  end
  #==end  
  
  def query_join(desc)
    #�V�����⍇������
    if desc then 
      @queryconditions ||= Array.new(0)
      @queryconditions.push(desc)
      session[:qcond_selection] ||= Array.new
      session[:qcond_selection].push(1) # ���̏�����I������
    end
    session[:queryconditions] = @queryconditions
  end

  def cross_search
    # @result_at_server=ResultAtServer.new(0)

    #==begin �ꎞ�I��@keyvalues�Ƃ����n�b�V��������Ă����i��X����, written by chiemi�j 
    @resultInfoList = {:all_servers => ResultInfo.new("all_servers")}
    #==end

    #�������Ȃ��Ă��� 
    query = get_selected_queries.join("&")
    
    params["show_kwfacets"] = 1
    params["show_kwvalues"] = 1
    params["show_spfacets"] = 1 
    params["no_result"]     = 1
    params["show_pathtree"] = 1

    #    @serverlist=session[:serverlist]
    l = 0
    @serverlist.each_key{|key| ############# �ύX
      @resultInfoList[key] = ResultInfo.new(key)     
      url = @serverlist[key].url
      check = @serverlist[key].check
      id = @serverlist[key].id
      if url == request.raw_host_with_port #localhost�̏ꍇ
        if check == 1 
          session[:server_selection][id] = 1
          xmlString = categorized_search()
          # uu=URI.parse("http://davis.gfd-dennou.org/experimental/gfdnavi")
          # Net::HTTP.start(uu.host,uu.port){|http|
           # http.read_timeout = 240
            #puts http.get("#{uu.path}/data/find(kw.long_name=temperature).xml?show_kwfacets=1&show_kwvalues=1&show_spfacets=1&no_result=1&show_pathtree=1").body
          #}
          #puts uu.host,uu.port,uu.path
          doc = xmlString.to_xml
          get_results(doc, key)
          @pathtrees ||= Hash.new
          @pathtrees[url] = @pathtree || "no result"
        end    
      else
        query = "all" if query == ""
        if check == 1
          # puts "cross search"
          crossurl = URI.encode("http://#{url}data/find(#{query}).xml?show_kwfacets=1&show_kwvalues=1&show_spfacets=1&no_result=1&show_pathtree=1")
          begin
            ## �ڑ����Ԃ�ݒ肷��ꍇ
            uu = URI.parse("http://#{url}")
            Net::HTTP.start(uu.host, uu.port){|http|
              http.read_timeout = 240
              doc = http.get(URI.encode("#{uu.path}data/find(#{query}).xml?show_kwfacets=1&show_kwvalues=1&show_spfacets=1&no_result=1&show_pathtree=1")).body
            }
            # open-uri�ŊJ���ꍇ�itimeout�͎w��ł��Ȃ��j
            #doc = open(crossurl).read.toutf8
            get_results(doc, key)   
            @pathtrees ||= Hash.new
            @pathtrees[url] = @pathtree || "no result"
          rescue
          end
        end
      end 
      l += 1
    }
    session[:serverlist] = @serverlist  
    #show_result
  end
  
  
  ##���ʂ̕\��##
  def show_result
    # puts @queryconditions
    render :update do |page| 	  
      @serverlist = session[:serverlist] 
      
      #/*Gfdnavi Servers*/#
      #  if $isInitial !=0 then #������ʂł����
      page.replace_html "servers", ""
      @serverlist.each{|key, obj|
        url = @serverlist[key].url
        page.insert_html :bottom, "servers", "<div id=\"servers_#{obj.id}\" style=\"font-size:18px;\"></div>"
        
        checked = (session[:server_selection][obj.id] == 1) ? "checked" : ""
        page.insert_html :bottom, "servers", "<div id= servers_#{obj.id} style=\"font-size:18px;\"><input type=\"checkbox\" name=\"server_selection_#{obj.id}\" value=\"1\" #{checked}>#{key} <nobr><a href=\"http://#{@serverlist[key].url}/\" TARGET=\"_blank\">\"http://#{@serverlist[key].url}\"</a></nobr></div>"
        if session[:server_selection][obj.id] == 1 then
          params["server_selection_#{obj.id}"] = 1 # ?
        end
      }
      # end
      
      #/*QueryConditions*/
      @queryconditions = session[:queryconditions]
      # if $checken == 0
      page.replace_html "qcond", ""
      if @queryconditions
        page.insert_html :top, "qcond", "<font>These terms define your current search.Please remove the check to remove a term. </font><br>"
      end
      spcond = 0
      
      unless @queryconditions.nil?
        page.replace_html "spconditions", ""
        @queryconditions.each_with_index{|q, i|
          qcnd_sel_flag = (session[:qcond_selection][i] == 1) # �I������Ă��邩�ǂ���
          joint = ((@queryconditions.size > i + 1) ? " & " : "") # �Ōォ�ǂ���
          html = "<span id=\"qcond_#{i}\" class=\"query_conditions\">"
          html << check_box_tag("qconds_#{i}", 1, qcnd_sel_flag, {:name => "qcond_selection_#{i}"})
          html << q << joint
          html << observe_field("qconds_#{i}", # �C�x���g�n���h���̓o�^
                                :url => {:action => 'queryconditions_selection', :id => i}, # �Ăяo���A�N�V�����̎w��
                                :with => "'qcond_selection_#{i}='+(value)", # �n���p�����[�^
                                :before => 'showLoadingIconAll()', # ���邭����A�C�R���̕\��
                                :complete => 'hideLoadingIconAll()') # ���邭����A�C�R���̒�~
          html << "</span>"
          page.insert_html :bottom, "qcond", html
          
          #��ԑ����ɑ΂���⍇�������͒n�}�ɕ\�����邽�߂�@spcondition�ɕۑ�
          if q =~ /sp.overlap\[(.*)\]/ then
            s = $1.split(/,/);
            str = <<-EOM
              (<span id=\"start_lon\">#{s[0]}</span>,
              <span id=\"start_lat\">#{s[1]}</span>)-
              (<span id=\"end_lon\">#{s[2]}</span>,
              <span id=\"end_lat\">#{s[3]}</span>)
            EOM
            page.insert_html :bottom, "spconditions", "<div id=\"spconditions_#{spcond}\">" << str << "</div>"
            spcond += 1
          end
        }
        page.insert_html :bottom, "qcond", " <center><submit_tag(\"selection\")></center>"
        session[:queryconditions] = @queryconditions
      end
      #/*Keyword*/
      page.replace_html "keyword", "<ul id='keyword_list'></ul>"
     #===begin keyvalues�����łɊi�[���Ă����i��\���ɂ��Ă����āA�N���b�N���ꂽ�猩���悤�ɂ���j written by chiemi
      @resultInfoList[:all_servers].get_keynames_sorted.each{|kname|
        count = @resultInfoList[:all_servers].get_keyvalue_count(kname, :all_values)
        page.insert_html :bottom, "keyword_list", "<li>#{link_to_remote(kname,{:url=>{:action=>'show_keyvalues',:keyname=>kname},:before=>'showLoadingIconAll()', :complete=>'hideLoadingIconAll()'})}(#{count})</li>"
        close = "[CLOSE]"
        page.insert_html :bottom, "keyword_list", "<ul><div id=\"kw_#{kname}\" style=\"display:none;\"></div><div id=\"kw_#{kname}_close\" style=\"display:none\">#{link_to_remote(close,{:url=>{:action=>'hide_keyvalues',:keyname=>kname}})}</div>"
        @resultInfoList[:all_servers].get_keyvalues_sorted(kname).each{|kvalue, count|
          if kvalue != :all_values then
            page.insert_html :bottom, "kw_#{kname}", "<li>#{link_to_remote(kvalue,{:url=>{:action=>'cross_kw',:keyname=>kname, :keyvalue=>kvalue},:before=>'showLoadingIconAll()', :complete=>'hideLoadingIconAll()'})}(#{count})</li>"
          end
        }
      }      
      #===end
      #/*partial_covered*/
      @serverlist = session[:serverlist]
      ppa = Hash.new()
      sum_partial = Hash.new()
      page.replace_html "partial_covered", ""
      i = 0
      @resultInfoList[:all_servers].partial.each_key{|reg|
        cnt = @resultInfoList[:all_servers].get_count_partial(reg)
        region = reg.split(/_/)	  
        page.insert_html :bottom, "partial_covered", "<div id=\"patial_group_#{i}\"></div>"
        page.replace_html "patial_group_#{i}", "(<span id=\"latitude_lb\">#{region[0]}</span>,<span id=\"longitude_lb\">#{region[1]}</span>)-(<span id=\"latitude_rt\">#{region[2]}</span>,<span id=\"longitude_rt\">#{region[3]}</span>) [#{cnt}]" 	
        i += 1
      }
      
      #/*point*/
      ppp = Hash.new()
      sum_point = Hash.new()
      page.replace_html "box", "<input type=\"checkbox\" name=\"points\" onclick=\"show_points(this);\" checked> point"
      page.replace_html "points", ""
      i = 0
      @resultInfoList[:all_servers].point.each_key{|pos|
        cnt = @resultInfoList[:all_servers].get_count_point(pos)
        position = pos.split(/_/) 
        page.insert_html :bottom, "points", "<div id=\"point_group_#{i}\"></div>"
        page.replace_html "point_group_#{i}", "(<span id=\"latitude\">#{position[0]}</span>,<span id=\"longitude\">#{position[1]}</span>)[<span id=\"count\">#{cnt}</span>]" 
        i += 1
      }
      
      #/*Result*/
      
      $analviz_tag = image_tag('tree/anal_viz.png', :alt=>'Anal/Vis',
                               :title=>'Analyze/visualize variables in this folder',
                               :border=>0,:align=>'absmiddle')
      $details = image_tag('tree/details.png',:alt=>'details', :title=>'Show details',:border=>0,:align=>'absmiddle')
      
      page.replace_html "results", ""

      return unless @pathtrees
      prevDepth = -1
      @pathtrees.each{|url, pathtree|
        # puts pt
        serverurl = url
        servername = url
        
        @serverlist = session[:serverlist]
        @serverlist.each{|key, val|
          ########## ���ϐ����폜�����֌W�ŕύX
          servername = key if val.url == url
        }
        page.insert_html :bottom, "results", "<h2>********** #{servername} **********</h2>"
        #���ʂ��[���������玟�̃T�[�o�ɐi��ł��܂�
        if pathtree == "no result" then
          page.insert_html :bottom, "results", pathtree
          next;
        end
        
        viewInfoList = ResultInfo.get_pathtree_viewinfo(pathtree, serverurl, relative_url_root)
        # app/helpers/cross_helper.rb �� HTML ����
        page.insert_html :bottom, "results", render_view_info_list(viewInfoList).join("")

      }# @pathtrees.keys.each{ |pt|
      #/*map*/
      page << "initialize();"
    end 
  end
  
  def close_subpathtree
    display_path = params["display_path"]
    input_path   = params["input_path"]
    input_server = params["input_server"]
    depth        = params["depth"].to_i
    count        = params["count"]
    restNodes    = params["restNodes"].map{|r| r.to_i}
    
    # HTML �^�O ID �̌`���ɂ��ẮA���̕����Ƃ̐��������m�F
    tagid = "st_" << input_server.gsub("/", "_") << "_" << input_path.gsub(/\/\z/, "").gsub("/", "_")
    render :update do |page|
      #�N���b�N�����s������������
      page.replace_html tagid, "" # innerHTML ������
      #�C���f���g�\��
      indents = ""
      for d in 0...depth
        imgname = (restNodes[d] > 0) ? "tate.png" : "space.gif"
        indents << "<img src=#{relative_url_root}/images/tree/#{imgname} class=\"tree_element\">"
      end
      imgname = (restNodes[depth] == 1) ? "last.png" : "t.png"
      indents << "<img src=#{relative_url_root}/images/tree/#{imgname} class=\"tree_element\">"

      nodepath = input_server + "_" + input_path.gsub("/", "_")
      nodename = display_path
      imgsrc = relative_url_root + "/images/tree/plus.gif"
      iconwithlink = link_to_remote(image_tag(imgsrc, :class => "tree_element"),
                                    {:url => {:controller => "cross",
                                        :action => "get_subpathtree",
                                        :display_path => nodename,
                                        :count => count,
                                        :input_path => input_path,
                                        :input_server => input_server,
                                        :restNodes => restNodes,
                                        :depth => depth}},
                                    {:class => "tree_element"})
      page.insert_html :bottom, tagid, indents << iconwithlink << " " << nodename << " (" << count.to_s << ")" # �^�O�ŕ�ޕK�v����
      
    end
    
  end
  
  def get_subpathtree
    @resultInfoList = {:all_servers => ResultInfo.new("all_servers")}
    
    display_path = params["display_path"]
    input_path   = params["input_path"]
    input_server = params["input_server"]
    depth        = params["depth"].to_i
    restNodes    = params["restNodes"].map{|r| r.to_i}
    
    #�\���p�p�����^�̐ݒ�
    options = {
      "show_kwfacets" => 0,
      "show_kwvalues" => 0,
      "show_spfacets" => 0, 
      "no_result"     => 1,
      "show_pathtree" => 1}
    
    #���������͂��łɎ��s���ꂽ���̂��ێ�����
    descriptions = Array.new
    @queryconditions = session[:queryconditions]
    if @queryconditions && !(@queryconditions.empty?)
      queries = []
      @queryconditions.each_with_index{|q, i|
        if session[:qcond_selection][i] == 1 then
          queries.push(q)
          descriptions.push(q)
        end
      }
      query = queries.join("&")
    else    
      query = "all"
    end

    #description��path�̏�����ǉ�����
    descriptions.push("path=#{input_path}")
    
    #�w�肳�ꂽ�T�[�o�ɖ⍇�����ɍs��    
    
    if input_server == request.raw_host_with_port then
      #(���[�J���j
      user = (login = session[:user]) && User.find_by_login(login) 
      eq = NodeQuery.new
      qstr = eq.make_query(descriptions, user)
      storage = temp_storage
      if qstr != "all" then
        tmptable_name = "tmptable_#{Time.now.to_i}"
        qstr = "create table #{tmptable_name} as #{qstr}"
        Node.connection.execute(qstr)
        xmlString = eq.generate_results_fromtmptable(0, tmptable_name, options, user)
        Node.connection.execute("drop table #{tmptable_name}") #delete temporary table
      else
        xmlString = eq.generate_results_fromtmptable(1, tmptable_name, options, user)
      end
      
      session[:isInitial] = 0
      doc = xmlString.to_xml
      
      #   puts doc
      get_results(doc, "localhost")
    else
      #�i�����[�g�j
      url = "http://#{input_server}data#{input_path}/find(#{query}).xml?show_kwfacets=0&show_spfacets=0&no_result=1&show_pathtree=1" 
      begin
        doc = open(url).read.toutf8              
        get_results(doc, input_server) #===begin �����ύX�̕K�v����(doc,�T�[�o��)�ɂ��Ȃ���΂Ȃ�Ȃ�
      rescue
      end
    end

    #���ʂ�\������
    viewInfoList = ResultInfo.get_pathtree_viewinfo(@pathtree, input_server, relative_url_root, depth, restNodes)
    if viewInfoList
      if display_path && viewInfoList[0][:display_path] && (display_path != viewInfoList[0][:display_path])
        if /\A(.*)#{display_path}(.*)\z/ =~ viewInfoList[0][:display_path]
          display_path = display_path << $2
        end
      end
      viewInfoList[0][:display_path] = display_path if display_path # �㏑��


      # HTML �^�O ID �̌`���ɂ��ẮA���̕����Ƃ̐��������m�F
      tagid = "st_" + input_server.gsub("/", "_") + "_" + input_path.gsub(/\/\z/, "").gsub("/", "_")
      newtagid = "st_" << viewInfoList[0][:nodepath]
      
      render :update do |page|
        #�N���b�N�����s������������
        # app/helpers/cross_helper.rb �� HTML ����
        html = render_view_info_list(viewInfoList)[1]
        unless tagid == newtagid # �^�O ID ���ύX���ꂽ���ɂ́AJavascript �� ID ��ύX
          html = "<script>$('#{tagid}').id = '#{newtagid}';</script>" << html
        end
        page.replace_html tagid, html # innerHTML �̏�������
      end # render :update do |page|
    end # if viewInfoList
  end

  
  ##localhost�ɂ�錟�����ʂ̎擾##
  def categorized_search
    selected_queries = get_selected_queries
    descriptions = Array.new
    params["show_kwfacets"] = 1
    params["show_kwvalues"] = 1
    params["show_spfacets"] = 1
    params["no_result"]     = 1
    params["show_pathtree"] = 1
    #if params[:nofquery] == 1  && @queryconditions == nil then 
    # params[:nofquery] == 1 �̈Ӑ}��������Ȃ�
    if @queryconditions.nil? # nil �ɂȂ�̂��H
      @queryconditions = "all" 
      descriptions.push("all")
    else
      unless selected_queries.nil? # nil �ɂȂ�悤�ȃR�[�h�ł͂����Ȃ�
        selected_queries.each{|sel_q| # �V�����ϐ� 
          unless sel_q.nil? 
            if /kw=/ =~ sel_q
            end
            unless sel_q  == "all"
              descriptions.push(sel_q)
            end
          end
        }
      end
    end
    options = {
      "show_kwfacets" => (params["show_kwfacets"] || 0),
      "show_spfacets" => (params["show_spfacets"] || 0),
      "show_kwvalues" => (params["show_kwvalues"] || 1),
      "no_result"     => (params["no_result"]     || 0),
      "show_pathtree" => (params["show_pathtree"] || 0)}
   
    user = (login = session[:user]) && User.find_by_login(login)
    eq = NodeQuery.new

    storage = temp_storage

    if @queryconditions.size > 0 then
      session[:isInitial] = 0

      if @queryconditions[0] == "all" then
        @queryconditions = Array.new(0)
      end
      qstr = eq.make_query(descriptions, user)
      tmptable_name = "tmptable_#{Time.now.to_i}"

      qstr = "create table #{tmptable_name} #{storage} as #{qstr}"

      Node.connection.execute(qstr)
      
    else # ���񌟍�
      session[:isInitial] = 1
    end
    res = eq.generate_results_fromtmptable(session[:isInitial], tmptable_name, options, user)
    if session[:isInitial] == 0 then
      Node.connection.execute("drop table #{tmptable_name}") #�ꎞ�e�[�u��������
    end
    return res
  end

  ###�t�@�Z�b�g�ƌ��ʂ̎擾### 
  def get_results(xmlString, servername)#===begin �T�[�o����n���悤�ɂ��܂���
    @results = CrossResult.new()
    @resultInfoList[servername] ||= ResultInfo.new(servername)

    #DOM����
    doc = REXML::Document.new(xmlString)

    #point
    point = Hash.new
    xpath = "//point" 
    l = 0
    doc.elements.each(xpath) do |e|      
      key = ["latitude-lb", "longitude-lb"].map{|s|
        e.elements[s].text
      }.join("_")
      point[key] = e.elements["count"].text
    end
    @resultInfoList[servername].add_point(point)
    @resultInfoList[:all_servers].add_point(point)
    
    #partial_covered
    partial = Hash.new
    xpath = "//partial-covered/partial-covered" 
    doc.elements.each(xpath) do |e|
      key = ["latitude-lb", "longitude-lb", "latitude-rt", "longitude-rt"].map{|s|
        e.elements[s].text
      }.join("_")
      partial[key] = e.elements["count"].text
    end
    @resultInfoList[servername].add_partial(partial) 
    @resultInfoList[:all_servers].add_partial(partial)
       
    #all_covered
    all_covered = Hash.new
    xpath = "//all-covered/all-covered" 
    doc.elements.each(xpath) do |e|
      key = ["latitude-lb", "longitude-lb", "latitude-rt", "longitude-rt"].map{|s|
        e.elements[s].text
      }.join("_")
      all_covered[key] = e.elements["count"].text
    end
    @resultInfoList[servername].add_all_covered(all_covered) 
    @resultInfoList[:all_servers].add_all_covered(all_covered)

    #keyword
    keyword = Hash.new
    l = 0
    xpath = "//keyword" 
    doc.elements.each(xpath) do |e|
      keyname = e.elements["keyname"].text
      count   = e.elements["count"].text
      @resultInfoList[servername].set_keyname_count(keyname, count)
      @resultInfoList[:all_servers].add_keyname_count(keyname, count)
      if keyname == "long_name" then
      end
                 
#===begin keyvalues��XML�Ɋ܂܂�Ă������̑Ώ� (written by chiemi at 20101101)-
      values = e.elements["keyvalues"]
      if values && values.class.to_s == "REXML::Element" then
        values.each{|v|
          if v.class.to_s == "REXML::Element" then
            keyvalue = v.elements["keyvalue"].text
            count    = v.elements["count"].text.to_i
            @resultInfoList[servername].add_keyvalue_count(keyname, keyvalue, count)
            @resultInfoList[:all_servers].add_keyvalue_count(keyname, keyvalue, count)
            if keyname == "long_name" && keyvalue == "temperature" then
              # ?
            end
          end
        }
      end
#===end
    end

    #pathtree
    @pathtree = doc.root.elements["//pathtree"]
  end
 
 ##�p�X�c���[�擾##   
  def get_pathtree(xmlString)   
    @pathtree = Array.new()
    doc = REXML::Document.new(xmlString)
    
    xpath1 = "//pathtree" 
    
    doc.elements.each(xpath1) do |e1|     
      str1 = e1.elements["name"]
      array = str1.text
      @pathtree.push(array)
      xpath2 = "//directories"
      e1.elements.each(xpath2) do |e2|
        str2 = e2.elements["directory"]
        i = 0
        e2.elements.each("directory") do |e3|       
          e3.elements.each("directory") do |e4|            
            # ?
          end
          str3 = e3.elements["name"]
          i += 1
        end   
      end
    end
    # return @path     
  end
  
  def display
    render :action => "_result.rhtml"
  end
  
  def cross_kw
    session[:isInitial] = 0
    desc = "kw.#{params[:keyname]}=#{params[:keyvalue]}"
    @serverlist = session[:serverlist]
    @queryconditions = session[:queryconditions]
    query_join(desc)
    cross_search
    show_result
  end
  
  def cross_freeword
    session[:isInitial] = 0
    @serverlist = session[:serverlist]
    @queryconditions = session[:queryconditions]
    @serverlist = session[:serverlist]     
    desc = "fw=#{params[:freeword]}"
    query_join(desc)
    cross_search
    show_result
    session[:serverlist] = @serverlist
  end
  
  def cross_space
    session[:isInitial] = 0
    desc = "sp.overlap[#{params[:start_lon]},#{params[:start_lat]},#{params[:end_lon]},#{params[:end_lat]}]"
    @serverlist = session[:serverlist]
    @queryconditions = session[:queryconditions]
    query_join(desc)
    cross_search
    show_result
  end
  
  def cross_time
    session[:isInitial] = 0	  
    desc = "tm=[#{params[:starttime]},#{params[:endtime]}]"
    @serverlist = session[:serverlist]
    @queryconditions = session[:queryconditions]
    @serverlist = session[:serverlist]
    query_join(desc)
    cross_search
    show_result
  end
  
  def queryconditions_selection
    # ���܂Ŏg�������������ꗗ�\�̃`�F�b�N�{�b�N�X���N���b�N�����Ƃ�
    session[:isInitial] = 0 # ���炩�ɏ��񌟍��ł͖���
    @serverlist = session[:serverlist]
    @queryconditions = session[:queryconditions]
    
    target = params[:id].to_i # �`�F�b�N�{�b�N�X���N���b�N���ꂽ��������
    session[:qcond_selection][target] ^= 1 # �l���g�O��������i�r���I�_���a�j
    selected_queries = get_selected_queries # �`�F�b�N����Ă�������̂ݎ��o��
    
    @serverlist.each{|key, obj|
      session[:server_selection][obj.id] = ((@serverlist[key].check == 1) ? 1 : 0)
    }
    
    if selected_queries.size == 0 # ���ׂẴ`�F�b�N���O����Ă����
      # ���ׂĂ̏��������������Ă��܂��i����ŗǂ��̂��H�j
      @queryconditions = Array.new
      session[:queryconditions] = @queryconditions
      session[:qcond_selection] = Array.new
      @resultInfoList = {:all_servers => ResultInfo.new("all_servers")}
      
      get_xml
    else # �`�F�b�N����Ă�����̂������
      cross_search 
      show_result
    end
  end
  
  def get_servers
    @serverlist = session[:serverlist] || Hash.new
#     url = GFDNAVI_CROSS_SEARCH_CENTER_SERVER

    url = "https://davis.gfd-dennou.org/gfdnavi_portal/display/server_list"
 
    serverlist = open(url, :proxy => nil).read.toutf8
    serverlist = REXML::Document.new(serverlist)

    # �g�p�ς� ID
    used_ids = @serverlist.map{|key, obj| obj.id.to_i}

    newid = 0
    xpath = "servers/server"
    serverlist.elements.each(xpath) do |e|    
      name = e.elements["name"].text
      unless @serverlist[name]
        while used_ids.include?(newid)
          newid += 1 # ���g�p ID ��T��
        end
        @serverlist[name] = ResultAtServer.new(name)
        @serverlist[name].isInitial = session[:isInitial]
        @serverlist[name].id = newid
        @serverlist[name].url = e.elements["url"].text 
      end
    end
    
    @results = CrossResult.new()
    session[:serverlist] = @serverlist
    session[:server_selection] = @serverlist.map{|key, obj| obj.check.to_i}
    
    render :update do |page|
      page.replace_html "servers", ""
      @serverlist.each{|key, obj|

        url = @serverlist[key].url
        if url == request.raw_host_with_port 
          page.insert_html :bottom, "servers", "<div id= servers_#{obj.id} style=\"font-size:18px;\"><input type=\"checkbox\" name=\"server_selection_#{obj.id}\" value=\"1\" checked>#{key} <nobr><a href=\"http://#{@serverlist[key].url}/\" TARGET=\"_blank\">\"http://#{@serverlist[key].url}\"</a></nobr></div>"
        else
          page.insert_html :bottom, "servers", "<div id= servers_#{obj.id} style=\"font-size:18px;\"><input type=\"checkbox\" name=\"server_selection_#{obj.id}\" value=\"1\">#{key} <nobr><a href=\"http://#{url}\" TARGET=\"_blank\">\"http://#{url}\"</a></nobr></div>" 
        end
      }
      
    end
  end
  
  def nodetype_selection
    # @result_at_server=ResultAtServer.new(0)
    @serverlist = session[:serverlist]
    @queryconditions = session[:queryconditions]
    @nodetypes = session[:nodetype] # �`�F�b�N���O��Ă���� 1 ������
   
    node_names = ["data", "knowledge", "function", "draw_method"]
    node_names.each{|nt|
      value = params["nodetype_#{nt}"] # �N���b�N����Ă��Ȃ���� nil
      if value # �N���b�N����Ă����ꍇ�́A�l�𔽉f
        @nodetypes[nt] = ((value == "1") ? 0 : 1) # �`�F�b�N����Ă���� "1"
      else
        @nodetypes[nt] ||= 0 # ����������Ă��Ȃ���΁A�`�F�b�N����Ă���ƌ���
      end
    }
    session[:nodetype] = @nodetypes

    # �`�F�b�N�̂��Ă���f�[�^�^�C�v�̂ݎ��o���Č���
    types_str = @nodetypes.reject{|key, val| val == 1}.keys.join(",")
    
    @queryconditions = session[:queryconditions] 
    
    selected_queries = get_selected_queries
    unless (!@queryconditions) || @queryconditions.empty? #�������������������
      flag = 0 # �ȑO�Ƀf�[�^�^�C�v�ōi�荞�񂾂��ǂ���
      @queryconditions.each_with_index{|q, qnum|
        if q =~ /\Adatatype=(.*)/
          # �ȑO datatype �Ō������Ă����ꍇ
          @queryconditions[qnum] = "datatype=#{types_str}" # �ȑO�̏������㏑��
          session[:qcond_selection][qnum] = 1 # ���̏�����I������
          flag = 1
        end
      } 
      
      if flag == 0 then #first datatype search
        @queryconditions.push("datatype=#{types_str}")
        session[:qcond_selection].push(1) # ���R���̏�����I������
      end
      
      session[:results] = @results
    else # ���������������Ȃ���΁A�P�ɍŏ��̌��������Ƃ��Ēǉ�
      @queryconditions = ["datatype=#{types_str}"]
      session[:qcond_selection] = [1] # ���R���̏�����I������
    end
    session[:queryconditions] = @queryconditions # �Z�b�V�����ɏ����߂�

    cross_search
    show_result
  end # def nodetype_selection
end
 

=begin
###�����l�̎擾###  
   def get_keyvalues(doc,servername)
    # @result_at_server=ResultAtServer.new(0)
    keyvalue= Hash.new
    
    doc=REXML::Document.new(doc)	
    doc.elements.each("//keyword") do  |e|	   
      # str=e.elements["keyname"].text
      e.elements.each("keyvalues/keyvalue") do |f|
        # if e.elements["keyname"].text == str then	
        str1=f.elements["keyvalue"].text
        str2=f.elements["count"].text
        keyvalue.store("#{str1}","#{str2}")
        
      end
      @result_at_server.keyvalue.push(keyvalue) 
          
      @serverlist["#{servername}"]=@result_at_server   
     
      return  @serverlist
    end
  end
=end
