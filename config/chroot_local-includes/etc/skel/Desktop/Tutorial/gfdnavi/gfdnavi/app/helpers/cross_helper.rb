# -*- coding: japanese-cp932 -*-
module CrossHelper
  def render_view_info_list(viewInfoList)
    outer_html_begin = nil
    outer_html_end = nil
    outer_script = nil
    html = ""
    script = ""
    unless viewInfoList.nil? then
      if viewInfoList.kind_of?(Array) && viewInfoList.size > 0 then
        viewInfoList.each_with_index{|viewInfo, i|
          divid = "st_" << viewInfo[:nodepath]
          tmpscript = ""
          if viewInfo[:parent_nodepath]
            parent_id = "st_" << viewInfo[:parent_nodepath] # �e�f�B���N�g���� DIV �v�f�ɕ��
            # �v�f��K�؂ȏꏊ�ɑ}�����Ȃ��� Javascript �̃R�[�h�𐶐�
            tmpscript << "$('" << parent_id << "').appendChild($('" << divid << "'));\n"
          end

          # ���̃f�B���N�g���p�� DIV �v�f���쐬
          htmldiv = "<div id=\"#{divid}\" class=\"tree_element\">"
          if i == 0
            outer_html_begin = htmldiv
            outer_script = tmpscript
          else
            html << htmldiv
            script << tmpscript
          end

          action = (viewInfo[:treeicon] == "plus") ? "get_subpathtree" : "close_subpathtree"
          iconwithlink = link_to_remote(image_tag(viewInfo[:imgsrc], {:class => "tree_element"}),
                                        {:url => {:controller => "cross", 
                                            :action => action,
                                            :display_path => viewInfo[:display_path],
                                            :count => viewInfo[:count],
                                            :input_path => viewInfo[:input_path],
                                            :input_server => viewInfo[:input_server],
                                            :restNodes => viewInfo[:restNodes],
                                            :depth => viewInfo[:depth]}},
                                        {:class => "tree_element"})
          html << '<nobr><span class="tree_element">'
          html << viewInfo[:indent].to_s << iconwithlink << " " << viewInfo[:display_path].to_s << " (" << viewInfo[:count].to_s << ")"
          html << "</span></nobr>"

          unless viewInfo[:treeicon] == "plus" # �q�I�u�W�F�N�g�̕`��
            if viewInfo.has_key?(:objInfoList)
              viewInfo[:objInfoList].each{|objInfo|
                str = '<div><span class="tree_element"><nobr>'
                str << objInfo[:indent].to_s << "<img src=\"" << objInfo[:imgsrc].to_s << "\" class=\"tree_element\"> " << objInfo[:objname].to_s
                str << " "
                str << link_to(image_tag('tree/details.png', :alt => 'details', :title => 'Show details', :class => "tree_element"),
                               objInfo[:url], :target => '_brank')
                if objInfo.has_key?(:analysis)
                  str << " "
                  str << link_to(image_tag('tree/anal_viz.png', :alt => 'Anal/Vis', :title => 'Analysis/Visualize', :class => "tree_element"),
                                 objInfo[:analysis], :target => '_brank')
                end
                if objInfo.has_key?(:knowledge)
                  str << " "
                  str << link_to(image_tag('tree/show.png', :alt => 'Show knowledge', :class => "tree_element"),
                                 objInfo[:knowledge], :target => '_brank')
                end
                str << "</nobr></span></div>"

                html << str
              }
            end
          end
          # ���̃f�B���N�g���p�� DIV �v�f�̏I�[
          if i == 0
            outer_html_end = "</div>\n"
          else
            html << "</div>\n"
          end
        }
      end
    end
    # outerHTML ���s�v�̏ꍇ�i�����c���[�̍X�V�j�̏ꍇ�̂��߂ɁAouterHTML ����菜����悤�ɂ���
    html << "<script>" << script << "</script>" if html && script
    outer_html_end << "<script>" << outer_script << "</script>" if outer_html_end && outer_script
    return [outer_html_begin, html, outer_html_end]
  end
end

