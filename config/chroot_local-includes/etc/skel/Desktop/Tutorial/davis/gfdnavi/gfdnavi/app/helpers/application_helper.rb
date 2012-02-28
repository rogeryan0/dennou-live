# Methods added to this helper will be available to all templates in the application.
module ApplicationHelper

  @@locale = 'en'  # or 'ja' etc etc
  def self.locale= (locale)
    @@locale = locale
  end

  def render_erb(src)
    ::ERB.new(src).result(binding)
  end

  def relative_url_root
    if ActionController::Base.respond_to?(:relative_url_root)
      # for >= 2.2.2
      return ActionController::Base.relative_url_root  || ""
    elsif controller.request.respond_to?(:relative_url_root)
      # for < 2.2.2
      return controller.request.relative_url_root
    else
      raise "bug"
    end
  end

  def help_popup(html_file_partial)
    if base_path
      dir = File.join(base_path, controller.controller_name)
    elsif defined?(finder)
      dir = File.join(finder.view_paths[0], controller.controller_name)
    else
      dir = File.join(view_paths.to_a[0], controller.controller_name)
    end
    prefix = dir + '/__help_' + html_file_partial
    if File.exists?( path = prefix+'.'+@@locale+'.html' ) ||
	File.exists?( path = prefix+'.html' ) || 
	File.exists?( path = prefix+'.en.html' )
      help_text = File.read( path )
    else
      help_text = '[BUG] Sorry, no help text was found for '+html_file_partial
    end

    html =<<-EOS
      <label id='#{'help_'+html_file_partial}' style="cursor:pointer">
      #{image_tag('helpmark16.png', :title=>'Help')}
      </label>
      <script>
        new PopupMenu('#{'help_'+html_file_partial}',
                      '#{'help_'+html_file_partial}', 
                      '#{escape_javascript(help_text)}',
                      'Help' );
      </script>
    EOS
    html
  end

  def host_information_table
    request = controller.request
    html =<<-"EOS"
      <table>
        <tr><td>Host</td><td>#{request.host_with_port}</td></tr>
        <tr><td>Gfdnavi Root</td><td>/#{request.relative_url_root}</td></tr>
        <tr><td>Path</td><td>#{request.path}</td></tr>
        <tr><td>Env</td><td>#{RAILS_ENV}</td></tr>
        <tr><td>Server Software</td><td>#{request.server_software}</td></tr>
      </table>
    EOS
    return html
  end

  def image_path(obj)
    case obj
    when Image, Node
      data_dl_url(:path => obj.path.sub(/^\//,""))
    when DiagramCache
      url_for(:controller => 'analysis', :action => 'show_image', :id => obj.id)
    when String
      image_tag(obj)
    else
      nil
    end
  end

  def tree_lines(lines)
    imgs = Array.new
    lines = lines.dup
    first = true
    while (!lines.empty?)
      line = lines.pop
      if line.nil?
        imgs = imgs[1..-1]
      else
        if line
          if first
            type = "last"
          else
            type = "none"
          end
        else
          if first
            type = "t"
          else
            type = "tate"
          end
        end
        imgs.unshift image_tag("tree/#{type}.gif", :border => "0", :align => "middle", :class => "tree") << "\n"
      end
      first = false
    end
    return imgs.join
  end

end
