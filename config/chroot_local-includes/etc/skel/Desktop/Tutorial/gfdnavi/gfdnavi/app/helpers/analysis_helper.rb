module AnalysisHelper
  def line_index_generator(draw_method, param, title)
    index = (@analysis["#{draw_method}_#{param}"] || 1).to_i
    width = index % 10
    color = index / 10
    html = <<EOF
      #{title} width
      <select id="analysis_#{draw_method}_#{param}width" onchange="
        $('analysis_#{draw_method}_#{param}').value = 
          parseInt($('analysis_#{draw_method}_#{param}width').selectedIndex) + 1
          + parseInt($('analysis_#{draw_method}_#{param}color').value) * 10;
      ">
EOF
    for i in 1..9
      if (width % 10) == i
        html << '<option selected="selected">'
      else
        html << '<option>'
      end
      html << "#{i}</option>\n"
    end
    html << <<EOF
      </select>
      <br/>
      #{title} color (0--99)
      <input type="text" id="analysis_#{draw_method}_#{param}color" size="2" maxlength="2"
       value="#{color}"
       onchange="
         $('analysis_#{draw_method}_#{param}').value = 
           parseInt($('analysis_#{draw_method}_#{param}width').selectedIndex) + 1
           + parseInt($('analysis_#{draw_method}_#{param}color').value) * 10;
      ">
EOF
    html << "<br/>"
    html << hidden_field("analysis","#{draw_method}_#{param}",:size => 3,:maxlength => 3)
    return html
  end

  def analysis_dimension_selector(arg_number, text)
    html = <<EOF
      <label for="analysis[function_arguments][#{arg_number}]">
        the dimensions for #{text}
      </label>
      <select id="analysis[function_arguments][#{arg_number}]" name="analysis[function_arguments][#{arg_number}][]" multiple>
EOF
    if (@analysis.function_arguments.length==0)
      fa = @analysis.common_dimensions[0][:name] 
    else
      fa = @analysis.function_arguments[arg_number]
    end
    fa = [fa] unless fa.is_a?(Array)
    fa = fa & (@analysis.common_dimensions.collect{|c| c[:name]})
    @analysis.common_dimensions.each_with_index{|dim,i|
      dname = dim[:name]
      if fa.empty?
        s = i==0
      else
        s = fa.include?(dname)
      end
      html << <<EOF
        <option #{s ? "selected" : ""}>#{h(dname)}</option>
EOF
    }
    html << <<EOF
      </select>
      <br>
      CTL key + left click to select multiple dimentions
EOF
    return html
  end
end
