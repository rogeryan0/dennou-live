class AnalysisHistory < Array
  MAX_LENGTH = 10
  def push(analysis)
    raise("Analysis is expected") unless Analysis === analysis
    case Analysis::ACTION_TYPE[analysis.action_type]
    when "draw"
      dl = "Draw: "
      if analysis.draw_method.nvars >= 2
        dl << analysis.draw_variables_order.collect{|ary| "["+ary.collect{|i| analysis.variables[i].name}.join(",")+"]" }.join(",")
      else
        dl << analysis.variables.collect{|v| v.name}.join(",")
      end
      dl << " " << analysis.diagram_label
    when "analysis"
      dl = "Analysis: "
      if analysis.function.nvars > 1
        dl << analysis.function_variables_order.collect{|i| analysis.variables[i].name }.join(",")
      else
        dl << analysis.variables[0].name
      end
      dl << ",  " << analysis.function.name
      arg = analysis.function_arguments
      if analysis.function.function_arguments.length > 0
        dl << "(" << analysis.function_arguments.join(",") << ")"
      end
    end
    delete_if{|a| a[1] == dl}
    super( [analysis,dl] )
    @offset = 0 if !@offset
    if length >= MAX_LENGTH + 1
      shift
      @offset += 1
    end
    self
  end

  def offset
    @offset || 0
  end

end
