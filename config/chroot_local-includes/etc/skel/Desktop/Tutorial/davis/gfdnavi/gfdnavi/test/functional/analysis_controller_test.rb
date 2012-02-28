require File.dirname(__FILE__) + '/../test_helper'

# Re-raise errors caught by the controller.
class AnalysisController; def rescue_action(e) raise e end; end

class AnalysisControllerTest < ActionController::TestCase
  fixtures :variables, :draw_methods, :draw_method_options, :users

  def test_index
    v_id = 1
    var = Variable.find(v_id)
    get :index
    assert_not_nil session[:analysis]
    session[:variables_list] = [var.path]
    get :index
    assert_tag :tag => "input", :attributes => {:type => "checkbox", :id => "variables_#{var.path}"}
=begin # CURRENTLY DISABLED
    var = Variable.new(:name => "tmp")
    session[:temp_variables_list] = [var]
    get :index
#    assert_tag :tag => "input", :attributes => {:type => "checkbox", :id => "variables_temp_0"}
    assert_match /variables_temp_0/, @response.body
=end
  end

  def test_clear
    get :clear
    assert_response :redirect
    assert_redirected_to :action => "index"
    assert_nil session[:analysis]
    assert_nil session[:diagrams]
    assert_nil session[:variables_list]
=begin # CURRENTLY DISABLED
    assert_nil session[:temp_variables_list]
=end
  end

  def test_variables_selected
    get :variables_selected
    assert_redirected_to :action => "index"
    assert_nil session[:analysis]
    v_id = 1
    get :index
    assert_response :success
    xhr :post, :variables_selected, :variables => {Variable.find(v_id).path => "1"}
    assert_response :success
    analysis = session[:analysis]
    assert_kind_of Analysis, analysis
    assert_equal 1, analysis.variables.length

    var = Variable.find(v_id)
    var2 = analysis.variables[0]
    # extract wrapped Node
    var2 = var2.get_object if var2.is_a?(NumRu::GfdnaviData::Local)
    var2 = var2.instance_variable_get(:@original_nodes) if var2.is_a?(VirtualData) && var2.type == "node"
    var2 = var2[0] if var2.is_a?(Array) && var2.length == 1
    assert_equal var, var2

    assert_equal 3, analysis.dimensions.length
    #assert_raise( ActiveRecord::RecordNotFound ){
      #xhr( :post, :variables_selected, :variables => {"2" => "1"} )
    assert_raise( RuntimeError ){
      xhr( :post, :variables_selected, :variables => {"/non_existent_path" => "1"} )
    }
=begin # CURRENTLY DISABLED
    assert_raise( NoMethodError ){
      xhr( :post, :variables_selected, :variables => {"temp_0" => "1"} )
    }
    session[:temp_variables_list] = []
    assert_raise( NoMethodError ){
      xhr( :post, :variables_selected, :variables => {"temp_0" => "1"} )
    }
=end
  end

  def test_action_type_selected
    get :action_type_selected
    assert_nil session[:analysis]
    v_id = 1
    action_type = 0
    var = Variable.find(v_id)
    analysis = Analysis.new
    analysis.variables.push var
    session[:analysis] = analysis
    xhr :post, :action_type_selected, :action_type => action_type
    assert_response :success
    analysis = session[:analysis]
    assert_kind_of Analysis, analysis
    assert_equal action_type, analysis.action_type
    assert_equal 1, analysis.draw_type
    assert_match /draw_general_tab/, @response.body
    assert_no_match /Function/, @response.body
    analysis = Analysis.new
    analysis.variables.push Variable.find(2).path
    analysis.variables.push Variable.find(3).path
    session[:analysis] = analysis
    xhr :post, :action_type_selected, :action_type => action_type
    assert_equal 2, analysis.draw_type
    action_type = 1
    xhr :post, :action_type_selected, :action_type => action_type
    assert_response :success
    assert_no_match /draw_general_tab/, @response.body
    assert_match /Function/, @response.body
    session[:analysis] = nil
    xhr :post, :action_type_selected, :action_type => action_type
    assert_no_match /draw_general_tab/, @response.body
    assert_no_match /Function/, @response.body
    action_type = 999
    xhr :post, :action_type_selected, :action_type => action_type
    assert_no_match /draw_general_tab/, @response.body
    assert_no_match /Function/, @response.body
  end

  def test_execute
    v_id = 1
    var = Variable.find(v_id)
    action_type = 0
    axes = {"lon"=>{"max"=>"350", "min"=>"0"}, "level"=>{"min"=>"1000"}, "lat"=>{"max"=>"-90", "min"=>"90"}}
    draw_method = "tone_contour"
    draw_x_axis = "lon"
    draw_y_axis = "lat"
    draw_keep = true
    draw_size = "400,400"
    tone_contour_tone = true
    tone_contour_contour = false
    get :index
    assert_response :success
    #xhr :post, :execute, :variables => {v_id => "1"}, :action_type => Analysis::ACTION_TYPE[action_type], :analysis => {:draw_type => draw_type, :x_axis => x_axis, :y_axis => y_axis, :region => region, :draw_keep => draw_keep, :draw_size => draw_size, :tone => tone, :contour => contour }
    xhr :post, :variables_selected, :variables => {var.path => "1"}
    xhr :post, :execute, :action_type => Analysis::ACTION_TYPE[action_type], :analysis => {:draw_method => draw_method, :draw_x_axis => draw_x_axis, :draw_y_axis => draw_y_axis, :axes => axes, :draw_keep => draw_keep, :draw_size => draw_size, :tone_contour_tone => tone_contour_tone, :tone_contour_contour => tone_contour_contour }
    assert_response :success
    analysis = session[:analysis]
    assert_equal draw_method, analysis.draw_method.name
    assert_equal draw_x_axis, analysis.draw_x_axis
    assert_equal draw_y_axis, analysis.draw_y_axis
    assert_equal axes, analysis.axes
    assert_equal draw_keep, analysis.draw_keep
    assert_equal draw_size, analysis.draw_size.join(",")
    assert_equal tone_contour_tone, analysis.tone_contour_tone
    assert_equal tone_contour_contour, analysis.tone_contour_contour
    #diagram = assigns(:diagrams)
    diagram = assigns(:diagram)
    #diagram = @response.session[:diagrams][0]
    assert_kind_of Array, diagram
    #assert_equal 4, diagram.length
    #id, path, diagram_id, saved = diagram
    assert_equal 2, diagram.length
    id, path = diagram
    assert_equal 0, diagram_id
    diagram = session[:diagrams][diagram_id]
    viz = diagram[:vizshot]
    assert_kind_of NumRu::VizShot, viz
    assert @response.body.include?(path)

    action_type = 1
    function_id = 1
    func = Function.find(function_id)
    session[:variables_list] = [var]
    xhr :post, :execute, :variables => {v_id.to_s => "1"}, :action_type => Analysis::ACTION_TYPE[action_type], :analysis => {:function => function_id, :region => region }
    assert_response :success
    assert_equal session[:analysis], assigns(:analysis)
    analysis = assigns(:analysis)
    assert_equal function_id, analysis.function.id
    variables = assigns(:variables)
    assert_equal 2, variables.length
    assert_equal var.id, variables[0][0]
    assert_equal var.name, variables[0][1]
    title = var.keyword_attributes.find_by_stdname("title")
    assert_equal title.name, variables[0][2][0][0]
    assert_equal title.value, variables[0][2][0][1]
    assert_equal 1, session[:temp_variables_list].length
    var2 = session[:temp_variables_list][0]
    assert_equal var2.id || 0, variables[1][0]
    assert_equal var2.name, variables[1][1]
    title = var2.keyword_attributes[0]
    assert_equal title.name, variables[1][2][0][0]
    assert_equal title.value, variables[1][2][0][1]
  end


end
