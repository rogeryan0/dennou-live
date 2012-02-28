require 'test_helper'

class Analysis2ControllerTest < ActionController::TestCase
  test "index" do
    # index without variables/diagrams
    get :index
    assert_response :success

    # index with a variable without diagrams
    @request.session[:variables_list] = [variables(:ncep_jan_t).path]
    get :index
    assert_response :success

    # index with a variable and a diagram
    @request.session[:variables_list] = [variables(:ncep_jan_t).path]
    @request.session[:diagrams] = ["/samples/reanalysis/ncep/T.jan.nc/T/plot(tone_contour;x_axis=lon,y_axis=lat)[0]"]
    get :index
    assert_response :success
  end

  test "get_axes" do
    get :get_axes, :path => variables(:ncep_jan_t).path
    assert_response :success

    obj = ActiveSupport::JSON.decode(@response.body)
    gphys = variables(:ncep_jan_t).to_gphys
    assert_kind_of Array, obj
    assert_equal gphys.rank, obj.length
    assert_equal gphys.axis(0).name, obj[0]['name']
    assert_equal gphys.axis(0).pos.units.to_s, obj[0]['units']
    assert_equal gphys.axis(0).length, obj[0]['ary'].length
  end

  test "get_diagrams" do
    diagrams = ["/samples/reanalysis/ncep/T.jan.nc/T/plot(tone_contour;x_axis=lon,y_axis=lat)[0]",
                "/samples/reanalysis/ncep/T.jan.nc/T/plot(tone_contour;x_axis=lat,y_axis=level)[0]"]
    @request.session[:diagrams] = diagrams

    get :get_diagrams
    obj = ActiveSupport::JSON.decode(@response.body)
    assert_equal diagrams.length, obj.length
  end

  test "set_diagrams" do
    diagrams = ["/samples/reanalysis/ncep/T.jan.nc/T/plot(tone_contour;x_axis=lon,y_axis=lat)[0]",
                "/samples/reanalysis/ncep/T.jan.nc/T/plot(tone_contour;x_axis=lat,y_axis=level)[0]"]
    get :set_diagrams, :paths => diagrams.to_json

    newdiagrams = @response.session[:diagrams]
    assert_equal diagrams.length, newdiagrams.length
    assert_equal diagrams[0], newdiagrams[0]
  end

  test "clear" do
    @request.session[:diagrams] = ["/samples/reanalysis/ncep/T.jan.nc/T/plot(tone_contour;x_axis=lon,y_axis=lat)[0]"]
    @request.session[:variables_list] = [variables(:ncep_jan_t)]

    get :clear
    assert_kind_of Array, @response.session[:diagrams]
    assert @response.session[:diagrams].empty?
    assert_kind_of Array, @response.session[:variables_list]
    assert @response.session[:variables_list].empty?
  end
end
