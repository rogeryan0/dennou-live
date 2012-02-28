require "numru/gfdnavi_data"
include NumRu

def usage
  print "Usage: ruby #$0 [gfdnavi_webservice_portal_URL] [number]\n"
end

if ARGV.include?("-h") || ARGV.include?("--help")
  usage
  eixt
end

if arg = ARGV.shift
  if /\Ahttp/ =~ arg
    url_prefix = arg.sub(/\/\Z/, "")
    arg = nil
  end
end
url_prefix ||= "http://localhost:3000/data"

if arg ||= ARGV.shift
  if /\A\d+\Z/ =~ arg
    @num = arg.to_i
  else
    usage
    raise "argument is invalid"
  end
end


def display(plot)
  IO.popen("display","w"){|io| io.print(plot.to_png)}
end


@n = 1
def invoke(title)
  unless @num && @n!=@num
    print "#{@n}: #{title}\n"
    yield
    print "\n"
  end
  @n += 1
end


@url_T = url_prefix + "/samples/reanalysis/ncep/T.jan.nc/T"
@url_Troot = url_prefix + "/samples/reanalysis/ncep/T.jan_only_root.nc/T"
@url_U = url_prefix + "/samples/reanalysis/ncep/UV.jan.nc/U"
@url_V = url_prefix + "/samples/reanalysis/ncep/UV.jan.nc/V"
@url_UV = url_prefix + "/[/samples/reanalysis/ncep/UV.jan.nc/U,/samples/reanalysis/ncep/UV.jan.nc/V]"

invoke "plot" do
  t = GfdnaviData.open(@url_T)
  display( t.plot("tone_contour") )
end


invoke "plot with authorization" do
  t = GfdnaviData.open(@url_Troot, "root")
  display( t.plot("tone_contour") )
end


invoke "analysis" do
  t = GfdnaviData.open(@url_T)
  t_xm = t.analysis("mean", "lon")
  p t_xm.to_gphys
end


invoke "plot of analized result" do
  t = GfdnaviData.open(@url_T)
  t_xm = t.analysis("mean", "lon")
  display( t_xm.plot("tone_contour") )
end


invoke "plot of multiple variables (one diagram)" do
  u = GfdnaviData.open(@url_U)
  v = GfdnaviData.open(@url_V)
  uv = GfdnaviData::Array[u,v]
  display( uv.plot("vector") )
end


invoke "plot of multiple variables (multiple diagrams)" do
  uv = GfdnaviData.open(@url_UV)
  uv.plot("tone_contour").each do |gd|
    display( gd )
  end
end

invoke "analysis for array which contains analyzed result" do
  t = GfdnaviData.open(@url_T)
  t_xm = t.analysis("mean", "lon")
  t_eddy = GfdnaviData::Array[t, t_xm].analysis("subtraction")
  p t_eddy.to_gphys
  display( t_eddy.plot("tone_contour") )
end

invoke "plot with options" do
  t = GfdnaviData.open(@url_T)
  display( t.plot("tone_contour", "contour"=>false) )
end

invoke "overlay two plots" do
  t = GfdnaviData.open(@url_T)
  u = GfdnaviData.open(@url_U)
  v = GfdnaviData.open(@url_V)
  uv = GfdnaviData::Array[u,v]
  display( GfdnaviData::Array[t.plot("tone_contour"), uv.plot("vector")].overlay )
end

invoke "another way to overlay two plots (slice before overlay)" do
  t = GfdnaviData.open(@url_T)
  u = GfdnaviData.open(@url_U)
  v = GfdnaviData.open(@url_V)
  uv = GfdnaviData::Array[u,v]
  display( GfdnaviData::Array[t.plot("tone_contour")[0], uv.plot("vector")[0]].overlay )
end

invoke "get script to overlay two plots" do
  t = GfdnaviData.open(@url_T)
  u = GfdnaviData.open(@url_U)
  v = GfdnaviData.open(@url_V)
  uv = GfdnaviData::Array[u,v]
  puts rb = GfdnaviData::Array[t.plot("tone_contour"), uv.plot("vector")].overlay.to_rb
  result = eval(rb, TOPLEVEL_BINDING)
  display(result)
end
