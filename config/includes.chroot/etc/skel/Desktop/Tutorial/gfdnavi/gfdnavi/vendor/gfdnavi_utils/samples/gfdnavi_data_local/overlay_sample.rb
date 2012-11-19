=begin

Usage:

type the following command at the top directory of Gfdnavi:

ruby script/console < vendor/gfdnavi_utils/samples/gfdnavi_data_local/overlay_sample.rb

=end

require "numru/gfdnavi_data"
include NumRu

def display(plot)
  IO.popen("display","w"){|io| io.print(plot.to_png)}
end

t = GfdnaviData.open("/samples/reanalysis/ncep/T.jan.nc/T")
u = GfdnaviData.open("/samples/reanalysis/ncep/UV.jan.nc/U")
v = GfdnaviData.open("/samples/reanalysis/ncep/UV.jan.nc/V")

# tone + vector
tone = t.plot("tone_contour")[0]
vect = GfdnaviData::Array[u,v].plot("vector")[0]
result = NumRu::GfdnaviData::Array[tone, vect].overlay
p result.path
display(result)

# tone + vector with map projection
tone = t.plot("tone_contour", "projection" => 10)[0]
vect = GfdnaviData::Array[u,v].plot("vector")[0]
result = NumRu::GfdnaviData::Array[tone, vect].overlay
p result.path
display(result)

# tone + contour
tone = u.plot("tone_contour", "tone" => true, "contour" => false)[0]
cont = v.plot("tone_contour", "tone" => false, "contour" => true)[0]
result = NumRu::GfdnaviData::Array[tone, cont].overlay
p result.path
display(result)

# tone + contour with map projection
tone = u.plot("tone_contour", "tone" => true, "contour" => false, "projection" => 10)[0]
cont = v.plot("tone_contour", "tone" => false, "contour" => true)[0]
result = NumRu::GfdnaviData::Array[tone, cont].overlay
p result.path
display(result)
