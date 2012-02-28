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

invoke "save and delete image" do
  t = GfdnaviData.open(@url_T)
  new_path = "/usr/root/test.png"
  t.plot("tone_contour")[0].save_as(new_path, "root")
  img = GfdnaviData.open(url_prefix+new_path, "root")
  display(img)
  img.delete
end

