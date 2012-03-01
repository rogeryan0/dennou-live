require "zlib"

module Tar

  Header = [
    ["name"    , 100],
    ["mode"    ,   8],
    ["uid"     ,   8],
    ["gid"     ,   8],
    ["size"    ,  12],
    ["mtime"   ,  12],
    ["chksum"  ,   8],
    ["typeflag",   1],
    ["linkname", 100],
    ["magic"   ,   6],
    ["version" ,   2],
    ["uname"   ,  32],
    ["gname"   ,  32],
    ["devmajor",   8],
    ["devminor",   8],
    ["prefix"  , 155]
   ]

  TMAGIC = "ustar "
  TVERSION = " "

  REGTYPE = 0
  DIRTYPE = "5"

  def cf(io, files)
    lclose = false
    if String === io
      io = File.open(io,"wb")
      lclose = true
    elsif ! IO === io
      raise "the first argument must be IO or String"
    end
    begin
      files.each{|name|
        type = name[-1..-1] == "/" ? "d" : "f"
        if type == "f"
          size = File.stat(name).size
        else
          size = 0
        end
        io.write create_header(name, type, size)
        if type == "f"
          File.open(name,"rb"){|file|
            while (buf = file.read(512))
              io.write buf
              io.write "\000"*(512 - buf.length) unless buf.length == 512
            end
          }
        end
      }
      io.write "\000"*1024
    ensure
      io.close if lclose
    end
    return nil
  end
  module_function :cf

  def zcf(io, files)
    if IO === io
      gz = Zlib::GzipWriter.new(io)
      lclose = false
    elsif String === io
      gz = Zlib::GzipWriter.open(io)
      lclose = true
    else
      raise "the first argument must be IO or String"
    end
    begin
      cf(gz, files)
    ensure
      lclose ? gz.close : gz.finish
    end
    return nil
  end
  module_function :zcf

  def create_header(name, type, size)
    hash = Hash.new
    hash["name"] = name
    case type
    when "f"
      hash["mode"] = "0100644"
      hash["typeflag"] = "%i"%REGTYPE
    when "d"
      hash["mode"] = "0000755"
      hash["typeflag"] = "%i"%DIRTYPE
    end
    hash["uid"] = "0001000"
    hash["gid"] = "0001000"
    hash["size"] = "%0#11o"%size
    hash["mtime"] = ("%#o"%Time.now.to_i)[1..-1]
    hash["linkname"] = ""
    hash["magic"] = TMAGIC
    hash["version"] = TVERSION
    hash["uname"] = "gfdnavi"
    hash["gname"] = "gfdnavi"
    hash["devmajor"] = ""
    hash["devminor"] = ""
    hash["prefix"] = ""
    hash["chksum"] = " "*8
    chksum = 0
    hash.each{|n,v|
      v.each_byte{|byte| chksum +=  byte }
    }
    chksum = "%#o"%chksum
    len = chksum.size
    hash["chksum"][0...len] = chksum
    hash["chksum"][len..len] = "\000"

    header = ""
    Header.each{|ary|
      str = hash[ary[0]]
      str << "\000"*(ary[1]-str.length) if ary[1] > str.length
      header << str
    }
    header << "\000"*(512-header.length%512)
    return header
  end
  module_function :create_header

end
