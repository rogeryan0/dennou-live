require "numru/dcl"
include NumRu

file1 = File.open("./47646.y2d","r")

DCL::gropn(1)

while( line = file1.gets ) # ファイル末尾までループ

  # ヘッダーの場合

  if( line =~ /^#(\d{5})(\d{4})(\d{2})(\d{2})(\d{2})(\d{4})([\d ]{4})$/ )

    station, year, mon, day, hour, rtime, levels = $1, $2, $3, $4, $5, $6, $7

    DCL::grfrm
    DCL::grsvpt(0.1,0.9,0.1,0.9)
    DCL::grswnd(-70,30,0,35)
    DCL::grstrf
    DCL::usdaxs

    DCL::uxsttl("t","##{station} #{year}-#{mon}-#{day} #{hour}UTC",-1)
    DCL::uxsttl("b","Temperature [degC]",0)
    DCL::uysttl("l","Geopotential Height [km]",0)


  # データの場合

  elsif( line =~ /^(\d)(\d)([\d ]{6})([AB ])([\d -]{5})([AB ])([\d -]{5})([AB ])([\d -]{5})([\d -]{5})([\d -]{5})$/ )

    mjtype, mntype, pres, presf, gph, gphf, temp, tempf, dpd, winddir, wind = 
      $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11

    DCL::sgpmzu([temp.to_f/10.0], [gph.to_f/1000.0], 5, 3, 0.02)

  else
    raise "invalid line: #{line}"  # どちらでもなければエラーを吐いて終了
#    p line
  end

end

DCL::grcls

file1.close
