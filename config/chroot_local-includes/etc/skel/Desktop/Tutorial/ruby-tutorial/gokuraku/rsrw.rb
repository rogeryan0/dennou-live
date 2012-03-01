require "numru/dcl"
include NumRu

file1 = File.open("./47646.y2d","r")

file2 = File.open("./47646_200511","w") # "w"�Ͻ񤭹��ߥ⡼��

while( line = file1.gets ) # �ե����������ޤǥ롼��

  # �إå����ξ��

  if( line =~ /^#(\d{5})(\d{4})(\d{2})(\d{2})(\d{2})(\d{4})([\d ]{4})$/ )

    station, year, mon, day, hour, rtime, levels = $1, $2, $3, $4, $5, $6, $7

    if( year.to_i == 2005 && mon.to_i == 11 )
      wflag = true
      file2.puts(line)
    else
      wflag = false
    end

  # �ǡ����ξ��

  elsif( line =~ /^(\d)(\d)([\d ]{6})([AB ])([\d -]{5})([AB ])([\d -]{5})([AB ])([\d -]{5})([\d -]{5})([\d -]{5})$/ )

    mjtype, mntype, pres, presf, gph, gphf, temp, tempf, dpd, winddir, wind = 
      $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11

    if wflag
      file2.print(mjtype+mntype+pres+presf+gph+gphf+temp+tempf+"\n")
    end

  else
    raise "invalid line: #{line}"  # �ɤ���Ǥ�ʤ���Х��顼���Ǥ��ƽ�λ
#    p line
  end

end

file2.close

file1.close
