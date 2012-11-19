#
# NetCDF�t�@�C����gfdnavi�f�[�^�x�[�X�ɓo�^����
# ���߂̃N���X
#
class NetCDFparser

  def initialize(file,rw="r",topdir="/",quiet=false)
    @filename=file
    @rw=rw
    @io=NetCDF.open(@filename,@rw)
    @temporal_rangeinfo={}
    @spatial_rangeinfo={}
#    @topdir=File.expand_path(topdir)
    @topdir=File.expand_path(GFDNAVI_DATA_PATH)
    @quiet=quiet
   
    #�ܓx�o�x���Ԃ̑��������i�b��I�ɒ������j
    @varname={}
    @varname["longitude"]=["lon","longitude"]
    @varname["latitude"]=["lat","latitude"]
    @varname["time"]=["time"]
  end

  def close
    @io.close
  end 

  # �t�@�C��@filename���f�[�^�x�[�X�ɓo�^����
  def regist 
    regist_directory(@filename)
    regist_globalattributes

    find_spatial_variables("longitude")
    find_spatial_variables("latitude")
    find_temporal_variable

    @io.each_var do |var|    
      vardb=regist_variable(var)
    end
  end

  #�t�@�C������directories�e�[�u���ɓo�^����
  def regist_directory(fname)
    @dir = Directory.new
    @dir.name = File.basename(fname)
    @dir.path = File.expand_path(fname).sub(/^#{@topdir}\//,'/')
    pdir=Directory.find(:first, :conditions => "path=\"#{File.dirname(File.expand_path(fname))}\"")
    if pdir==nil 
       #pdir=regist_directory(File.dirname(fname))
       print "WARNING: #{File.dirname(File.expand_path(fname))} is not found in Directory table.\n"
    else
       @dir.parent_id = pdir.id
       @dir.save
    end
    if !@quiet
      print "Directory #{@dir.name} is registered (id=#{@dir.id}).\n"
    end
  end

  #�t�@�C���̃O���[�o������directory�ɑ΂���L�[���[�h�����Ƃ���
  #keyword_attributes�e�[�u���ɓo�^����
  def regist_globalattributes
    @io.each_att do |att|
       key = KeywordAttribute.new
       key.directory = @dir
       key.name=att.name
       key.data_type=att.atttype
       key.value=att.get.to_s
       key.save
    end
  end

  #�evariable�̃L�[���[�h������keyword_attributes�e�[�u���ɓo�^����
  def regist_keyword_attributes(var,vardb)
     var.each_att do |att|
       key = KeywordAttribute.new
       key.variable=vardb
       key.name = att.name
       key.data_type = att.atttype
       key.value = att.get.to_s
       key.save
     end
  end

  #nc�t�@�C�����Œ�`����Ă��鎞�ԑ��������o��
  #@temporal_rangeinfo�ɂ���Ă���
  #@temporal_rangeinfo�̓n�b�V���e�[�u���̑��d�z��
  #  @temporal_rangeinfo[varname][type]
  #    varname:������
  #    type:{"start_time","end_time","ndims"}������
  #         ndims�͑����̎�����
  def find_temporal_variable
    @varname["time"].each do |v|
      var=@io.var(v)
      if var!=nil then
        if var.ndims==0 then
          start_time=end_time=var.get()[0]
        elsif var.ndims==1 then
          timearray=var.get()
          start_time = timearray.min()
          end_time = timearray.max()
        else
         print "Error: time attribute' dimensions cannot be more than 2.\n"
         return
        end
        unit=var.att("units").get
        if unit!=nil then
          start_time=parse_time_since(start_time,unit)
          end_time=parse_time_since(end_time,unit)
        end
        info={}
        info["ndims"]=var.ndims
        info["start_time"]=start_time
        info["end_time"]=end_time
        @temporal_rangeinfo[v]=info
      end
    end
  end

  #nc�t�@�C�����Œ�`����Ă����ԑ��������o��
  #@spatial_rangeinfo�ɂ���Ă���
  # @spatial_rangeinfo[category][varname][type]
  #    category:{"longitude","latitude"}�̂����ꂩ
  #�@�@varname:������
  #    type:{"var","range"}�̂����ꂩ������
  #      "var"�F�������̂���
  #�@�@�@"range"�F�����l�̂Ƃ肤��͈͂̔z��
  #          [[min_0,max_0],...,[min_n,max_n]]
  def find_spatial_variables(category)
    ranges = Array.new
    @varname[category].each do |v|
      var=@io.var(v)
      if var!=nil then
         unit=var.att("units").get
         if var.ndims<2 then
           #point or grid
           values=var.get()
           ranges.push([spunit_convert(category,values.min,unit),spunit_convert(category,values.max,unit)])
         elsif var.ndims()==2 then
           #swath
           dnum=Array.new()
           each_dim do |d|
             dnum.push(d.length_u10())
           end
           i=0
           interval=dnum[0]
           while i<dnum[1]-1 do
             endpoint=i+interval
             if endpoint>=dnum[1] then
               endpoint=dnum[1]-1
             end
             var.get({"start"=>[0,i],"end"=>[dnum[0]-1,endpoint]})
             values.sort()
             ranges.push([spunit_convert(category,values.first,unit),spunit_convert(category,values.last,unit)])
             i=endpoint
           end
         end
         if @spatial_rangeinfo[category]==nil
           @spatial_rangeinfo[category]={}
         end
         if @spatial_rangeinfo[category][v]==nil
           @spatial_rangeinfo[category][v]={}
         end
         @spatial_rangeinfo[category][v]["var"]=var
         @spatial_rangeinfo[category][v]["range"]=ranges
     end
     #check gloval attributes
     att=@io.att(v)
     if att!=nil then
       @spatial_rangeinfo[category][v]["var"]=att
       @spatial_rangeinfo[category][v]["range"]=[[att.get,att.get]]
     end
   end
 end
 
 #spunit_convert(category,value,unit)
 def spunit_convert(category,value,unit)
   if unit!=nil
     if category=="latitude"
        if unit!="degree_east"
           ud_to=Units.new('degree_east')
           ud_from=Units.new(unit)
           value=ud_from.convert(value,ud_to)
        end
     elsif category="longitude" 
        if unit!="degree_north"
           ud_to=Units.new('degree_north')
           ud_from=Units.new(unit)
           value=ud_from.convert(value,ud_to)
        end       
     end
   end
   return value
 end

 #variable����variables�e�[�u���ɓo�^����
 def regist_variable(var)
   vardb = Variable.new
   vardb.path = "#{@dir.path}"+'@'+"#{var.name}"
   vardb.name = var.name
   vardb.directory = @dir
   unless vardb.save
     $stderr.print vardb.errors.full_messages.join(", "), "\n"
   end
      
   regist_spatial_attributes(var,vardb)
   regist_temporal_attribute(var,vardb)
   regist_keyword_attributes(var,vardb)

   if !@quiet
     print "Variable #{vardb.path} is registered (id=#{vardb.id})\n"
   end
 end

 #variable�̎��ԏ���variables�e�[�u���ɒǉ�����
 def regist_temporal_attribute(var,vardb)
  if @io.dim(var.name)!=nil then
    return
  end
  dnames=var.dim_names
  dnames.each do |dname|
   if @temporal_rangeinfo[dname]!=nil then
     vardb.starttime=@temporal_rangeinfo[dname]["start_time"]
     vardb.endtime=@temporal_rangeinfo[dname]["end_time"]     
     vardb.save
     return
   end
  end
  @temporal_rangeinfo.each_value do |rangeinfo|
   if rangeinfo["ndims"]==0 then
     vardb.starttime=rangeinfo["start_time"]
     vardb.endtime=rangeinfo["end_time"]
     vardb.save  
     return
   end
  end
 end

 #variable�̋�ԏ���spatial_attribute�ɓo�^����
 def regist_spatial_attributes(var,vardb)
  #�������p�̃f�[�^�Z�b�g�������ꍇ�͓o�^���Ȃ�
  @io.each_dim do |d|
     if d.name==var.name
       return
     end
  end
  
  if var.dims==0 then
    #0�����̏ꍇ��Dir��KeywordAttribute�ɂ��Ă��܂�
    keyattr=KeywordAttribute.new
    keyattr.directory=@dir
    keyattr.name=var.name
    keyattr.data_type=var.vartype
    keyattr.value=var.value
    keyattr.save
  end

  lon=get_spatial_rangeinfo_by_dims("longitude",var)
  lat=get_spatial_rangeinfo_by_dims("latitude",var)
  #(1) lon,lat�Ƃ��Ɏ�����Ȃ� 
  if lon==nil && lat==nil then 
    lon0th=get_spatial_rangeinfo_by_ndims("longitude",0)
    lat0th=get_spatial_rangeinfo_by_ndims("latitude",0)
    if lon0th!=nil && lat0th!=nil then
      #(1-a) lon,lat�Ƃ��ɂO�����̑��������遨point�^
      add_spatial_attribute(vardb,lon0th["range"][0],lat0th["range"][0])
      return
    elsif lon0th!=nil then
      #(1-b) lon,lat�̂ǂ��炩���O�����̑�����grid�^
      add_spatial_attribute(vardb,lon0th["range"][0],[-90,90])
      return
    elsif lat0th!=nil then
      add_spatial_attribute(vardb,[0,360],lat0th["range"][0])
      return
    end
    #(1-c) lon,lat��1�����ł��̎����f�[�^�Z�b�g�̎��Ɠ�����point�^
    lon1st=get_spatial_rangeinfo_by_ndims("longitude",1)
    lat1st=get_spatial_rangeinfo_by_ndims("latitude",1)
    if lon1st!=nil && lat1st!=nil && be_present_in(lon1st["var"].dims,var.dims) && be_present_in(lat1st["var"].dims,var.dims) then 
       i=0
       lon1st["range"].each do |l|
         add_spatial_attribute(vardb,l,lat1st["range"][i])
         i+=1 
       end
    end
    #(1-d) lon,lat��2�����ł��̎����f�[�^�Z�b�g�̎��Ɠ�����swath�^
    lon2nd=get_spatial_rangeinfo_by_ndims("longitude",2)
    lat2nd=get_spatial_rangeinfo_by_ndims("latitude",2)
    if lon2nd!=nil && lat2nd!=nil && be_present_in(lon2nd["var"].dims,var.dims) && be_present_in(lat2nd["var"],var.dims) then
       i=0
         lon2nd["range"].each do |range|
         add_spatial_attribute(vardb,range,lat2nd["range"][i])
         i+=1
       end
       return
    end
  #(2) latitude����
  elsif lon==nil
    lon0th=get_spatial_rangeinfo_by_ndims("longitude",0)
    if lon0th!=nil
      add_spatial_attribute(vardb,lon0th["range"][0],lat["range"][0])
    else
      add_spatial_attribute(vardb,[0,360],lat["range"][0])
    end
  #(3) longitude����
  elsif lat==nil
    lat0th=get_spatial_rangeinfo_by_ndims("latitude",0)
    if lat0th!=nil
      add_spatial_attribute(vardb,lon["range"][0],lat0th["range"][0])
    else
      add_spatial_attribute(vardb,lon["range"][0],[-90,90])
    end
  #(4) longitude,latitude�Ƃ��Ɏ�
  else
    add_spatial_attribute(vardb,lon["range"][0],lat["range"][0])
  end 
 end

 #@spatial_rangeinfo�ɂ����ԑ����̃��X�g����
 #��������ndims�ł�����̂����o��
 #�o�͂����̂̓n�b�V���e�[�u��
 # rangeinfo[type]
 #   type: "var","range"�̂����ꂩ
 def get_spatial_rangeinfo_by_ndims(category,ndims)
  @spatial_rangeinfo[category].each_value do |info|
      if info["var"].ndims==ndims then
         return info
      end
    end
    return
 end

 #@spatial_rangeinfo�ɂ����ԑ����̃��X�g����
 #������var�ł�����͈̂̔͏������o��
 #�o�͂����̂̓n�b�V���e�[�u��
 # rangeinfo[type]
 #   type: "var","range"�̂����ꂩ
 def get_spatial_rangeinfo_by_dims(category,var)
    @spatial_rangeinfo[category].each_key do |v|
      dims=var.dims
      dims.each do |d|
        if d.name==v then
         return @spatial_rangeinfo[category][v] 
        end
      end  
    end
    return
 end

 #dimb�̒���dima�̗v�f���S���͂����Ă�����true
 #�����łȂ����false��Ԃ�
 def be_present_in(dima,dimb)
  i=0; j=0; 
  while i<dima.size do
   flg=false
   while j<dimb.size do
    if dima[i].name==dimb[j].name
      flg=true
    end
    j+=1
   end
   if flg==false then
     return false
   end
   i+=1
  end
  return true
 end

 #spatial_attributes�e�[�u���ւ̒ǉ�
 def add_spatial_attribute(var,lon,lat)
  sp=SpatialAttribute.new
  sp.variable=var
  sp.longitude_lb=lon[0]
  sp.longitude_rt=lon[1]
  sp.latitude_lb=lat[0]
  sp.latitude_rt=lat[1]
  sp.save
 end

 #udunits�\���ɂ�鎞��time�ƒP��units�̒l��
 #�f�[�^�x�[�X��DateTime�^�ŔF���ł���`�ɂ���
 def parse_time_since(time, units)
  if /(\w+) since (.*)/ =~ units
    units = $1
    since = $2
  else
    raise ArgumentError, 
          "units '#{units}' is not in the expected form: units since date&time"
  end
  case units
  when /^s$/i,/^sec/i
    time = time/86400.0
  when /^min/i
    time = time/1440.0
  when /^h$/i,/^hour/i
    time = time/24.0
  when /^day/i
  else
    raise ArgumentError, "Sorry, #{units} is not supported. Is it a time unit?"
  end
  eps = 1e-7   # adjuster to avoid unwanted round off
  DateTime.parse(since) + ( time + eps )
end

end
