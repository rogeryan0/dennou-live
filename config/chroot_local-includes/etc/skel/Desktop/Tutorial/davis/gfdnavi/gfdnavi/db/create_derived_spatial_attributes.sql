create view derived_spatial_attributes 
as 
select s2.id,s.variable_id,s2.directory_id,s2.longitude_lb,s2.latitude_lb,
       s2.longitude_rt,s2.latitude_rt,s2.lon_ax_name,s2.lat_ax_name 
from spatial_attributes s, spatial_attributes s2, variables v, directories d  
where s.longitude_lb=0 and s.latitude_lb=0 and s.longitude_rt=0 and s.latitude_rt=0 
      and v.id = s.variable_id and v.directory_id = d.id and d.id = s2.directory_id 
union 
select * 
from spatial_attributes 
where not(longitude_lb=0 and latitude_lb=0 and longitude_rt=0 and latitude_rt=0) 
and not(variable_id IS NULL);