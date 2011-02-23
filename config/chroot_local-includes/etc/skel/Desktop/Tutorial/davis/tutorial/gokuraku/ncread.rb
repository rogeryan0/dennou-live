require "numru/netcdf"
require "numru/dcl"
include NumRu

file1 = NetCDF.open("T.jan.nc")        # NetCDF�ե����륪�֥�������

var_temp = file1.var("T")              # �ѿ��򥪡��ץ�
temp = var_temp.get                    # �ѿ����ͤ���Ф�

var_level = file1.var("level")
level = var_level.get

file1.close

p temp

DCL::gropn(1)

for k in 0..level.shape[0]-1           # ��ľ�ؿ����������֤�
  DCL::grfrm
  DCL::grstrn(10)                      # ����������ˡ
  DCL::grswnd(0.0,360.0,-90.0,90.0)    # ���褹����ٷ��٤λ���
#  DCL::grsvpt(0.2,0.8,0.25,0.7)        # ������ɥ���������ΰ�λ���
  DCL::umpfit                          # ���ȤϤ��ޤ���
  DCL::grstrf                          # ����

  DCL::uelset("ltone",true)            # �����դ���
  DCL::uetone(temp[true,-1..0,k])      # k���ܤε���������
                                       # �ǡ������̤������äƤ���Τ�
                                       # -1..0 �Ȥ��ƽ��֤�Ҥä����֤�

  DCL::umpglb                          # ��Ȱ��ٷ������򤫤�
  DCL::umpmap("coast_world")           # �Ͽޤ�����

  DCL::uxsttl("t","Temperature at #{level[k]}hPa",0.0)

end

DCL::grcls

