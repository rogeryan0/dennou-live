require "numru/dcl"

include NumRu
include NMath                        # NArray��Math�⥸�塼��

nt = 50
nz = 50
tmin, tmax = 0.0, 5.0
zmin, zmax = 20.0,50.0

t = NArray.sfloat(nt+1,   1).indgen! * (tmax-tmin)/nt # ���� (nt+1,1)����
z = NArray.sfloat(   1,nz+1).indgen! * (zmax-zmin)/nz # �ļ� (1,nz+1)����

uz = exp(-0.2*z)*(z**0.5)
tz = -2.0*exp(-0.1*z)
u = uz*sin(3.0*(tz+t))               # (nt+1,nz+1)������ˤʤ롣

p "umin,umax="
p u.min, u.max

=begin
���ޥ�ɥ饤�󥪥ץ�����-ps���ޤޤ�Ƥ����PS�ե��������(gropn(2))��
�ʤ���Х�����ɥ��򳫤�(gropn(1))
=end
if ARGV.index("-ps")
  DCL::gropn(2)     
else
  DCL::gropn(1)     
end

DCL::grfrm
DCL::grswnd(tmin, tmax, zmin, zmax)         # ��������������κ�ɸ��
DCL::uspfit                                 # ���ȤϤ��ޤ���
DCL::grstrf                                 # ��ɸ�γ���

DCL::sglset('lfprop',true)                  # �ץ�ݡ�����ʥ�ե����
DCL::ussttl('TIME', 'YEAR', 'HEIGHT', 'km') # �����ȥ������
if ARGV.index("-color")
  DCL::uelset("ltone",true)                 # ���Ĥ�
  DCL::uetone(u)                            # �̤�櫓
end
DCL::usdaxs                                 # ��ɸ��������
DCL::udcntr(u)                              # ��������Ҥ�

DCL::sgstxs(0.02)                           # ʸ�����礭��������
DCL::sgtxr(0.7,0.9, "umin="+u.min.to_s+" umax="+u.max.to_s)
                                            # to_s ʸ������Ѵ�
require "date"                              # require������ˤ��äƤ��
DCL::sgtxr(0.75,0.85, "created by #{$0} #{DateTime.now}")

DCL::grcls
