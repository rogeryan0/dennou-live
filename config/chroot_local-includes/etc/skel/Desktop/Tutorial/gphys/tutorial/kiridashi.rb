require "numru/gphys"
include NumRu
gphys = GPhys::NetCDF_IO.open('T.jan.nc', 'T')
print "gphys: rank=#{gphys.rank} shape=#{gphys.shape.inspect}\n"

gp135 = gphys.cut('lon'=>0..90)           # lon �Ȥ���̾���μ��� 0��90 �٤ޤ�
print "gp135: rank=#{gp135.rank} shape=#{gp135.shape.inspect}\n"

gprg1 = gphys.cut( 100.0..150.0, 30..50, 850) # 1,2�����ܤ��ϰϡ�3�����ܤ��ͤ�
print "gprg1 rank=#{gprg1.rank} shape=#{gprg1.shape.inspect}",
      "  0th dim: #{gprg1.coord(0).min} .. #{gprg1.coord(0).max}\n"

gprg2 = gphys[ 0..3, -3..-1, 0]               # ���Ʊ�ͤ�������ź���ǻ���
print "gprg2 rank=#{gprg2.rank} shape=#{gprg2.shape.inspect}",
      "  0th dim: #{gprg2.coord(0).min} .. #{gprg2.coord(0).max}\n"
p gprg2     # ���ޤ���ɽ��
