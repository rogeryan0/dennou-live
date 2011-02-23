require "numru/dcl"
include NumRu

def plot(x, y=nil, options=nil)
   if y == nil
      y = x
      x = NArray.float(y.length).indgen!
   end
   DCL.grfrm
   if options
      if (val=options[:xtitle]);  DCL.uscset("cxttl", val); end
      if (val=options[:ytitle]);  DCL.uscset("cyttl", val); end
   end
   DCL.usgrph(x, y)
end

x = NArray.float(20).indgen! - 10
z = x ** 2 / 20

DCL.gropn(1)
plot(z)                                      # �L�[���[�h�����Ȃ� 
plot(z, nil, {:xtitle=>"X", :ytitle=>"Y"})   # Hash�ɂ��u�L�[���[�h�����v
plot(x,   z,  :xtitle=>"X", :ytitle=>"Y")    # �ďo�������ł� {} �͏ȗ���
DCL.grcls

