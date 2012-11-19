require "narray"
require "gsl"


module Analysis

def covariance(x,y)
  if x.length != y.length then
    print "error\n"
    exi
  end

  len = x.length

  sum = x.mul_add(y,0)
  n = len

  return  sum/(n-1)
end

def covariance_matrix( x )
  if x.rank !=2
    raise "covariance_matrix: x.rank must be 2"
  end

  dim = x.shape[0]  
  cov = ( NArray.sfloat(dim, dim).fill!(0.0) ).to_gm
  total = 0

  for i in 0..dim-1
    for j in 0..i
      elm = covariance(x[i,true],x[j,true])
      cov[j, i] = elm  unless i == j
      cov[i, j] = elm 
      total += elm
    end
  end
  return cov, total
end

def eof( x )

  if x.shape.length!=2 then
    print "err\n"
    exit
  end

  dim,nle = x.shape

  p "calc anomary"
  x = x - x.mean(1)

  p "make covariance matrix"
  cova, total = covariance_matrix(x)

  p "calc eigen value"
  val, vec = GSL::Eigen::symmv(cova)
  vec = vec.transpose.to_na

  [val/total,vec]
end

module_function :eof, :covariance_matrix, :covariance

end

