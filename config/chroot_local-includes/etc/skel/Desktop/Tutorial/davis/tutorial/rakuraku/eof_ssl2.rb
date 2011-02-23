require "narray"
require "numru/ssl2"


module Analysis
def covariance( x,y,rmiss=nil )
  if x.length != y.length then
    print "error\n"
    exi
  end

  len = x.length

  if rmiss.nil? then
    sum = x.mul_add(y,0)
    n = len
  else
     mask = x.ne(rmiss) & y.ne(rmiss)
     sum = x.mask(mask).mul_add(y.mask(mask),0)
     n = mask.count_true
  end

  if n>len/2 then
    sum/(n-1)
  else
    rmiss
  end

end

def covariance_matrix( x, rmiss=nil, lmean=false, ltotal=false)
  if x.rank !=2
    raise "covariance_matrix: x.rank must be 2"
  end
  dim, len = x.shape

  rmiss = nil if !rmiss.nil? && x.ne(rmiss).all?

  if lmean
    if rmiss.nil?
      x = x - x.mean(1)
    else
      for i in 0...dim
	mask = x[i,true].ne(rmiss)
	tmp = x[i,true].mask(mask)
	if tmp.length >= len/2 then
	  x[i,mask] = tmp - tmp.mean
	else
	  x[i,true] = rmiss
	end
      end
    end
  end

  cov = NArray.new(x.typecode,dim*(dim+1)/2)
  sind = 0
  total = 0
  if rmiss.nil?
    for i in 0...dim
      cov[sind..sind+i] = x[0..i,true].mul_add(x[i,true].reshape!(1,len),1)/(len-1)
      total += cov[sind+i]
      sind += i+1
    end
  else
    for i in 0...dim
      for j in 0...i
	cov[sind] = covariance(x[i,true],x[j,true],rmiss)
	sind += 1
      end
      cov[sind] = x[i,true].mask(x[i,true].ne(rmiss)).stddev**2
      total += cov[sind]
      sind += 1
    end
  end

  if ltotal
    return [cov,total]
  else
    return cov
  end

end

def eof( x, rmiss=nil, ndim=nil )

  if x.shape.length!=2 then
    print "err\n"
    exit
  end

  dim,nle = x.shape

  rmiss = nil if !rmiss.nil? && x.ne(rmiss).all?

  p "calc anomary"
  if rmiss.nil? then
    x = x - x.mean(1)

  else
    ary = NArray.byte(dim)
    for i in 0..dim-1
      mask = x[i,true].ne(rmiss)
      tmp = x[i,true]
      tmp2 = tmp.mask(mask)
      if tmp2.length >= nle*3/4 then
        tmp[mask] = tmp2 - tmp2.mean
	x[i,true] = tmp
	ary[i] = 1
      else
	ary[i] = 0
      end
    end
    if !ary.all? then
      tdim = ary.where.length
      y = NArray.sfloat(tdim,nle)
      k=0
      for i in 0..dim-1
	if ary[i]==1 then
	  y[k,true] = x[i,true]
	  k+=1
	end
      end
      if k!=tdim then
	p "BUG: eof of eof_ssl2 k!=tdim"
	exit
      end
      x = y
      tmp = dim
      dim = tdim
      tdim = tmp
    end
  end



  p "make covariance matrix"
  cova, total = covariance_matrix(x,rmiss,false,true)

  p "calc eigen value"
  if ndim.nil? then
    ndim=dim
  end
  val, vec = SSL2.seig2(cova,ndim)

  if !rmiss.nil? && !ary.all? then
    vec2 = NArray.sfloat(tdim,val.length)
    k=0
    for i in 0..tdim-1
      if ary[i]==1 then
	vec2[i,true] = vec[k,true]
	k+=1
      else
	vec2[i,true] = rmiss
      end
    end
    if k!=dim then
      p "BUG: eof of eof_ssl2 k!=dim"
      exit
    end
    vec = vec2
  end


  [val/total,vec]

end

module_function :eof, :covariance_matrix, :covariance

end
