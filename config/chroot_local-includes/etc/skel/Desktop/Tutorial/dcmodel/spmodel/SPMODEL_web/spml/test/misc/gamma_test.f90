program gamma_test

  implicit none
  real(8)  :: factrl, gammaln
  real(8)  :: pi, ans
  integer  :: i
  integer,parameter :: iend=10
  external factrl, gammaln

  pi = atan(1.0D0)*4.0D0

  write(6,*)'x, Gamma(x)[calculated, exact]'
  ans = 1.0
  do i=0,iend
     write(6,*)i, factrl(i), ans
     if ( i >= 1 ) ans = ans*(i+1)
  end do

  write(6,*)
  write(6,*)'x, Gamma(x)[calculated, exact]'
  ans=sqrt(PI)
  do i=0,iend
     write(6,*)i+0.5, exp(gammaln(0.5D0+i)), ans
     ans = ans * (i+0.5)
  end do

end program gamma_test
