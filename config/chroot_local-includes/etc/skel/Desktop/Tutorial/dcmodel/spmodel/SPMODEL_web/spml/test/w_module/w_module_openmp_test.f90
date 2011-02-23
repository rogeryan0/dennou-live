program w_module_openmp_test

  use w_module

  implicit none

  integer, parameter :: im=512, jm=256, nm=170
  integer, parameter :: lm=(nm+1)*(nm+1)
  integer, parameter :: np=8
  integer, parameter :: itr=100

  real(8), dimension(lm)     ::  w_data
  real(8), dimension(im,jm)  ::  xy_data

  real(8) :: time0, time1
  integer :: i,l

  call w_Initial( nm, im, jm, np )

  do l=1,lm
     w_data(l)=1+0.00001D0 * l
  enddo

  call APTIME(TIME0)

  do i=1,itr
     xy_data = xy_w(w_data)
     w_data = w_xy(xy_data)
  enddo

  call APTIME(TIME1)

  print *,'TIME: ',TIME1-TIME0

  do l=1,lm
     print *, l,w_data(l)
  end do

end program w_module_openmp_test
