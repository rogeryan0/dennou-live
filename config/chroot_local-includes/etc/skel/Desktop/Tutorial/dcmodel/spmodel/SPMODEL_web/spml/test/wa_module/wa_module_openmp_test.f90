program wa_module_openmp_test

  use wa_module

  implicit none

  integer, parameter :: im=512, jm=256, nm=170, km=16
  integer, parameter :: lm=(nm+1)*(nm+1)
  integer, parameter :: np=8
  integer, parameter :: itr=1

  real(8), dimension(lm,km)     ::  wa_data
  real(8), dimension(im,jm,km)  ::  xya_data

  real(8) :: time0, time1
  integer :: i,l,k

  call wa_Initial( nm, im, jm, km, np )

  do k=1,km
     do l=1,lm
        wa_data(l,k)=k+1.0D-5 * l
     enddo
  enddo

  call APTIME(TIME0)

  do i=1,itr
     xya_data = xya_wa(wa_data)
     wa_data = wa_xya(xya_data)
  enddo

  call APTIME(TIME1)

  print *,'TIME: ',TIME1-TIME0

  do k=1,km
     do l=1,lm
        print *, l,k,wa_data(l,k)
     end do
  enddo

end program wa_module_openmp_test
