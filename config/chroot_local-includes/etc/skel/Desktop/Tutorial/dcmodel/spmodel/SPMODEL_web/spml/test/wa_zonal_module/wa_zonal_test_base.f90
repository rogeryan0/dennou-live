!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wa_module テストプログラム :: 基本変換関数のテスト
!
!履歴  2008/12/29  竹広真一
!
program wa_zonal_test_base

  use dc_message, only : MessageNotify
  use wa_zonal_module
  implicit none

  integer, parameter :: im=1, jm=16, nm=10, km=3

  real(8), dimension(nm+1,km)         ::  wa_data
  real(8), dimension(0:im-1,1:jm,km)  ::  xya_data
  real(8), parameter                  ::  eps = 1.0D-10
  real(8) :: pi

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','wa_zonal_test_base', &
                         'wa_zonal_module basic functions tests') 

  call wa_Initial( nm, im, jm, km )

  !---- Y_*^0 のテスト ----
  wa_data= 0.0D0 
  xya_data(:,:,1) = sqrt(3.0D0)*sin(xy_Lat)                   ! Y_1^0
  wa_data(l_nm(1,0),1)=1.0D0

  xya_data(:,:,2) = sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0)  ! Y_2^0
  wa_data(l_nm(2,0),2)=1.0D0

  xya_data(:,:,3) = sqrt(7.0D0)*(5.0/2*sin(xy_Lat)**3-3/2.0*sin(xy_Lat))! Y_3^0
  wa_data(l_nm(3,0),3)=1.0D0

  if ( maxval(abs(wa_xya(xya_data)-wa_data)) > eps ) then
     call MessageNotify('E','Test of Y_1^*','Spectral transform error too large') 
  endif
  if ( maxval(abs(xya_wa(wa_xya(xya_data))-xya_data)) > eps ) then
     call MessageNotify('E','Test of Y_*^0',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of Y_*^0','Test of Y_*^0 succeeded!') 

  !---- 一般的関数のテスト ----
  xya_data(:,:,1) = (sin(xy_Lat)-1)**2*(sin(xy_Lat)-0.5)*(sin(xy_Lat)+1)
  xya_data(:,:,2) = (sin(xy_Lat)-1)**2*(sin(xy_Lat)+1)**2
  xya_data(:,:,3) = (sin(xy_Lat)-1)*(sin(xy_Lat)-0.5)**2*(sin(xy_Lat)+1)

  if ( maxval(abs(xya_wa(wa_xya(xya_data))-xya_data)) > eps ) then
     write(6,*) maxval(abs(xya_wa(wa_xya(xya_data))-xya_data))
     call MessageNotify('E','Test of general function',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of genral function', &
       'Test of general function succeeded!') 

  call MessageNotify('M','wa_zonal_test_base', &
                         'wa_zonal_module basic functions tests succeeded!') 

end program wa_zonal_test_base
