!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wa_zonal_module テストプログラム :: 積分・平均関数のテスト
!
!履歴  2008/12/29  竹広真一
!
program wa_zonal_test_integral

  use dc_message, only : MessageNotify
  use wa_zonal_module
  use dc_trace, only : SetDebug
  implicit none

  integer, parameter :: im=1, jm=64, nm=42, km=2

  real(8), dimension(0:im-1,1:jm,km) ::  xya_data            ! 元の関数
  real(8), dimension(0:im-1,km)      ::  xa_idata            ! 積分・平均の正解
  real(8), dimension(1:jm,km)        ::  ya_idata            ! 積分・平均の正解
  real(8), dimension(km)             ::  a_idata             ! 積分・平均の正解
  real(8), parameter                 ::  eps = 1.0D-10
  real(8) :: pi

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','wa_zonal_test_integral', &
                         'wa_module integral/average function tests') 

  call wa_Initial( nm, im, jm, km )

  !---- 積分のテスト ----
  xya_data(:,:,1) = sin(xy_Lat)**2
  xya_data(:,:,2) = sin(xy_Lat)**4

  a_idata(1) = 4*pi/3       ! a_IntLonLat_xya
  a_idata(2) = 4*pi/5       ! a_IntLonLat_xya

  if ( maxval(abs(a_IntLonLat_xya(xya_data)-a_idata)) > eps ) then
     call MessageNotify('E','Test of IntLonLat_xya','Error too large') 
  endif
  call MessageNotify('M','Test of a_IntLonLat_xya', 'Test succeeded!') 

  ya_idata(:,1) = 2*pi*sin(y_Lat)**2       ! y_IntLon_xya
  ya_idata(:,2) = 2*pi*sin(y_Lat)**4       ! y_IntLon_xya
  if ( maxval(abs(ya_IntLon_xya(xya_data)-ya_idata)) > eps ) then
     call MessageNotify('E','Test of ya_IntLon_xya','Error too large') 
  endif
  call MessageNotify('M','Test of ya_IntLon_xya','Test succeeded!') 

  if ( maxval(abs(a_IntLat_ya(ya_idata)-a_idata)) > eps ) then
     call MessageNotify('E','Test of IntLat_y','Error too large') 
  endif
  call MessageNotify('M','Test of a_IntLat_ya','Test succeeded!') 

  xa_idata(:,1) = 2.0D0/3       ! x_IntLat_xya
  xa_idata(:,2) = 2.0D0/5       ! x_IntLat_xya
  if ( maxval(abs(xa_IntLat_xya(xya_data)-xa_idata)) > eps ) then
     call MessageNotify('E','Test of x_IntLat_xya','Error too large') 
  endif
  call MessageNotify('M','Test of x_Intat_xya','Test succeeded!') 

  if ( maxval(abs(a_IntLon_xa(xa_idata)-a_idata)) > eps ) then
     call MessageNotify('E','Test of a_IntLon_xa','Error too large') 
  endif
  call MessageNotify('M','Test of a_IntLon_xa','Test succeeded!') 

  !---- 平均のテスト ----
  xya_data(:,:,1) = sin(xy_Lat)**2
  xya_data(:,:,2) = sin(xy_Lat)**4

  a_idata(1) = 1.0D0/3       ! AvrLonLat_xya
  a_idata(2) = 1.0D0/5       ! AvrLonLat_xya
  if ( maxval(abs(a_AvrLonLat_xya(xya_data)-a_idata)) > eps ) then
     call MessageNotify('E','Test of a_AvrLonLat_xya','Error too large') 
  endif
  call MessageNotify('M','Test of a_AvrLonLat_xya', 'Test succeeded!') 

  ya_idata(:,1) = sin(y_Lat)**2       ! y_AvrLon_xya
  ya_idata(:,2) = sin(y_Lat)**4       ! y_AvrLon_xya
  if ( maxval(abs(ya_AvrLon_xya(xya_data)-ya_idata)) > eps ) then
     call MessageNotify('E','Test of y_AvrLon_xya','Error too large') 
  endif
  call MessageNotify('M','Test of y_AvrLon_xya','Test succeeded!') 

  if ( maxval(abs(a_AvrLat_ya(ya_idata)-a_idata)) > eps ) then
     call MessageNotify('E','Test of a_AvrLat_ya','Error too large') 
  endif
  call MessageNotify('M','Test of a_AvrLat_ya','Test succeeded!') 

  xa_idata(:,1) = 1.0D0/3       ! x_AvrLat_xya
  xa_idata(:,2) = 1.0D0/5       ! x_AvrLat_xya
  if ( maxval(abs(xa_AvrLat_xya(xya_data)-xa_idata)) > eps ) then
     call MessageNotify('E','Test of xa_AvrLat_xya','Error too large') 
  endif
  call MessageNotify('M','Test of xaa_Avrat_xya','Test succeeded!') 

  if ( maxval(abs(a_AvrLon_xa(xa_idata)-a_idata)) > eps ) then
     call MessageNotify('E','Test of a_AvrLon_xa','Error too large') 
  endif
  call MessageNotify('M','Test of a_AvrLon_xa','Test succeeded!') 


  call MessageNotify('M','wa_zonal_test_integral', &
                         'wa_module integral/average function tests succeded!') 

end program wa_zonal_test_integral
