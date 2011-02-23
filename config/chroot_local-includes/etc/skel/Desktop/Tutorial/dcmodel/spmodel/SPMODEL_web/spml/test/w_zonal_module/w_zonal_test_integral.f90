!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  w_zonal_module テストプログラム :: 積分・平均関数のテスト
!
!履歴  2008/12/28  竹広真一
!
program w_zonal_test_integral

  use dc_message, only : MessageNotify
  use w_zonal_module
  implicit none

  integer, parameter :: im=1, jm=64, nm=42

  real(8), dimension(0:im-1,1:jm) ::  xy_data            ! 元の関数
  real(8), dimension(0:im-1)      ::  x_idata            ! 積分・平均の正解
  real(8), dimension(1:jm)        ::  y_idata            ! 積分・平均の正解
  real(8)                         ::  idata              ! 積分・平均の正解
  real(8), parameter              ::  eps = 1.0D-10
  real(8) :: pi

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','w_test_integral', &
                         'w_integral_module function tests') 

  call w_Initial( nm, im, jm )

  !---- 積分のテスト ----
  xy_data = sin(xy_Lat)**2

  idata = 4*pi/3                     ! IntLonLat_xy
  if ( abs(IntLonLat_xy(xy_data)-idata) > eps ) then
     call MessageNotify('E','Test of IntLonLat_xy','Error too large') 
  endif
  call MessageNotify('M','Test of IntLonLat_xy', 'Test succeeded!') 

  y_idata = 2*pi*sin(y_Lat)**2       ! y_IntLon_xy
  if ( maxval(abs(y_IntLon_xy(xy_data)-y_idata)) > eps ) then
     call MessageNotify('E','Test of y_IntLon_xy','Error too large') 
  endif
  call MessageNotify('M','Test of y_IntLon_xy','Test succeeded!') 

  if ( abs(IntLat_y(y_idata)-idata) > eps ) then
     call MessageNotify('E','Test of IntLat_y','Error too large') 
  endif
  call MessageNotify('M','Test of IntLat_y','Test succeeded!') 

  x_idata = 2.0D0/3                  ! x_IntLat_xy
  if ( maxval(abs(x_IntLat_xy(xy_data)-x_idata)) > eps ) then
     call MessageNotify('E','Test of x_IntLat_xy','Error too large') 
  endif
  call MessageNotify('M','Test of x_Intat_xy','Test succeeded!') 

  if ( abs(IntLon_x(x_idata)-idata) > eps ) then
     call MessageNotify('E','Test of IntLon_x','Error too large') 
  endif
  call MessageNotify('M','Test of IntLon_x','Test succeeded!') 

  !---- 平均のテスト ----
  xy_data = sin(xy_Lat)**2

  idata = 1.0D0/3       ! AvrLonLat_xy
  if ( abs(AvrLonLat_xy(xy_data)-idata) > eps ) then
     call MessageNotify('E','Test of AvrLonLat_xy','Error too large') 
  endif
  call MessageNotify('M','Test of AvrLonLat_xy', 'Test succeeded!') 

  y_idata = sin(y_Lat)**2       ! y_AvrLon_xy
  if ( maxval(abs(y_AvrLon_xy(xy_data)-y_idata)) > eps ) then
     call MessageNotify('E','Test of y_AvrLon_xy','Error too large') 
  endif
  call MessageNotify('M','Test of y_AvrLon_xy','Test succeeded!') 

  if ( abs(AvrLat_y(y_idata)-idata) > eps ) then
     call MessageNotify('E','Test of AvrLat_y','Error too large') 
  endif
  call MessageNotify('M','Test of AvrLat_y','Test succeeded!') 

  x_idata = 1.0D0/3             ! x_AvrLat_xy
  if ( maxval(abs(x_AvrLat_xy(xy_data)-x_idata)) > eps ) then
     call MessageNotify('E','Test of x_AvrLat_xy','Error too large') 
  endif
  call MessageNotify('M','Test of x_Avrat_xy','Test succeeded!') 

  if ( abs(AvrLon_x(x_idata)-idata) > eps ) then
     call MessageNotify('E','Test of AvrLon_x','Error too large') 
  endif
  call MessageNotify('M','Test of AvrLon_x','Test succeeded!') 


  call MessageNotify('M','w_test_integral', &
                         'w_zonal_module integral function tests succeded!') 

end program w_zonal_test_integral
