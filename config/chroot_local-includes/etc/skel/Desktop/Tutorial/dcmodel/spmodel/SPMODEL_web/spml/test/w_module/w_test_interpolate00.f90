!----------------------------------------------------------------------
!     Copyright (c) 2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  w_interpolate_module テストプログラム :: 補間関数のテスト
!
!履歴  2007/10/26  竹広真一
!      2008/06/28  佐々木洋平  配列の宣言を変更
!
program w_test_interpolate00

  use dc_message, only : MessageNotify
  use w_module
  implicit none

  integer, parameter :: im=16, jm=8, nm=10

  real(8), dimension((nm+1)**2)   ::  w_data
  real(8), dimension(0:im-1,1:jm) ::  xy_data

  real(8), parameter            ::  eps = 1.0D-10

  real(8) :: Alon
  real(8) :: Alat
  real(8) :: xy_data_sol
  real(8) :: error
  real(8) :: pi

  pi = atan(1.0D0)*4.0D0
  Alon = pi/5 ; Alat = pi/3

  call w_Initial( nm, im, jm )

  call MessageNotify('M','w_test_Interpolate', &
                         'w_interpolate_module interpolate(00) tests') 


  !---- Y_1^0 のテスト ----
  xy_data = sqrt(3.0D0)*sin(xy_Lat)                      ! Y_1^0
  xy_data_sol = sqrt(3.0D0)*sin(ALat)
  w_data = w_xy(xy_data)
  error = Interpolate_w(w_data,alon,alat)-xy_data_sol
  if ( abs(error) > eps ) then
     call MessageNotify('E','Test of Y_1^0','Interpolate error too large') 
  endif
  call MessageNotify('M','Test of Y_1^0', &
       'Interpolate test of Y_1^0 succeeded!') 

  !---- Y_1^1 のテスト ----
  xy_data = sqrt(3.0D0/2)*cos(xy_Lat)*cos(xy_Lon)          ! Y_1^1
  xy_data_sol = sqrt(3.0D0/2)*cos(ALat)*cos(ALon)
  w_data = w_xy(xy_data)
  error = Interpolate_w(w_data,alon,alat)-xy_data_sol
  if ( abs(error) > eps ) then
     call MessageNotify('E','Test of Y_1^1','Interpolate error too large') 
  endif
  call MessageNotify('M','Test of Y_1^1', &
       'Interpolate test of Y_1^1 succeeded!') 

  !---- Y_1^-1 のテスト ----
  xy_data = -sqrt(3.0D0)*cos(xy_Lat)*sin(xy_Lon)         ! Y_1^{-1}
  xy_data_sol = -sqrt(3.0D0)*cos(ALat)*sin(ALon)
  w_data = w_xy(xy_data)
  error = Interpolate_w(w_data,alon,alat)-xy_data_sol
  if ( abs(error) > eps ) then
     call MessageNotify('E','Test of Y_1^-1','Interpolate error too large') 
  endif
  call MessageNotify('M','Test of Y_1^-1', &
       'Interpolate test of Y_1^-1 succeeded!') 

  !---- Y_2^0 のテスト ----
  xy_data = sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0)                 ! Y_2^0
  xy_data_sol = sqrt(5.0D0)*(3.0/2*sin(alat)**2-1.0/2)
  w_data = w_xy(xy_data)
  error = Interpolate_w(w_data,alon,alat)-xy_data_sol
  if ( abs(error) > eps ) then
     call MessageNotify('E','Test of Y_2^0','Interpolate error too large') 
  endif
  call MessageNotify('M','Test of Y_2^0', &
       'Interpolate test of Y_2^0 succeeded!') 

  !---- Y_2^1 のテスト ----
  xy_data = sqrt(5.0D0/6)*3.0*sin(xy_Lat)*cos(xy_Lat) * cos(xy_Lon)! Y_2^1
  xy_data_sol = sqrt(5.0D0/6)*3.0*sin(ALat)*cos(ALat) * cos(ALon)
  w_data = w_xy(xy_data)
  error = Interpolate_w(w_data,alon,alat)-xy_data_sol
  if ( abs(error) > eps ) then
     call MessageNotify('E','Test of Y_2^1','Interpolate error too large') 
  endif
  call MessageNotify('M','Test of Y_2^1', &
       'Interpolate test of Y_2^1 succeeded!') 

  !---- Y_2^-1 のテスト ----
  xy_data = -sqrt(5.0D0/6)*3.0*sin(xy_Lat)*cos(xy_Lat) * sin(xy_Lon)! Y_2^{-1}
  xy_data_sol = -sqrt(5.0D0/6)*3.0*sin(ALat)*cos(ALat) * sin(ALon)
  w_data = w_xy(xy_data)
  error = Interpolate_w(w_data,alon,alat)-xy_data_sol
  if ( abs(error) > eps ) then
     call MessageNotify('E','Test of Y_2^-1','Interpolate error too large') 
  endif
  call MessageNotify('M','Test of Y_2^-1', &
       'Interpolate test of Y_2^-1 succeeded!') 

  !---- Y_2^2 のテスト ----
  xy_data = sqrt(5.0D0/24)*3.0*cos(xy_Lat)**2*cos(2*xy_Lon)             ! Y_2^2
  xy_data_sol = sqrt(5.0D0/24)*3.0*cos(ALat)**2*cos(2*ALon)
  w_data = w_xy(xy_data)
  error = Interpolate_w(w_data,alon,alat)-xy_data_sol
  if ( abs(error) > eps ) then
     call MessageNotify('E','Test of Y_2^2','Interpolate error too large') 
  endif
  call MessageNotify('M','Test of Y_2^2', &
       'Interpolate test of Y_2^2 succeeded!') 

  !---- Y_2^-2 のテスト ----
  xy_data = -sqrt(5.0D0/24)*3.0*cos(xy_Lat)**2*sin(2*xy_Lon)          ! Y_2^-2
  xy_data_sol = -sqrt(5.0D0/24)*3.0*cos(ALat)**2*sin(2*ALon)
  w_data = w_xy(xy_data)
  error = Interpolate_w(w_data,alon,alat)-xy_data_sol
  if ( abs(error) > eps ) then
     call MessageNotify('E','Test of Y_2^-2','Interpolate error too large') 
  endif
  call MessageNotify('M','Test of Y_2^-2', &
       'Interpolate test of Y_2^2 succeeded!') 

  !---- 一般的なのテスト ----
  xy_data = cos(2*xy_Lon-pi/3) &
       *(sin(xy_Lat)-1)**2*(sin(xy_Lat)-0.5)*(sin(xy_Lat)+1)
  xy_data_sol = cos(2*ALon-pi/3) &
       *(sin(ALat)-1)**2*(sin(ALat)-0.5)*(sin(ALat)+1)
  w_data = w_xy(xy_data)
  error = Interpolate_w(w_data,alon,alat)-xy_data_sol
  if ( abs(error) > eps ) then
     call MessageNotify('E','Test of Y_2^-2','Interpolate error too large') 
  endif
  call MessageNotify('M','Test of general func.', &
       'Interpolate test of general func succeeded!') 

  call MessageNotify('M','w_test_Interpolate', &
                      'w_interpolate_module interpolate(00) tests succeeded') 
end program w_test_interpolate00
