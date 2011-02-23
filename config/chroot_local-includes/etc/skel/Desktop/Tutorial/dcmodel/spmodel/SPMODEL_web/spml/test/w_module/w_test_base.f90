!----------------------------------------------------------------------
!     Copyright (c) 2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  w_base_module テストプログラム :: 基本変換関数のテスト
!
!履歴  2007/10/28  竹広真一
!      2008/06/28  佐々木洋平  配列の宣言を変更
!
program w_test_base

  use dc_message, only : MessageNotify
  use w_module
  implicit none

  integer, parameter :: im=128, jm=64, nm=42

  real(8), dimension((nm+1)**2) ::  w_data
  real(8), dimension(0:im-1,1:jm)     ::  xy_data
  real(8), parameter            ::  eps = 1.0D-10
  real(8) :: pi

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','w_test_base', &
                         'w_base_module functions tests') 

  call w_Initial( nm, im, jm )

  !---- Y_1^0 のテスト ----
  xy_data = sqrt(3.0D0)*sin(xy_Lat)                      ! 
  w_data= 0.0D0 ; w_data(l_nm(1,0))=1.0D0

  if ( maxval(abs(w_xy(xy_data)-w_data)) > eps ) then
     call MessageNotify('E','Test of Y_1^0','Spectral transform error too large') 
  endif
  if ( maxval(abs(xy_w(w_xy(xy_data))-xy_data)) > eps ) then
     call MessageNotify('E','Test of Y_1^0',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of Y_1^0','Test of Y_1^0 succeeded!') 

  !---- Y_1^1 のテスト ----
  xy_data = sqrt(3.0D0/2)*cos(xy_Lat)*cos(xy_Lon)          ! Y_1^1
  w_data= 0.0D0 ; w_data(l_nm(1,1))=1.0D0/sqrt(2.0D0)

  if ( maxval(abs(w_xy(xy_data)-w_data)) > eps ) then
     call MessageNotify('E','Test of Y_1^1','Spectral transform error too large') 
  endif
  if ( maxval(abs(xy_w(w_xy(xy_data))-xy_data)) > eps ) then
     call MessageNotify('E','Test of Y_1^1',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of Y_1^1','Test of Y_1^1 succeeded!') 

  !---- Y_1^{-1} のテスト ----
  xy_data = -sqrt(3.0D0/2)*cos(xy_Lat)*sin(xy_Lon)         ! Y_1^{-1}
  w_data= 0.0D0 ; w_data(l_nm(1,-1))=1.0D0/sqrt(2.0D0)

  if ( maxval(abs(w_xy(xy_data)-w_data)) > eps ) then
     call MessageNotify('E','Test of Y_1^-1','Spectral transform error too large') 
  endif
  if ( maxval(abs(xy_w(w_xy(xy_data))-xy_data)) > eps ) then
     call MessageNotify('E','Test of Y_1^-1',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of Y_1^-1','Test of Y_1^-1 succeeded!') 

  !---- Y_2^0 のテスト ----
  xy_data = sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0)                 ! Y_2^0
  w_data= 0.0D0 ; w_data(l_nm(2,0))=1.0D0

  if ( maxval(abs(w_xy(xy_data)-w_data)) > eps ) then
     call MessageNotify('E','Test of Y_2^0','Spectral transform error too large') 
  endif
  if ( maxval(abs(xy_w(w_xy(xy_data))-xy_data)) > eps ) then
     call MessageNotify('E','Test of Y_2^0',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of Y_2^0','Test of Y_2^0 succeeded!') 

  !---- Y_2^1 のテスト ----
  xy_data = sqrt(5.0D0/6)*3.0*sin(xy_Lat)*cos(xy_Lat) * cos(xy_Lon)! Y_2^1
  w_data= 0.0D0 ; w_data(l_nm(2,1))=1.0D0/sqrt(2.0D0)

  if ( maxval(abs(w_xy(xy_data)-w_data)) > eps ) then
     call MessageNotify('E','Test of Y_2^1','Spectral transform error too large') 
  endif
  if ( maxval(abs(xy_w(w_xy(xy_data))-xy_data)) > eps ) then
     call MessageNotify('E','Test of Y_2^1',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of Y_2^1','Test of Y_2^1 succeeded!') 

  !---- Y_2^-1 のテスト ----
  xy_data = -sqrt(5.0D0/6)*3.0*sin(xy_Lat)*cos(xy_Lat) * sin(xy_Lon)! Y_2^-1
  w_data= 0.0D0 ; w_data(l_nm(2,-1))=1.0D0/sqrt(2.0D0)

  if ( maxval(abs(w_xy(xy_data)-w_data)) > eps ) then
     write(6,*)l_nm(2,-1), w_xy(xy_data)
     call MessageNotify('E','Test of Y_2^-1','Spectral transform error too large') 
  endif
  if ( maxval(abs(xy_w(w_xy(xy_data))-xy_data)) > eps ) then
     call MessageNotify('E','Test of Y_2^-1',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of Y_2^-1','Test of Y_2^-1 succeeded!') 

  !---- Y_2^2 のテスト ----
  xy_data = sqrt(5.0D0/24)*3.0*cos(xy_Lat)**2*cos(2*xy_Lon)         ! Y_2^2
  w_data= 0.0D0 ; w_data(l_nm(2,2))=1.0D0/sqrt(2.0D0)

  if ( maxval(abs(w_xy(xy_data)-w_data)) > eps ) then
     call MessageNotify('E','Test of Y_2^2','Spectral transform error too large') 
  endif
  if ( maxval(abs(xy_w(w_xy(xy_data))-xy_data)) > eps ) then
     call MessageNotify('E','Test of Y_2^2',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of Y_2^2','Test of Y_2^2 succeeded!') 

  !---- Y_2^-2 のテスト ----
  xy_data = -sqrt(5.0D0/24)*3.0*cos(xy_Lat)**2*sin(2*xy_Lon)         ! Y_2^-2
  w_data= 0.0D0 ; w_data(l_nm(2,-2))=1.0D0/sqrt(2.0D0)

  if ( maxval(abs(w_xy(xy_data)-w_data)) > eps ) then
     call MessageNotify('E','Test of Y_2^-2','Spectral transform error too large') 
  endif
  if ( maxval(abs(xy_w(w_xy(xy_data))-xy_data)) > eps ) then
     call MessageNotify('E','Test of Y_2^-2',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of Y_2^-2','Test of Y_2^-2 succeeded!') 

  !---- 一般的関数のテスト ----
  xy_data = cos(2*xy_Lon-pi/3) &
       *(sin(xy_Lat)-1)**2*(sin(xy_Lat)-0.5)*(sin(xy_Lat)+1)

  if ( maxval(abs(xy_w(w_xy(xy_data))-xy_data)) > eps ) then
     write(6,*) maxval(abs(xy_w(w_xy(xy_data))-xy_data))
     call MessageNotify('E','Test of general function',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of genral function', &
       'Test of general function succeeded!') 

end program w_test_base
