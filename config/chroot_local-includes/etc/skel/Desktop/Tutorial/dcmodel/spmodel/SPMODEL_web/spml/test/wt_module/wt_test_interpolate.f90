!----------------------------------------------------------------------
!     Copyright (c) 2007--2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_test_interpolate テストプログラム :: 補間関数のテスト
!
!履歴  2007/11/03  竹広真一
!      2008/06/28  佐々木洋平  配列の宣言を変更
!      2009/01/04  竹広真一  spml 書法に反するため複数補間点対応版削除
!
program wt_test_interpolate

  use dc_message, only : MessageNotify
  use wt_module
  implicit none

  integer, parameter :: im=64, jm=32, km=16, nm=21, lm=16
  real(8), parameter :: ri = 0.5, ro= 1.5

  real(8), dimension((nm+1)**2,0:lm) ::  wt_data
  real(8), dimension(0:im-1,1:jm,0:km)     ::  xyz_data

  real(8), parameter            ::  eps = 1.0D-10

  real(8) :: Alon(3)
  real(8) :: Alat(3)
  real(8) :: Arad(3)
  real(8), dimension(3) :: data_sol
  real(8) :: error
  real(8) :: pi

  pi = atan(1.0D0)*4.0D0

  Alon = (/pi/3,pi/4,3*pi/5/)
  Alat = (/pi/3,pi/5,-pi/7/)
  Arad = (/0.75,0.99,1.333/) 

  call wt_Initial( im, jm, km, nm, lm, ri, ro )

  call MessageNotify('M','wt_test_Interpolate', &
                         'wt_module interpolate tests') 


  !---- Interpolaate_array000_wt のテスト ----
  xyz_data = sqrt(3.0D0/2)*cos(xyz_Lat)*cos(xyz_Lon) & ! Y_1^1
            * (xyz_Rad-ri) *  (xyz_Rad-ro)
  data_sol(1) = sqrt(3.0D0/2)*cos(ALat(1))*cos(ALon(1)) & ! Y_1^1
            * (ARad(1)-ri) *  (ARad(1)-ro)

  wt_data = wt_xyz(xyz_data)
  error = Interpolate_wt(wt_data,alon(1),alat(1),arad(1))-data_sol(1)
  if ( abs(error) > eps ) then
     call MessageNotify('E','Test of Interpolate_array000_wt',&
                            'Interpolate error too large') 
  endif
  call MessageNotify('M','Test of Interpolate_array000_wt',&
                         'Interpolate test succeeded!') 

end program wt_test_interpolate
