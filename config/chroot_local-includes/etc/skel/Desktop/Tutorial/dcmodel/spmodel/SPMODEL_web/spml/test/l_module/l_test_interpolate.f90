!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  l_module テストプログラム :: 補間関数のテスト
!
!履歴  2008/12/24  竹広真一
!
program l_test_interpolate

  use dc_message, only : MessageNotify
  use l_module
  implicit none

  integer, parameter :: jm=16, nm=10

  real(8), dimension(0:nm) ::  l_data
  real(8), dimension(1:jm) ::  y_data

  real(8), parameter       ::  eps = 1.0D-10

  real(8) :: Alat
  real(8) :: y_data_sol
  real(8) :: error
  real(8) :: pi

  pi = atan(1.0D0)*4.0D0
  Alat = pi/3

  call l_Initial( nm, jm )

  call MessageNotify('M','l_test_Interpolate', &
                         'l_module interpolate tests') 


  !---- P_1 のテスト ----
  y_data = sqrt(3.0D0)*sin(y_Lat)                      ! P_1
  y_data_sol = sqrt(3.0D0)*sin(ALat)
  l_data = l_y(y_data)
  error = Interpolate_l(l_data,alat)-y_data_sol
  if ( abs(error) > eps ) then
     call MessageNotify('E','Test of P_1','Interpolate error too large') 
  endif
  call MessageNotify('M','Test of P_1', &
       'Interpolate test of P_1 succeeded!') 

  !---- P_2^0 のテスト ----
  y_data = sqrt(5.0D0)*(3.0/2*sin(y_Lat)**2-1/2.0)                 ! Y_2^0
  y_data_sol = sqrt(5.0D0)*(3.0/2*sin(alat)**2-1.0/2)
  l_data = l_y(y_data)
  error = Interpolate_l(l_data,alat)-y_data_sol
  if ( abs(error) > eps ) then
     call MessageNotify('E','Test of P_2','Interpolate error too large') 
  endif
  call MessageNotify('M','Test of P_2', &
       'Interpolate test of P_2 succeeded!') 

  !---- 一般的な関数のテスト ----
  y_data = (sin(y_Lat)-1)**2*(sin(y_Lat)-0.5)*(sin(y_Lat)+1)
  y_data_sol = (sin(ALat)-1)**2*(sin(ALat)-0.5)*(sin(ALat)+1)
  l_data = l_y(y_data)
  error = Interpolate_l(l_data,alat)-y_data_sol
  if ( abs(error) > eps ) then
     call MessageNotify('E','Test of general func.', 'Interpolate error too large') 
  endif
  call MessageNotify('M','Test of general func.', &
       'Interpolate test of general func succeeded!') 

  call MessageNotify('M','w_test_Interpolate', &
                      'w_interpolate_module interpolate tests succeeded') 

end program l_test_interpolate
