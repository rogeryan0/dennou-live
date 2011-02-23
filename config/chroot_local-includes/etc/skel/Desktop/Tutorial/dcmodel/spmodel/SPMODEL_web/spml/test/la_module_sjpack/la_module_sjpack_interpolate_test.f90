!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  la_module_sjpack テストプログラム :: 補間関数のテスト
!
!履歴  2009/09/22  竹広真一
!
program la_module_sjpack_interpolate_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use la_module_sjpack
  implicit none

  integer, parameter :: jm=8, nm=10, km=3

  real(8), dimension(0:nm,km)   ::  la_data
  real(8), dimension(1:jm,km)   ::  ya_data

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  real(8) :: Alat
  real(8), dimension(km) :: a_data_sol
  real(8) :: pi

  pi = atan(1.0D0)*4.0D0
  Alat = pi/3

  call la_Initial( nm, jm )

  call MessageNotify('M','la_module_sjpack_interpolate_test', &
                         'la_module_sjpack interpolate function tests') 


  !---- P_1 のテスト ----
  ya_data(:,1)  = sqrt(3.0D0)*sin(y_Lat)                      ! P_1
  a_data_sol(1) = sqrt(3.0D0)*sin(ALat)

  ya_data(:,2)  = sqrt(5.0D0)*(3.0/2*sin(y_Lat)**2-1/2.0)    ! P_2
  a_data_sol(2) = sqrt(5.0D0)*(3.0/2*sin(alat)**2-1.0/2)

  ya_data(:,3)  = sqrt(7.0D0)*(5.0/2*sin(y_Lat)**3-3*sin(y_Lat)/2.0) ! P_3
  a_data_sol(3) = sqrt(7.0D0)*(5.0/2*sin(alat)**3-3.0*sin(alat)/2)

  la_data = la_ya(ya_data)

  call AssertEqual(&
    message='P_* Test of interpolate function',                   &
    answer = a_data_sol,                                          &
    check = a_Interpolate_la(la_data,alat),                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- 一般的な関数のテスト ----
  ya_data(:,1)  = (sin(y_Lat)-1)**2*(sin(y_Lat)-0.5)*(sin(y_Lat)+1)
  a_data_sol(1) = (sin(ALat)-1)**2*(sin(ALat)-0.5)*(sin(ALat)+1)
  ya_data(:,2)  = (sin(y_Lat)-1)**2*(sin(y_Lat)+1)**2
  a_data_sol(2) = (sin(aLat)-1)**2*(sin(aLat)+1)**2
  ya_data(:,3)  = (sin(y_Lat)-1)**3*(sin(y_Lat)+1)
  a_data_sol(3) = (sin(aLat)-1)**3*(sin(aLat)+1)

  la_data = la_ya(ya_data)

  call AssertEqual(&
    message='generaul function Test of interpolate function',     &
    answer = a_data_sol,                                          &
    check = a_Interpolate_la(la_data,alat),                       &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','la_module_sjpack_interpolate_test', &
                         'la_module_sjpack interpolate function tests succeeded!') 

end program la_module_sjpack_interpolate_test
