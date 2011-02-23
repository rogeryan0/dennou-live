!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_zonal_module_sjack テストプログラム :: 補間関数のテスト
!
!履歴  2009/09/26  竹広真一  wt_module_sjpack_interpolate_test.f90 より改造
!
program wt_zonal_module_sjpack_interpolate_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wt_zonal_module_sjpack
  implicit none

  integer, parameter :: im=1, jm=32, km=16, nm=21, lm=16
  real(8), parameter :: ri = 0.5, ro= 1.5

  real(8), dimension(nm+1,0:lm)            ::  wt_data
  real(8), dimension(0:im-1,1:jm,0:km)     ::  xyz_data

  ! 判定誤差設定
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11

  real(8) :: Alon
  real(8) :: Alat
  real(8) :: Arad
  real(8) :: data_sol
  real(8) :: pi

  pi = atan(1.0D0)*4.0D0

  Alon = pi/3
  Alat = pi/3
  Arad = 0.75
!!$  Alon =pi/4
!!$  Alat =pi/5
!!$  Arad =0.99
!!$  Alon = 3*pi/5
!!$  Alat = -pi/7
!!$  Arad = 1.333 

  call wt_Initial( im, jm, km, nm, lm, ri, ro )

  call MessageNotify('M','wt_zonal_module_sjpack_interpolate_test', &
                         'wt_zonal_module_sjpack interpolate tests') 


  !---- Interpolaate_array000_wt のテスト ----
  xyz_data = sqrt(3.0D0/2)*sin(xyz_Lat) & ! Y_1^0
            * (xyz_Rad-ri) *  (xyz_Rad-ro)
  data_sol = sqrt(3.0D0/2)*sin(alat)    & ! Y_1^0
            * (arad-ri) *  (arad-ro)

  wt_data = wt_xyz(xyz_data)

  call AssertEqual(&
       message='Interpolate_wt Y_1^-1',                              &
       answer = data_sol,                                            &
       check = Interpolate_wt(wt_data,alon,alat,arad),      &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call MessageNotify('M','wt_zonal_module_sjpack_interpolate_test',&
                         'wt_zonal_module_sjpack interpolate function test succeeded!') 

end program wt_zonal_module_sjpack_interpolate_test

