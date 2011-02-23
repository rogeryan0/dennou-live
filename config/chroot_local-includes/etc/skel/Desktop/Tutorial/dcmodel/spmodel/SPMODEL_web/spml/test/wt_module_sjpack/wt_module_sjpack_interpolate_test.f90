!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wt_module_sjack �ƥ��ȥץ���� :: ��ִؿ��Υƥ���
!
!����  2009/09/07  �ݹ�����  w_test_interpolate.f90 ��� sjpack �Ѥ˲�¤
!
program wt_module_sjpack_interpolate_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wt_module_sjpack
  implicit none

  integer, parameter :: im=64, jm=32, km=16, nm=21, lm=16
  real(8), parameter :: ri = 0.5, ro= 1.5

  real(8), dimension((nm+1)**2,0:lm)       ::  wt_data
  real(8), dimension(0:im-1,1:jm,0:km)     ::  xyz_data

  ! Ƚ�������
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

  call MessageNotify('M','wt_module_sjpack_interpolate_test', &
                         'wt_module_sjpack interpolate tests') 


  !---- Interpolaate_array000_wt �Υƥ��� ----
  xyz_data = sqrt(3.0D0/2)*cos(xyz_Lat)*cos(xyz_Lon) & ! Y_1^1
            * (xyz_Rad-ri) *  (xyz_Rad-ro)
  data_sol = sqrt(3.0D0/2)*cos(ALat)*cos(ALon) * (ARad-ri) *  (ARad-ro)

  wt_data = wt_xyz(xyz_data)

  call AssertEqual(&
       message='Interpolate_wt Y_1^-1',                              &
       answer = data_sol,                                            &
       check = Interpolate_wt(wt_data,alon,alat,arad),      &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call MessageNotify('M','wt_module_sjpack_interpolate_test',&
                         'wt_module_sjpack interpolate function test succeeded!') 

end program wt_module_sjpack_interpolate_test

