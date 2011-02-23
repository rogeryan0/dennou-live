!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_zonal_module テストプログラム :: 補間関数のテスト
!
!履歴  2008/12/30  竹広真一
!
program wt_zonal_test_interpolate

  use dc_message, only : MessageNotify
  use wt_zonal_module
  implicit none

  integer, parameter :: im=1, jm=32, km=16, nm=21, lm=16
  real(8), parameter :: ri = 0.5, ro= 1.5

  real(8), dimension(nm+1,0:lm)            ::  wt_data
  real(8), dimension(0:im-1,1:jm,0:km)     ::  xyz_data

  real(8), parameter            ::  eps = 1.0D-10

  real(8) :: Alon
  real(8) :: Alat
  real(8) :: Arad
  real(8) :: data_sol
  real(8) :: error
  real(8) :: pi

  pi = atan(1.0D0)*4.0D0

  Alon = pi/4
  Alat = pi/5
  Arad = 0.99

  call wt_Initial( im, jm, km, nm, lm, ri, ro )

  call MessageNotify('M','wt_test_Interpolate', &
                         'wt_module interpolate tests') 

  xyz_data = sqrt(3.0D0/2)*sin(xyz_Lat) & ! Y_1^0
            * (xyz_Rad-ri) *  (xyz_Rad-ro)
  data_sol = sqrt(3.0D0/2)*sin(alat)    & ! Y_1^0
            * (arad-ri) *  (arad-ro)

  wt_data = wt_xyz(xyz_data)
  error = Interpolate_wt(wt_data,alon,alat,arad)-data_sol
  if ( abs(error) > eps ) then
     call MessageNotify('E','Test of Interpolate_wt',&
                            'Interpolate error too large') 
  endif

  call MessageNotify('M','wt_test_Interpolate', &
                         'wt_module interpolate tests succeeded') 

end program wt_zonal_test_interpolate
