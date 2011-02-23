!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_module_sjpack テストプログラム
!
!      微分のテスト
!  
!履歴  2009/09/06  竹広真一  wt_test_derivative[1-3].f90 を SJPACK 用に改造
!
program wt_module_sjpack_deriv_test1

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wt_module_sjpack
  implicit none

  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ri=0.5, ro=1.5       ! 内外半径

  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_Data
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_DData

  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_VLon
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_VLat
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_VRad
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_Div
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_Psi


  ! 判定誤差設定
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11

  integer, parameter :: nmin=1, nmax=9

  integer :: n

  call MessageNotify('M','wt_module_sjpack_deriv1_test', &
       'wt_module_sjpack derivative function test #1')

  call wt_Initial(im,jm,km,nm,lm,ri,ro)

  !==================== wt_DivRad_wt ====================
  do n=nmin,nmax
     write(6,*) 'n=',n
     xyz_Data = xyz_Rad**n
     xyz_DData = (n+2)*xyz_Rad**(n-1)

     call AssertEqual(&
       message='wt_Divrat_wt with r^n',                              &
       answer = xyz_DData,                                           &
       check = xyz_wt(wt_DivRad_wt(wt_xyz(xyz_Data))),               &
       significant_digits = check_digits, ignore_digits = ignore     &
       )
  enddo

  !========== xyz_GradLon_wt, xyz_GradLat_wt, wt_Div_xyz_xyz_xyz ==========

  ! ------ r^2 Y_1^1 Toridal field -----
  n = 2
  write(6,*) 'n=',n
  xyz_Psi = xyz_Rad**n * cos(xyz_Lat)*sin(xyz_Lon)   ! r^2 Y_1^1
  xyz_VRad = 0

  xyz_VLon =  xyz_Rad**(n-1)*cos(xyz_Lon)
  call AssertEqual(&
       message='xyz_GradLon_wt with r^2 Y_1^-1',                     &
       answer = xyz_VLon,                                            &
       check = xyz_GradLon_wt(wt_xyz(xyz_Psi)),                      &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  xyz_VLat = -xyz_Rad**(n-1)*sin(xyz_Lat)*sin(xyz_Lon)
  call AssertEqual(&
       message='xyz_GradLat_wt with r^2 Y_1^-1',                     &
       answer = xyz_VLat,                                            &
       check = xyz_GradLat_wt(wt_xyz(xyz_Psi)),                     &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  xyz_Div = - 2 * xyz_Psi/xyz_Rad**2
  call AssertEqual(&
       message='wt_Div_xyz_xyz_xyz_wt with Toroiral r^2 Y_1^-1',     &
       answer = xyz_Div,                                             &
       check = xyz_wt(wt_Div_xyz_xyz_xyz(xyz_VLon,xyz_VLat,xyz_VRad)),&
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  ! ------ r^2Y_2^-1 Toridal field -----
  n = 2
  xyz_VRad = 0
  xyz_Psi = xyz_Rad**n * cos(xyz_Lat)*sin(xyz_Lat) * sin(xyz_Lon) ! r^2Y_2^-1

  xyz_VLon =  xyz_Rad**(n-1)*sin(xyz_Lat)*cos(xyz_Lon)
  call AssertEqual(&
       message='xyz_GradLon_wt with r^2 Y_2^-1',                     &
       answer = xyz_VLon,                                            &
       check =  xyz_GradLon_wt(wt_xyz(xyz_Psi)),                     &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  xyz_VLat = xyz_Rad**(n-1)*cos(2*xyz_Lat)*sin(xyz_Lon)
  call AssertEqual(&
       message='xyz_GradLat_wt with r^2 Y_2^-1',                     &
       answer = xyz_VLat,                                            &
       check = xyz_GradLat_wt(wt_xyz(xyz_Psi)),                      &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  xyz_Div = - 6* xyz_Psi/xyz_Rad**2
  call AssertEqual(&
       message='wt_Div_xyz_xyz_xyz_wt with Toroiral r2 Y_1^-1',      &
       answer = xyz_Div,                                             &
       check = xyz_wt(wt_Div_xyz_xyz_xyz(xyz_VLon,xyz_VLat,xyz_VRad)),&
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  !==================== wt_DivLon_xyz, wt_DivLat_xyz ====================
  n=2
  write(6,*)'n =',n

  xyz_VLon  = xyz_rad**n * cos(xyz_Lat)**2*sin(xyz_Lon)
  xyz_DData = xyz_rad**(n-1)*cos(xyz_Lat)*cos(xyz_Lon)
  call AssertEqual(&
       message='wt_DivLon_xyz with r^n cos^2 φ sinλ',              &
       answer = xyz_DData,                                           &
       check = xyz_wt(wt_DivLon_xyz(xyz_VLon)),                      &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  xyz_VLat   = xyz_rad**n * cos(xyz_Lat)**2*sin(xyz_Lon)
  xyz_DData = -3*xyz_rad**(n-1)*cos(xyz_Lat)*sin(xyz_Lat)*sin(xyz_Lon)
  call AssertEqual(&
       message='wt_DivLat_xyz with r^n cos^2 φ sinλ',              &
       answer = xyz_DData,                                           &
       check = xyz_wt(wt_DivLat_xyz(xyz_VLat)),                      &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call MessageNotify('M','wt_module_sjpack_deriv1_test', &
       'wt_module_sjpack derivative function test #1 succeeded!')

end program wt_module_sjpack_deriv_test1

