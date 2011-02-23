!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wq_module テストプログラム
!
!      wr_RotRad_wq のテスト (1/r d/dr r)
!      wr_DivRad_wq のテスト (1/r^2 d/dr r^2)
!      wq_Rad2Inv_wq のテスト (1/r^2)
!      wq_Lapla_wq のテスト (1/r^2 d/dr r^2)
!      xyz_GradLon_wq のテスト
!      xyz_GradLat_wq のテスト
!      wq_Div_xyz_xyz_xyz のテスト
!      wr_DivLon_xyr のテスト (1/r cosφ d/dλ)
!      wr_DivLat_xyr のテスト (1/r cosφ d/dφ cosφ)
!  
!履歴  2009/12/06  竹広真一  wq_test_derivative[1-3].f90 を SJPACK 用に改造
!
program wq_module_deriv1_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wq_module
  implicit none

  integer,parameter  :: im=32, jm=16, km=8   ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=15         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ra=1.5               ! 球半径

  real(8), dimension(0:im-1,1:jm,km)     :: xyr_Data
  real(8), dimension(0:im-1,1:jm,km)     :: xyr_Data1

  real(8), dimension(0:im-1,1:jm,km)     :: xyr_VLon
  real(8), dimension(0:im-1,1:jm,km)     :: xyr_VLat
  real(8), dimension(0:im-1,1:jm,km)     :: xyr_VRad
  real(8), dimension(0:im-1,1:jm,km)     :: xyr_GradLon
  real(8), dimension(0:im-1,1:jm,km)     :: xyr_GradLat
  real(8), dimension(0:im-1,1:jm,km)     :: xyr_Div
  real(8), dimension(0:im-1,1:jm,km)     :: xyr_DivLon
  real(8), dimension(0:im-1,1:jm,km)     :: xyr_DivLat
  real(8), dimension(0:im-1,1:jm,km)     :: xyr_Psi

  ! 判定誤差設定
  integer, parameter :: check_digits = 8
  integer, parameter :: ignore = -9

  integer, parameter :: nmin=1, nmax=10

  integer :: n

  call MessageNotify('M','wq_module_deriv1_test', &
       'wq_module derivative function test #1')

  call wq_Initial(im,jm,km,nm,lm,ra)

  !==================== wr_RotRad_wq ====================

  write( 6,* ) 'Test for wr_RotDRad_wq (even mode)'
  do n=nmin+1,nmax,2
     write(6,*) 'n=',n
     xyr_Data = xyr_Rad**n
     xyr_Data1 = (n+1)*xyr_Rad**(n-1)

     xyr_Data = xyr_wr(wr_RotDRad_wq(wq_xyr(xyr_Data)))

     call AssertEqual(&
       message='wq_RotRad_wq with r^n (even n)',                     &
       answer = xyr_Data1,                                           &
       check = xyr_Data,                                             &
       significant_digits = check_digits, ignore_digits = ignore     &
       )
  enddo

  do n=nmin,nmax,2
     xyr_Data = xyr_Rad**n*cos(xyr_Lat)*sin(xyr_Lon)
     xyr_Data1 = (n+1)*xyr_Rad**(n-1)*cos(xyr_Lat)*sin(xyr_Lon)
     xyr_Data = xyr_wr(wr_RotDRad_wq(wq_xyr(xyr_Data)))

     call AssertEqual(&
       message='wq_RotRad_wq with r^n Y_1^-1 (odd n)',               &
       answer = xyr_Data1,                                           &
       check = xyr_Data,                                             &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  enddo

  !==================== wr_DivRad_wq ====================
  do n=nmin,nmax,2
     write(6,*) 'n=',n
     xyr_Data = xyr_Rad**n * cos(xyr_Lat)*sin(xyr_Lon)
     xyr_Data1 = (n+2)*xyr_Rad**(n-1)*cos(xyr_Lat)*sin(xyr_Lon)

     xyr_Data = xyr_wr(wr_DivRad_wq(wq_xyr(xyr_Data)))

     call AssertEqual(&
       message='wr_DivRad_wq with r^n Y_1^-1 (odd n)',               &
       answer = xyr_Data1,                                           &
       check = xyr_Data,                                             &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  enddo

  do n=nmin+1,nmax,2
     write(6,*) 'n=',n
     xyr_Data = xyr_Rad**n
     xyr_Data1 = (n+2)*xyr_Rad**(n-1)

     xyr_Data = xyr_wr(wr_DivRad_wq(wq_xyr(xyr_Data)))

     call AssertEqual(&
       message='wr_DivRad_wq with r^n (even n)',                     &
       answer = xyr_Data1,                                           &
       check = xyr_Data,                                             &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  enddo

  !==================== wq_Rad2Inv_wq ====================
  do n=nmin+2,nmax,2
     write(6,*) 'n=',n
     xyr_Data = xyr_Rad**n * cos(xyr_Lat)*sin(xyr_Lon)
     xyr_Data1 = xyr_Data/xyr_Rad**2

     xyr_Data = xyr_wq(wq_Rad2Inv_wq(wq_xyr(xyr_Data)))

     call AssertEqual(&
       message='wq_Rad2Inv_wq with r^n Y_1^-1 (odd n)',              &
       answer = xyr_Data1,                                           &
       check = xyr_Data,                                             &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  enddo

  do n=nmin+1,nmax,2
     write(6,*) 'n=',n
     xyr_Data = xyr_Rad**n
     xyr_Data1 = xyr_Data/xyr_Rad**2

     xyr_Data = xyr_wq(wq_Rad2Inv_wq(wq_xyr(xyr_Data)))

     call AssertEqual(&
       message='wq_Rad2Inv_wq with r^n (even n)',                    &
       answer = xyr_Data1,                                           &
       check = xyr_Data,                                             &
       significant_digits = check_digits, ignore_digits = ignore     &
       )
  enddo

  !==================== wq_Lapla_wq ====================
  do n=nmin,nmax,2
     write(6,*) 'n=',n
     xyr_Data = xyr_Rad**n * cos(xyr_Lat)*sin(xyr_Lon)
     xyr_Data1 = (n*(n+1)-2)*xyr_Data/xyr_Rad**2

     xyr_Data = xyr_wq(wq_Lapla_wq(wq_xyr(xyr_Data)))

     call AssertEqual(&
       message='wq_Lapla_wq with r^n Y_1^-1 (odd n)',                &
       answer = xyr_Data1,                                           &
       check = xyr_Data,                                             &
       significant_digits = check_digits, ignore_digits = ignore     &
       )
  enddo

  write( 6,* ) 'Test for wq_Lapla_wq(even mode)'
  do n=nmin+1,nmax,2
     write(6,*) 'n=',n
     xyr_Data = xyr_Rad**n
     xyr_Data1 = n*(n+1)*xyr_Data/xyr_Rad**2

     xyr_Data = xyr_wq(wq_Lapla_wq(wq_xyr(xyr_Data)))

     call AssertEqual(&
       message='wq_Lapla_wq with r^n (even n)',                      &
       answer = xyr_Data1,                                           &
       check = xyr_Data,                                             &
       significant_digits = check_digits, ignore_digits = ignore     &
       )
  enddo

  !========== xyr_GradLon_wq, xyr_GradLat_wq, wr_Div_xyr_xyr_xyr ==========
  n=2

  ! ------ r^3 Y_1^1 Poloidal field -----
  xyr_VRad = 0.0D0
  xyr_Psi = xyr_Rad**(n+1) * cos(xyr_Lat)*sin(xyr_Lon)   ! r**3 P_1^1

  xyr_GradLon =  xyr_Rad**n * cos(xyr_Lon)
  xyr_GradLat = -xyr_Rad**n * sin(xyr_Lat)*sin(xyr_Lon)

  xyr_Div = - 2* xyr_Psi/xyr_Rad**2

  call AssertEqual(&
       message='xyr_GradLon_wq with r^3 Y_1^1',                      &
       answer = xyr_GradLon,                                         &
       check = xyr_GradLon_wq(wq_xyr(xyr_Psi)),                      &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='xyr_GradLat_wq with r^3 Y_1^1',                      &
       answer = xyr_GradLat,                                         &
       check = xyr_GradLat_wq(wq_xyr(xyr_Psi)),                      &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='wr_Div_xyr_xyr_xyr with r^3 Y_1^1',                  &
       answer = xyr_Div,                                             &
       check = xyr_wr(wr_Div_xyr_xyr_xyr(xyr_GradLon,xyr_GradLat,xyr_Vrad)), &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  ! ------ r^2 Y_2^1 Poloidal field -----
  xyr_VRad = 0.0D0
  xyr_Psi = xyr_Rad**n * cos(xyr_Lat)*sin(xyr_Lat) * sin(xyr_Lon) ! P_2^1

  xyr_GradLon =  xyr_Rad**(n-1)*sin(xyr_Lat)*cos(xyr_Lon)
  xyr_GradLat =  xyr_Rad**(n-1)*cos(2*xyr_Lat)*sin(xyr_Lon)

  xyr_Div = - 6* xyr_Psi/xyr_Rad**2

  call AssertEqual(&
       message='xyr_GradLon_wq with r^2 Y_2^1',                      &
       answer = xyr_GradLon,                                         &
       check = xyr_GradLon_wq(wq_xyr(xyr_Psi)),                      &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='xyr_GradLat_wq with r^2 Y_2^1',                      &
       answer = xyr_GradLat,                                         &
       check = xyr_GradLat_wq(wq_xyr(xyr_Psi)),                      &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='wr_Div_xyr_xyr_xyr with r^2 Y_2^1',                  &
       answer = xyr_Div,                                             &
       check = xyr_wr(wr_Div_xyr_xyr_xyr(xyr_GradLon,xyr_GradLat,xyr_Vrad)), &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  !========== wr_DivLon_xyr, wr_DivLat_xyr ==========

  xyr_VLon   = xyr_rad**n * cos(xyr_Lat)**2*sin(xyr_Lon)
  xyr_DivLon = xyr_rad**(n-1)*cos(xyr_Lat)*cos(xyr_Lon)

  xyr_VLat   = xyr_rad**n * cos(xyr_Lat)**2*sin(xyr_Lon)
  xyr_DivLat = -3*xyr_rad**(n-1)*cos(xyr_Lat)*sin(xyr_Lat)*sin(xyr_Lon)

  call AssertEqual(&
       message='wr_DivLon_xyr',                                      &
       answer = xyr_DivLon,                                          &
       check = xyr_wr(wr_DivLon_xyr(xyr_VLon)),                      &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='wr_DivLat_xyr',                                      &
       answer = xyr_DivLat,                                          &
       check = xyr_wr(wr_DivLat_xyr(xyr_VLat)),                      &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call MessageNotify('M','wq_module_deriv1_test', &
       'wq_module derivative function test #1 succeeded!')

end program wq_module_deriv1_test
