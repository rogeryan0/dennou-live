!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  w_base_module_sjpack テストプログラム :: 基本変換関数のテスト
!
!履歴  2009/09/03  竹広真一
!
program w_base_module_sjpack_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use w_module_sjpack
  implicit none

  integer, parameter :: imm=32, jmm=16, nmm=10

  real(8), dimension((nmm+1)**2)      ::  w_data
  real(8), dimension(0:imm-1,1:jmm)   ::  xy_data
  real(8), dimension(0:imm-1,1:jmm)   ::  xy_Ddata
  real(8) :: pi

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','w_base_module_sjpack', &
                         'w_base_module_sjpack functions tests') 

  call w_Initial( nmm, imm, jmm )

  !==================== Y_1^0 のテスト ====================
  xy_data = sqrt(3.0D0)*sin(xy_Lat)                      ! Y_1^0
  w_data= 0.0D0 ; w_data(l_nm(1,0))=1.0D0

  !---- 通常変換 ----
  call AssertEqual(&
    message='Y_1^0 Test of w_xy(xy_data)',                        &
    answer = w_data,                                              &
    check = w_xy(xy_data),                                        &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Y_1^0 Test of xy_w(w_xy(xy_data))',                  &
    answer = xy_data,                                             &
    check = xy_w(w_xy(xy_data)),                                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- X 微分変換 ----
  xy_Ddata = 0.0D0
  call AssertEqual(&
    message='Y_1^0 Test of xy_w(w_xy(xy_data,iflag=-1))',         &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data,iflag=-1)),                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Y_1^0 Test of xy_w(w_xy(xy_data),iflag=-1)',         &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data),iflag=-1),                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Y 微分変換 ----
  xy_Ddata = sqrt(3.0D0) * ( cos(xy_Lat)**2 - 2*sin(xy_Lat)**2 )
  call AssertEqual(&
    message='Y_1^0 Test of xy_w(w_xy(xy_data,iflag=1,ipow=0))',   &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data,iflag=1,ipow=0)),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xy_Ddata = sqrt(3.0D0)*cos(xy_Lat)
  call AssertEqual(&
    message='Y_1^0 Test of xy_w(w_xy(xy_data),iflag=1,ipow=1)',   &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data),iflag=1,ipow=1),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- SIN 掛け算変換 ----
  xy_Ddata = sqrt(3.0D0) * sin(xy_Lat)**2
  call AssertEqual(&
    message='Y_1^0 Test of xy_w(w_xy(xy_data,iflag=2))',          &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data,iflag=2)),                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Y_1^0 Test of xy_w(w_xy(xy_data),iflag=2)',          &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data),iflag=2),                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','Test of Y_1^0','Test of Y_1^0 succeeded!') 

  !==================== Y_1^1 のテスト ====================
  xy_data = sqrt(3.0D0/2)*cos(xy_Lat)*cos(xy_Lon)          ! Y_1^1
  w_data= 0.0D0 ; w_data(l_nm(1,1))=1.0D0/2.0D0

  !---- 通常変換 ----
  call AssertEqual(&
    message='Y_1^1 Test of w_xy(xy_data)',                        &
    answer = w_data,                                              &
    check = w_xy(xy_data),                                        &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Y_1^1 Test of xy_w(w_xy(xy_data))',                  &
    answer = xy_data,                                             &
    check = xy_w(w_xy(xy_data)),                                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- X 微分変換 ----
  xy_Ddata = -sqrt(3.0D0/2)*cos(xy_Lat)*sin(xy_Lon)          ! Y_1^1
  call AssertEqual(&
    message='Y_1^1 Test of xy_w(w_xy(xy_data,iflag=-1))',         &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data,iflag=-1)),                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Y_1^1 Test of xy_w(w_xy(xy_data),iflag=-1)',         &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data),iflag=-1),                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Y 微分変換 ----
  xy_Ddata = - sqrt(3.0D0/2) * 3 * sin(xy_Lat) * cos(xy_Lat) * cos(xy_Lon)
  call AssertEqual(&
    message='Y_1^1 Test of xy_w(w_xy(xy_data,iflag=1,ipow=0))',   &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data,iflag=1,ipow=0)),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xy_Ddata = - sqrt(3.0D0/2) * sin(xy_Lat) * cos(xy_Lon)
  call AssertEqual(&
    message='Y_1^1 Test of xy_w(w_xy(xy_data),iflag=1,ipow=1)',   &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data),iflag=1,ipow=1),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- SIN 掛け算変換 ----
  xy_Ddata = sqrt(3.0D0/2) * sin(xy_Lat)* cos(xy_Lat) * cos(xy_Lon)
  call AssertEqual(&
    message='Y_1^1 Test of xy_w(w_xy(xy_data,iflag=2))',          &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data,iflag=2)),                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Y_1^1 Test of xy_w(w_xy(xy_data),iflag=2)',          &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data),iflag=2),                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','Test of Y_1^1','Test of Y_1^1 succeeded!') 

  !==================== Y_1^{-1} のテスト ====================
  xy_data = -sqrt(3.0D0/2)*cos(xy_Lat)*sin(xy_Lon)         ! Y_1^{-1}
  w_data= 0.0D0 ; w_data(l_nm(1,-1))=1.0D0/2.0D0

  !---- 通常変換 ----
  call AssertEqual(&
    message='Y_1^1 Test of w_xy(xy_data)',                        &
    answer = w_data,                                              &
    check = w_xy(xy_data),                                        &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Y_1^1 Test of xy_w(w_xy(xy_data))',                  &
    answer = xy_data,                                             &
    check = xy_w(w_xy(xy_data)),                                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- X 微分変換 ----
  xy_Ddata = -sqrt(3.0D0/2)*cos(xy_Lat)*cos(xy_Lon)          ! Y_1^{-1}
  call AssertEqual(&
    message='Y_1^1 Test of xy_w(w_xy(xy_data,iflag=-1))',         &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data,iflag=-1)),                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Y_1^1 Test of xy_w(w_xy(xy_data),iflag=-1)',         &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data),iflag=-1),                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Y 微分変換 ----
  xy_Ddata = sqrt(3.0D0/2) * 3 * sin(xy_Lat) * cos(xy_Lat) * sin(xy_Lon)
  call AssertEqual(&
    message='Y_1^1 Test of xy_w(w_xy(xy_data,iflag=1,ipow=0))',   &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data,iflag=1,ipow=0)),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xy_Ddata = sqrt(3.0D0/2) * sin(xy_Lat) * sin(xy_Lon)
  call AssertEqual(&
    message='Y_1^1 Test of xy_w(w_xy(xy_data),iflag=1,ipow=1)',   &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data),iflag=1,ipow=1),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- SIN 掛け算変換 ----
  xy_Ddata = - sqrt(3.0D0/2) * sin(xy_Lat)* cos(xy_Lat) * sin(xy_Lon)
  call AssertEqual(&
    message='Y_1^1 Test of xy_w(w_xy(xy_data,iflag=2))',          &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data,iflag=2)),                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Y_1^1 Test of xy_w(w_xy(xy_data),iflag=2)',          &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data),iflag=2),                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !==================== Y_2^0 のテスト ====================
  xy_data = sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0)                 ! Y_2^0
  w_data= 0.0D0 ; w_data(l_nm(2,0))=1.0D0

  !---- 通常変換 ----
  call AssertEqual(&
    message='Y_2^0 Test of w_xy(xy_data)',                        &
    answer = w_data,                                              &
    check = w_xy(xy_data),                                        &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Y_2^0 Test of xy_w(w_xy(xy_data))',                  &
    answer = xy_data,                                             &
    check = xy_w(w_xy(xy_data)),                                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- X 微分変換 ----
  xy_Ddata = 0.0D0
  call AssertEqual(&
    message='Y_2^0 Test of xy_w(w_xy(xy_data,iflag=-1))',         &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data,iflag=-1)),                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Y_2^0 Test of xy_w(w_xy(xy_data),iflag=-1)',         &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data),iflag=-1),                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Y 微分変換 ----
  xy_Ddata = sqrt(5.0D0)*(   3.0*sin(xy_Lat)*cos(xy_Lat)**2  &
                           - 3.0*sin(xy_Lat)**3 + sin(xy_Lat)  ) 
  call AssertEqual(&
    message='Y_2^0 Test of xy_w(w_xy(xy_data,iflag=1,ipow=0))',   &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data,iflag=1,ipow=0)),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xy_Ddata = sqrt(5.0D0)*3.0*sin(xy_Lat)*cos(xy_Lat)
  call AssertEqual(&
    message='Y_2^0 Test of xy_w(w_xy(xy_data),iflag=1,ipow=1)',   &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data),iflag=1,ipow=1),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- SIN 掛け算変換 ----
  xy_Ddata = sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0)*sin(xy_Lat)
  call AssertEqual(&
    message='Y_2^0 Test of xy_w(w_xy(xy_data,iflag=2))',          &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data,iflag=2)),                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Y_2^0 Test of xy_w(w_xy(xy_data),iflag=2)',          &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data),iflag=2),                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','Test of Y_2^0','Test of Y_2^0 succeeded!') 

  !==================== Y_2^1 のテスト ====================
  xy_data = sqrt(5.0D0/6)*3.0*sin(xy_Lat)*cos(xy_Lat) * cos(xy_Lon)! Y_2^1
  w_data= 0.0D0 ; w_data(l_nm(2,1))=1.0D0/2.0D0

  !---- 通常変換 ----
  call AssertEqual(&
    message='Y_2^1 Test of w_xy(xy_data)',                        &
    answer = w_data,                                              &
    check = w_xy(xy_data),                                        &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Y_2^1 Test of xy_w(w_xy(xy_data))',                  &
    answer = xy_data,                                             &
    check = xy_w(w_xy(xy_data)),                                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- X 微分変換 ----
  xy_Ddata = -sqrt(5.0D0/6)*3.0*sin(xy_Lat)*cos(xy_Lat)*sin(xy_Lon)! Y_2^1
  call AssertEqual(&
    message='Y_2^1 Test of xy_w(w_xy(xy_data,iflag=-1))',         &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data,iflag=-1)),                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Y_2^1 Test of xy_w(w_xy(xy_data),iflag=-1)',         &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data),iflag=-1),                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Y 微分変換 ----
  xy_Ddata = sqrt(5.0D0/6)*3.0 &
       *(cos(xy_Lat)**3-3*sin(xy_Lat)**2*cos(xy_Lat))*cos(xy_Lon)
  call AssertEqual(&
    message='Y_2^1 Test of xy_w(w_xy(xy_data,iflag=1,ipow=1))',   &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data,iflag=1,ipow=0)),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xy_Ddata = sqrt(5.0D0/6)*3.0 * cos(2*xy_Lat) * cos(xy_Lon)
  call AssertEqual(&
    message='Y_2^1 Test of xy_w(w_xy(xy_data),iflag=1,ipow=1)',   &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data),iflag=1,ipow=1),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- SIN 掛け算変換 ----
  xy_Ddata = sqrt(5.0D0/6)*3.0*sin(xy_Lat)**2*cos(xy_Lat) * cos(xy_Lon)
  call AssertEqual(&
    message='Y_2^1 Test of xy_w(w_xy(xy_data,iflag=2))',          &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data,iflag=2)),                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Y_2^1 Test of xy_w(w_xy(xy_data),iflag=2)',          &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data),iflag=2),                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','Test of Y_2^1','Test of Y_2^1 succeeded!') 

  !==================== Y_2^-1 のテスト ====================
  xy_data = -sqrt(5.0D0/6)*3.0*sin(xy_Lat)*cos(xy_Lat) * sin(xy_Lon)! Y_2^-1
  w_data= 0.0D0 ; w_data(l_nm(2,-1))=1.0D0/2.0D0

  !---- 通常変換 ----
  call AssertEqual(&
    message='Y_2^-1 Test of w_xy(xy_data)',                       &
    answer = w_data,                                              &
    check = w_xy(xy_data),                                        &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Y_2^-1 Test of xy_w(w_xy(xy_data))',                 &
    answer = xy_data,                                             &
    check = xy_w(w_xy(xy_data)),                                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- X 微分変換 ----
  xy_Ddata = -sqrt(5.0D0/6)*3.0*sin(xy_Lat)*cos(xy_Lat)*cos(xy_Lon)! Y_2^-1
  call AssertEqual(&
    message='Y_2^-1 Test of xy_w(w_xy(xy_data,iflag=-1))',        &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data,iflag=-1)),                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Y_2^-1 Test of xy_w(w_xy(xy_data),iflag=-1)',        &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data),iflag=-1),                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Y 微分変換 ----
  xy_Ddata = -sqrt(5.0D0/6)*3.0 &
       *(cos(xy_Lat)**3-3*sin(xy_Lat)**2*cos(xy_Lat))*sin(xy_Lon)
  call AssertEqual(&
    message='Y_2^-1 Test of xy_w(w_xy(xy_data,iflag=1,ipow=0))',  &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data,iflag=1,ipow=0)),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xy_Ddata = -sqrt(5.0D0/6)*3.0 * cos(2*xy_Lat) * sin(xy_Lon)
  call AssertEqual(&
    message='Y_2^-1 Test of xy_w(w_xy(xy_data),iflag=1,ipow=1)',  &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data),iflag=1,ipow=1),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- SIN 掛け算変換 ----
  xy_Ddata = -sqrt(5.0D0/6)*3.0*sin(xy_Lat)**2*cos(xy_Lat) * sin(xy_Lon)
  call AssertEqual(&
    message='Y_2^-1 Test of xy_w(w_xy(xy_data,iflag=2))',         &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data,iflag=2)),                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Y_2^-1 Test of xy_w(w_xy(xy_data),iflag=2)',         &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data),iflag=2),                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','Test of Y_2^-1','Test of Y_2^-1 succeeded!') 

  !==================== Y_2^2 のテスト ====================
  xy_data = sqrt(5.0D0/24)*3.0*cos(xy_Lat)**2*cos(2*xy_Lon)         ! Y_2^2
  w_data= 0.0D0 ; w_data(l_nm(2,2))=1.0D0/2.0D0

  !---- 通常変換 ----
  call AssertEqual(&
    message='Y_2^2 Test of w_xy(xy_data)',                        &
    answer = w_data,                                              &
    check = w_xy(xy_data),                                        &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Y_2^2 Test of xy_w(w_xy(xy_data))',                  &
    answer = xy_data,                                             &
    check = xy_w(w_xy(xy_data)),                                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- X 微分変換 ----
  xy_Ddata = sqrt(5.0D0/24)*3.0*cos(xy_Lat)**2* (-2)* sin(2*xy_Lon)  ! Y_2^2
  call AssertEqual(&
    message='Y_2^2 Test of xy_w(w_xy(xy_data,iflag=-1))',         &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data,iflag=-1)),                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Y_2^2 Test of xy_w(w_xy(xy_data),iflag=-1)',         &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data),iflag=-1),                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Y 微分変換 ----
  xy_Ddata = - sqrt(5.0D0/24)*3.0*4*sin(xy_Lat)*cos(xy_Lat)**2*cos(2*xy_Lon)
  call AssertEqual(&
    message='Y_2^2 Test of xy_w(w_xy(xy_data,iflag=1,ipow=0))',   &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data,iflag=1,ipow=0)),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xy_Ddata = -sqrt(5.0D0/24)*3.0*2*cos(xy_Lat)*sin(xy_Lat)*cos(2*xy_Lon)
  call AssertEqual(&
    message='Y_2^2 Test of xy_w(w_xy(xy_data),iflag=1,ipow=1)',   &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data),iflag=1,ipow=1),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- SIN 掛け算変換 ----
  xy_Ddata = sqrt(5.0D0/24)*3.0*cos(xy_Lat)**2*sin(xy_Lat)*cos(2*xy_Lon)
  call AssertEqual(&
    message='Y_2^2 Test of xy_w(w_xy(xy_data,iflag=2))',          &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data,iflag=2)),                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Y_2^2 Test of xy_w(w_xy(xy_data),iflag=2)',          &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data),iflag=2),                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','Test of Y_2^2','Test of Y_2^2 succeeded!') 

  !==================== Y_2^-2 のテスト ====================
  xy_data = -sqrt(5.0D0/24)*3.0*cos(xy_Lat)**2*sin(2*xy_Lon)         ! Y_2^-2
  w_data= 0.0D0 ; w_data(l_nm(2,-2))=1.0D0/2.0D0

  !---- 通常変換 ----
  call AssertEqual(&
    message='Y_2^-2 Test of w_xy(xy_data)',                        &
    answer = w_data,                                              &
    check = w_xy(xy_data),                                        &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Y_2^-2 Test of xy_w(w_xy(xy_data))',                 &
    answer = xy_data,                                             &
    check = xy_w(w_xy(xy_data)),                                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- X 微分変換 ----
  xy_Ddata = -sqrt(5.0D0/24)*3.0*cos(xy_Lat)**2* 2*cos(2*xy_Lon)  ! Y_2^2
  call AssertEqual(&
    message='Y_2^-2 Test of xy_w(w_xy(xy_data,iflag=-1))',        &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data,iflag=-1)),                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Y_2^-2 Test of xy_w(w_xy(xy_data),iflag=-1)',        &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data),iflag=-1),                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Y 微分変換 ----
  xy_Ddata = sqrt(5.0D0/24)*3.0*4*sin(xy_Lat)*cos(xy_Lat)**2*sin(2*xy_Lon)
  call AssertEqual(&
    message='Y_2^-2 Test of xy_w(w_xy(xy_data,iflag=1,ipow=0))',  &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data,iflag=1,ipow=0)),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xy_Ddata = sqrt(5.0D0/24)*3.0*2*cos(xy_Lat)*sin(xy_Lat)*sin(2*xy_Lon)
  call AssertEqual(&
    message='Y_2^-2 Test of xy_w(w_xy(xy_data),iflag=1,ipow=1)',  &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data),iflag=1,ipow=1),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- SIN 掛け算変換 ----
  xy_Ddata = -sqrt(5.0D0/24)*3.0*cos(xy_Lat)**2*sin(xy_Lat)*sin(2*xy_Lon)
  call AssertEqual(&
    message='Y_2^-2 Test of xy_w(w_xy(xy_data,iflag=2))',         &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data,iflag=2)),                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Y_2^-2 Test of xy_w(w_xy(xy_data),iflag=2)',         &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data),iflag=2),                          &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','Test of Y_2^-2','Test of Y_2^-2 succeeded!') 

  !==================== 一般的関数のテスト ====================
  xy_data = cos(2*xy_Lon-pi/3) &
       *(sin(xy_Lat)-1)**2*(sin(xy_Lat)-0.5)*(sin(xy_Lat)+1)

  !---- 通常変換 ----
  call AssertEqual(&
    message='general function of xy_w(w_xy(xy_data))',            &
    answer = xy_data,                                             &
    check = xy_w(w_xy(xy_data)),                                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Y 微分変換 ----
  xy_data  = cos(xy_Lat)
  xy_Ddata = -2*sin(xy_Lat)

  call AssertEqual(&
    message='general function test of xy_w(w_xy(xy_data,iflag=1,ipow=1))',  &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data,iflag=1,ipow=1)),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xy_data  = cos(xy_Lat)**2
  xy_Ddata = -2*sin(xy_Lat)
  call AssertEqual(&
    message='general function test of xy_w(w_xy(xy_data,iflag=1,ipow=2))',  &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data,iflag=1,ipow=2)),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xy_data  = sin(xy_Lat)
  xy_Ddata = cos(xy_Lat)**2

  call AssertEqual(&
    message='general function test of xy_w(w_xy(xy_data),iflag=1,ipow=0)',  &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data),iflag=1,ipow=0),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xy_data  = cos(xy_Lat)**2
  xy_Ddata = -2.0D0 * sin(xy_Lat)

  call AssertEqual(&
    message='general function test of xy_w(w_xy(xy_data),iflag=1,ipow=2)',  &
    answer = xy_Ddata,                                            &
    check = xy_w(w_xy(xy_data),iflag=1,ipow=2),                   &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','Test of genral function', &
       'Test of general function succeeded!') 

end program w_base_module_sjpack_test
