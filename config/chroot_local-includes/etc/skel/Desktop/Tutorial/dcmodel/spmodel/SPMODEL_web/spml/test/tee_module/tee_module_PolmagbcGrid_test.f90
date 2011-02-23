!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  tee_module テストプログラム (境界値問題, ポロイダル磁場, 選点法)
!
!履歴  2009/12/19  竹広真一
!
program tee_module_polmagbcgrid_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use tee_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=64, km=16       ! 格子点の設定(X,Y)
  integer, parameter :: lm=10, mm=21, nm=16       ! 切断波数の設定(X,Y)

 !---- 変数 ----
  real(8)            :: zyx_Polmag(0:km,0:jm-1,0:im-1)      ! 格子データ
  real(8)            :: zyx_Polmag_orig(0:km,0:jm-1,0:im-1) ! 格子データ

  real(8)            :: tee_Polmag(0:nm,-mm:mm,-lm:lm)      ! スペクトルデータ

  real(8)            :: tee_Kh(0:nm,-mm:mm,-lm:lm)          ! 水平全波数

  real(8)            :: zee_Boundary(0:km,-mm:mm,-lm:lm)    ! 境界条件
  real(8)            :: ee_Null(-mm:mm,-lm:lm)=0.0D0        ! 0 

  integer            :: l=2, m=5
  integer            :: l1, m1

 !---- 座標変数など ----
  real(8), parameter :: xmin = 0.0d0, xmax=2.0d0
  real(8), parameter :: ymin = 0.0d0, ymax=2.0d0
  real(8), parameter :: zmin = -1.0d0, zmax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  call MessageNotify('M','tee_module_PolmagbcGrid_test', &
       'tee_module poloidal mag. field B.C. subruoutine tests')

 !---------------- 座標値の設定 ---------------------
  ! スペクトル初期化
  call tee_initial(im,jm,km,lm,mm,nm,xmin,xmax,ymin,ymax,zmin,zmax) 

  do m1=-mm,mm
     do l1=-lm,lm
        tee_Kh(:,m1,l1) = sqrt(dble(l1**2+m1**2))
     enddo
  enddo

 !-------------------BC-DD(Grid) ----------------------
  zyx_Polmag = sin(l*pi*zyx_X) * sin(m*pi*zyx_Y) * zyx_Z
  zyx_Polmag_orig = zyx_Polmag

  tee_Polmag = tee_zyx(zyx_Polmag)

  call tee_PolmagBoundariesGrid(tee_Polmag)
  zyx_Polmag = zyx_tee(tee_Polmag)

  call AssertEqual(&
    message='tee_PolmagBoundariesGrid [internal]',&
    answer = zyx_Polmag_orig(1:km-1,:,:),                         &
    check = zyx_Polmag(1:km-1,:,:),                               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  zee_Boundary = zee_tee(tee_Dz_tee(tee_Polmag) + tee_Kh * tee_Polmag)
  call AssertEqual(&
    message='tee_PolmagBoundariesGrid [Top]',&
    answer = ee_Null,                                             &
    check = zee_Boundary(0,:,:),                                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  zee_Boundary = zee_tee(tee_Dz_tee(tee_Polmag) - tee_Kh * tee_Polmag)
  call AssertEqual(&
    message='tee_PolmagBoundariesGrid [Bottom]',&
    answer = ee_Null,                                             &
    check = zee_Boundary(km,:,:),                                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','tee_module_PolmagbcGrid_test', &
       'tee_module poloidal mag. field B.C. subruoutine tests succeeded!')

end program tee_module_polmagbcgrid_test
