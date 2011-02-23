!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  eee_module テストプログラム
!
!履歴  2008/05/05  竹広真一
!
program eee_test_EnergyHelicity

  use dc_message, only : MessageNotify
  use eee_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=64, km=32          ! 格子点の設定(X,Y,Z)
  integer, parameter :: lm=10, mm=21, nm=10          ! 切断波数の設定(X,Y,Z)

 !---- 変数 ----
  real(8)            :: zyx_VelX(0:km-1,0:jm-1,0:im-1)    ! 格子データ(速度 X)
  real(8)            :: zyx_VelY(0:km-1,0:jm-1,0:im-1)    ! 格子データ(速度 Y)
  real(8)            :: zyx_VelZ(0:km-1,0:jm-1,0:im-1)    ! 格子データ(速度 Z)

  real(8)            :: zyx_VorX(0:km-1,0:jm-1,0:im-1)    ! 格子データ(渦度 X)
  real(8)            :: zyx_VorY(0:km-1,0:jm-1,0:im-1)    ! 格子データ(渦度 Y)
  real(8)            :: zyx_VorZ(0:km-1,0:jm-1,0:im-1)    ! 格子データ(渦度 Z)

  real(8)            :: eee_VorX(-nm:nm,-mm:mm,-lm:lm)    ! スペクトルデータ
  real(8)            :: eee_VorY(-nm:nm,-mm:mm,-lm:lm)    ! スペクトルデータ
  real(8)            :: eee_VorZ(-nm:nm,-mm:mm,-lm:lm)    ! スペクトルデータ

  real(8)            :: eee2_Zeta(-nm:nm,-mm:mm,-lm:lm,2) ! スペクトルデータx2

  real(8)            :: Energy, Helicity
  real(8)            :: EnergyHelicity(2)

 !---- 座標変数など ----
  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! 判定誤差

  integer :: l, m, n

  call MessageNotify('M','eee_test_EnergyHelicity', &
       'eee_module Energy and Helicity calculation tests')

 !---------------- 座標値の設定 ---------------------
  call eee_initial(im,jm,km,lm,mm,nm)

 !---------------- 速度・渦度の設定 ---------------------
!!$  write(6,*) '  Input wavenumbers of the grid data, l and m :'
!!$  read(5,*) l,m
  l = 2; m=1
  n = -(l+m)
  write(6,*) '  l,m,n = ', l,m,n

  zyx_VelX = cos(l*zyx_X)*sin(m*zyx_Y)*sin(n*zyx_Z)
  zyx_VelY = sin(l*zyx_X)*cos(m*zyx_Y)*sin(n*zyx_Z)
  zyx_VelZ = sin(l*zyx_X)*sin(m*zyx_Y)*cos(n*zyx_Z)

  zyx_VorX = (m-n)*sin(l*zyx_X)*cos(m*zyx_Y)*cos(n*zyx_Z)
  zyx_VorY = (n-l)*cos(l*zyx_X)*sin(m*zyx_Y)*cos(n*zyx_Z)
  zyx_VorZ = (l-m)*cos(l*zyx_X)*cos(m*zyx_Y)*sin(n*zyx_Z)

  eee_VorX = eee_zyx(zyx_VorX)
  eee_VorY = eee_zyx(zyx_VorY)
  eee_VorZ = eee_zyx(zyx_VorZ)

 !------------- 渦度 ζ への変換と Energy Helicity 計算 -----------------
  eee2_Zeta = eee2_ZetaFromVor_eee_eee_eee(eee_VorX,eee_VorY,eee_VorZ)
  EnergyHelicity = EnergyHelicityFromZeta_eee2(eee2_Zeta)

 !---------------- 格子点で Energy, Helicity の計算 ---------------------
  
  Energy = 0.5 * AvrZYX_zyx(zyx_VelX**2+zyx_VelY**2+zyx_VelZ**2)

  Helicity = AvrZYX_zyx(zyx_VelX**zyx_VorX +zyx_VelY**zyx_VorY + zyx_VelZ**zyx_VorZ)
  

 !---------------- 計算チェック ---------------------
  call check0d(EnergyHelicity(1)-Energy, eps, 'Calculation of Energy')
  call check0d(EnergyHelicity(2)-Helicity, eps, 'Calculation of Helicity')

  call MessageNotify('M','eee_test_EnergyHelicity', &
       'eee_module Energy Helicity calulcation tests suceeded')

 stop
contains

  subroutine check0d(var,eps,funcname) ! 絶対値が eps 以上の var の要素を出力
    real(8) :: var                     ! 判定する配列
    real(8) :: eps                     ! 誤差
    character(len=*), optional :: funcname

    if ( present(funcname) )then
       write(6,*) '  Checking ', funcname, '...'
    endif

    if (abs(var) .gt. eps ) then
       write(6,*) '    Value larger than EPS : ', var
       call MessageNotify('E','eee_test_EnergyHelicity', &
                          'calculation error too large')
    end if

  end subroutine check0d


end program eee_test_EnergyHelicity
