!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  eee_module テストプログラム
!
!履歴  2008/05/04  竹広真一
!
program eee_test_RotVelxVor

  use dc_message, only : MessageNotify
  use eee_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=64, km=32          ! 格子点の設定(X,Y,Z)
  integer, parameter :: lm=10, mm=21, nm=10          ! 切断波数の設定(X,Y,Z)

  integer, parameter :: im2=64, jm2=64, km2=64       ! 格子点の設定(X,Y,Z)
  integer, parameter :: lm2=21, mm2=21, nm2=21       ! 切断波数の設定(X,Y,Z)

 !---- 変数 ----
  real(8)  :: zyx_VelX(0:km-1,0:jm-1,0:im-1)    ! 格子データ(速度 X)
  real(8)  :: zyx_VelY(0:km-1,0:jm-1,0:im-1)    ! 格子データ(速度 Y)
  real(8)  :: zyx_VelZ(0:km-1,0:jm-1,0:im-1)    ! 格子データ(速度 Z)

  real(8)  :: zyx_VorX(0:km-1,0:jm-1,0:im-1)    ! 格子データ(渦度 X)
  real(8)  :: zyx_VorY(0:km-1,0:jm-1,0:im-1)    ! 格子データ(渦度 Y)
  real(8)  :: zyx_VorZ(0:km-1,0:jm-1,0:im-1)    ! 格子データ(渦度 Z)

  real(8)  :: zyx_VelxVorX(0:km-1,0:jm-1,0:im-1)! 格子データ(速度x渦度 X)
  real(8)  :: zyx_VelxVorY(0:km-1,0:jm-1,0:im-1)! 格子データ(速度x渦度 Y)
  real(8)  :: zyx_VelxVorZ(0:km-1,0:jm-1,0:im-1)! 格子データ(速度x渦度 Z)

  real(8)  :: eee_RotVelxVorX(-nm:nm,-mm:mm,-lm:lm)    ! スペクトルデータ
  real(8)  :: eee_RotVelxVorY(-nm:nm,-mm:mm,-lm:lm)    ! スペクトルデータ
  real(8)  :: eee_RotVelxVorZ(-nm:nm,-mm:mm,-lm:lm)    ! スペクトルデータ

  real(8)  :: eee_VorX(-nm:nm,-mm:mm,-lm:lm)    ! スペクトルデータ
  real(8)  :: eee_VorY(-nm:nm,-mm:mm,-lm:lm)    ! スペクトルデータ
  real(8)  :: eee_VorZ(-nm:nm,-mm:mm,-lm:lm)    ! スペクトルデータ

  real(8)  :: eee2_Zeta(-nm:nm,-mm:mm,-lm:lm,2) ! スペクトルデータx2


  real(8)  :: zyx_VelX2(0:km2-1,0:jm2-1,0:im2-1)    ! 格子データ(速度 X)
  real(8)  :: zyx_VelY2(0:km2-1,0:jm2-1,0:im2-1)    ! 格子データ(速度 Y)
  real(8)  :: zyx_VelZ2(0:km2-1,0:jm2-1,0:im2-1)    ! 格子データ(速度 Z)

  real(8)  :: zyx_VorX2(0:km2-1,0:jm2-1,0:im2-1)    ! 格子データ(渦度 X)
  real(8)  :: zyx_VorY2(0:km2-1,0:jm2-1,0:im2-1)    ! 格子データ(渦度 Y)
  real(8)  :: zyx_VorZ2(0:km2-1,0:jm2-1,0:im2-1)    ! 格子データ(渦度 Z)

  real(8)  :: zyx_VelxVorX2(0:km2-1,0:jm2-1,0:im2-1)! 格子データ(速度x渦度 X)
  real(8)  :: zyx_VelxVorY2(0:km2-1,0:jm2-1,0:im2-1)! 格子データ(速度x渦度 Y)
  real(8)  :: zyx_VelxVorZ2(0:km2-1,0:jm2-1,0:im2-1)! 格子データ(速度x渦度 Z)

  real(8)  :: eee_RotVelxVorX2(-nm2:nm2,-mm2:mm2,-lm2:lm2)    ! スペクトルデータ
  real(8)  :: eee_RotVelxVorY2(-nm2:nm2,-mm2:mm2,-lm2:lm2)    ! スペクトルデータ
  real(8)  :: eee_RotVelxVorZ2(-nm2:nm2,-mm2:mm2,-lm2:lm2)    ! スペクトルデータ

  real(8)  :: eee_VorX2(-nm2:nm2,-mm2:mm2,-lm2:lm2)    ! スペクトルデータ
  real(8)  :: eee_VorY2(-nm2:nm2,-mm2:mm2,-lm2:lm2)    ! スペクトルデータ
  real(8)  :: eee_VorZ2(-nm2:nm2,-mm2:mm2,-lm2:lm2)    ! スペクトルデータ

  real(8)  :: eee2_Zeta2(-nm2:nm2,-mm2:mm2,-lm2:lm2,2) ! スペクトルデータx2

 !---- 座標変数など ----
  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! 判定誤差

  integer :: l, m, n, i
  integer :: id1, id2

  call MessageNotify('M','eee_test_RotVelxVor', &
       'eee_module RotVelxVor functions tests')

 !---------------- 座標値の設定 ---------------------
  call eee_initial(im,jm,km,lm,mm,nm,id1)
  call eee_initial(im2,jm2,km2,lm2,mm2,nm2,id2)

 !---------------- 速度・渦度の設定 ---------------------
!!$  write(6,*) '  Input wavenumbers of the grid data, l and m :'
!!$  read(5,*) l,m
  l = 1 ; m=-2
  n = -(l+m)
  write(6,*) '  l,m,n = ', l,m,n

  ! id1
  write(*,*) 'for id1'
  call eee_ChangeResolution(id1)

  zyx_VelX = cos(l*zyx_X)*sin(m*zyx_Y)*sin(n*zyx_Z)
  zyx_VelY = sin(l*zyx_X)*cos(m*zyx_Y)*sin(n*zyx_Z)
  zyx_VelZ = sin(l*zyx_X)*sin(m*zyx_Y)*cos(n*zyx_Z)

  zyx_VorX = (m-n)*sin(l*zyx_X)*cos(m*zyx_Y)*cos(n*zyx_Z)
  zyx_VorY = (n-l)*cos(l*zyx_X)*sin(m*zyx_Y)*cos(n*zyx_Z)
  zyx_VorZ = (l-m)*cos(l*zyx_X)*cos(m*zyx_Y)*sin(n*zyx_Z)

  eee_VorX = eee_zyx(zyx_VorX)
  eee_VorY = eee_zyx(zyx_VorY)
  eee_VorZ = eee_zyx(zyx_VorZ)

 !---------------- 渦度 ζ への変換 ---------------------
  eee2_Zeta = eee2_ZetaFromVor_eee_eee_eee(eee_VorX,eee_VorY,eee_VorZ)

 !---------------- 渦度 ζ から▽x(速度x渦度)の計算 ---------------------
  eee2_Zeta = eee2_RotVelxVor_eee2(eee2_Zeta)

 !---------------- 格子点で▽x(速度x渦度)の計算 ---------------------
  zyx_VelxVorX = zyx_VelY*zyx_VorZ - zyx_VelZ*zyx_VorY
  zyx_VelxVorY = zyx_VelZ*zyx_VorX - zyx_VelX*zyx_VorZ
  zyx_VelxVorZ = zyx_VelX*zyx_VorY - zyx_VelY*zyx_VorX

  eee_RotVelxVorX = eee_Dy_eee(eee_zyx(zyx_VelxVorZ)) &
                  - eee_Dz_eee(eee_zyx(zyx_VelxVorY))
  eee_RotVelxVorY = eee_Dz_eee(eee_zyx(zyx_VelxVorX)) &
                  - eee_Dx_eee(eee_zyx(zyx_VelxVorZ))
  eee_RotVelxVorZ = eee_Dx_eee(eee_zyx(zyx_VelxVorY)) &
                  - eee_Dy_eee(eee_zyx(zyx_VelxVorX))


 !---------------- 計算チェック ---------------------
  call check3d(eee_VorFromZeta_eee2(eee2_Zeta,1)-eee_RotVelxVorX, eps, &
       'Calculation of RotVelxVorX')
  call check3d(eee_VorFromZeta_eee2(eee2_Zeta,2)-eee_RotVelxVorY, eps, &
       'Calculation of RotVelxVorY')
  call check3d(eee_VorFromZeta_eee2(eee2_Zeta,3)-eee_RotVelxVorZ, eps, &
       'Calculation of RotVelxVorZ')

 !---------------- 速度・渦度の設定 ---------------------
  ! id2
  write(*,*) 'for id2'
  call eee_ChangeResolution(id2)

  zyx_VelX2 = cos(l*zyx_X)*sin(m*zyx_Y)*sin(n*zyx_Z)
  zyx_VelY2 = sin(l*zyx_X)*cos(m*zyx_Y)*sin(n*zyx_Z)
  zyx_VelZ2 = sin(l*zyx_X)*sin(m*zyx_Y)*cos(n*zyx_Z)

  zyx_VorX2 = (m-n)*sin(l*zyx_X)*cos(m*zyx_Y)*cos(n*zyx_Z)
  zyx_VorY2 = (n-l)*cos(l*zyx_X)*sin(m*zyx_Y)*cos(n*zyx_Z)
  zyx_VorZ2 = (l-m)*cos(l*zyx_X)*cos(m*zyx_Y)*sin(n*zyx_Z)

  eee_VorX2 = eee_zyx(zyx_VorX2)
  eee_VorY2 = eee_zyx(zyx_VorY2)
  eee_VorZ2 = eee_zyx(zyx_VorZ2)

 !---------------- 渦度 ζ への変換 ---------------------
  eee2_Zeta2 = eee2_ZetaFromVor_eee_eee_eee(eee_VorX2,eee_VorY2,eee_VorZ2)

 !---------------- 渦度 ζ から▽x(速度x渦度)の計算 ---------------------
  eee2_Zeta2 = eee2_RotVelxVor_eee2(eee2_Zeta2)

 !---------------- 格子点で▽x(速度x渦度)の計算 ---------------------
  zyx_VelxVorX2 = zyx_VelY2*zyx_VorZ2 - zyx_VelZ2*zyx_VorY2
  zyx_VelxVorY2 = zyx_VelZ2*zyx_VorX2 - zyx_VelX2*zyx_VorZ2
  zyx_VelxVorZ2 = zyx_VelX2*zyx_VorY2 - zyx_VelY2*zyx_VorX2

  eee_RotVelxVorX2 = eee_Dy_eee(eee_zyx(zyx_VelxVorZ2)) &
                  - eee_Dz_eee(eee_zyx(zyx_VelxVorY2))
  eee_RotVelxVorY2 = eee_Dz_eee(eee_zyx(zyx_VelxVorX2)) &
                  - eee_Dx_eee(eee_zyx(zyx_VelxVorZ2))
  eee_RotVelxVorZ2 = eee_Dx_eee(eee_zyx(zyx_VelxVorY2)) &
                  - eee_Dy_eee(eee_zyx(zyx_VelxVorX2))


 !---------------- 計算チェック ---------------------
  call check3d(eee_VorFromZeta_eee2(eee2_Zeta2,1)-eee_RotVelxVorX2, eps, &
       'Calculation of RotVelxVorX')
  call check3d(eee_VorFromZeta_eee2(eee2_Zeta2,2)-eee_RotVelxVorY2, eps, &
       'Calculation of RotVelxVorY')
  call check3d(eee_VorFromZeta_eee2(eee2_Zeta2,3)-eee_RotVelxVorZ2, eps, &
       'Calculation of RotVelxVorZ')

  call MessageNotify('M','eee_test_RotVelxVor', &
       'eee_module RotVelxVor functions tests suceeded')

 stop
contains

  subroutine check3d(var,eps,funcname) ! 絶対値が eps 以上の var の要素を出力
    real(8) :: var(:,:,:)              ! 判定する配列
    real(8) :: eps                     ! 誤差
    character(len=*), optional :: funcname
    integer i, j, k

    if ( present(funcname) )then
       write(6,*) '  Checking ', funcname, '...'
    endif

    do k=1,size(var,3)
       do j=1,size(var,2)
          do i=1,size(var,1)
             if (abs(var(i,j,k)) .gt. eps ) then
                write(6,*) &
                  '    Value larger than EPS : i= ', i, '  j= ', j, '  k= ', k, &
                  var(i,j,k)
                call MessageNotify('E','eee_test_transform', &
                  'transform error too large')
             endif
          enddo
       enddo
    enddo
  end subroutine check3d

end program eee_test_RotVelxVor
