!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  eee_mpi_module テストプログラム
!
!履歴  2008/05/21  竹広真一
!
program eee_mpi_test_RotVelxVor

  use dc_message, only : MessageNotify
  use eee_mpi_module
  implicit none
  include 'mpif.h'

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=32, km=32          ! 格子点の設定(X,Y,Z)
  integer, parameter :: lm=10, mm=10, nm=10          ! 切断波数の設定(X,Y,Z)

 !---- 変数 ----
  real(8), allocatable :: zxv_VelX(:,:,:)    ! 格子データ(速度 X)
  real(8), allocatable :: zxv_VelY(:,:,:)    ! 格子データ(速度 Y)
  real(8), allocatable :: zxv_VelZ(:,:,:)    ! 格子データ(速度 Z)

  real(8), allocatable :: zxv_VorX(:,:,:)    ! 格子データ(渦度 X)
  real(8), allocatable :: zxv_VorY(:,:,:)    ! 格子データ(渦度 Y)
  real(8), allocatable :: zxv_VorZ(:,:,:)    ! 格子データ(渦度 Z)

  real(8), allocatable :: zxv_VelxVorX(:,:,:)! 格子データ(速度x渦度 X)
  real(8), allocatable :: zxv_VelxVorY(:,:,:)! 格子データ(速度x渦度 Y)
  real(8), allocatable :: zxv_VelxVorZ(:,:,:)! 格子データ(速度x渦度 Z)

  real(8), allocatable :: eef_RotVelxVorX(:,:,:)    ! スペクトルデータ
  real(8), allocatable :: eef_RotVelxVorY(:,:,:)    ! スペクトルデータ
  real(8), allocatable :: eef_RotVelxVorZ(:,:,:)    ! スペクトルデータ

  real(8), allocatable :: eef_VorX(:,:,:)    ! スペクトルデータ
  real(8), allocatable :: eef_VorY(:,:,:)    ! スペクトルデータ
  real(8), allocatable :: eef_VorZ(:,:,:)    ! スペクトルデータ

  real(8), allocatable :: ee2f_Zeta(:,:,:,:) ! スペクトルデータx2

 !---- 座標変数など ----
  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! 判定誤差

  integer            :: np, ip, ierr
  integer            :: l=2, m=1, n, i

  call MessageNotify('M','eee_mpi_test_RotVelxVor', &
       'eee_mpi_module RotVelxVor functions tests')

 !---------------- MPI スタート ---------------------
  call MPI_INIT(IERR)
  call MPI_COMM_RANK(MPI_COMM_WORLD,IP,IERR)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

 !---------------- 座標値の設定 ---------------------
  call eee_mpi_initial(im,jm,km,lm,mm,nm)

 !---------------- 変数の割付け ---------------------
  allocate(zxv_VelX(0:km-1,0:im-1,js(ip):je(ip)))
  allocate(zxv_VelY(0:km-1,0:im-1,js(ip):je(ip)))
  allocate(zxv_VelZ(0:km-1,0:im-1,js(ip):je(ip)))

  allocate(zxv_VorX(0:km-1,0:im-1,js(ip):je(ip)))
  allocate(zxv_VorY(0:km-1,0:im-1,js(ip):je(ip)))
  allocate(zxv_VorZ(0:km-1,0:im-1,js(ip):je(ip)))

  allocate(zxv_VelxVorX(0:km-1,0:im-1,js(ip):je(ip)))
  allocate(zxv_VelxVorY(0:km-1,0:im-1,js(ip):je(ip)))
  allocate(zxv_VelxVorZ(0:km-1,0:im-1,js(ip):je(ip)))

  allocate(eef_RotVelxVorX(-nm:nm,-mm:mm,2*lc(ip)))
  allocate(eef_RotVelxVorY(-nm:nm,-mm:mm,2*lc(ip)))
  allocate(eef_RotVelxVorZ(-nm:nm,-mm:mm,2*lc(ip)))

  allocate(eef_VorX(-nm:nm,-mm:mm,2*lc(ip)))
  allocate(eef_VorY(-nm:nm,-mm:mm,2*lc(ip)))
  allocate(eef_VorZ(-nm:nm,-mm:mm,2*lc(ip)))

  allocate(ee2f_Zeta(-nm:nm,-mm:mm,2,2*lc(ip)))

 !---------------- 速度・渦度の設定 ---------------------
!!$  write(6,*) '  Input wavenumbers of the grid data, l and m :'
!!$  read(5,*) l,m
  n = -(l+m)
  write(6,*) '  l,m,n = ', l,m,n

  zxv_VelX = cos(l*zxv_X)*sin(m*zxv_Y)*sin(n*zxv_Z)
  zxv_VelY = sin(l*zxv_X)*cos(m*zxv_Y)*sin(n*zxv_Z)
  zxv_VelZ = sin(l*zxv_X)*sin(m*zxv_Y)*cos(n*zxv_Z)

  zxv_VorX = (m-n)*sin(l*zxv_X)*cos(m*zxv_Y)*cos(n*zxv_Z)
  zxv_VorY = (n-l)*cos(l*zxv_X)*sin(m*zxv_Y)*cos(n*zxv_Z)
  zxv_VorZ = (l-m)*cos(l*zxv_X)*cos(m*zxv_Y)*sin(n*zxv_Z)

  eef_VorX = eef_zxv(zxv_VorX)
  eef_VorY = eef_zxv(zxv_VorY)
  eef_VorZ = eef_zxv(zxv_VorZ)

 !---------------- 渦度 ζ への変換 ---------------------
  ee2f_Zeta = ee2f_ZetaFromVor_eef_eef_eef(eef_VorX,eef_VorY,eef_VorZ)

 !---------------- 渦度 ζ から▽x(速度x渦度)の計算 ---------------------
  ee2f_Zeta = ee2f_RotVelxVor_ee2f(ee2f_Zeta)

 !---------------- 格子点で▽x(速度x渦度)の計算 ---------------------
  zxv_VelxVorX = zxv_VelY*zxv_VorZ - zxv_VelZ*zxv_VorY
  zxv_VelxVorY = zxv_VelZ*zxv_VorX - zxv_VelX*zxv_VorZ
  zxv_VelxVorZ = zxv_VelX*zxv_VorY - zxv_VelY*zxv_VorX

  eef_RotVelxVorX = eef_Dy_eef(eef_zxv(zxv_VelxVorZ)) &
                  - eef_Dz_eef(eef_zxv(zxv_VelxVorY))
  eef_RotVelxVorY = eef_Dz_eef(eef_zxv(zxv_VelxVorX)) &
                  - eef_Dx_eef(eef_zxv(zxv_VelxVorZ))
  eef_RotVelxVorZ = eef_Dx_eef(eef_zxv(zxv_VelxVorY)) &
                  - eef_Dy_eef(eef_zxv(zxv_VelxVorX))


 !---------------- 計算チェック ---------------------
  call check3d(eef_VorFromZeta_ee2f(ee2f_Zeta,1)-eef_RotVelxVorX, eps, &
       'Calculation of RotVelxVorX')
  call check3d(eef_VorFromZeta_ee2f(ee2f_Zeta,2)-eef_RotVelxVorY, eps, &
       'Calculation of RotVelxVorY')
  call check3d(eef_VorFromZeta_ee2f(ee2f_Zeta,3)-eef_RotVelxVorZ, eps, &
       'Calculation of RotVelxVorZ')

  call MessageNotify('M','eee_mpi_test_RotVelxVor', &
       'eee_module RotVelxVor functions tests suceeded')

  call MPI_FINALIZE(IERR)

 stop
contains

  subroutine check3d(var,eps,funcname) ! 絶対値が eps 以上の var の要素を出力
    real(8) :: var(:,:,:)              ! 判定する配列
    real(8) :: eps                     ! 誤差
    character(len=*), optional :: funcname
    character(len=3) ::cip
    integer i, j, k

    if ( present(funcname) )then
       write(6,*) 'IP=',IP
       write(cip,'(I3)') IP
       write(6,*) '  Checking ', funcname, ' for IP='//trim(adjustl(cip))
    endif

    do k=1,size(var,3)
       do j=1,size(var,2)
          do i=1,size(var,1)
             if (abs(var(i,j,k)) .gt. eps ) then
                write(6,*) &
                  '    Value larger than EPS : i= ', i, '  j= ', j, '  k= ', k, &
                  var(i,j,k),ls,le
                call MessageNotify('E','eee_test_transform', &
                  'transform error too large')
             endif
          enddo
       enddo
    enddo
  end subroutine check3d

end program eee_mpi_test_RotVelxVor
