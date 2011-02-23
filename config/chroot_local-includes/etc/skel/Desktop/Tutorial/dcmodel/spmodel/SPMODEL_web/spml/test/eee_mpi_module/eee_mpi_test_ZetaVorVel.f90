!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  eee_mpi_module テストプログラム
!
!履歴  2008/05/21  竹広真一
!
program eee_mpi_test_ZetaVorVel

  use dc_message, only : MessageNotify
  use eee_mpi_module
  implicit none
  include 'mpif.h'

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=64, km=16          ! 格子点の設定(X,Y,Z)
  integer, parameter :: lm=10, mm=21, nm=5           ! 切断波数の設定(X,Y,Z)

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

  real(8), allocatable :: eef_VelX(:,:,:)    ! スペクトルデータ
  real(8), allocatable :: eef_VelY(:,:,:)    ! スペクトルデータ
  real(8), allocatable :: eef_VelZ(:,:,:)    ! スペクトルデータ

  real(8), allocatable :: eef_VorX(:,:,:)    ! スペクトルデータ
  real(8), allocatable :: eef_VorY(:,:,:)    ! スペクトルデータ
  real(8), allocatable :: eef_VorZ(:,:,:)    ! スペクトルデータ

  real(8), allocatable :: ee2f_Zeta(:,:,:,:) ! スペクトルデータx2

 !---- 座標変数など ----
  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! 判定誤差

  integer            :: np, ip, ierr
  integer :: l=2, m=1, n

  call MessageNotify('M','eee_mpi_test_ZetaVelVor', &
       'eee_mpi_module Zeta-Vor,Vel transform functions tests')

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

  allocate(eef_VelX(-nm:nm,-mm:mm,2*lc(ip)))
  allocate(eef_VelY(-nm:nm,-mm:mm,2*lc(ip)))
  allocate(eef_VelZ(-nm:nm,-mm:mm,2*lc(ip)))

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

  eef_VelX = eef_zxv(zxv_VelX)
  eef_VelY = eef_zxv(zxv_VelY)
  eef_VelZ = eef_zxv(zxv_VelZ)

  zxv_VorX = (m-n)*sin(l*zxv_X)*cos(m*zxv_Y)*cos(n*zxv_Z)
  zxv_VorY = (n-l)*cos(l*zxv_X)*sin(m*zxv_Y)*cos(n*zxv_Z)
  zxv_VorZ = (l-m)*cos(l*zxv_X)*cos(m*zxv_Y)*sin(n*zxv_Z)

  eef_VorX = eef_zxv(zxv_VorX)
  eef_VorY = eef_zxv(zxv_VorY)
  eef_VorZ = eef_zxv(zxv_VorZ)

 !---------------- 渦度 ζ への変換 ---------------------
  ee2f_Zeta = ee2f_ZetaFromVor_eef_eef_eef(eef_VorX,eef_VorY,eef_VorZ)

 !---------------- 渦度 ζ からの逆変換(渦度) ---------------------
  call check3d(eef_VorFromZeta_ee2f(ee2f_Zeta,1)-eef_VorX, eps, &
       'Transform and Inverse Transform of VorX')
  call check3d(eef_VorFromZeta_ee2f(ee2f_Zeta,2)-eef_VorY, eps, &
       'Transform and Inverse Transform of VorY')
  call check3d(eef_VorFromZeta_ee2f(ee2f_Zeta,3)-eef_VorZ, eps, &
       'Transform and Inverse Transform of VorZ')

 !---------------- 渦度 ζ からの逆変換(速度) ---------------------
  call check3d(eef_VelFromZeta_ee2f(ee2f_Zeta,1)-eef_VelX, eps, &
       'Transform and Inverse Transform of VelX')
  call check3d(eef_VelFromZeta_ee2f(ee2f_Zeta,2)-eef_VelY, eps, &
       'Transform and Inverse Transform of VelY')
  call check3d(eef_VelFromZeta_ee2f(ee2f_Zeta,3)-eef_VelZ, eps, &
       'Transform and Inverse Transform of VelZ')


  call MessageNotify('M','eee_mpi_test_ZetaVorVel', &
       'eee_mpi_module Zeta-Vor,Vel transform functions tests')

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
       write(cip,'(I3)') IP
       write(6,*) '  Checking ', funcname, ' for IP='//trim(adjustl(cip))
    endif

    do k=1,size(var,3)
       do j=1,size(var,2)
          do i=1,size(var,1)
             if (abs(var(i,j,k)) .gt. eps ) then
                write(6,*) &
                  '    Value larger than EPS : i= ', i, '  j= ', j, '  k= ', k, &
                  var(i,j,k)
                call MessageNotify('E','eee_mpi_test_transform', &
                  'transform error too large')
             endif
          enddo
       enddo
    enddo
  end subroutine check3d

end program eee_mpi_test_ZetaVorVel
