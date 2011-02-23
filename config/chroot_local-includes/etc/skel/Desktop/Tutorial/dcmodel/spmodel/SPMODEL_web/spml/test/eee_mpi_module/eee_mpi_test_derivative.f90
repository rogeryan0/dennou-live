!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  eee_mpi_module テストプログラム (微分計算)
!
!履歴  2008/05/21  竹広真一
!
program eee_mpi_test_derivative

  use dc_message, only : MessageNotify
  use eee_mpi_module
  implicit none
  include 'mpif.h'

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=64, km=16          ! 格子点の設定(X,Y,Z)
  integer, parameter :: lm=10, mm=21, nm=5          ! 切断波数の設定(X,Y,Z)

 !---- 変数 ----
  real(8), allocatable :: zxv_Data(:,:,:)            ! 格子データ
  real(8), allocatable :: zxv_Deriv(:,:,:)            ! 格子データ

  integer            :: l=5,m=3,n=2
  integer            :: np, ip, ierr

 !---- 座標変数など ----
  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-8            ! 判定誤差

  call MessageNotify('M','eee_mpi_test_derivative', &
       'eee_mpi_module derivative function tests')

 !---------------- MPI スタート ---------------------
  call MPI_INIT(IERR)
  call MPI_COMM_RANK(MPI_COMM_WORLD,IP,IERR)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

 !---------------- 座標値の設定 ---------------------
  call eee_mpi_initial(im,jm,km,lm,mm,nm)

 !---------------- 変数の割付け ---------------------
  allocate(zxv_Data(0:km-1,0:im-1,js(ip):je(ip)))
  allocate(zxv_Deriv(0:km-1,0:im-1,js(ip):je(ip)))

 !------------------- 初期値設定 ----------------------
  write(6,*) '*** Test of eee_mpi_module : derivative function check.'
  write(6,*) '  The result will be printed '
  write(6,*) '  only when the error is larger than ', eps
  write(6,*)

!!$  write(6,*) '  Input wavenumbers of the grid data, l,m and n :'
!!$  read(5,*) l,m,n
  write(6,*) '  l,m,n = ', l,m,n

  zxv_Data = sin(l*zxv_X) * sin(m*zxv_Y) * sin(n*zxv_Z)
  zxv_Deriv = l*cos(l*zxv_X) * sin(m*zxv_Y)* sin(n*zxv_Z)
  call check3d(zxv_eef(eef_Dx_eef(eef_zxv(zxv_Data)))-zxv_Deriv, &
       eps, 'Dx(sin(l*X)*sin(m*Y)*sin(n*Z))')
  zxv_Deriv = m*sin(l*zxv_X) * cos(m*zxv_Y) * sin(n*zxv_Z)
  call check3d(zxv_eef(eef_Dy_eef(eef_zxv(zxv_Data)))-zxv_Deriv, &
       eps, 'Dy(sin(l*X)*sin(m*Y)*sin(n*Z))')
  zxv_Deriv = n*sin(l*zxv_X) * sin(m*zxv_Y) * cos(n*zxv_Z)
  call check3d(zxv_eef(eef_Dz_eef(eef_zxv(zxv_Data)))-zxv_Deriv, &
       eps, 'Dz(sin(l*X)*sin(m*Y)*sin(n*Z))')

  zxv_Deriv = -(l**2 + m**2 + n**2) &
                 * sin(l*zxv_X) * sin(m*zxv_Y) * sin(n*zxv_Z)
  call check3d(zxv_eef(eef_Lapla_eef(eef_zxv(zxv_Data)))-zxv_Deriv, &
       eps, 'Lapla(sin(l*X)*sin(m*Y)*sin(n*Z))')
  zxv_Deriv = -1.0/(l**2 + m**2+ n**2) &
                 * sin(l*zxv_X) * sin(m*zxv_Y) * sin(n*zxv_Z)
  call check3d(zxv_eef(eef_LaplaInv_eef(eef_zxv(zxv_Data)))-zxv_Deriv, &
       eps, 'LaplaInv(sin(l*X)*sin(m*Y)*sin(n*Z))')

  zxv_Data = cos(l*zxv_X) * cos(m*zxv_Y) * cos(n*zxv_Z)
  zxv_Deriv = -l*sin(l*zxv_X) * cos(m*zxv_Y)* cos(n*zxv_Z)
  call check3d(zxv_eef(eef_Dx_eef(eef_zxv(zxv_Data)))-zxv_Deriv, &
       eps, 'Dx(cos(l*X)*cos(m*Y)*cos(n*Z))')
  zxv_Deriv = -m*cos(l*zxv_X) * sin(m*zxv_Y) * cos(n*zxv_Z)
  call check3d(zxv_eef(eef_Dy_eef(eef_zxv(zxv_Data)))-zxv_Deriv, &
       eps, 'Dy(cos(l*X)*cos(m*Y)*cos(n*Z))')
  zxv_Deriv = -n*cos(l*zxv_X) * cos(m*zxv_Y) * sin(n*zxv_Z)
  call check3d(zxv_eef(eef_Dz_eef(eef_zxv(zxv_Data)))-zxv_Deriv, &
       eps, 'Dz(cos(l*X)*cos(m*Y)*cos(n*Z))')

  zxv_Deriv = -(l**2 + m**2 + n**2) &
                 * cos(l*zxv_X) * cos(m*zxv_Y) * cos(n*zxv_Z)
  call check3d(zxv_eef(eef_Lapla_eef(eef_zxv(zxv_Data)))-zxv_Deriv, &
       eps, 'Lapla(cos(l*X)*cos(m*Y)*cos(n*Z))')
  zxv_Deriv = -1.0/(l**2 + m**2+ n**2) &
                 * cos(l*zxv_X) * cos(m*zxv_Y) * cos(n*zxv_Z)
  call check3d(zxv_eef(eef_LaplaInv_eef(eef_zxv(zxv_Data)))-zxv_Deriv, &
       eps, 'LaplaInv(cos(l*X)*cos(m*Y)*cos(n*Z))')

  call MessageNotify('M','eee_mpi_test_derivative', &
       'eee_mpi_module derivative function tests succeeded!')

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

end program eee_mpi_test_derivative
