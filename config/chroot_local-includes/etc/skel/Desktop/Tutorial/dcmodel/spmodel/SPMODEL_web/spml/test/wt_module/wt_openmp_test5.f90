!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!   wt_KxRGrad_wt, xyz_KGrad_wt, wt_QOperator_wt のテスト
!  
program wttest5

  use wt_module
  implicit none

  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ri=0.5, ro=1.5      ! 内外半径
  integer,parameter  :: np=8                ! OPENMP 最大スレッド数

  real(8), dimension(im,jm,0:km)     :: xyz_Data
  real(8), dimension(im,jm,0:km)     :: xyz_Psi
  real(8), dimension(im,jm,0:km)     :: xyz_KxRGrad
  real(8), dimension(im,jm,0:km)     :: xyz_KGrad
  real(8), dimension(im,jm,0:km)     :: xyz_QOperator


  integer, parameter :: n=2
  real(8), parameter :: eps = 1D-8

  integer :: i,j,k

  write( 6,* ) 'Test for wt_KxRGrad_wt, xyz_KGrad_wt, wt_QOperator_wt'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

  call wt_initial(im,jm,km,nm,lm,ri,ro,np)

! ----------------- 例 1 --------------------
  xyz_Psi = xyz_rad**n * cos(xyz_lat)*sin(xyz_lon)   ! r**2 Y_1^1

  xyz_KxRGrad = xyz_rad**n * cos(xyz_lat)*cos(xyz_lon)

  ! k ・▽ r**n Y_1^1 = (n-1)*r**(n-1)* Y_2^1
  xyz_KGrad = (n-1)*xyz_rad**(n-1)* cos(xyz_lat)*sin(xyz_lat)*sin(xyz_lon) 

  ! Q r**n Y_1^1 = -3*(n-1)*r**(n-1)* Y_2^1
  xyz_QOperator = - 3*(n-1)*xyz_rad**(n-1)* cos(xyz_lat)*sin(xyz_lat)*sin(xyz_lon) 

  write(6,*)
  write(6,*)'Y_1^1 field'
  call checkresult

! ----------------- 例 2 --------------------
  xyz_Psi = cos(xyz_lat)*sin(xyz_lat) * sin(xyz_lon) ! Y_2^1

  xyz_KxRGrad = cos(xyz_lat)*sin(xyz_lat) * cos(xyz_lon) ! Y_2^1

  ! k・▽ Y_2^1 = (- 4 Y_3^1 / 15 - Y_1^1 /5) 
  xyz_KGrad = cos(2*xyz_lat)*cos(xyz_lat)*sin(xyz_lon)/xyz_rad

  xyz_QOperator = (16*sin(xyz_lat)**2 - 5)*cos(xyz_lat)*sin(xyz_lon)/xyz_rad

  write(6,*)
  write(6,*)'Y_2^1 field'
  call checkresult

  stop
contains

  subroutine checkresult
    write(6,*)'Checking k x r grad '
    xyz_Data = xyz_wt(wt_KxRGrad_wt(wt_xyz(xyz_Psi)))

    do k=0,km
       do j=1,jm
          do i=1,im
             if ( abs(xyz_Data(i,j,k)-xyz_KxRGrad(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_Data(i,j,k), xyz_KxRGrad(i,j,k)
             endif
          end do
       end do
    end do

    write(6,*)'Checking k grad '
    xyz_Data = xyz_KGrad_wt(wt_xyz(xyz_Psi))

    do k=0,km
       do j=1,jm
          do i=1,im
             if ( abs(xyz_Data(i,j,k)-xyz_KGrad(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_Data(i,j,k), xyz_KGrad(i,j,k)
             endif
          end do
       end do
    end do

    write(6,*)'Checking Q operator '
    xyz_Data = xyz_wt(wt_QOperator_wt(wt_xyz(xyz_Psi)))

    do k=0,km
       do j=1,jm
          do i=1,im
             if ( abs(xyz_Data(i,j,k)-xyz_QOperator(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_Data(i,j,k), xyz_QOperator(i,j,k)
             endif
          end do
       end do
    end do
  end subroutine checkresult
end program wttest5

