!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_zonal_module テストプログラム
!
!   wt_KxRGrad_wt, xyz_KGrad_wt, wt_QOperator_wt のテスト
!
!履歴  2008/12/30  竹広真一
!  
program wt_zonal_test_derivative4

  use dc_message, only : MessageNotify
  use wt_zonal_module
  implicit none

  integer,parameter  :: im=1, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16        ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ri=0.5, ro=1.5      ! 内外半径

  real(8), dimension(0:im-1,jm,0:km)     :: xyz_Data
  real(8), dimension(0:im-1,jm,0:km)     :: xyz_Psi
  real(8), dimension(0:im-1,jm,0:km)     :: xyz_KxRGrad
  real(8), dimension(0:im-1,jm,0:km)     :: xyz_KGrad
  real(8), dimension(0:im-1,jm,0:km)     :: xyz_QOperator


  integer, parameter :: n=2
  real(8), parameter :: eps = 1D-8

  integer :: i,j,k

  call MessageNotify('M','wt_test_derivative4', &
       'wt_module derivative function test #4')

  write( 6,* ) 'Test for wt_KxRGrad_wt, xyz_KGrad_wt, wt_QOperator_wt'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

  call wt_initial(im,jm,km,nm,lm,ri,ro)

! ----------------- 例 1 --------------------
  xyz_Psi = xyz_rad**n * sin(xyz_Lat)   ! r**2 Y_1^0

  xyz_KxRGrad = 0.0D0

  ! k ・▽ r**n Y_1^1 = (n-1)*r**(n-1)* Y_2^1
  xyz_KGrad = n*xyz_rad**(n-1)*sin(xyz_Lat)**2 &
            + xyz_rad**(n-1)*cos(xyz_Lat)**2

  ! Q r**n Y_1^0 = -(n-1)*r**(n-1)* (3sin^2φ-1)
  xyz_QOperator = -(n-1)*xyz_rad**(n-1) * (3*sin(xyz_Lat)**2 -1)

  write(6,*)
  write(6,*)'Y_1^0 field'
  call checkresult

! ----------------- 例 2 --------------------
  xyz_Psi = xyz_Rad*(3.0D0*sin(xyz_Lat)**2 - 1.0D0)/2.0D0 ! r Y_2^0

  xyz_KxRGrad = 0.0D0

  ! k・▽ Y_2^0
  xyz_KGrad =-3.0D0/2.0D0*sin(xyz_Lat)**3 + 5.0/2.0*sin(xyz_Lat)

  xyz_QOperator = -9.0/2.0*sin(xyz_Lat)*cos(xyz_Lat)**2 &
                 +15.0/2.0*sin(xyz_Lat)**3 - 15.0/2.0*sin(xyz_Lat)

  write(6,*)
  write(6,*)'Y_2^0 field'
  call checkresult

  call MessageNotify('M','wt_test_derivative4', &
       'wt_module derivative function test #4 succeeded!')

  stop
contains

  subroutine checkresult
    write(6,*)'Checking k x r grad '
    xyz_Data = xyz_wt(wt_KxRGrad_wt(wt_xyz(xyz_Psi)))

    do k=0,km
       do j=1,jm
          do i=0,im-1
             if ( abs(xyz_Data(i,j,k)-xyz_KxRGrad(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_Data(i,j,k), xyz_KxRGrad(i,j,k)
                call MessageNotify('E','wt_test_derivative4', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    write(6,*)'Checking k grad '
    xyz_Data = xyz_KGrad_wt(wt_xyz(xyz_Psi))

    do k=0,km
       do j=1,jm
          do i=0,im-1
             if ( abs(xyz_Data(i,j,k)-xyz_KGrad(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_Data(i,j,k), xyz_KGrad(i,j,k)
                call MessageNotify('E','wt_test_derivative4', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    write(6,*)'Checking Q operator '
    xyz_Data = xyz_wt(wt_QOperator_wt(wt_xyz(xyz_Psi)))

    do k=0,km
       do j=1,jm
          do i=0,im-1
             if ( abs(xyz_Data(i,j,k)-xyz_QOperator(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_Data(i,j,k), xyz_QOperator(i,j,k)
                call MessageNotify('E','wt_test_derivative4', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do
  end subroutine checkresult

end program wt_zonal_test_derivative4

