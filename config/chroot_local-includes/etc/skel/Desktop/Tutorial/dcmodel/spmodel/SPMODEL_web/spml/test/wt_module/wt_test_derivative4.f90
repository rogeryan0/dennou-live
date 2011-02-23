!----------------------------------------------------------------------
!     Copyright (c) 2002-2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_module テストプログラム
!
!   wt_KxRGrad_wt, xyz_KGrad_wt, wt_QOperator_wt のテスト
!
!履歴  2002/??/??  竹広真一
!      2007/11/09  竹広真一  エラーメッセージ追加
!      2008/06/28  佐々木洋平  配列の宣言を変更
!  
program wt_test_derivative4

  use dc_message, only : MessageNotify
  use wt_module
  implicit none

  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)
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

end program wt_test_derivative4

