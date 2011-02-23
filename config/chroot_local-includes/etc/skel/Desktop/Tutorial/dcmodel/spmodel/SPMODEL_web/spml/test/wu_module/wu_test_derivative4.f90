!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_module テストプログラム
!
!   wt_KxRGrad_wt, xyr_KGrad_wt, wt_QOperator_wt のテスト
!
!履歴  2008/01/02  竹広真一
!      2008/06/28  佐々木洋平  配列の宣言を変更
!  
program wu_test_derivative4

  use dc_message, only : MessageNotify
  use wu_module
  implicit none

  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ra=3.0               ! 球半径

  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_Data
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_Psi
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_KxRGrad
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_KGrad
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_QOperator


  integer, parameter :: n=3
  real(8), parameter :: eps = 1D-10

  integer :: i,j,k

  call MessageNotify('M','wu_test_derivative4', &
       'wu_module derivative function test #4')

  write( 6,* ) 'Test for wu_KxRGrad_wu, xyr_KGrad_wu, wu_QOperator_wu'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

  call wu_initial(im,jm,km,nm,lm,ra)

! ----------------- 例 1 --------------------
  xyr_Psi = xyr_rad**n * cos(xyr_lat)*sin(xyr_lon)   ! r**2 Y_1^1

  xyr_KxRGrad = xyr_Rad**n * cos(xyr_lat)*cos(xyr_lon)

  ! k ・▽ r**n Y_1^1 = (n-1)*r**(n-1)* Y_2^1
  xyr_KGrad = (n-1)*xyr_rad**(n-1)* cos(xyr_lat)*sin(xyr_lat)*sin(xyr_lon) 

  ! Q r**n Y_1^1 = -3*(n-1)*r**(n-1)* Y_2^1
  xyr_QOperator = - 3*(n-1)*xyr_rad**(n-1)* cos(xyr_lat)*sin(xyr_lat)*sin(xyr_lon) 

  write(6,*)
  write(6,*)'Y_1^1 field'
  call checkresult

! ----------------- 例 2 --------------------
  xyr_Psi = 3*cos(xyr_lat)*sin(xyr_lat) * sin(xyr_lon) * xyr_Rad**2 ! Y_2^1

  xyr_KxRGrad = 3*cos(xyr_lat)*sin(xyr_lat) * cos(xyr_lon) * xyr_Rad**2 ! Y_2^1

  ! k・▽r^2 Y_2^1 = 3r Y_1^1 
  xyr_KGrad = 3*xyr_Rad*cos(2*xyr_Lat)*cos(xyr_Lat)*sin(xyr_Lon) &
             +3*xyr_Rad*sin(2*xyr_Lat)*sin(xyr_Lat)*sin(xyr_Lon)

  xyr_QOperator = -9*cos(xyr_lat)*sin(xyr_lon)*xyr_rad
!!$  xyr_QOperator = (12*sin(xyr_lat)**2 - 69.0/10.0)*cos(xyr_lat)*sin(xyr_lon)*xyr_rad

  write(6,*)
  write(6,*)'Y_2^1 field'
  call checkresult

  call MessageNotify('M','wu_test_derivative4', &
       'wu_module derivative function test #4 succeeded!')

  stop
contains

  subroutine checkresult
    write(6,*)'Checking k x r grad '
    xyr_Data = xyr_wu(wu_KxRGrad_wu(wu_xyr(xyr_Psi)))

    do k=0,km
       do j=1,jm
          do i=0,im-1
             if ( abs(xyr_Data(i,j,k)-xyr_KxRGrad(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyr_Data(i,j,k), xyr_KxRGrad(i,j,k)
                call MessageNotify('E','wu_test_derivative4', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    write(6,*)'Checking k grad '
    xyr_Data = xyr_KGrad_wu(wu_xyr(xyr_Psi))

    do k=0,km
       do j=1,jm
          do i=0,im-1
             if ( abs(xyr_Data(i,j,k)-xyr_KGrad(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyr_Data(i,j,k), xyr_KGrad(i,j,k)
                call MessageNotify('E','wu_test_derivative4', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    write(6,*)'Checking Q operator '
    xyr_Data = xyr_wu(wu_QOperator_wu(wu_xyr(xyr_Psi)))

    do k=0,km
       do j=1,jm
          do i=0,im-1
             if ( abs(xyr_Data(i,j,k)-xyr_QOperator(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyr_Data(i,j,k), xyr_QOperator(i,j,k)
                call MessageNotify('E','wu_test_derivative4', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do
  end subroutine checkresult

end program wu_test_derivative4

