!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wu_module テストプログラム
!
!      関数のテスト
!      xyz_GradLon_wu, xyz_GradLat_wu, wu_Div_xyz_xyz_xyz
!  
!履歴  2008/01/01  竹広真一
!      2008/06/28  佐々木洋平  配列の宣言を変更
!
program wu_test_derivative2

  use dc_message, only : MessageNotify
  use wu_module
  implicit none

  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ra=0.5               ! 球半径

  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_VLon
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_VLat
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_VRad
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_GradLon
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_GradLat
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_Div
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_Data
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_Psi

  real(8), parameter :: eps = 1D-10
  integer, parameter :: n=2

  integer :: i,j,k

  call MessageNotify('M','wu_test_derivative2', &
       'wu_module derivative function test #2')

  call wu_Initial(im,jm,km,nm,lm,ra)

  write( 6,* ) 'Test for xyr_GradLon_wu, xyr_GradLat_wu, wu_Div_xyr_xyr_xyr.'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

! ----------------- 例 1 --------------------
  xyr_VRad = 0.0D0
  xyr_Psi = xyr_Rad**n * cos(xyr_Lat)*sin(xyr_Lon)   ! r**2 P_1^1

  xyr_GradLon =  xyr_Rad**(n-1)*cos(xyr_Lon)
  xyr_GradLat = -xyr_Rad**(n-1)*sin(xyr_Lat)*sin(xyr_Lon)

  xyr_Div = - 2* xyr_Psi/xyr_Rad**2

  write(6,*)
  write(6,*)'P11 field'
  call checkresult

! ----------------- 例 2 --------------------
  xyr_VRad = 0.0D0
  xyr_Psi = xyr_Rad**n * cos(xyr_Lat)*sin(xyr_Lat) * sin(xyr_Lon) ! P_2^1

  xyr_GradLon =  xyr_Rad**(n-1)*sin(xyr_Lat)*cos(xyr_Lon)
  xyr_GradLat =  xyr_Rad**(n-1)*cos(2*xyr_Lat)*sin(xyr_Lon)

  xyr_Div = - 6* xyr_Psi/xyr_Rad**2

  write(6,*)
  write(6,*)'P21 field'
  call checkresult

  call MessageNotify('M','wu_test_derivative2', &
       'wu_module derivative function test #2 succeeded!')

  stop
contains

  subroutine checkresult

    xyr_VLon =  xyr_GradLon_wu(wu_xyr(xyr_Psi))
    write(6,*)'Checking GradLon (1/r cos(Lat) d/dLon)'
    do k=0,km
       do j=1,jm
          do i=0,im-1
             if ( abs(xyr_VLon(i,j,k)-xyr_GradLon(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyr_VLon(i,j,k), xyr_GradLon(i,j,k)
                call MessageNotify('E','wu_test_derivative2', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    xyr_VLat =  xyr_GradLat_wu(wu_xyr(xyr_Psi))
    write(6,*)'Checking GradLat (1/r dLat)'
    do k=0,km
       do j=1,jm
          do i=0,im-1
             if ( abs(xyr_VLat(i,j,k)-xyr_GradLat(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyr_VLat(i,j,k), xyr_GradLat(i,j,k)
                call MessageNotify('E','wu_test_derivative2', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    xyr_Data = xyr_wr(wr_Div_xyr_xyr_xyr(xyr_VLon,xyr_VLat,xyr_VRad))

    write(6,*)'Checking Divergence'
    do k=0,km
       do j=1,jm
          do i=0,im-1
             if ( abs(xyr_Data(i,j,k)-xyr_Div(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyr_Data(i,j,k), xyr_Div(i,j,k)
                call MessageNotify('E','wu_test_derivative2', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

  end subroutine checkresult

end program wu_test_derivative2

