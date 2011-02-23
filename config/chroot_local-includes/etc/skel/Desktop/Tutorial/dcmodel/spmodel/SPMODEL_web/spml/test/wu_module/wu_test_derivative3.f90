!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wu_module テストプログラム
!
!      関数のテスト
!           wr_DivLon_xyr, wr_DivLat_xyr
!
!履歴  2008/01/01  竹広真一
!      2008/06/28  佐々木洋平  配列の宣言を変更
!  
program wu_test_derivative3

  use dc_message, only : MessageNotify
  use wu_module
  implicit none

  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ra=1.5               ! 球半径

  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_VLon
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_VLat
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_DivLon
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_DivLat
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_Data

  real(8), parameter :: eps = 1D-10
  integer, parameter :: n=2

  integer :: i,j,k

  call MessageNotify('M','wu_test_derivative3', &
       'wu_module derivative function test #3')

  call wu_Initial(im,jm,km,nm,lm,ra)

  write( 6,* ) 'Test for wu_DivLon_xyr, wu_DivLat_xyr'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

! ----------------- 例 1 --------------------
  xyr_VLon   = xyr_rad**n * cos(xyr_Lat)**2*sin(xyr_Lon)
  xyr_DivLon = xyr_rad**(n-1)*cos(xyr_Lat)*cos(xyr_Lon)

  xyr_VLat   = xyr_rad**n * cos(xyr_Lat)**2*sin(xyr_Lon)
  xyr_DivLat = -3*xyr_rad**(n-1)*cos(xyr_Lat)*sin(xyr_Lat)*sin(xyr_Lon)

  write(6,*)
  write(6,*)'Example 1'
  write(6,*)'  xyr_VLon=xyr_rad**n * cos(xyr_Lat)**2*sin(xyr_Lon)'
  write(6,*)'  xyr_VLat=xyr_rad**n * cos(xyr_Lat)**2*sin(xyr_Lon)'
  call checkresult

  call MessageNotify('M','wu_test_derivative3', &
       'wu_module derivative function test #3 succeeded!')

  stop
contains

  subroutine checkresult

    write(6,*)'Checking DivLon (1/r cos(Lat) dLon)'
    xyr_Data =  xyr_wr(wr_DivLon_xyr(xyr_VLon))

    do k=0,km
       do j=1,jm
          do i=0,im-1
             if ( abs(xyr_Data(i,j,k)-xyr_DivLon(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyr_Data(i,j,k), xyr_DivLon(i,j,k)
                call MessageNotify('E','wu_test_derivative3', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    xyr_Data =  xyr_wr(wr_DivLat_xyr(xyr_VLat))
    write(6,*)'Checking DivLat (1/r cos(Lat) dLat cos(Lat))'
    do k=0,km
       do j=1,jm
          do i=0,im-1
             if ( abs(xyr_Data(i,j,k)-xyr_DivLat(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyr_Data(i,j,k), xyr_DivLat(i,j,k)
                call MessageNotify('E','wu_test_derivative3', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do
  end subroutine checkresult


end program wu_test_derivative3

