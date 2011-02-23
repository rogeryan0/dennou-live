!----------------------------------------------------------------------
!     Copyright (c) 2002-2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_module テストプログラム
!
!      wt_DivRad_wt のテスト (1/r^2 d/dr r^2)
!  
!履歴  2002/??/??  竹広真一
!      2007/11/09  竹広真一  エラーメッセージ追加
!      2008/06/28  佐々木洋平  配列の宣言を変更
!
program wt_test_derivative1

  use dc_message, only : MessageNotify
  use wt_module
  implicit none

  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ri=0.5, ro=1.5      ! 内外半径

  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_Data
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_Data1
  real(8), dimension((nm+1)**2,0:lm) :: wt_Data
  real(8), parameter :: eps=1.0D-10
  integer, parameter :: nmin=1, nmax=10

  integer :: n

  call MessageNotify('M','wt_test_derivative1', &
       'wt_module derivative function test #1')

  write( 6,* ) 'Test for wt_Divrat_wt'
!!$  write(6,*) 'exponent N, of Radial dependence for r^N?'
!!$
!!$  write(6,*) 'n?'
!!$  read(5,*) n

  call wt_Initial(im,jm,km,nm,lm,ri,ro)

  do n=nmin,nmax
     write(6,*) 'n=',n
     xyz_Data = xyz_Rad**n
     xyz_Data1 = (n+2)*xyz_Rad**(n-1)

     xyz_Data = xyz_wt(wt_DivRad_wt(wt_xyz(xyz_Data)))

     if ( maxval(abs(xyz_Data-xyz_Data1)) .gt. eps ) then
        write( 6,* ) 'Error is...', maxval(abs(xyz_Data-xyz_Data1))
        call MessageNotify('E','wt_test_derivative1', &
             'Derivative error too large.')
     end if

  enddo
  call MessageNotify('M','wt_test_derivative1', &
       'wt_module derivative function test #1 succeeded!')

end program wt_test_derivative1
