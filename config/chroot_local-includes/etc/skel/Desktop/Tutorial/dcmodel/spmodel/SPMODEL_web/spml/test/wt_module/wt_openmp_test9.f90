!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  SPCPACK/ISPACK-F90 テストプログラム
!      磁場トロイダルポテンシャルの境界値問題
!
!履歴  2002/06/10  竹広真一
!      2002/11/19  竹広真一 wt_TorMagBoundariesGrid を追加
!
program wttest_tormag

  use wt_module

  implicit none

  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ri=0.5, ro=1.5      ! 内外半径
  integer,parameter  :: np=8                ! OPENMP 最大スレッド数

  real(8), dimension(im,jm,0:km)           :: xyz_TORMAG
  real(8), dimension((nm+1)*(nm+1),0:lm)   :: wt_TORMAG

  real(8), dimension((nm+1)*(nm+1),0:km)   :: wz_n   ! 全波数

  real(8), parameter  :: pi=3.1415926535897932385D0
  real(8), parameter  :: eps=1D-10

  integer :: i,j 

  write( 6,* ) 'Test for wt_TormagBoundaries'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

  call wt_initial(im,jm,km,nm,lm,ri,ro,np)

  ! P_10
  xyz_TORMAG = sin(xyz_lat) * cos( pi*(xyz_rad-ri)/(ro-ri) )
  ! P_1_1
  !xyz_TORMAG = cos(xyz_lat)*cos(xyz_lon)* cos( pi*(xyz_rad-ri)/(ro-ri) )
  !xyz_TORMAG = 2*sin(xyz_lat)**2 * cos( pi*(xyz_rad-ri)/(ro-ri) )

  wt_TORMAG = wt_xyz(xyz_TORMAG)
!  call wt_TormagBoundaries(wt_TORMAG)
  call wt_TormagBoundariesGrid(wt_TORMAG)
  xyz_TORMAG = xyz_wt(wt_TORMAG)

  do j=1,jm
     do i=1,im
        if ( abs(xyz_TORMAG(i,j,0)) > eps ) then
           write(6,*) 'Top B.C. : ', i,j,xyz_TORMAG(i,j,0)
        endif
        if ( abs(xyz_TORMAG(i,j,km)) > eps ) then
           write(6,*) 'Bottom B.C. : ', i,j,xyz_TORMAG(i,j,km)
        endif
     enddo
  enddo

end program wttest_tormag
