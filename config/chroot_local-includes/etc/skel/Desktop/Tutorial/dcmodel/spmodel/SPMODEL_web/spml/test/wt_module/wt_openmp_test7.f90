!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  SPCPACK/ISPACK-F90 テストプログラム
!      ポロイダルポテンシャルの境界値問題
!
!履歴  2002/02/13  竹広真一
!
program wttest7

  use wt_module

  implicit none

  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ri=0.5, ro=1.5      ! 内外半径
  integer,parameter  :: np=8                ! OPENMP 最大スレッド数

  real(8), dimension(im,jm,0:km)     :: xyz_Poloidal
  real(8), dimension(im,jm,0:km)     :: xyz_LaplaPol
  real(8), dimension(im,jm,0:km)     :: xyz_LaplaPol1

  real(8), parameter  :: pi=3.1415926535897932385D0
  real(8), parameter  :: eps=1D-10

  integer :: k, l, i, j

  !write( 6,* ) 'Test for wz_LaplaPol2pol_wz'
  write( 6,* ) 'Test for wt_LaplaPol2polGrid_wt'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

  call wt_initial(im,jm,km,nm,lm,ri,ro,np)

  ! P_10
  !xyz_LaplaPol = sin(xyz_Lat) * sin( pi*(xyz_Rad-ri)/(ro-ri) )
  ! P_1_1
  !xyz_LaplaPol = cos(xyz_Lat)*cos(xyz_Lon)* sin( pi*(xyz_Rad-ri)/(ro-ri) )
  xyz_LaplaPol = 2*sin(xyz_Lat)**2 * sin( pi*(xyz_Rad-ri)/(ro-ri) )

  !xyz_Poloidal = xyz_wz(wz_LaplaPol2pol_wz(wz_xyz(xyz_LaplaPol),'FF'))
  xyz_Poloidal = xyz_wt(wt_LaplaPol2PolGrid_wt(wt_xyz(xyz_LaplaPol),'FF'))

  xyz_LaplaPol1 = xyz_wt(wt_Lapla_wt(wt_xyz(xyz_Poloidal)))

  call gropn(1)
  call grfrm
  call grsvpt( 0.2, 0.8, 0.2, 0.8 )
  call grswnd( -90.0, 90.0, real(ri), real(ro) )
  call grstrf
  call uwsgxa(real(y_Lat/pi*180),jm)
  call uwsgya(real(z_Rad),km+1)
  call usdaxs
  call udcntr(real(xyz_LaplaPol(1,:,:)),jm,jm,km+1)

  call grfrm
  call grsvpt( 0.2, 0.8, 0.2, 0.8 )
  call grswnd( -90.0, 90.0, real(ri), real(ro) )
  call grstrf
  call uwsgxa(real(y_Lat/pi*180),jm)
  call uwsgya(real(z_Rad),km+1)
  call usdaxs
  call udcntr(real(xyz_Poloidal(1,:,:)),jm,jm,km+1)

  call grfrm
  call grsvpt( 0.2, 0.8, 0.2, 0.8 )
  call grswnd( -90.0, 90.0, real(ri), real(ro) )
  call grstrf
  call uwsgxa(real(y_Lat/pi*180),jm)
  call uwsgya(real(z_Rad),km+1)
  call usdaxs
  call udcntr(real(xyz_LaplaPol1(1,:,:)),jm,jm,km+1)

  call grcls

end program wttest7
