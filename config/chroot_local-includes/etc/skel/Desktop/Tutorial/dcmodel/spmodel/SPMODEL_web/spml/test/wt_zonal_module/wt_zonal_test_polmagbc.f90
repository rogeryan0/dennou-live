!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_zonal_module テストプログラム
!
!      磁場ポロイダルポテンシャルの境界値問題
!
!履歴  2008/12/30  竹広真一
!
program wt_zonal_test_polmagbc

  use dc_message, only : MessageNotify
  use wt_zonal_module

  implicit none

  integer,parameter  :: im=1, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16        ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ri=0.5, ro=1.5      ! 内外半径

  real(8), dimension(0:im-1,1:jm,0:km)           :: xyz_POLMAG
  real(8), dimension(nm+1,0:lm)   :: wt_POLMAG

  real(8), dimension(nm+1,0:km)   :: wz_TopBoundary
  real(8), dimension(nm+1,0:km)   :: wz_BottomBoundary

  real(8), dimension(nm+1,0:km)   :: wz_n   ! 全波数

  real(8), parameter  :: pi=3.1415926535897932385D0
  real(8), parameter  :: eps=1D-10

  integer :: k, n, nn(2)

  call MessageNotify('M','wt_zonal_test_polmagbc', &
       'wt_zonal_module  wt_PolmagBoundaries subroutine test')

  write( 6,* ) 'Output is displayed if computational error is larger than',eps

  call wt_initial(im,jm,km,nm,lm,ri,ro)

  !=================== wt_PolmagBoundaries =======================
  ! P_10
  xyz_POLMAG = sin(xyz_lat) * sin( pi*(xyz_rad-ri)/(ro-ri) )

  ! P_1_1
  !xyz_POLMAG = cos(xyz_lat)*cos(xyz_lon)* sin( pi*(xyz_rad-ri)/(ro-ri) )
  !xyz_POLMAG = 2*sin(xyz_lat)**2 * sin( pi*(xyz_rad-ri)/(ro-ri) )

  wt_POLMAG = wt_xyz(xyz_POLMAG)
  call wt_PolmagBoundariesGrid(wt_POLMAG)
  xyz_POLMAG = xyz_wt(wt_POLMAG)

  do k=0,km
     do n=1,nm+1
        nn=nm_l(n)
        wz_n(n,k) = nn(1)
     enddo
  enddo

  wz_TopBoundary = wz_wt(wt_DRad_wt(wt_POLMAG)) &
                     + (wz_n +1)*wz_wt(wt_POLMAG)/wz_RAD
  wz_BottomBoundary = wz_wt(wt_DRad_wt(wt_POLMAG)) &
                     - wz_n * wz_wt(wt_POLMAG)/wz_RAD 
  do n=1,nm+1
     if ( abs(wz_TopBoundary(n,0)) > eps ) then
        write(6,*) 'Top B.C. : ', nm_l(n), wz_TopBoundary(n,0)
        call MessageNotify('E','wt_test_polmagbc','Top B.C. error too large')
     endif
     if ( abs(wz_BottomBoundary(n,km)) > eps ) then
        write(6,*) 'Bottom B.C. : ',nm_l(n), wz_BottomBoundary(n,km)
        call MessageNotify('E','wt_test_polmagbc','Bottom B.C. error too large')
     endif
  enddo
  call MessageNotify('M','wt_test_polmagbc', &
       'wt_PolmagBoundaries test succeeded!')

  !=================== wt_PolmagBoundariesGrid =======================
  ! P_10
  xyz_POLMAG = sin(xyz_lat) * sin( pi*(xyz_rad-ri)/(ro-ri) )

  ! P_1_1
  !xyz_POLMAG = cos(xyz_lat)*cos(xyz_lon)* sin( pi*(xyz_rad-ri)/(ro-ri) )
  !xyz_POLMAG = 2*sin(xyz_lat)**2 * sin( pi*(xyz_rad-ri)/(ro-ri) )

  wt_POLMAG = wt_xyz(xyz_POLMAG)
  call wt_PolmagBoundariesGrid(wt_POLMAG)
  xyz_POLMAG = xyz_wt(wt_POLMAG)

  do k=0,km
     do n=1,nm+1
        nn=nm_l(n)
        wz_n(n,k) = nn(1)
     enddo
  enddo

  wz_TopBoundary = wz_wt(wt_DRad_wt(wt_POLMAG)) &
                     + (wz_n +1)*wz_wt(wt_POLMAG)/wz_RAD
  wz_BottomBoundary = wz_wt(wt_DRad_wt(wt_POLMAG)) &
                     - wz_n * wz_wt(wt_POLMAG)/wz_RAD 
  do n=1,nm+1
     if ( abs(wz_TopBoundary(n,0)) > eps ) then
        write(6,*) 'Top B.C. : ', nm_l(n), wz_TopBoundary(n,0)
        call MessageNotify('E','wt_test_polmagbc','Top B.C. error too large')
     endif
     if ( abs(wz_BottomBoundary(n,km)) > eps ) then
        write(6,*) 'Bottom B.C. : ',nm_l(n), wz_BottomBoundary(n,km)
        call MessageNotify('E','wt_test_polmagbc','Bottom B.C. error too large')
     endif
  enddo
  call MessageNotify('M','wt_zonal_test_polmagbc', &
       'wt_PolmagBoundariesGrid test succeeded!')


  call MessageNotify('M','wt_zonal_test_polmagbc', &
       'wt_zonal_module  wt_PolmagBoundaries subroutine test succeded')

end program wt_zonal_test_polmagbc
