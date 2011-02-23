!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_module テストプログラム
!
!      磁場ポロイダルポテンシャルの境界値問題
!
!履歴  2008/01/13  竹広真一  wt_test_polmagbc.f90 より改変
!
program wtu_test_polmagbc

  use dc_message, only : MessageNotify
  use wtu_module

  implicit none

  integer,parameter  :: im=32, jm=16         ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: kmo=16, kmi=8        ! 格子点の設定(球殻動径, 球動径)
  integer,parameter  :: nm=10                ! 切断波数の設定(水平)
  integer,parameter  :: lmo=16, lmi=8       ! 切断波数の設定(球殻動径, 球動径)
  real(8),parameter  :: ri=0.5, ro=1.5       ! 内外半径

  real(8), dimension(im,jm,0:kmo)           :: xyz_POLMAG
  real(8), dimension((nm+1)*(nm+1),0:lmo)   :: wt_POLMAG

  real(8), dimension((nm+1)*(nm+1),0:kmo)   :: wz_TopBoundary
  real(8), dimension((nm+1)*(nm+1),0:kmo)   :: wz_BottomBoundary

  real(8), dimension((nm+1)*(nm+1),0:kmo)   :: wz_n   ! 全波数

  real(8), parameter  :: pi=3.1415926535897932385D0
  real(8), parameter  :: eps=1D-10

  integer :: k, n, nn(2)

  call MessageNotify('M','wtu_test_polmagbc', &
       'wt_module  wt_PolmagBoundaries subroutine test')

  write( 6,* ) 'Output is displayed if computational error is larger than',eps

  call wtu_Initial(im,jm,kmi,kmo,nm,lmi,lmo,ri,ro)

  !=================== wt_PolmagBoundaries =======================
  ! P_10
  xyz_POLMAG = sin(xyz_lat) * sin( pi*(xyz_rad-ri)/(ro-ri) )

  ! P_1_1
  !xyz_POLMAG = cos(xyz_lat)*cos(xyz_lon)* sin( pi*(xyz_rad-ri)/(ro-ri) )
  !xyz_POLMAG = 2*sin(xyz_lat)**2 * sin( pi*(xyz_rad-ri)/(ro-ri) )

  wt_POLMAG = wt_xyz(xyz_POLMAG)
  call wt_PolmagBoundariesGrid(wt_POLMAG)
  xyz_POLMAG = xyz_wt(wt_POLMAG)

  do k=0,kmo
     do n=1,(nm+1)**2
        nn=nm_l(n)
        wz_n(n,k) = nn(1)
     enddo
  enddo

  wz_TopBoundary = wz_wt(wt_DRad_wt(wt_POLMAG)) &
                     + (wz_n +1)*wz_wt(wt_POLMAG)/wz_RAD
  wz_BottomBoundary = wz_wt(wt_DRad_wt(wt_POLMAG)) &
                     - wz_n * wz_wt(wt_POLMAG)/wz_RAD 
  do n=1,(nm+1)*(nm+1)
     if ( abs(wz_TopBoundary(n,0)) > eps ) then
        write(6,*) 'Top B.C. : ', nm_l(n), wz_TopBoundary(n,0)
        call MessageNotify('E','wtu_test_polmagbc','Top B.C. error too large')
     endif
     if ( abs(wz_BottomBoundary(n,kmo)) > eps ) then
        write(6,*) 'Bottom B.C. : ',nm_l(n), wz_BottomBoundary(n,kmo)
        call MessageNotify('E','wtu_test_polmagbc','Bottom B.C. error too large')
     endif
  enddo
  call MessageNotify('M','wtu_test_polmagbc', &
       'wt_PolmagBoundaries test succeeded!')

  !=================== wt_PolmagBoundariesGrid =======================
  ! P_10
  !xyz_POLMAG = sin(xyz_lat) * sin( pi*(xyz_rad-ri)/(ro-ri) )

  ! P_1_1
  !xyz_POLMAG = cos(xyz_lat)*cos(xyz_lon)* sin( pi*(xyz_rad-ri)/(ro-ri) )
  xyz_POLMAG = 2*sin(xyz_lat)**2 * sin( pi*(xyz_rad-ri)/(ro-ri) )

  wt_POLMAG = wt_xyz(xyz_POLMAG)
  call wt_PolmagBoundariesGrid(wt_POLMAG)
  xyz_POLMAG = xyz_wt(wt_POLMAG)

  do k=0,kmo
     do n=1,(nm+1)**2
        nn=nm_l(n)
        wz_n(n,k) = nn(1)
     enddo
  enddo

  wz_TopBoundary = wz_wt(wt_DRad_wt(wt_POLMAG)) &
                     + (wz_n +1)*wz_wt(wt_POLMAG)/wz_RAD
  wz_BottomBoundary = wz_wt(wt_DRad_wt(wt_POLMAG)) &
                     - wz_n * wz_wt(wt_POLMAG)/wz_RAD 
  do n=1,(nm+1)*(nm+1)
     if ( abs(wz_TopBoundary(n,0)) > eps ) then
        write(6,*) 'Top B.C. : ', nm_l(n), wz_TopBoundary(n,0)
        call MessageNotify('E','wtu_test_polmagbc','Top B.C. error too large')
     endif
     if ( abs(wz_BottomBoundary(n,kmo)) > eps ) then
        write(6,*) 'Bottom B.C. : ',nm_l(n), wz_BottomBoundary(n,kmo)
        call MessageNotify('E','wtu_test_polmagbc','Bottom B.C. error too large')
     endif
  enddo
  call MessageNotify('M','wtu_test_polmagbc', &
       'wt_PolmagBoundariesGrid test succeeded!')


  call MessageNotify('M','wtu_test_polmagbc', &
       'wt_module  wt_PolmagBoundaries subroutine test succeded')

end program wtu_test_polmagbc

