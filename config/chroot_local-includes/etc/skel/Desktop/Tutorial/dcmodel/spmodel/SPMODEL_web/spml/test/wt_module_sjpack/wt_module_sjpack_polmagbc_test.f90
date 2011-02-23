!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_module_sjpack テストプログラム
!
!      磁場ポロイダルポテンシャルの境界値問題
!
!履歴  2009/09/06  竹広真一   wt_test_polmagbc.f90 より SJPACK 用に改造
!
program wt_module_sjpack_polmagbc_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wt_module_sjpack

  implicit none

  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ri=0.5, ro=1.5       ! 内外半径

  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_POLMAG
  real(8), dimension((nm+1)*(nm+1),0:lm)   :: wt_POLMAG

  real(8), dimension((nm+1)*(nm+1),0:km)   :: wz_TopBoundary
  real(8), dimension((nm+1)*(nm+1),0:km)   :: wz_BottomBoundary

  real(8), dimension((nm+1)*(nm+1),0:km)   :: wz_Zero

  real(8), dimension((nm+1)*(nm+1),0:km)   :: wz_n   ! 全波数

  ! 判定誤差設定
  integer, parameter :: check_digits = 9
  integer, parameter :: ignore = -10

  real(8), parameter  :: pi=3.1415926535897932385D0

  integer :: k, n, nn(2)

  call MessageNotify('M','wt_module_sjpack_polmagbc_test', &
       'wt_module_sjpack wt_PolmagBoundaries subroutine test')

  call wt_initial(im,jm,km,nm,lm,ri,ro)

  !=================== wt_PolmagBoundaries =======================
  ! P_10
  xyz_POLMAG = sin(xyz_lat) * sin( pi*(xyz_rad-ri)/(ro-ri) )

  ! P_1_1
  !xyz_POLMAG = cos(xyz_lat)*cos(xyz_lon)* sin( pi*(xyz_rad-ri)/(ro-ri) )
  !xyz_POLMAG = 2*sin(xyz_lat)**2 * sin( pi*(xyz_rad-ri)/(ro-ri) )

  wt_POLMAG = wt_xyz(xyz_POLMAG)
  call wt_PolmagBoundaries(wt_POLMAG)
  xyz_POLMAG = xyz_wt(wt_POLMAG)

  do k=0,km
     do n=1,(nm+1)**2
        nn=nm_l(n)
        wz_n(n,k) = nn(1)
     enddo
  enddo

  wz_Zero = 0.0D0
  wz_TopBoundary = wz_wt(wt_DRad_wt(wt_POLMAG)) &
                     + (wz_n +1)*wz_wt(wt_POLMAG)/wz_RAD
  wz_BottomBoundary = wz_wt(wt_DRad_wt(wt_POLMAG)) &
                     - wz_n * wz_wt(wt_POLMAG)/wz_RAD 
  call AssertEqual(&
       message='wt_PolmagBoundaries (Top B.C.)',                 &
       answer = wz_Zero(:,0),                                        &
       check = wz_TopBoundary(:,0),                                  &
       significant_digits = check_digits, ignore_digits = ignore     &
       )
  call AssertEqual(&
       message='wt_PolmagBoundaries (Bottom B.C.)',              &
       answer = wz_Zero(:,km),                                       &
       check = wz_BottomBoundary(:,km),                                 &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  !=================== wt_PolmagBoundariesGrid =======================
  ! P_10
  !xyz_POLMAG = sin(xyz_lat) * sin( pi*(xyz_rad-ri)/(ro-ri) )

  ! P_1_1
  !xyz_POLMAG = cos(xyz_lat)*cos(xyz_lon)* sin( pi*(xyz_rad-ri)/(ro-ri) )
  xyz_POLMAG = 2*sin(xyz_lat)**2 * sin( pi*(xyz_rad-ri)/(ro-ri) )

  wt_POLMAG = wt_xyz(xyz_POLMAG)
  call wt_PolmagBoundariesGrid(wt_POLMAG)
  xyz_POLMAG = xyz_wt(wt_POLMAG)

  do k=0,km
     do n=1,(nm+1)**2
        nn=nm_l(n)
        wz_n(n,k) = nn(1)
     enddo
  enddo

  wz_TopBoundary = wz_wt(wt_DRad_wt(wt_POLMAG)) &
                     + (wz_n +1)*wz_wt(wt_POLMAG)/wz_RAD
  wz_BottomBoundary = wz_wt(wt_DRad_wt(wt_POLMAG)) &
                     - wz_n * wz_wt(wt_POLMAG)/wz_RAD 

  wz_Zero = 0.0D0
  wz_TopBoundary = wz_wt(wt_DRad_wt(wt_POLMAG)) &
                     + (wz_n +1)*wz_wt(wt_POLMAG)/wz_RAD
  wz_BottomBoundary = wz_wt(wt_DRad_wt(wt_POLMAG)) &
                     - wz_n * wz_wt(wt_POLMAG)/wz_RAD 
  call AssertEqual(&
       message='wt_PolmagBoundariesTau (Top B.C.)',                  &
       answer = wz_Zero(:,0),                                        &
       check = wz_TopBoundary(:,0),                                  &
       significant_digits = check_digits, ignore_digits = ignore     &
       )
  call AssertEqual(&
       message='wt_PolmagBoundariesTau (Bottom B.C.)',               &
       answer = wz_Zero(:,km),                                       &
       check = wz_BottomBoundary(:,km),                                 &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call MessageNotify('M','wt_module_sjpack_polmagbc_test', &
       'wt_module_sjpack wt_PolmagBoundaries subroutine test succeeded!')

end program wt_module_sjpack_polmagbc_test

