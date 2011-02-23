!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wq_module テストプログラム
!
!      磁場ポロイダルポテンシャルの境界値問題
!
!履歴  2009/12/11  竹広真一   wq_test_polmagbc.f90 より SJPACK 用に改造
!
program wq_module_polmagbc_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wq_module

  implicit none

  integer,parameter  :: im=32, jm=16, km=8   ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=15         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ra=2.5               ! 球半径

  real(8), dimension(0:im-1,1:jm,km)       :: xyr_POLMAG
  real(8), dimension(0:im-1,1:jm,km)       :: xyr_POLMAG_orig
  real(8), dimension((nm+1)*(nm+1),0:lm)   :: wq_POLMAG
  real(8), dimension((nm+1)*(nm+1),0:lm)   :: wq_POLMAG_orig
  real(8), dimension((nm+1)*(nm+1),km)     :: wr_POLMAG

  real(8), dimension((nm+1)*(nm+1),km)     :: wr_TopBoundary
  real(8), dimension((nm+1)*(nm+1),km)     :: wr_Zero = 0.0D0

  real(8), dimension((nm+1)*(nm+1),km)     :: wr_n   ! 全波数

  ! 判定誤差設定
  integer, parameter :: check_digits = 13
  integer, parameter :: ignore = -14

  real(8), parameter  :: pi=3.1415926535897932385D0

  integer :: k, n, nn(2)

  call MessageNotify('M','wq_module_polmagbc_test', &
       'wq_module wq_PolmagBoundary subroutine test')

  call wq_initial(im,jm,km,nm,lm,ra)

  !=================== wq_PolmagBoundary =======================
  ! P_10
  xyr_POLMAG = sin(xyr_lat) * sin( pi*xyr_rad/ra )

  ! P_1_1
  !xyr_POLMAG = cos(xyr_lat)*cos(xyr_lon)* sin( pi*(xyr_rad-ra)/ra )
  !xyr_POLMAG = 2*sin(xyr_lat)**2 * sin( pi*(xyr_rad-ra)/ra )

  xyr_POLMAG_orig = xyr_POLMAG
  wq_POLMAG = wq_xyr(xyr_POLMAG)
  wq_POLMAG_orig = wq_POLMAG
  call wq_PolmagBoundary(wq_POLMAG)

  call AssertEqual(&
       message='wq_PolmagBoundary (internal value)',                 &
       answer = wq_Polmag(:,0:lm-1),                                 &
       check = wq_Polmag_orig(:,0:lm-1),                             &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  do k=1,km
     do n=1,(nm+1)**2
        nn=nm_l(n)
        wr_n(n,k) = nn(1)
     enddo
  enddo

  wr_TopBoundary = wr_wq(wq_RadDRad_wq(wq_POLMAG))/wr_RAD &
                     + (wr_n +1)*wr_wq(wq_POLMAG)/wr_RAD

  call AssertEqual(&
       message='wq_PolmagBoundary (Top B.C.)',                       &
       answer = wr_TopBoundary(:,km),                                &
       check = wr_Zero(:,km),                                        &
       significant_digits = check_digits, ignore_digits = ignore     &
       )


  !=================== wq_PolmagBoundaryGrid =======================
  ! P_10
  !xyr_POLMAG = sin(xyr_lat) * sin( pi*(xyr_rad-ri)/(ro-ri) )

  ! P_1_1
  !xyr_POLMAG = cos(xyr_lat)*cos(xyr_lon)* sin( pi*(xyr_rad-ri)/(ro-ri) )
  xyr_POLMAG = 2*sin(xyr_lat)**2 * sin( pi*(xyr_rad-ra)/ra ) * xyr_Rad

  xyr_POLMAG_orig = xyr_POLMAG
  wr_POLMAG = wr_xyr(xyr_POLMAG)
  call wr_PolmagBoundaryGrid(wr_POLMAG)
  xyr_POLMAG = xyr_wr(wr_POLMAG)
  wq_POLMAG  = wq_wr(wr_POLMAG)

  call AssertEqual(&
       message='wq_PolmagBoundaryGrid (internal value)',             &
       answer = xyr_Polmag(:,:,1:km-1),                              &
       check = xyr_Polmag_orig(:,:,1:km-1),                          &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  do k=1,km
     do n=1,(nm+1)**2
        nn=nm_l(n)
        wr_n(n,k) = nn(1)
     enddo
  enddo

  wr_TopBoundary = wr_wq(wq_RadDRad_wq(wq_POLMAG))/wr_RAD &
                      + (wr_n +1)*wr_POLMAG/wr_RAD
  call AssertEqual(&
       message='wq_PolmagBoundaryGrid (Top B.C.)',                   &
       answer = wr_TopBoundary(:,km),                                &
       check = wr_Zero(:,km),                                        &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call MessageNotify('M','wq_module_polmagbc_test', &
       'wq_module wq_PolmagBoundary subroutine test succeeded!')

end program wq_module_polmagbc_test

