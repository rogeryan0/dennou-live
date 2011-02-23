!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wtu_module テストプログラム
!
!      磁場トロイダルポテンシャルの境界値問題
!
!履歴  2008/01/13  竹広真一  wt_test_tormagbc.f90 より改変
!
program wtu_test_tormagbc

  use dc_message, only : MessageNotify
  use wtu_module

  implicit none

  integer,parameter  :: im=32, jm=16         ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: kmo=16, kmi=8        ! 格子点の設定(球殻動径, 球動径)
  integer,parameter  :: nm=10                ! 切断波数の設定(水平)
  integer,parameter  :: lmo=16, lmi=8       ! 切断波数の設定(球殻動径, 球動径)
  real(8),parameter  :: ri=0.5, ro=1.5       ! 内外半径

  real(8), dimension(im,jm,0:kmo)           :: xyz_TORMAG
  real(8), dimension((nm+1)*(nm+1),0:lmo)   :: wt_TORMAG

  real(8), dimension((nm+1)*(nm+1),0:kmo)   :: wz_n   ! 全波数

  real(8), parameter  :: pi=3.1415926535897932385D0
  real(8), parameter  :: eps=1D-10

  integer :: i,j 

  call MessageNotify('M','wtu_test_tormagbc', &
       'wt_module  wt_TormagBoundaries subroutine test')

  write( 6,* ) 
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

  call wtu_Initial(im,jm,kmi,kmo,nm,lmi,lmo,ri,ro)

 !==================== wt_TormagBoundaries =========================
  ! P_10
  xyz_TORMAG = sin(xyz_lat) * cos( pi*(xyz_rad-ri)/(ro-ri) )
  ! P_1_1
  !xyz_TORMAG = cos(xyz_lat)*cos(xyz_lon)* cos( pi*(xyz_rad-ri)/(ro-ri) )
  !xyz_TORMAG = 2*sin(xyz_lat)**2 * cos( pi*(xyz_rad-ri)/(ro-ri) )

  wt_TORMAG = wt_xyz(xyz_TORMAG)
  call wt_TormagBoundaries(wt_TORMAG)
  xyz_TORMAG = xyz_wt(wt_TORMAG)

  do j=1,jm
     do i=1,im
        if ( abs(xyz_TORMAG(i,j,0)) > eps ) then
           write(6,*) 'Top B.C. : ', i,j,xyz_TORMAG(i,j,0)
           call MessageNotify('E','wtu_test_tormagbc',&
                              'Top B.C. error too large')
        endif
        if ( abs(xyz_TORMAG(i,j,kmo)) > eps ) then
           write(6,*) 'Bottom B.C. : ', i,j,xyz_TORMAG(i,j,kmo)
           call MessageNotify('E','wtu_test_tormagbc',&
                              'Bottom B.C. error too large')
        endif
     enddo
  enddo

  call MessageNotify('M','wtu_test_tormagbc', &
       'wt_TormagBoundaries test succeeded!')

 !==================== wt_TormagBoundariesGrid =========================
  ! P_10
  !xyz_TORMAG = sin(xyz_lat) * cos( pi*(xyz_rad-ri)/(ro-ri) )
  ! P_1_1
  xyz_TORMAG = cos(xyz_lat)*cos(xyz_lon)* cos( pi*(xyz_rad-ri)/(ro-ri) )
  !xyz_TORMAG = 2*sin(xyz_lat)**2 * cos( pi*(xyz_rad-ri)/(ro-ri) )

  wt_TORMAG = wt_xyz(xyz_TORMAG)
  call wt_TormagBoundariesGrid(wt_TORMAG)
  xyz_TORMAG = xyz_wt(wt_TORMAG)

  do j=1,jm
     do i=1,im
        if ( abs(xyz_TORMAG(i,j,0)) > eps ) then
           write(6,*) 'Top B.C. : ', i,j,xyz_TORMAG(i,j,0)
           call MessageNotify('E','wtu_test_tormagbc',&
                              'Top B.C. error too large')
        endif
        if ( abs(xyz_TORMAG(i,j,kmo)) > eps ) then
           write(6,*) 'Bottom B.C. : ', i,j,xyz_TORMAG(i,j,kmo)
           call MessageNotify('E','wtu_test_tormagbc',&
                              'Bottom B.C. error too large')
        endif
     enddo
  enddo

  call MessageNotify('M','wtu_test_tormagbc', &
       'wt_TormagBoundariesGrid test succeeded!')


  call MessageNotify('M','wtu_test_tormagbc', &
       'wt_module  wt_TormagBoundaries subroutine test succeded')


end program wtu_test_tormagbc
