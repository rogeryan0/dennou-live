!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wtu_module テストプログラム
!
!      磁場トロイダルポテンシャルの境界値問題
!
!履歴  2008/01/13  竹広真一  wu_test_tormagbc.f90 より改変
!
program wtu_test_tormagbci

  use dc_message, only : MessageNotify
  use wtu_module

  implicit none

  integer,parameter  :: im=32, jm=16         ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: kmo=16, kmi=8        ! 格子点の設定(球殻動径, 球動径)
  integer,parameter  :: nm=10                ! 切断波数の設定(水平)
  integer,parameter  :: lmo=16, lmi=8        ! 切断波数の設定(球殻動径, 球動径)
  real(8),parameter  :: ri=0.5, ro=1.5       ! 内外半径

  real(8), dimension(im,jm,0:kmi)           :: xyr_TORMAG
  real(8), dimension((nm+1)*(nm+1),0:lmi)   :: wu_TORMAG

  real(8), parameter  :: pi=3.1415926535897932385D0
  real(8), parameter  :: eps=1D-16

  integer :: i,j 

  call MessageNotify('M','wtu_test_tormagbc', &
       'wu_module  wu_TormagBoundary subroutine test')

  write( 6,* ) 
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

  call wtu_Initial(im,jm,kmi,kmo,nm,lmi,lmo,ri,ro)

 !==================== wu_TormagBoundary =========================
  ! P_10
  xyr_TORMAG = sin(xyr_lat) * cos( pi*(xyr_rad-ri)/ri ) * xyr_Rad
  ! P_1_1
  !xyr_TORMAG = cos(xyr_lat)*cos(xyr_lon)* cos( pi*(xyr_rad-ri)/(ro-ri) )
  !xyr_TORMAG = 2*sin(xyr_lat)**2 * cos( pi*(xyr_rad-ri)/(ro-ri) )

  wu_TORMAG = wu_xyr(xyr_TORMAG)
  call wu_TormagBoundary(wu_TORMAG)
  xyr_TORMAG = xyr_wu(wu_TORMAG)

  do j=1,jm
     do i=1,im
        if ( abs(xyr_TORMAG(i,j,0)) > eps ) then
           write(6,*) 'Top B.C. : ', i,j,xyr_TORMAG(i,j,0)
           call MessageNotify('E','wtu_test_tormagbc',&
                              'Top B.C. error too large')
        endif
     enddo
  enddo

  call MessageNotify('M','wtu_test_tormagbc', &
       'wu_TormagBoundary test succeeded!')

 !==================== wu_TormagBoundaryGrid =========================
  ! P_10
  !xyr_TORMAG = sin(xyr_lat) * cos( pi*(xyr_rad-ri)/ri )
  ! P_1_1
  xyr_TORMAG = cos(xyr_lat)*cos(xyr_lon)* cos( pi*(xyr_rad-ri)/ri )*xyr_Rad
  !xyr_TORMAG = 2*sin(xyr_lat)**2 * cos( pi*(xyr_rad-ri)/ri )

  wu_TORMAG = wu_xyr(xyr_TORMAG)
!!$  call wu_TormagBoundary(wu_TORMAG)
  call wu_TormagBoundaryGrid(wu_TORMAG)
  xyr_TORMAG = xyr_wu(wu_TORMAG)

  do j=1,jm
     do i=1,im
        if ( abs(xyr_TORMAG(i,j,0)) > eps ) then
           write(6,*) 'Top B.C. : ', i,j,xyr_TORMAG(i,j,0)
           call MessageNotify('E','wtu_test_tormagbc',&
                              'Top B.C. error too large')
        endif
     enddo
  enddo

  call MessageNotify('M','wtu_test_tormagbc', &
       'wu_TormagBoundaryGrid test succeeded!')


  call MessageNotify('M','wtu_test_tormagbc', &
       'wu_module  wu_TormagBoundary subroutine test succeded')


end program wtu_test_tormagbci
