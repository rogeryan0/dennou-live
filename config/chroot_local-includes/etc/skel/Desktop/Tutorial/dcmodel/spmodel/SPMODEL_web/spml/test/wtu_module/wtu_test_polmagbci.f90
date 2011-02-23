!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wtu_module テストプログラム
!
!      磁場ポロイダルポテンシャルの境界値問題
!
!履歴  2008/01/13  竹広真一  wu_test_polmagbc.f90 より改変
!
program wtu_test_polmagbci

  use dc_message, only : MessageNotify
  use wtu_module

  implicit none

  integer,parameter  :: im=32, jm=16         ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: kmo=16, kmi=8        ! 格子点の設定(球殻動径, 球動径)
  integer,parameter  :: nm=10                ! 切断波数の設定(水平)
  integer,parameter  :: lmo=16, lmi=8        ! 切断波数の設定(球殻動径, 球動径)
  real(8),parameter  :: ri=0.5, ro=1.5       ! 内外半径

  real(8), dimension(im,jm,0:kmi)           :: xyr_POLMAG
  real(8), dimension(im,jm,0:kmi)           :: xyr_POLMAG_orig
  real(8), dimension((nm+1)*(nm+1),0:lmi)   :: wu_POLMAG

  real(8), dimension((nm+1)*(nm+1),0:kmi)   :: wr_TopBoundary

  real(8), dimension((nm+1)*(nm+1),0:kmi)   :: wr_n   ! 全波数

  real(8), parameter  :: pi=3.1415926535897932385D0
  real(8), parameter  :: eps=1D-14

  integer :: i, j, k, n, nn(2)

  call MessageNotify('M','wu_test_polmagbc', &
       'wu_module  wu_PolmagBoundary subroutine test')

  write( 6,* ) 'Output is displayed if computational error is larger than',eps

  call wtu_Initial(im,jm,kmi,kmo,nm,lmi,lmo,ri,ro)

  !=================== wu_PolmagBoundary =======================
  ! P_10
  xyr_POLMAG = sin(xyr_lat) * sin( pi*(xyr_rad-ri)/ri )

  ! P_1_1
  !xyr_POLMAG = cos(xyr_lat)*cos(xyr_lon)* sin( pi*(xyr_rad-ri)/(ro-ri) )
  !xyr_POLMAG = 2*sin(xyr_lat)**2 * sin( pi*(xyr_rad-ri)/(ro-ri) )

  xyr_POLMAG_orig = xyr_POLMAG
  wu_POLMAG = wu_xyr(xyr_POLMAG)
!!$  call wu_PolmagBoundary(wu_POLMAG)
  call wu_PolmagBoundaryGrid(wu_POLMAG)
  xyr_POLMAG = xyr_wu(wu_POLMAG)

  do k=1,kmi
     do j=1,jm
        do i=1,im
           if ( abs(xyr_Polmag(i,j,k)-xyr_POLMAG_orig(i,j,k)) > eps ) then
              write(6,*) 'internal value. : ', i,j,k, &
                          xyr_Polmag(i,j,k)-xyr_POLMAG_orig(i,j,k)
              call MessageNotify('E','wu_test_polvelbc',&
                           'internal value error too large')
           endif
        enddo
     enddo
  enddo

  call MessageNotify('M','wu_test_polmagbc', &
                         'internal value test succeeded!')
  do k=0,kmi
     do n=1,(nm+1)**2
        nn=nm_l(n)
        wr_n(n,k) = nn(1)
     enddo
  enddo

  wr_TopBoundary = wr_DRad_wu(wu_POLMAG) &
                     + (wr_n +1)*wr_wu(wu_POLMAG)/wr_RAD
  do n=1,(nm+1)*(nm+1)
     if ( abs(wr_TopBoundary(n,0)) > eps ) then
        write(6,*) 'Top B.C. : ', nm_l(n), wr_TopBoundary(n,0)
        call MessageNotify('E','wu_test_polmagbc','Top B.C. error too large')
     endif
  enddo
  call MessageNotify('M','wu_test_polmagbc', &
       'wu_PolmagBoundary test succeeded!')

  !=================== wu_PolmagBoundaryGrid =======================
  ! P_10
  !xyr_POLMAG = sin(xyr_lat) * sin( pi*(xyr_rad-ri)/(ro-ri) )

  ! P_1_1
  !xyr_POLMAG = cos(xyr_lat)*cos(xyr_lon)* sin( pi*(xyr_rad-ri)/(ro-ri) )
  xyr_POLMAG = 2*sin(xyr_lat)**2 * sin( pi*(xyr_rad-ri)/ri ) * xyr_Rad

  xyr_POLMAG_orig = xyr_POLMAG
  wu_POLMAG = wu_xyr(xyr_POLMAG)
!!$  call wu_PolmagBoundary(wu_POLMAG)
  call wu_PolmagBoundaryGrid(wu_POLMAG)
  xyr_POLMAG = xyr_wu(wu_POLMAG)

  do k=1,kmi
     do j=1,jm
        do i=1,im
           if ( abs(xyr_Polmag(i,j,k)-xyr_POLMAG_orig(i,j,k)) > eps ) then
              write(6,*) 'internal value. : ', i,j,k, &
                          xyr_Polmag(i,j,k)-xyr_POLMAG_orig(i,j,k)
              call MessageNotify('E','wu_test_polvelbc',&
                           'internal value error too large')
           endif
        enddo
     enddo
  enddo

  call MessageNotify('M','wu_test_polmagbc', &
                         'internal value test succeeded!')

  do k=0,kmi
     do n=1,(nm+1)**2
        nn=nm_l(n)
        wr_n(n,k) = nn(1)
     enddo
  enddo

  wr_TopBoundary = wr_DRad_wu(wu_POLMAG) &
                     + (wr_n +1)*wr_wu(wu_POLMAG)/wr_RAD
  do n=1,(nm+1)*(nm+1)
     if ( abs(wr_TopBoundary(n,0)) > eps ) then
        write(6,*) 'Top B.C. : ', nm_l(n), wr_TopBoundary(n,0)
        call MessageNotify('E','wu_test_polmagbc','Top B.C. error too large')
     endif
  enddo
  call MessageNotify('M','wu_test_polmagbc', &
       'wu_PolmagBoundaryGrid test succeeded!')


  call MessageNotify('M','wu_test_polmagbc', &
       'wu_module  wu_PolmagBoundary subroutine test succeded')

end program wtu_test_polmagbci

