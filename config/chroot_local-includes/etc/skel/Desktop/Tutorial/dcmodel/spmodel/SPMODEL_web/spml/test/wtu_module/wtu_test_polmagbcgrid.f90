!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wtu_module テストプログラム
!
!      磁場トロイダルポテンシャルの境界値問題
!
!履歴  2008/01/13  竹広真一  wtu_test_tormagbci.f90 より改変
!
program wtu_test_polmagbcgrid

  use dc_message, only : MessageNotify
  use wtu_module

  implicit none

  integer,parameter  :: im=32, jm=16         ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: kmo=16, kmi=8        ! 格子点の設定(球殻動径, 球動径)
  integer,parameter  :: nm=10                ! 切断波数の設定(水平)
  integer,parameter  :: lmo=16, lmi=8        ! 切断波数の設定(球殻動径, 球動径)
  real(8),parameter  :: ri=0.5, ro=1.5       ! 内外半径

  real(8), dimension(im,jm,0:kmo)           :: xyz_POLMAG
  real(8), dimension(im,jm,0:kmo)           :: xyz_POLMAG_orig
  real(8), dimension((nm+1)*(nm+1),0:lmo)   :: wt_POLMAG
  real(8), dimension(im,jm,0:kmi)           :: xyr_POLMAG
  real(8), dimension(im,jm,0:kmi)           :: xyr_POLMAG_orig
  real(8), dimension((nm+1)*(nm+1),0:lmi)   :: wu_POLMAG
  real(8), dimension(im,jm,0:kmo)           :: xyz_DPOLDR
  real(8), dimension(im,jm,0:kmi)           :: xyr_DPOLDR

  real(8), dimension((nm+1)*(nm+1),0:kmo)   :: wz_TopBoundary
  real(8), dimension((nm+1)*(nm+1),0:kmo)    :: wz_n   ! 全波数

  real(8), parameter  :: pi=3.1415926535897932385D0
  real(8), parameter  :: eps=1D-12

  integer :: n,k, nn(2)

  call MessageNotify('M','wtu_test_polmagbcgrid', &
       'wu_module  wtu_PolmagBoundariesGrid subroutine test')

  write( 6,* ) 
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

  call wtu_Initial(im,jm,kmi,kmo,nm,lmi,lmo,ri,ro)

  do k=0,kmo
     do n=1,(nm+1)**2
        nn=nm_l(n)
        wz_n(n,k) = nn(1)
     enddo
  enddo

  ! P_10

  xyz_POLMAG = sin(xyz_lat) * cos( pi*(xyz_rad-ri)/(ro-ri) )
  xyr_POLMAG = sin(xyr_lat) * cos( pi*(xyr_rad-ri)/ri ) * xyr_Rad

  xyz_POLMAG_orig = xyz_POLMAG ;  xyr_POLMAG_orig = xyr_POLMAG
  wt_POLMAG = wt_xyz(xyz_POLMAG)
  wu_POLMAG = wu_xyr(xyr_POLMAG)
  call wtu_PolmagBoundariesGrid(wt_POLMAG,wu_POLMAG)
  xyz_POLMAG = xyz_wt(wt_POLMAG)
  xyr_POLMAG = xyr_wu(wu_POLMAG)

  xyz_DPOLDR = xyz_wt(wt_DRad_wt(wt_POLMAG))
  xyr_DPOLDR = xyr_wr(wr_DRad_wu(wu_POLMAG))
  wz_TopBoundary = wz_wt(wt_DRad_wt(wt_POLMAG)) &
                     + (wz_n +1)*wz_wt(wt_POLMAG)/wz_RAD

  call checkresult

  ! P_1_1
  xyz_POLMAG = cos(xyz_lat)*cos(xyz_lon)* cos( pi*(xyz_rad-ri)/(ro-ri) )
  xyr_POLMAG = cos(xyr_lat)*cos(xyr_lon)* cos( pi*(xyr_rad-ri)/ri )*xyr_Rad

  xyz_POLMAG_orig = xyz_POLMAG ;  xyr_POLMAG_orig = xyr_POLMAG
  wt_POLMAG = wt_xyz(xyz_POLMAG)
  wu_POLMAG = wu_xyr(xyr_POLMAG)
  call wtu_PolmagBoundariesGrid(wt_POLMAG,wu_POLMAG)
  xyz_POLMAG = xyz_wt(wt_POLMAG)
  xyr_POLMAG = xyr_wu(wu_POLMAG)

  xyz_DPOLDR = xyz_wt(wt_DRad_wt(wt_POLMAG))
  xyr_DPOLDR = xyr_wr(wr_DRad_wu(wu_POLMAG))

  call checkresult

  call MessageNotify('M','wtu_test_polmagbcgrid', &
       'wu_PolmagBoundaryGrid test succeeded!')

  call MessageNotify('M','wtu_test_polmagbcgrid', &
       'wu_module  wu_PolmagBoundary subroutine test succeded')

contains

  subroutine checkresult
    integer :: i, j, k, n

    do k=1,kmo-1
       do j=1,jm
          do i=1,im
             if ( abs(xyz_POLMAG(i,j,k)-xyz_POLMAG_orig(i,j,k)) > eps ) then
                write(6,*) 'internal value. : ', i,j,k, &
                            xyz_POLMAG(i,j,k)-xyz_POLMAG_orig(i,j,k)
                call MessageNotify('E','wtu_test_polmaglbc',&
                             'internal value (shell) error too large')
             endif
          enddo
       enddo
    enddo

    do k=1,kmi
       do j=1,jm
          do i=1,im
             if ( abs(xyr_POLMAG(i,j,k)-xyr_POLMAG_orig(i,j,k)) > eps ) then
                write(6,*) 'internal value. : ', i,j,k, &
                            xyr_POLMAG(i,j,k)-xyr_POLMAG_orig(i,j,k)
                call MessageNotify('E','wtu_test_polmaglbc',&
                             'internal value (sphere) error too large')
             endif
          enddo
       enddo
    enddo

    do n=1,(nm+1)*(nm+1)
       if ( abs(wz_TopBoundary(n,0)) > eps ) then
          write(6,*) 'Top B.C. : ', nm_l(n), wz_TopBoundary(n,0)
          call MessageNotify('E','wt_test_polmagbcgrid','Top B.C. error too large')
       endif
    enddo
    do j=1,jm
       do i=1,im
          if ( abs(xyz_POLMAG(i,j,kmo)-xyr_POLMAG(i,j,0)) > eps ) then
             write(6,*) 'Inner B.C. : ',i,j, &
             xyz_POLMAG(i,j,kmo),xyr_POLMAG(i,j,0), &
             xyz_POLMAG(i,j,kmo)-xyr_POLMAG(i,j,0)
             call MessageNotify('E','wtu_test_polmagbcgrid',&
                  'Inner B.C.(value conituity) error too large')
          endif
          if ( abs(xyz_DPOLDR(i,j,kmo)-xyr_DPOLDR(i,j,0)) > eps ) then
             write(6,*) 'Inner B.C. : ',i,j, &
             xyz_DPOLDR(i,j,kmo),xyr_DPOLDR(i,j,0), &
             xyz_DPOLDR(i,j,kmo)-xyr_DPOLDR(i,j,0)
             call MessageNotify('E','wtu_test_polmagbcgrid',&
                  'Inner B.C.(derivative conituity) error too large')
          end if
       enddo
    enddo

  end subroutine checkresult

end program wtu_test_polmagbcgrid
