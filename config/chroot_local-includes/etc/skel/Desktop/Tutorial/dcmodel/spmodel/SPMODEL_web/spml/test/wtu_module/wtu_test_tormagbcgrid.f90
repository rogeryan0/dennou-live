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
program wtu_test_tormagbcgrid

  use dc_message, only : MessageNotify
  use wtu_module

  implicit none

  integer,parameter  :: im=32, jm=16         ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: kmo=16, kmi=8        ! 格子点の設定(球殻動径, 球動径)
  integer,parameter  :: nm=10                ! 切断波数の設定(水平)
  integer,parameter  :: lmo=16, lmi=8        ! 切断波数の設定(球殻動径, 球動径)
  real(8),parameter  :: ri=0.5, ro=1.5       ! 内外半径

  real(8), dimension(im,jm,0:kmo)           :: xyz_TORMAG
  real(8), dimension(im,jm,0:kmo)           :: xyz_TORMAG_orig
  real(8), dimension((nm+1)*(nm+1),0:lmo)   :: wt_TORMAG
  real(8), dimension(im,jm,0:kmi)           :: xyr_TORMAG
  real(8), dimension(im,jm,0:kmi)           :: xyr_TORMAG_orig
  real(8), dimension((nm+1)*(nm+1),0:lmi)   :: wu_TORMAG
  real(8), dimension(im,jm,0:kmo)           :: xyz_DTORDR
  real(8), dimension(im,jm,0:kmi)           :: xyr_DTORDR

  real(8), parameter  :: Pmo = 1.0           ! 球殻の磁気プランドル数
  real(8), parameter  :: Pmi = 2.0           ! 内球の磁気プランドル数

  real(8), parameter  :: pi=3.1415926535897932385D0
  real(8), parameter  :: eps=1D-12


  call MessageNotify('M','wtu_test_tormagbc', &
       'wu_module  wtu_TormagBoundariesGrid subroutine test')

  write( 6,* ) 
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

  call wtu_Initial(im,jm,kmi,kmo,nm,lmi,lmo,ri,ro)

  ! P_10

  xyz_TORMAG = sin(xyz_lat) * cos( pi*(xyz_rad-ri)/(ro-ri) )
  xyr_TORMAG = sin(xyr_lat) * cos( pi*(xyr_rad-ri)/ri ) * xyr_Rad

  xyz_TORMAG_orig = xyz_TORMAG ;  xyr_TORMAG_orig = xyr_TORMAG
  wt_TORMAG = wt_xyz(xyz_TORMAG)
  wu_TORMAG = wu_xyr(xyr_TORMAG)
  call wtu_TormagBoundariesGrid(wt_TORMAG,wu_TORMAG,Pmo,Pmi)
  xyz_TORMAG = xyz_wt(wt_TORMAG)
  xyr_TORMAG = xyr_wu(wu_TORMAG)

  xyz_DTORDR = xyz_wt(wt_DRad_wt(wt_TORMAG))
  xyr_DTORDR = xyr_wr(wr_DRad_wu(wu_TORMAG))

  call checkresult

  ! P_1_1
  xyz_TORMAG = cos(xyz_lat)*cos(xyz_lon)* cos( pi*(xyz_rad-ri)/(ro-ri) )
  xyr_TORMAG = cos(xyr_lat)*cos(xyr_lon)* cos( pi*(xyr_rad-ri)/ri )*xyr_Rad

  xyz_TORMAG_orig = xyz_TORMAG ;  xyr_TORMAG_orig = xyr_TORMAG
  wt_TORMAG = wt_xyz(xyz_TORMAG)
  wu_TORMAG = wu_xyr(xyr_TORMAG)
  call wtu_TormagBoundariesGrid(wt_TORMAG,wu_TORMAG,Pmo,Pmi)
  xyz_TORMAG = xyz_wt(wt_TORMAG)
  xyr_TORMAG = xyr_wu(wu_TORMAG)

  xyz_DTORDR = xyz_wt(wt_DRad_wt(wt_TORMAG))
  xyr_DTORDR = xyr_wr(wr_DRad_wu(wu_TORMAG))

  call checkresult

  call MessageNotify('M','wtu_test_tormagbc', &
       'wu_TormagBoundaryGrid test succeeded!')

  call MessageNotify('M','wtu_test_tormagbc', &
       'wu_module  wu_TormagBoundary subroutine test succeded')

contains

  subroutine checkresult
    integer :: i, j, k

    do k=1,kmo-1
       do j=1,jm
          do i=1,im
             if ( abs(xyz_TORMAG(i,j,k)-xyz_TORMAG_orig(i,j,k)) > eps ) then
                write(6,*) 'internal value. : ', i,j,k, &
                            xyz_TORMAG(i,j,k)-xyz_TORMAG_orig(i,j,k)
                call MessageNotify('E','wtu_test_tormaglbc',&
                             'internal value (shell) error too large')
             endif
          enddo
       enddo
    enddo

    do k=1,kmi
       do j=1,jm
          do i=1,im
             if ( abs(xyr_TORMAG(i,j,k)-xyr_TORMAG_orig(i,j,k)) > eps ) then
                write(6,*) 'internal value. : ', i,j,k, &
                            xyr_TORMAG(i,j,k)-xyr_TORMAG_orig(i,j,k)
                call MessageNotify('E','wtu_test_tormaglbc',&
                             'internal value (sphere) error too large')
             endif
          enddo
       enddo
    enddo

    do j=1,jm
       do i=1,im
          if ( abs(xyz_TORMAG(i,j,0)) > eps ) then
             write(6,*) 'Top B.C. : ', i,j,xyz_TORMAG(i,j,0)
             call MessageNotify('E','wtu_test_tormagbc',&
                                'Top B.C. error too large')
          endif
          if ( abs(xyz_TORMAG(i,j,kmo)-xyr_TORMAG(i,j,0)) > eps ) then
             write(6,*) 'Inner B.C. : ',i,j, &
             xyz_TORMAG(i,j,kmo),xyr_TORMAG(i,j,0), &
             xyz_TORMAG(i,j,kmo)-xyr_TORMAG(i,j,0)
             call MessageNotify('E','wtu_test_tormagbc',&
                  'Inner B.C.(value conituity) error too large')
          endif
          if ( abs(Pmo*xyz_DTORDR(i,j,kmo)-Pmi*xyr_DTORDR(i,j,0)) > eps ) then
             write(6,*) 'Inner B.C. : ',i,j, &
             Pmo*xyz_DTORDR(i,j,kmo),Pmi*xyr_DTORDR(i,j,0), &
             Pmo*xyz_DTORDR(i,j,kmo)-Pmi*xyr_DTORDR(i,j,0)
             call MessageNotify('E','wtu_test_tormagbc',&
                  'Inner B.C.(derivative conituity) error too large')
          end if
       enddo
    enddo

  end subroutine checkresult

end program wtu_test_tormagbcgrid
