!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wtu_module �ƥ��ȥץ����
!
!      ����ȥ�����ݥƥ󥷥��ζ���������
!
!����  2008/01/13  �ݹ�����  wtu_test_tormagbci.f90 ������
!
program wtu_test_polmagbctau

  use dc_message, only : MessageNotify
  use wtu_module

  implicit none

  integer,parameter  :: im=32, jm=16         ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: kmo=16, kmi=8        ! �ʻ���������(���ư��, ��ư��)
  integer,parameter  :: nm=10                ! �����ȿ�������(��ʿ)
  integer,parameter  :: lmo=10, lmi=5        ! �����ȿ�������(���ư��, ��ư��)
  real(8),parameter  :: ri=0.5, ro=1.5       ! �⳰Ⱦ��

  real(8), dimension(im,jm,0:kmo)           :: xyz_POLMAG
  real(8), dimension((nm+1)*(nm+1),0:lmo)   :: wt_POLMAG
  real(8), dimension(im,jm,0:kmi)           :: xyr_POLMAG
  real(8), dimension((nm+1)*(nm+1),0:lmi)   :: wu_POLMAG
  real(8), dimension(im,jm,0:kmo)           :: xyz_DPOLDR
  real(8), dimension(im,jm,0:kmi)           :: xyr_DPOLDR

  real(8), dimension((nm+1)*(nm+1),0:kmo)   :: wz_TopBoundary
  real(8), dimension((nm+1)*(nm+1),0:kmo)    :: wz_n   ! ���ȿ�

  real(8), parameter  :: pi=3.1415926535897932385D0
  real(8), parameter  :: eps=1D-4

  integer :: n,k, nn(2)

  call MessageNotify('M','wtu_test_tormagbc', &
       'wu_module  wtu_TormagBoundariesGrid subroutine test')

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

  wt_POLMAG = wt_xyz(xyz_POLMAG)
  wu_POLMAG = wu_xyr(xyr_POLMAG)
  call wtu_PolmagBoundariesTau(wt_POLMAG,wu_POLMAG)
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

  wt_POLMAG = wt_xyz(xyz_POLMAG)
  wu_POLMAG = wu_xyr(xyr_POLMAG)
  call wtu_PolmagBoundariesTau(wt_POLMAG,wu_POLMAG)
  xyz_POLMAG = xyz_wt(wt_POLMAG)
  xyr_POLMAG = xyr_wu(wu_POLMAG)

  xyz_DPOLDR = xyz_wt(wt_DRad_wt(wt_POLMAG))
  xyr_DPOLDR = xyr_wr(wr_DRad_wu(wu_POLMAG))

  call checkresult

  call MessageNotify('M','wtu_test_polmagbctau', &
       'wu_TormagBoundaryGrid test succeeded!')

  call MessageNotify('M','wtu_test_polmagbctau', &
       'wu_module  wu_TormagBoundary subroutine test succeded')

contains

  subroutine checkresult
    integer :: i, j, n

    do n=1,(nm+1)*(nm+1)
       if ( abs(wz_TopBoundary(n,0)) > eps ) then
          write(6,*) 'Top B.C. : ', nm_l(n), wz_TopBoundary(n,0)
          call MessageNotify('E','wt_test_polmagbctau','Top B.C. error too large')
       endif
    enddo
    do j=1,jm
       do i=1,im
          if ( abs(xyz_POLMAG(i,j,kmo)-xyr_POLMAG(i,j,0)) > eps ) then
             write(6,*) 'Inner B.C. : ',i,j, &
             xyz_POLMAG(i,j,kmo),xyr_POLMAG(i,j,0), &
             xyz_POLMAG(i,j,kmo)-xyr_POLMAG(i,j,0)
             call MessageNotify('E','wtu_test_tormagbc',&
                  'Inner B.C.(value conituity) error too large')
          endif
          if ( abs(xyz_DPOLDR(i,j,kmo)-xyr_DPOLDR(i,j,0)) > eps ) then
             write(6,*) 'Inner B.C. : ',i,j, &
             xyz_DPOLDR(i,j,kmo),xyr_DPOLDR(i,j,0), &
             xyz_DPOLDR(i,j,kmo)-xyr_DPOLDR(i,j,0)
             call MessageNotify('E','wtu_test_tormagbc',&
                  'Inner B.C.(derivative conituity) error too large')
          end if
       enddo
    enddo

  end subroutine checkresult

end program wtu_test_polmagbctau
