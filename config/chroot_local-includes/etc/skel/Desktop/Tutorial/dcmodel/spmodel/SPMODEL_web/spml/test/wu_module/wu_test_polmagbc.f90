!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wu_module �ƥ��ȥץ����
!
!      ����ݥ�����ݥƥ󥷥��ζ���������
!
!����  2008/01/02  �ݹ�����
!      2008/06/28  ��������ʿ  �����������ѹ�
!
program wu_test_polmagbc

  use dc_message, only : MessageNotify
  use wu_module

  implicit none

  integer,parameter  :: im=32, jm=16, km=32  ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=32         ! �����ȿ�������(��ʿ, ư��)
  real(8),parameter  :: ra=2.5               ! ��Ⱦ��

  real(8), dimension(0:im-1,1:jm,0:km)           :: xyr_POLMAG
  real(8), dimension(0:im-1,1:jm,0:km)           :: xyr_POLMAG_orig
  real(8), dimension((nm+1)**2,0:lm)   :: wu_POLMAG

  real(8), dimension((nm+1)**2,0:km)   :: wr_TopBoundary

  real(8), dimension((nm+1)**2,0:km)   :: wr_n   ! ���ȿ�

  real(8), parameter  :: pi=3.1415926535897932385D0
  real(8), parameter  :: eps=5.0D-14

  integer :: i, j, k, n, nn(2)

  call MessageNotify('M','wu_test_polmagbc', &
       'wu_module  wu_PolmagBoundary subroutine test')

  write(6,*) 'Output is displayed if computational error is larger than', eps

  call wu_initial(im,jm,km,nm,lm,ra)

  !=================== wu_PolmagBoundary =======================
  ! P_10
  xyr_POLMAG = sin(xyr_lat) * sin( pi*(xyr_rad-ra)/ra )

  ! P_1_1
  !xyr_POLMAG = cos(xyr_lat)*cos(xyr_lon)* sin( pi*(xyr_rad-ri)/(ro-ri) )
  !xyr_POLMAG = 2*sin(xyr_lat)**2 * sin( pi*(xyr_rad-ri)/(ro-ri) )

  xyr_POLMAG_orig = xyr_POLMAG
  wu_POLMAG = wu_xyr(xyr_POLMAG)
!!$  call wu_PolmagBoundary(wu_POLMAG)
  call wu_PolmagBoundaryGrid(wu_POLMAG)
  xyr_POLMAG = xyr_wu(wu_POLMAG)

  do k=1,km
    do j=1,jm
      do i=0,im-1
        if ( abs(xyr_Polmag(i,j,k)-xyr_POLMAG_orig(i,j,k)) > eps ) then
          write(6,*) 'internal value. : ', i,j,k, &
            xyr_Polmag(i,j,k)-xyr_POLMAG_orig(i,j,k)
          call MessageNotify('E','wu_test_polmagbc',&
            'internal value error too large')
        endif
      enddo
    enddo
  enddo
  
  call MessageNotify('M','wu_test_polmagbc', &
                         'internal value test succeeded!')
  do k=0,km
     do n=1,(nm+1)**2
        nn=nm_l(n)
        wr_n(n,k) = nn(1)
     enddo
  enddo

  wr_TopBoundary = wr_DRad_wu(wu_POLMAG) &
                     + (wr_n +1)*wr_wu(wu_POLMAG)/wr_RAD

  do n=1,(nm+1)**2
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
  xyr_POLMAG = 2*sin(xyr_lat)**2 * sin( pi*(xyr_rad-ra)/ra ) * xyr_Rad

  xyr_POLMAG_orig = xyr_POLMAG
  wu_POLMAG = wu_xyr(xyr_POLMAG)
!!$  call wu_PolmagBoundary(wu_POLMAG)
  call wu_PolmagBoundaryGrid(wu_POLMAG)
  xyr_POLMAG = xyr_wu(wu_POLMAG)

  do k=1,km
    do j=1,jm
      do i=0,im-1
        if ( abs(xyr_Polmag(i,j,k)-xyr_POLMAG_orig(i,j,k)) > eps ) then
          write(6,*) 'internal value. : ', i,j,k, &
            xyr_Polmag(i,j,k)-xyr_POLMAG_orig(i,j,k)
          call MessageNotify('E','wu_test_polmagbc',&
            'internal value error too large')
        endif
      enddo
    enddo
  enddo
  
  call MessageNotify('M','wu_test_polmagbc', &
                         'internal value test succeeded!')

  do k=0,km
    do n=1,(nm+1)**2
      nn=nm_l(n)
      wr_n(n,k) = nn(1)
    enddo
  enddo
  
  wr_TopBoundary = wr_DRad_wu(wu_POLMAG) &
                     + (wr_n +1)*wr_wu(wu_POLMAG)/wr_RAD
  do n=1,(nm+1)**2
    if ( abs(wr_TopBoundary(n,0)) > eps ) then
      write(6,*) 'Top B.C. : ', nm_l(n), wr_TopBoundary(n,0)
      call MessageNotify('E','wu_test_polmagbc','Top B.C. error too large')
    endif
  enddo

  call MessageNotify('M','wu_test_polmagbc', &
       'wu_PolmagBoundaryGrid test succeeded!')

  call MessageNotify('M','wu_test_polmagbc', &
       'wu_module  wu_PolmagBoundary subroutine test succeded')

end program wu_test_polmagbc

