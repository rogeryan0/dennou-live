!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wu_module �ƥ��ȥץ����
!
!      ����ȥ�����ݥƥ󥷥��ζ���������
!
!����  2008/01/02  �ݹ�����
!      2008/06/28  ��������ʿ  �����������ѹ�
!
program wu_test_tormagbc

  use dc_message, only : MessageNotify
  use wu_module

  implicit none

  integer,parameter  :: im=32, jm=16, km=32  ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=32         ! �����ȿ�������(��ʿ, ư��)
  real(8),parameter  :: ra=0.5               ! ��Ⱦ��

  real(8), dimension(0:im-1,1:jm,0:km)           :: xyr_TORMAG
  real(8), dimension((nm+1)**2,0:lm)   :: wu_TORMAG

  real(8), parameter  :: pi=3.1415926535897932385D0
  real(8), parameter  :: eps=1D-16

  integer :: i,j 

  call MessageNotify('M','wu_test_tormagbc', &
       'wu_module  wu_TormagBoundary subroutine test')

  write( 6,* ) 
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

  call wu_initial(im,jm,km,nm,lm,ra)

 !==================== wu_TormagBoundary =========================
  ! P_10
  xyr_TORMAG = sin(xyr_lat) * cos( pi*(xyr_rad-ra)/ra ) * xyr_Rad
  ! P_1_1
  !xyr_TORMAG = cos(xyr_lat)*cos(xyr_lon)* cos( pi*(xyr_rad-ri)/(ro-ri) )
  !xyr_TORMAG = 2*sin(xyr_lat)**2 * cos( pi*(xyr_rad-ri)/(ro-ri) )

  wu_TORMAG = wu_xyr(xyr_TORMAG)
  call wu_TormagBoundary(wu_TORMAG)
  xyr_TORMAG = xyr_wu(wu_TORMAG)

  do j=1,jm
     do i=0,im-1
        if ( abs(xyr_TORMAG(i,j,0)) > eps ) then
           write(6,*) 'Top B.C. : ', i,j,xyr_TORMAG(i,j,0)
           call MessageNotify('E','wu_test_tormagbc',&
                              'Top B.C. error too large')
        endif
     enddo
  enddo

  call MessageNotify('M','wu_test_tormagbc', &
       'wu_TormagBoundary test succeeded!')

 !==================== wu_TormagBoundaryGrid =========================
  ! P_10
  !xyr_TORMAG = sin(xyr_lat) * cos( pi*(xyr_rad-ra)/ra )
  ! P_1_1
  xyr_TORMAG = cos(xyr_lat)*cos(xyr_lon)* cos( pi*(xyr_rad-ra)/ra )*xyr_Rad
  !xyr_TORMAG = 2*sin(xyr_lat)**2 * cos( pi*(xyr_rad-ra)/ra )

  wu_TORMAG = wu_xyr(xyr_TORMAG)
!!$  call wu_TormagBoundary(wu_TORMAG)
  call wu_TormagBoundaryGrid(wu_TORMAG)
  xyr_TORMAG = xyr_wu(wu_TORMAG)

  do j=1,jm
     do i=0,im-1
        if ( abs(xyr_TORMAG(i,j,0)) > eps ) then
           write(6,*) 'Top B.C. : ', i,j,xyr_TORMAG(i,j,0)
           call MessageNotify('E','wu_test_tormagbc',&
                              'Top B.C. error too large')
        endif
     enddo
  enddo

  call MessageNotify('M','wu_test_tormagbc', &
       'wu_TormagBoundaryGrid test succeeded!')


  call MessageNotify('M','wu_test_tormagbc', &
       'wu_module  wu_TormagBoundary subroutine test succeded')


end program wu_test_tormagbc
