!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wu_module �ƥ��ȥץ������
!
!      �ȥ�������ݥƥ󥷥��ζ���������
!
!����  2008/01/02  �ݹ�����
!      2007/11/11  �ݹ�����  ���顼��å������ɲ�
!      2008/06/28  ��������ʿ  �����������ѹ�
!
program wu_test_torvelbc

  use dc_message, only : MessageNotify
  use wu_module

  implicit none

  integer,parameter  :: im=32, jm=16, km=32  ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=32         ! �����ȿ�������(��ʿ, ư��)
  real(8),parameter  :: ra=1.0               ! ��Ⱦ��

  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_TorVel0
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_TorVel
  real(8), dimension((nm+1)**2,0:lm)       :: wu_TorVel
  character(len=1), dimension(2),parameter :: BCond=(/'F','R'/)

  real(8), dimension(0:im-1,1:jm,0:km)           :: xyr_True

  real(8), parameter  :: eps=1D-15

  integer :: i,j,k,l

  call MessageNotify('M','wu_test_torvelbc', &
       'wu_module  wu_TorBoundaryGrid subroutine test')

  call wu_initial(im,jm,km,nm,lm,ra)

  do l=1,2

     ! P_10
     !xyr_TorVel = sin(xyr_lat) * cos( pi*(xyr_rad-ri)/(ro-ri) )
     ! P_1_1
     !xyr_TorVel = cos(xyr_lat)*cos(xyr_lon)* cos( pi*(xyr_rad-ri)/(ro-ri) )
     xyr_TorVel = 2*sin(xyr_lat)**2 * (xyr_rad-ra)**2 * xyr_Rad**2

     xyr_TorVel0 = xyr_TorVel
     wu_TorVel = wu_xyr(xyr_TorVel)
     call wu_TorBoundaryGrid(wu_TorVel,cond=BCond(l),new=.true.)
!!$     call wu_TorBoundary(wu_TorVel,cond=BCond(l),new=.true.)
     xyr_TorVel = xyr_wu(wu_TorVel)


     ! ���������å�
     xyr_True = xyr_TorVel - xyr_TorVel0

     do k=1,km
        do j=1,jm
           do i=0,im-1
              if ( abs(xyr_True(i,j,k)) > eps ) then
                 write(6,*) 'internal value. : ', i,j,xyr_True(i,j,k)
                 call MessageNotify('E','wu_test_torvelbc',&
                              'internal value error too large')
              endif
           enddo
        enddo
     enddo

     ! ��ü���������å�
     if( BCond(l) == 'F' ) then
        xyr_True = xyr_wr(wr_DRad_wu(wu_Torvel)- (wr_wu(wu_Torvel)/wr_Rad))
     else
        xyr_True = xyr_TorVel
     endif

     do j=1,jm
        do i=0,im-1
           if ( abs(xyr_True(i,j,0)) > eps ) then
              write(6,*) 'Top B.C. : ', i,j,xyr_True(i,j,0)
              call MessageNotify('E','wu_test_torvelbc',&
                              'Top B.C. error too large')
           endif
        enddo
     enddo
     call MessageNotify('M','wu_test_torvelbc', &
                        BCond(l)//'-Top B.C. test succeeded!')

  enddo

  call MessageNotify('M','wu_test_torvelbc', &
       'wu_module  wu_TorgBoundaryGrid subroutine test succeeded!')

end program wu_test_torvelbc
