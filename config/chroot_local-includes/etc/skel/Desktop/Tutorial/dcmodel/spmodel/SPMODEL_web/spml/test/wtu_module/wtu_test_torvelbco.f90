!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wtu_module �ƥ��ȥץ����
!
!      �ȥ�����ݥƥ󥷥��ζ���������
!
!����  2008/01/13  �ݹ�����  wt_test_torvelbc.f90 ������
!
program wtu_test_torvelbc

  use dc_message, only : MessageNotify
  use wtu_module

  implicit none

  integer,parameter  :: im=32, jm=16         ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: kmo=16, kmi=8        ! �ʻ���������(���ư��, ��ư��)
  integer,parameter  :: nm=10                ! �����ȿ�������(��ʿ)
  integer,parameter  :: lmo=16, lmi=8       ! �����ȿ�������(���ư��, ��ư��)
  real(8),parameter  :: ri=0.5, ro=1.5       ! �⳰Ⱦ��

  real(8), dimension(im,jm,0:kmo)           :: xyz_TorVel0
  real(8), dimension(im,jm,0:kmo)           :: xyz_TorVel
  real(8), dimension((nm+1)*(nm+1),0:lmo)   :: wt_TorVel
  character(len=2), dimension(4),parameter :: BCond=(/'FF','FR','RF','RR'/)

  real(8), dimension(im,jm,0:kmo)           :: xyz_True

  real(8), parameter  :: pi=3.1415926535897932385D0
  real(8), parameter  :: eps=1D-10

  integer :: i,j,k,l

  call MessageNotify('M','wtu_test_torvelbc', &
       'wt_module  wt_TorgBoundariesGrid subroutine test')

  call wtu_Initial(im,jm,kmi,kmo,nm,lmi,lmo,ri,ro)

  do l=1,4

     ! P_10
     !xyz_TorVel = sin(xyz_lat) * cos( pi*(xyz_rad-ri)/(ro-ri) )
     ! P_1_1
     !xyz_TorVel = cos(xyz_lat)*cos(xyz_lon)* cos( pi*(xyz_rad-ri)/(ro-ri) )
     xyz_TorVel = 2*sin(xyz_lat)**2 * cos( pi*(xyz_rad-ri)/(ro-ri) )

     xyz_TorVel0 = xyz_TorVel
     wt_TorVel = wt_xyz(xyz_TorVel)
     call wt_TorBoundariesGrid(wt_TorVel,cond=BCond(l),new=.true.)
     xyz_TorVel = xyz_wt(wt_TorVel)


     ! ���������å�
     xyz_True = xyz_TorVel - xyz_TorVel0

     do k=1,kmo-1
        do j=1,jm
           do i=1,im
              if ( abs(xyz_True(i,j,k)) > eps ) then
                 write(6,*) 'internal value. : ', i,j,xyz_True(i,j,k)
                 call MessageNotify('E','wtu_test_torvelbc',&
                              'internal value error too large')
              endif
           enddo
        enddo
     enddo

     ! ��ü���������å�
     if( BCond(l)(1:1) == 'F' ) then
        xyz_True = xyz_wt(wt_DRad_wt(wt_xyz(xyz_TorVel/xyz_Rad)))
     else
        xyz_True = xyz_TorVel
     endif

     do j=1,jm
        do i=1,im
           if ( abs(xyz_True(i,j,0)) > eps ) then
              write(6,*) 'Top B.C. : ', i,j,xyz_True(i,j,0)
              call MessageNotify('E','wtu_test_torvelbc',&
                              'Top B.C. error too large')
           endif
        enddo
     enddo
     call MessageNotify('M','wtu_test_torvelbc', &
                        BCond(l)//'-Top B.C. test succeeded!')

     ! ��ü���������å�
     if( BCond(l)(2:2) == 'F' ) then
        xyz_True = xyz_wt(wt_DRad_wt(wt_xyz(xyz_TorVel/xyz_Rad)))
     else
        xyz_True = xyz_TorVel
     endif

     do j=1,jm
        do i=1,im
           if ( abs(xyz_True(i,j,kmo)) > eps ) then
              write(6,*) 'Bottom B.C. : ', i,j,xyz_True(i,j,kmo)
              call MessageNotify('E','wtu_test_torvelbc',&
                              'Bottom B.C. error too large')
           endif
        enddo
     enddo

     call MessageNotify('M','wtu_test_torvelbc', &
                        BCond(l)//'-Bottom B.C. test succeeded!')
  enddo

  call MessageNotify('M','wtu_test_torvelbc', &
       'wt_module  wt_TorgBoundariesGrid subroutine test succeeded!')

end program wtu_test_torvelbc

