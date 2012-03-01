!----------------------------------------------------------------------
!   Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  aq_module ����ץ�ץ����
!
!      �Ȼ������� d temp/dt = kappa [1/r d/dr (r d temp/dr) - m^2 temp /r^2]
!
!����  2007/12/28  �ݹ�����
!
program aq_diff_disk

  use lumatrix
  use aq_module
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=16, km=31       ! �ʻ�����, �����ȿ�

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: ra=2.0D0            ! Ⱦ��
  real(8), parameter :: alpha=1.0D0         ! Ÿ��¿�༰�ѥ�᥿��  0 < �� <= 1
  real(8), parameter :: beta= 1.0D0         ! Ÿ��¿�༰�ѥ�᥿��  0 < ��
  integer, parameter :: mind=5              ! Ÿ�����ڥ��ȥ���ռ���

 !---- �ѿ� ----
  real(8), dimension(im)    :: g_temp
  real(8), dimension(0:km)  :: q_temp

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=1e-6                  ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=10000, ndisp=1000    ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  real(8), parameter :: tempbndry=0.0           ! ������
  character(len=2), parameter :: tempbc='N'     ! �������(D/N)

!!$  real(8), parameter :: sigma=0.1               ! ����ͤ���
  real(8), parameter :: kappa=1.0               ! �Ȼ�����
 
  integer :: it=0

 !---------------- ��ɸ�ͤ����� ---------------------
  call aq_Initial(im,km,ra,alpha,beta,(/mind/))

 !------------------- ��������� ----------------------
  g_temp = g_R**mind * (ra-g_R)
  q_temp = q_g(g_temp)

  call output_gtool4_init                            ! �ҥ��ȥ꡼�����
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt
     q_temp = q_temp &
          + dt * kappa * q_g(1.0D0/g_R**2*g_q(q_rDr_q(q_rDr_q(q_temp))&
                                              -mind**2 * q_temp))

     if ( tempbc == 'D' ) then
        call aq_Boundary_D(q_temp,tempbndry)
!!$        call aq_BoundaryGrid_D(q_temp,tempbndry)
     else if ( tempbc == 'N' ) then
        call aq_Boundary_N(q_temp,tempbndry)
!!$        call aq_BoundaryGrid_N(q_temp,tempbndry)
     else
        write(6,*) 'B.C. not supported'
     endif

     if(mod(it,ndisp) .eq. 0)then                    ! ����
        call output_gtool4
     endif

  enddo
  call output_gtool4_close                           ! �ҥ��ȥ꡼�����

  stop

  contains

    subroutine output_gtool4_init
      call HistoryCreate( &                               ! �ҥ��ȥ꡼����
           file='aq_diff_disk.nc', &
           title='Diffusion equation in a sphere', &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club davis/spmodel project',&
           dims=(/'r','t'/), dimsizes=(/im,0/),    &
           longnames=(/'radial coordinate',&
                       'time             '/),&
           units=(/'1','1'/),                           &
           origin=0.0, interval=real(ndisp*dt) )

      call HistoryPut('r',g_R)                            ! �ѿ�����

      call HistoryAddVariable( &                          ! �ѿ����
           varname='temp', dims=(/'r','t'/), & 
           longname='voticity', units='1', xtype='double')

    end subroutine output_gtool4_init

    subroutine output_gtool4
      g_temp = g_q(q_temp)

      write(6,*) 'it = ',it
      call HistoryPut('t',real(it*dt))
      call HistoryPut('temp',g_temp)
    end subroutine output_gtool4
 
    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

end program aq_diff_disk
