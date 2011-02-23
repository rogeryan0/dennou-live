!----------------------------------------------------------------------
!   Copyright (c) 2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  at_module ����ץ�ץ����
!
!      �Ȼ������� d temp/dt = kappa 1/r d/dr (r d temp/dr)
!
!����  2007/12/28  �ݹ�����
!
program au_diff_disk

  use lumatrix
  use au_module
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=16                ! �ʻ���������
  integer, parameter :: km=16               ! �����ȿ�������

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: ra=2.0D0            ! Ⱦ��

 !---- �ѿ� ----
  real(8), dimension(0:im)  :: g_temp
  real(8), dimension(0:km)  :: u_temp

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=1e-5                   ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=100000, ndisp=10000    ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  real(8), parameter :: tempbndry=0.0           ! ������
  character(len=2), parameter :: tempbc='D'     ! �������(D/N)

!!$  real(8), parameter :: sigma=0.1               ! ����ͤ���
  real(8), parameter :: kappa=1.0               ! �Ȼ�����
 
  integer :: it=0

 !---------------- ��ɸ�ͤ����� ---------------------
  call au_initial(im,km,ra,(/0/))

 !------------------- ��������� ----------------------
  g_temp = 1.0D0
  u_temp = u_g(g_temp)

  call output_gtool4_init                            ! �ҥ��ȥ꡼�����
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt
     u_temp = u_temp &
          + dt * kappa * u_g(1.0D0/g_R * g_Dr_u(u_g(g_R*g_Dr_u(u_temp))))

     if ( tempbc == 'D' ) then
        call au_Boundary_D(u_temp,tempbndry)
     else if ( tempbc == 'N' ) then
        call au_Boundary_N(u_temp,tempbndry)
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
           file='au_diff_disk.nc', &
           title='Diffusion equation in a sphere', &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club davis/spmodel project',&
           dims=(/'r','t'/), dimsizes=(/im+1,0/),    &
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
      g_temp = g_u(u_temp)

      write(6,*) 'it = ',it
      call HistoryPut('t',real(it*dt))
      call HistoryPut('temp',g_temp)
    end subroutine output_gtool4
 
    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

end program au_diff_disk
