!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  esc_module
!      2 ������ϩ�ΰ� �Ȼ�����
!           d\zeta/dt  = \nu\nabla^2\zeta
!
!����  2002/08/19  �ݹ�����
!      2004/02/15  ������ ��ʿ gt4f90io �λ��Ѥ�ȼ���饤�֥��̾�ѹ�
!      2005/03/25  ������ ��ʿ esc_module �� suffix �ѹ���ȼ�ʤ�����
!      2005/06/03  ������ ��ʿ it=0 ������Ū�����
!
program esc_diff_c

  use esc_module
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=32            ! �ʻ���������(X,Y)
  integer, parameter :: km=10, lm=10            ! �����ȿ�������(X,Y)

 !---- �ѿ� ----
  real(8)            :: yx_Zeta(0:jm,0:im-1)    ! �ʻҥǡ���
  real(8)            :: ec_Zeta(-km:km,0:lm)    ! ���ڥ��ȥ�ǡ���

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin=0.0, xmax=1.0
  real(8), parameter :: ymin=0.0, ymax=1.0

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=1e-4                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=1000, ndisp=100      ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  real(8), parameter :: nu=1.0                  ! Ǵ������
  real(8), parameter :: x1=(xmin+xmax)/2.0      ! ���ʬ�� X ��ɸ
  real(8), parameter :: y1=(xmin+xmax)/2.0      ! ���ʬ�� Y ��ɸ
  real(8), parameter :: sigma=0.1               ! ���ʬ�ۤ��礭��

  integer :: it=0                           ! DO �ѿ�


 !---------------- ��ɸ�ͤ����� ---------------------
  call esc_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! ���ڥ��ȥ�����

 !------------------- ��������� ----------------------
  yx_Zeta=  exp(-((yx_X-x1)**2+(yx_Y-y1)**2)/(2*sigma**2))
  ec_Zeta = ec_yx(yx_Zeta)

  call output_gtool4_init                            ! �ҥ��ȥ꡼�����
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt                                         ! Euler ˡ�ˤ�������ʬ
     ec_Zeta = ec_Zeta + dt * nu * ec_Lapla_ec(ec_Zeta) 

     if(mod(it,ndisp) .eq. 0)then                    ! ����
        yx_Zeta = yx_ec(ec_Zeta)
        call output_gtool4
     endif
  enddo

  call output_gtool4_close                           ! �ҥ��ȥ꡼�����
  stop

contains

  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! �ҥ��ȥ꡼����
           file='esc_diff_c.nc', title='2D diffusion model', &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club spmodel project',     &
           dims=(/'x','y','t'/), dimsizes=(/im,jm+1,0/),      &
           longnames=(/'X-coordinate','Y-coordinate','time        '/),&
           units=(/'1','1','1'/),                           &
           origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('x',x_X)                                  ! �ѿ�����
    call HistoryAddattr('x','topology','circular')            ! ����°��
    call HistoryAddattr('x','modulo',xmax-xmin)               ! ����°��
    call HistoryPut('y',y_Y)                                  ! �ѿ�����

    call HistoryAddVariable( &                                ! �ѿ����
           varname='zeta', dims=(/'x','y','t'/), & 
           longname='vorticity', units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it
    call HistoryPut('t',real(it*dt))
    call HistoryPut('zeta',transpose(yx_Zeta))
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program esc_diff_c
