!----------------------------------------------------------------------
!   Copyright (c) 2002--2005 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  at_module
!      1 �����ΰ� �Ȼ�����
!           d\zeta/dt  = \nu\nabla^2\zeta
!
!����  2002/08/20  �ݹ�����
!      2004/02/15  ������ ��ʿ gt4f90io �λ��Ѥ�ȼ���饤�֥��̾�ѹ�
!      2004/11/27  �ݹ�����  ���������������ΥХ�����
!      2005/03/16  �ݹ�����
!      2005/06/03  ������ ��ʿ it=0 ������Ū�����
!
program at_diff1

  use gt4_history
  use at_module
  use lumatrix

  implicit none
  integer, parameter :: im=8, km=5, nm=4
  real(8), parameter :: xmin=-1.0D0, xmax=1.0D0

  real(8), dimension(0:im) :: g_Temp
  real(8), dimension(0:im) :: g_Temp_Right
  real(8), dimension(0:km) :: t_Temp
  real(8), parameter       :: Kappa = 1.0

  logical, parameter       :: tfix_top    = .false.        ! ��ü�������
  logical, parameter       :: tfix_bottom = .false.        ! ��ü�������
  real(8), parameter       :: tvalue_top    = 0.0
  real(8), parameter       :: tvalue_bottom = 0.0
  real(8), parameter       :: dt=1e-2
  integer, parameter       :: nt=100
  integer, parameter       :: ndisp=10

  real(8), parameter       :: pi=3.1415926535897932385D0
  integer  :: it=0

  call at_Initial(im,km,xmin,xmax)
  g_Temp = sin(pi*g_X/2)
  g_Temp_Right = g_Temp

  t_Temp = t_g(g_Temp)

  call output_gtool4_init
  call output_gtool4

  do it=1,nt
     t_Temp = t_Temp + Kappa * t_Dx_t(t_Dx_t(t_Temp)) * dt

     if ( (tfix_bottom .eqv. .false.).AND.(tfix_top .eqv. .false.) )then
        call at_boundaries_NN(t_Temp,(/tvalue_top,tvalue_bottom/))
     elseif ( (tfix_bottom .eqv. .true.).AND.(tfix_top .eqv. .false.) )then
        call at_boundaries_ND(t_Temp,(/tvalue_top,tvalue_bottom/))
     elseif ( (tfix_bottom .eqv. .false.).AND.(tfix_top .eqv. .true.) )then
        call at_boundaries_DN(t_Temp,(/tvalue_top,tvalue_bottom/))
     else
        call at_boundaries_DD(t_Temp,(/tvalue_top,tvalue_bottom/))
     endif

     if ( mod(it,ndisp) == 0 ) then
        g_Temp_Right = sin(pi*g_X/2)*exp(-Kappa*pi**2/4*it*dt)
        g_Temp=g_t(t_Temp)
        call output_gtool4
     endif
  enddo

  call output_gtool4_close
  stop

contains

  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! �ҥ��ȥ꡼����
           file='at_diff1.nc', title='1D diffusion model', &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club davis project',     &
           dims=(/'x','t'/), dimsizes=(/im+1,0/),      &
           longnames=(/'X-coordinate','time        '/),&
           units=(/'1','1'/),                          &
           origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('x',g_X)                                 ! �ѿ�����

    call HistoryAddVariable( &                                ! �ѿ����
           varname='temp', dims=(/'x','t'/), & 
           longname='temperature', units='1', xtype='double')

    call HistoryAddVariable( &                                ! �ѿ����
           varname='temp_right', dims=(/'x','t'/), & 
           longname='temperature', units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it
    call HistoryPut('t',real(dt*it))
    call HistoryPut('temp',g_Temp)
    call HistoryPut('temp_right',g_Temp_Right)
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program at_diff1

