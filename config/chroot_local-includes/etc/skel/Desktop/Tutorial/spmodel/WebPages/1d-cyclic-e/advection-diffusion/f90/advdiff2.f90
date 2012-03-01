!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
! Sample program for gt4_history/gt4f90io and ISPACK   2002/09/03 S.Takehiro
!                                                      2004/01/26 M.Odaka 
! 
! Solving advection-diffusion equation
!     d\zeta/dt  = -u d\zeta/dx + D d^2\zeta/dx^2
!
! Time integration is performed by the Adams-Bashforth scheme (advection)
! and Clank-Nicholson scheme (diffusion). 
!
program advdiff1

  use ae_module
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=64              ! �ʻ���������(X)
  integer, parameter :: km=31              ! �����ȿ�������(X)

 !---- �ѿ� ----
  real(8)            :: g_Zeta(0:im-1)        ! �ʻҥǡ���
  real(8)            :: g_Zeta_exact(0:im-1)  ! ���ϲ�

  real(8)            :: e_Zeta(-km:km)        ! ���ڥ��ȥ�ǡ���(t=t, t+��t)
  real(8)            :: e_Zeta_old(-km:km)    ! ���ڥ��ȥ�ǡ���(t=t-��t)

  real(8)            :: e_Lapla(-km:km)       ! d2/dx^2 �η���

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin=0.0, xmax=5.0

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=1e-3               ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=200, ndisp=10      ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  real(8), parameter :: U = 10.0              ! ��ή®��
  real(8), parameter :: D = 1.0               ! �Ȼ�����

  real(8), parameter :: X0=(xmax+xmin)/4.0      ! ���ʬ�� X ��ɸ
  real(8), parameter :: Zeta0=1.0               ! ���ʬ�ۤο���
  real(8), parameter :: T0=1e-2                 ! ���ʬ�ۤ���

  real(8) :: pi                                 ! �߼�Ψ
  real(8) :: Time                               ! ����
  integer :: it, k                              ! DO �ѿ�

  pi = 4.0D0*atan(1.0D0)

  call ae_Initial(im,km,xmin,xmax)                ! ���ڥ��ȥ�����

  e_Lapla = e_Dx_e(e_Dx_e((/(1.0D0,k=-km,km)/)))

 !------------------- ��������� ----------------------
  g_Zeta= Zeta0 * exp( - (g_X-X0)**2/(4*D*T0) ) / ( 2*sqrt(pi*D*T0) )
  g_Zeta_exact = g_Zeta
  e_Zeta = e_g(g_Zeta)

  it = 0                                             ! �����

  call output_gtool4_init                            ! �ҥ��ȥ꡼�����
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt
     Time = it * dt
     e_Zeta_old = e_Zeta
     e_Zeta = ( e_Zeta - dt * (   3/2.0 * U * e_Dx_e(e_Zeta)        &
                                - 1/2.0 * U * e_Dx_e(e_Zeta_old) )  &
                       + dt * 1/2.0 * D * e_Dx_e(e_Dx_e(e_Zeta)) )  &
              / ( 1 - dt/2.0 * D * e_Lapla )

     if(mod(it,ndisp) .eq. 0)then                    ! ����
        g_Zeta = g_e(e_Zeta)
        g_Zeta_exact = Zeta0 * exp( - (g_X-X0-U*Time)**2/(4*D*(T0+Time)) ) &
                               / ( 2*sqrt(pi*D*(T0+Time)) )
        call output_gtool4
     endif
  enddo

  call output_gtool4_close                           ! �ҥ��ȥ꡼�����
  stop

contains
  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! �ҥ��ȥ꡼����
           file='advdiff2.nc', &
           title='Adv. Diff. equation model (Adams-Bashforth & Crank Nicholson scheme)',&
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club spmodel/davis project',     &
           dims=(/'x','t'/), dimsizes=(/im,0/),             &
           longnames=(/'X-coordinate','time        '/),     &
           units=(/'1','1'/),                               &
           origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('x',g_X)                                  ! �ѿ�����
    call HistoryAddattr('x','topology','circular')            ! ����°��
    call HistoryAddattr('x','modulo',xmax-xmin)               ! ����°��

    call HistoryAddVariable( &                                ! �ѿ����
           varname='zeta', dims=(/'x','t'/), & 
           longname='displacement', units='1', xtype='double')

    call HistoryAddVariable( &                                ! �ѿ����
           varname='zeta_exact', dims=(/'x','t'/), & 
           longname='displacement(exact)', units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it
    call HistoryPut('t',real(Time))
    call HistoryPut('zeta',g_Zeta)
    call HistoryPut('zeta_exact',g_Zeta_exact)
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program advdiff1
