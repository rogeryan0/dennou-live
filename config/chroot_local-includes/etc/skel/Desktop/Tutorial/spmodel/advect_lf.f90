!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
! Sample program for gt4_history/gt4f90io and spmodel  2002/01/25 S.Takehiro
!                                                      2004/01/26 M.Odaka
!                                                      2009/01/09 S.Takehiro
!                                                      2010/02/17 S.Takehiro
! 
! Solving advection equation
!     d\zeta/dt  = -c d\zeta/dx 
!
! Time integration is performed by the leap frog scheme. 
!
program advect_lf

  use ae_module
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=128            ! �ʻ���������(X)
  integer, parameter :: km=42             ! �����ȿ�������(X)

 !---- �ѿ� ----
  real(8)            :: g_Zeta(0:im-1)     ! �ʻҥǡ���
  real(8)            :: e_Zeta(-km:km)     ! ���ڥ��ȥ�ǡ���

  real(8)            :: e_Zeta0(-km:km)    ! ���ڥ��ȥ�ǡ���(Leap frog ��)
  real(8)            :: e_Zeta1(-km:km)    ! ���ڥ��ȥ�ǡ���(Leap frog ��)

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin=0.0, xmax=3.0

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=1e-6                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=10000, ndisp=100     ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  real(8), parameter :: c=300.0D0               ! ��ή®��

  real(8), parameter :: X1=(xmax+xmin)/2.0      ! ���ʬ�� X ��ɸ
  real(8), parameter :: U1=720.0                ! ���ʬ�ۤο���

  real(8), parameter :: X2=(xmax+3*xmin)/4.0    ! ���ʬ�� X ��ɸ
  real(8), parameter :: U2=1440.0               ! ���ʬ�ۤο���

  real(8) :: pi
  integer :: it=0                               ! DO �ѿ�

  pi = atan(1.0D0)*4.0D0

  call ae_initial(im,km,xmin,xmax)              ! ���ڥ��ȥ�����

 !------------------- ��������� ----------------------
  g_Zeta= U1*sech((g_X-X1)/sqrt(12/u1))**2 &
         + U2*sech((g_X-X2)/sqrt(12/u2))**2 

  e_Zeta = e_g(g_Zeta)
  e_Zeta1 = e_Zeta ; e_Zeta0 = e_Zeta

  call output_gtool4_init                        ! �ҥ��ȥ꡼�����
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt

     e_Zeta = e_Zeta0 + 2*dt *( -c * e_Dx_e(e_Zeta1) )  ! Leap frog ������ʬ

     e_Zeta0 = e_Zeta1 ; e_Zeta1 = e_Zeta               ! Leap frog ������ʬ

     if(mod(it,ndisp) .eq. 0)then                       ! ����
        g_Zeta = g_e(e_Zeta)
        call output_gtool4
     endif
  enddo

  call output_gtool4_close                           ! �ҥ��ȥ꡼�����
  stop

contains
  function sech(x)
    double precision :: x(0:im-1)
    double precision :: sech(0:im-1)

    sech = 2/(exp(x)+ exp(-x))

  end function sech

  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! �ҥ��ȥ꡼����
           file='advect_lf.nc', &
           title='advection equation model (Leap frog scheme)',  &
           source='Sample program of gtool_history/gtool4',      &
           institution='GFD_Dennou Club spmodel/davis project',  &
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
  end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it
    call HistoryPut('t',real(it*dt))
    call HistoryPut('zeta',g_Zeta)
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program advect_lf
