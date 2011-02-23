!----------------------------------------------------------------------
!     Copyright (c) 2006 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  SPMODEL 2 ������ϩ�ΰ�֥��ͥ�����ή��ǥ�
!      �����ӥ����ե��顼����ˡ�ˤ��׻�(���ٸ�����)
!
!      ������ʬ : Euler ˡ
!
! Solving 2-D Diffusioon equation (DD problem)
!     dT/dt = \nabla T
!     T = 0 at y=0,1
!
!����  2002/01/31  �ݹ�����
!
program et_galerkin_DD_euler_diff

  use lumatrix
  use et_module
  use et_galerkin_module
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter  :: im=64, jm=16      ! �ʻ���������(X,Y)
  integer, parameter  :: km=21, lm=10      ! �����ȿ�������(X,Y)

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter  :: xmin=0, xmax=2     ! X ��ɸ�ϰ�
  real(8), parameter  :: ymin=0, ymax=1     ! Y ��ɸ�ϰ�

 !---- �ѿ� ----
  real(8)             :: yx_Temp(0:jm,0:im-1)     ! �ʻҥǡ���(����)
  real(8)             :: ed_Temp(-km:km,2:lm)     ! ���顼����ǡ���(����)

  real(8)             :: yx_TempInit(0:jm,0:im-1) ! �ʻҥǡ���(����, �����)
  real(8)             :: yx_TempSol(0:jm,0:im-1)  ! �ʻҥǡ���(����, ���ϲ�)
  real(8)             :: yx_TempError(0:jm,0:im-1)! �ʻҥǡ���(����, ��)

 !---- ������ʬ�ѥ�᥿�� ----

  real(8), parameter :: dt=1e-4                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=100, ndisp=10        ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  real(8), parameter          :: Kappa=2.0      ! �Ȼ�����

 !---- ����¾ ----
  real(8), parameter  :: pi=3.1415926535897932385D0
  integer :: it=0
  real(8) :: time=0.0D0
  integer :: k, l

 !---------------- ��ɸ�ͤ����� ---------------------
  call et_galerkin_Initial(im,jm,km,lm,xmin,xmax,ymin,ymax,DD=.true.)

 !------------------- ��������� ----------------------
  write(6,*) 'k,l?'
  read(5,*)  k,l

  if ( k .ge. 0 ) then
     yx_Temp = cos(2*k*Pi*yx_X/(xmax-xmin)) * sin(l*Pi*yx_Y/(ymax-ymin))
  else
     yx_Temp = sin(2*k*Pi*yx_X/(xmax-xmin)) * sin(l*Pi*yx_Y/(ymax-ymin))
  endif
  yx_TempInit = yx_Temp
  
  ed_Temp = ed_et(et_yx(yx_Temp))

  call output_gtool4_init                            ! �ҥ��ȥ꡼�����
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt               ! Adams-Bashfort & Crank-Nicolson ˡ�ˤ�������ʬ
     time =it*dt

     ed_Temp = ed_Temp + dt * ed_et(kappa * et_Lapla_et(et_ed(ed_Temp)))

     if(mod(it,ndisp) .eq. 0)then                    ! ����
        call output_gtool4
     endif
  enddo

  call output_gtool4_close                           ! �ҥ��ȥ꡼�����
  stop

  contains

   !------------------- ���� ----------------------
    subroutine output_gtool4_init
      call HistoryCreate( &                               ! �ҥ��ȥ꡼����
           file='et_galerkin_DD_euler_diff.nc', &
           title='Diffusion equation (DD)', &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club davis/spmodel project',&
           dims=(/'x','y','t'/), dimsizes=(/im,jm+1,0/),    &
           longnames=(/'X-coordinate','Y-coordinate','time        '/),&
           units=(/'1','1','1'/),                           &
           origin=0.0, interval=real(ndisp*dt) )

      call HistoryPut('x',x_X)                            ! �ѿ�����
      call HistoryAddattr('x','topology','circular')      ! ����°��
      call HistoryAddattr('x','modulo',xmax-xmin)         ! ����°��
      call HistoryPut('y',y_Y)                            ! �ѿ�����

      call HistoryAddVariable( &                          ! �ѿ����
           varname='temp', dims=(/'x','y','t'/), & 
           longname='temperature', units='1', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
           varname='tempsol', dims=(/'x','y','t'/), & 
           longname='temperature(analytic)', units='1', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
           varname='temperror', dims=(/'x','y','t'/), & 
           longname='temperature(Error)', units='1', xtype='double')

     !---- �¸��ѥ�᥿����°���Ȥ������, ����(Global °��) ----
      call HistoryAddAttr('x','+kappa', kappa )

    end subroutine output_gtool4_init

    subroutine output_gtool4
      yx_Temp = yx_et(et_ed(ed_Temp))

      yx_TempSol = yx_TempInit &
           * exp( - kappa * (  (2*PI*k/(xmax-xmin))**2 &
                              +(PI*l/(ymax-ymin))**2     )*time )

      yx_TempError = yx_Temp-yx_TempSol

      write(6,*) 'it = ',it
      call HistoryPut('t',real(time))
      call HistoryPut('temp',transpose(yx_Temp))
      call HistoryPut('tempsol',transpose(yx_TempSol))
      call HistoryPut('temperror',transpose(yx_TempError))
    end subroutine output_gtool4

    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

end program et_galerkin_DD_euler_diff
