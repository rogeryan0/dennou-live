!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  et_module
!      2 ������ϩ�ΰ� �Ȼ�����
!           d\zeta/dt  = \nu\nabla^2\zeta
!
!����  2002/08/20  �ݹ�����
!      2004/02/15  ������ ��ʿ gt4f90io �λ��Ѥ�ȼ���饤�֥��̾�ѹ�
!      2005/06/03  ������ ��ʿ it=0 ������Ū�����
!
program et_diff1

  use et_module
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter  :: im=32, jm=16      ! �ʻ���������(X,Y)
  integer, parameter  :: km=10, lm=16      ! �����ȿ�������(X,Y)

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter  :: xmin=0, xmax=1     ! X ��ɸ�ϰ�
  real(8), parameter  :: ymin=0, ymax=1     ! Y ��ɸ�ϰ�

 !---- �ѿ� ----
  real(8)             :: xy_Temp(0:jm,0:im-1)   ! �ʻҥǡ���(����/���پ���)
  real(8)             :: et_Temp(-km:km,0:lm)   ! ���ڥ��ȥ�ǡ���(����)

  real(8)             :: x_TempBndry(0:im-1,2)  ! ������
  real(8)             :: e_TempBndry(-km:km,2)  ! ������

 !---- ������ʬ�ѥ�᥿�� ----

  real(8), parameter :: dt=2e-5                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=5000, ndisp=500      ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  character(len=2), parameter :: Tempbc='DD'    ! ���ٶ������(DD/DN/ND/NN)
  real(8), parameter :: Kappa=1.0               ! �Ȼ�����
  real(8), parameter :: X1=(xmin+xmax)/2.0      ! ���ʬ�� X ��ɸ
  real(8), parameter :: Y1=(ymin+ymax)/2.0      ! ���ʬ�� Y ��ɸ
  real(8), parameter :: Sigma=0.1               ! ���ʬ�ۤ��礭��

 !---- ����¾ ----
  integer :: it=0

 !---------------- ��ɸ�ͤ����� ---------------------
  call et_Initial(im,jm,km,lm,xmin,xmax,ymin,ymax)

 !------------------- ��������� ----------------------
  xy_Temp=  exp(-((yx_X-X1)**2+(yx_Y-Y1)**2)/(2*Sigma**2))
  et_Temp = et_yx(xy_Temp)

 !------------------- ���������� ----------------------
  x_TempBndry(:,1) = 0.0
  x_TempBndry(:,2) = 0.0                            ! ��ʿ���ͤʶ���
  !x_TempBndry(:,2) = sin(2*pi*x_X)                   ! ��ʿ���ٺ��Τ��붭��
  e_TempBndry(:,1) = e_x(x_TempBndry(:,1))
  e_TempBndry(:,2) = e_x(x_TempBndry(:,2))

  call output_gtool4_init                            ! �ҥ��ȥ꡼�����
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt                                         ! Euler ˡ�ˤ�������ʬ
     et_Temp = et_Temp + dt * Kappa * et_Lapla_et(et_Temp)

     call et_Boundaries(et_Temp,e_TempBndry,cond=Tempbc)

     if(mod(it,ndisp) .eq. 0)then                    ! ����
        call output_gtool4
     endif
  enddo

  call output_gtool4_close                           ! �ҥ��ȥ꡼�����
  stop

contains

    subroutine output_gtool4_init
      call HistoryCreate( &                               ! �ҥ��ȥ꡼����
           file='et_diff1.nc',  title='2D diffusion model',   &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club spmodel project',     &
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

    end subroutine output_gtool4_init

    subroutine output_gtool4
      xy_Temp = yx_et(et_Temp)

      write(6,*) 'it = ',it
      call HistoryPut('t',real(it*dt))
      call HistoryPut('temp',transpose(xy_Temp))
    end subroutine output_gtool4

    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

  end program et_diff1

