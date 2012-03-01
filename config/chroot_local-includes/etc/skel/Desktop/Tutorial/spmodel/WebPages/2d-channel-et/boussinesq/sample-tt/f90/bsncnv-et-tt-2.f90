!----------------------------------------------------------------------
!  Copyright (c) 2002--2005 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  SPMODEL 2D-CHANNEL �֥��ͥ�����ή��ǥ�
!
! Solving 2-D Boussinesq fluid system (TT problem)
!     d\zeta/dt + J(\psi,\zeta) = PrRa dT/dx + Pr\nabla\zeta
!     dT/dt + J(\psi,T) - d\psi/dx = \nabla T
!     \nabla\psi = \zeta
!     psi = zeta = T = 0 at y=0,1
!
!����  2002/01/25  �ݹ�����
!      2002/04/15  �ݹ�����
!      2002/08/19  �ݹ�����
!      2004/01/26  ��������: gt4f90io ���б�
!      2005/03/15  �ݹ�����  xy -> yx ���ѹ�
!
program bsncnv1

  use et_module
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter  :: im=128, jm=16      ! �ʻ���������(X,Y)
  integer, parameter  :: km=42, lm=16       ! �����ȿ�������(X,Y)

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter  :: xmin=0, xmax=8     ! X ��ɸ�ϰ�
  real(8), parameter  :: ymin=0, ymax=1     ! Y ��ɸ�ϰ�

 !---- �ѿ� ----
  real(8)             :: yx_psi(0:jm,0:im-1)    ! �ʻҥǡ���(ή��)
  real(8)             :: yx_Temp(0:jm,0:im-1)   ! �ʻҥǡ���(����)
  real(8)             :: yx_Zeta(0:jm,0:im-1)   ! �ʻҥǡ���(����)

  real(8)             :: et_Psi(-km:km,0:lm)    ! ���ڥ��ȥ�ǡ���(ή��)
  real(8)             :: et_Temp(-km:km,0:lm)   ! ���ڥ��ȥ�ǡ���(����)
  real(8)             :: et_Zeta(-km:km,0:lm)   ! ���ڥ��ȥ�ǡ���(����)

  real(8)             :: x_TempBndry(0:im-1,2)  ! ������
  real(8)             :: e_TempBndry(-km:km,2)  ! ������

  real(8)             :: PsiBndry(2)=(/0.0,0.0/)  ! ������

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=1e-5                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=50000, ndisp=2500    ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  character(len=2), parameter :: TempBC='DD'    ! ���ٶ������(DD/DN/ND/NN)
  character(len=2), parameter :: PsiBC='FF'     ! ή�춭�����(RR/RF/FR/FF)
  real(8), parameter :: Ra=1.0e4                ! �쥤�꡼��
  real(8), parameter :: Pr=1.0                  ! �ץ��ɥ��

 !---- ����¾ ----
  real(8), parameter  :: pi=3.1415926535897932385D0
  integer :: it

 !---------------- ��ɸ�ͤ����� ---------------------
  call et_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)

 !------------------- ��������� ----------------------
  yx_Temp = 1 -yx_y
  yx_Temp(jm/2,im/2) = yx_Temp(jm/2,im/2) + 0.01        ! ���پ����
  yx_Psi  = 0.0
  yx_Zeta = 0.0

  et_Temp = et_yx(yx_Temp)
  et_Psi  = et_yx(yx_Psi)
  et_Zeta = et_yx(yx_Zeta)

 !------------------- ���������� ----------------------

  x_TempBndry(:,1) = 0
  x_TempBndry(:,2) = 1
  e_TempBndry(:,1) = e_x(x_TempBndry(:,1))
  e_TempBndry(:,2) = e_x(x_TempBndry(:,2))

  call output_gtool4_init                            ! �ҥ��ȥ꡼�����
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt                                         ! Euler ˡ�ˤ�������ʬ
     et_Temp = et_Temp + &
          dt*( -et_Jacobian_et_et(et_Psi,et_Temp) &
               + et_Lapla_et(et_Temp) )

     call et_Boundaries(et_Temp,e_TempBndry,cond=TempBC)

     et_Zeta = et_Zeta + &
          dt*( - et_Jacobian_et_et(et_Psi,et_Zeta) &
               + Pr*Ra*et_Dx_et(et_Temp) + Pr*et_Lapla_et(et_Zeta)    )

     et_Psi = at_ay(ey_Vor2Strm_ey(ay_at(et_Zeta),PsiBndry,PsiBC))
     et_Zeta = et_Lapla_et(et_Psi)
 
     if(mod(it,ndisp) .eq. 0)then                    ! ����
        yx_Zeta = yx_et(et_Zeta)
        call output_gtool4
     endif
  enddo

  call output_gtool4_close                           ! �ҥ��ȥ꡼�����
  stop

contains

    subroutine output_gtool4_init
      call HistoryCreate( &                               ! �ҥ��ȥ꡼����
           file='bsncnv_1_et_ff.nc', title='Boussinesq convection (TT)', &
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
           varname='psi', dims=(/'x','y','t'/), & 
           longname='stream function', units='1', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
           varname='zeta', dims=(/'x','y','t'/), & 
           longname='voticity', units='1', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
           varname='temp', dims=(/'x','y','t'/), & 
           longname='temperature', units='1', xtype='double')

    end subroutine output_gtool4_init

    subroutine output_gtool4
      yx_Temp = yx_et(et_Temp)
      yx_Psi  = yx_et(et_Psi)
      yx_Zeta = yx_et(et_Zeta)

      write(6,*) 'it = ',it
      call HistoryPut('t',real(it*dt))
      call HistoryPut('psi',transpose(yx_Psi))
      call HistoryPut('zeta',transpose(yx_Zeta))
      call HistoryPut('temp',transpose(yx_Temp))
    end subroutine output_gtool4

    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

end program bsncnv1
