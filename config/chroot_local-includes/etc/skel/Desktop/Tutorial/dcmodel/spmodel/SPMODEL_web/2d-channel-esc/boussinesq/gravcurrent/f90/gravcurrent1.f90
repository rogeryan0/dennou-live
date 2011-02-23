!--------------------------------------------------------------------------
! Copyright (C) 2002--2005 SPMODEL Development Group. All rights reserved.
!--------------------------------------------------------------------------
! Sample program for SPML(based on gt4f90io and ISPACK)  
!
!    2002/09/25 S.Takehiro
!    2004/01/26 M.Odaka
!    2004/06/17 M.Odaka
!    2005/03/25 S.Takehiro
! 
! Solving 2-D Boussinesq fluid system 
!     d\zeta/dt 
!        + + J(\psi,\zeta) + g/\rho_0 d\rho/dx - nu\lapla\zeta = 0,
!
!     \nabla\psi = \zeta
!
!     d\rho/dt + J(\psi,\rho) = D\nabla\rho
!
!     psi = zeta = d\rho/dy = 0 at y=0,1
!
  program gravcurrent1

    use esc_module
    use gt4_history
    implicit none

 !---- ���ֲ��������� ----
    integer, parameter :: km=42 , lm=42         ! �����ȿ�������(X,Y)
    integer, parameter :: im=128, jm=64         ! �ʻ���������(X,Y)
!!$    integer, parameter :: km=85 , lm=85          ! �����ȿ�������(X,Y)
!!$    integer, parameter :: im=256, jm=128         ! �ʻ���������(X,Y)

 !---- �ѿ� ----
    real(8)            :: yx_Psi(0:jm,0:im-1)     ! �ʻҥǡ���(ή��)
    real(8)            :: yx_Zeta(0:jm,0:im-1)    ! �ʻҥǡ���(����)
    real(8)            :: yx_Rho(0:jm,0:im-1)     ! �ʻҥǡ���(̩��)

    real(8)            :: es_Psi(-km:km,lm)       ! ���ڥ��ȥ�ǡ���(ή��)
    real(8)            :: es_ZetaA(-km:km,lm)     ! ���ڥ��ȥ�ǡ���(����,t+1)
    real(8)            :: ec_RhoA(-km:km,0:lm)    ! ���ڥ��ȥ�ǡ���(̩��,t+1)

    real(8)            :: es_ZetaB(-km:km,lm)     ! ���ڥ��ȥ�ǡ���(����,t)
    real(8)            :: ec_RhoB(-km:km,0:lm)    ! ���ڥ��ȥ�ǡ���(̩��,t)

 !---- ��ɸ�ѿ��ʤ� ----
    real(8), parameter :: xmin=0.0, xmax=120.0    ! �ΰ��ϰ�(X ����) 
    real(8), parameter :: ymin=0.0, ymax=20.0     ! �ΰ��ϰ�(Y ����) 

 !---- ������ʬ�ѥ�᥿�� ----
    real(8), parameter :: dt=1e-4                  ! ���֥��ƥå״ֳ�
    integer, parameter :: nt=300000, ndisp=10000   ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
    real(8), parameter :: Grav=9.8d2               ! ���ϲ�®��
    real(8), parameter :: Visc=1.0d-1              ! ưǴ������
    real(8), parameter :: Diff=1.0d-5              ! �Ȼ�����

    real(8), parameter :: Rho0     = 1.0d0         ! ʿ��̩��
    real(8), parameter :: deltaRho = 0.001         ! ̩�ٺ�
    real(8), parameter :: Ax0   = 10.0             ! ̩�پ�����礭��(X)
    real(8), parameter :: Sigma = 2.0              ! ̩�پ���������ؤ���

    integer            :: it                       ! DO �ѿ�
    real(8)            :: X1, X2

 !---------------- ��ɸ�ͤ����� ---------------------
    call esc_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)      ! ISPACK�����

 !------------------- ��������� ----------------------

   ! ή®ʬ��
    yx_Psi = 0.0  ; es_Psi   = es_yx(yx_Psi)
    yx_Zeta = 0.0 ; es_ZetaA  = es_yx(yx_Zeta) ; es_ZetaB = es_ZetaA

   ! ̩��ʬ��
    X1 = (xmin+xmax)/2 - Ax0
    X2 = (xmin+xmax)/2 + Ax0
    yx_Rho    =  Rho0  + deltaRho * (   tanh((yx_X-X1)/Sigma) &
                                      - tanh((yx_X-X2)/Sigma) )/2
    ec_RhoA    = ec_yx(yx_Rho) ; ec_RhoB = ec_RhoA

    call output_gtool4_init                              ! �ҥ��ȥ꡼�����
    call output_gtool4                                   ! ����ͽ���

 !------------------- ������ʬ ----------------------
    do it=1,nt
       ec_RhoA = ec_RhoB + &
            dt*( - ec_Jacobian_es_ec(es_Psi,ec_RhoB) &
                 + Diff * ec_Lapla_ec(ec_RhoB) )

       es_ZetaA = es_ZetaB +                               &
            dt*( - es_Jacobian_es_es(es_Psi,es_ZetaB)      &
                 - Grav/Rho0 * es_yx(yx_ec(ec_Dx_ec(ec_RhoB))) &
                 + Visc * es_lapla_es(es_zetaB) )

       es_Psi = es_LaplaInv_es(es_ZetaA)

       es_ZetaB = es_ZetaA
       ec_RhoB  = ec_RhoA

       if(mod(it,ndisp) .eq. 0)then                        ! ����
          call output_gtool4
       endif
    enddo

    call output_gtool4_close
    stop

  contains
    subroutine output_gtool4_init
      call HistoryCreate( &                               ! �ҥ��ȥ꡼����
           file='gravcurrent1.nc', title='Gravity current', &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club davis/spmodel project',&
           dims=(/'x','y','t'/), dimsizes=(/im,jm+1,0/),    &
           longnames=(/'X-coordinate','Y-coordinate','time        '/),&
           units=(/'cm','cm','s '/),                           &
           origin=0.0, interval=real(ndisp*dt) )

      call HistoryPut('x',x_X)                                   ! �ѿ�����
      call HistoryAddattr('x','topology','circular')             ! ����°��
      call HistoryAddattr('x','modulo',xmax-xmin)                ! ����°��
      call HistoryPut('y',y_Y)                                   ! �ѿ�����

      call HistoryAddVariable( &                          ! �ѿ����
           varname='psi', dims=(/'x','y','t'/), & 
           longname='stream function', units='1', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
           varname='zeta', dims=(/'x','y','t'/), & 
           longname='voticity', units='1', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
           varname='rho', dims=(/'x','y','t'/), & 
           longname='density', units='g/cm3', xtype='double')
    end subroutine output_gtool4_init

    subroutine output_gtool4
       yx_Psi  = yx_es(es_Psi)
       yx_Zeta = yx_es(es_ZetaA)
       yx_Rho = yx_ec(ec_RhoA)

      write(6,*) 'it = ',it
      call HistoryPut('t',dt*it)
      call HistoryPut('psi',transpose(yx_Psi))
      call HistoryPut('zeta',transpose(yx_Zeta))
      call HistoryPut('rho',transpose(yx_Rho))
    end subroutine output_gtool4

    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

  end program gravcurrent1
