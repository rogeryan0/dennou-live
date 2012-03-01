!--------------------------------------------------------------------------
!  Copyright (C) 2002--2005 SPMODEL Development Group. All rights reserved.
!-------------------------------------------------------------------------
! Sample program for SPML(based on g4f90io and ISPACK)   
!
!    2002/09/19 S.Takehiro
!    2004/01/26 M.Odaka
!    2004/06/17 M.Odaka
!    2005/03/16 S.Takehiro
! 
! Solving 2-D Boussinesq fluid system
!     d\zeta/dt + J(\psi,\zeta) = -g/\rho_0 d\rho/dx + \nu\nabla\zeta + F_\zeta
!     d\rho/dt + J(\psi,\rho) + d\rho_0/dz d\psi/dx = D\nabla\rho
!     \nabla\psi = \zeta
!     psi = zeta = d\rho/dy = 0 at y=0,1
!
  program igwave1

    use esc_module
    use gt4_history
    implicit none

 !---- ���ֲ��������� ----
    integer, parameter :: km=42,  lm=42          ! �����ȿ�������(X,Y)
    integer, parameter :: im=128, jm=64          ! �ʻ���������(X,Y)

 !---- �ѿ� ----
    real(8)            :: yx_Psi(0:jm,0:im-1)   ! �ʻҥǡ���(ή��)
    real(8)            :: yx_Rho(0:jm,0:im-1)   ! �ʻҥǡ���(̩�پ���)
    real(8)            :: yx_Zeta(0:jm,0:im-1)  ! �ʻҥǡ���(����)

    real(8)            :: yx_ZetaSrc(0:jm,0:im-1)  ! �ʻҥǡ���(���ٸ�)

    real(8)            :: es_Psi(-km:km,lm)
    real(8)            :: ec_RhoA(-km:km,0:lm)  !���ڥ��ȥ�ǡ���(̩�پ���,t+1)
    real(8)            :: es_ZetaA(-km:km,lm)   ! ���ڥ��ȥ�ǡ���(����,t+1)

    real(8)            :: ec_RhoB(-km:km,0:lm)  ! ���ڥ��ȥ�ǡ���(̩�پ���,t)
    real(8)            :: es_ZetaB(-km:km,lm)   ! ���ڥ��ȥ�ǡ���(����,t)

 !---- ��ɸ�ѿ��ʤ� ----
    real(8), parameter :: xmin=0.0, xmax=2.0    ! �ΰ��ϰ�(X ����) 
    real(8), parameter :: ymin=0.0, ymax=1.0    ! �ΰ��ϰ�(Y ����) 

 !---- ������ʬ�ѥ�᥿�� ----
    real(8)            :: time                  ! ����
    real(8), parameter :: dt=1e-1               ! ���֥��ƥå״ֳ�
    integer, parameter :: nt=4000, ndisp=50     ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
    real(8), parameter :: Grav=9.8d0              ! ���ϲ�®��

    real(8), parameter :: Visc=1.0d-5             ! ưǴ������
    real(8), parameter :: Diff=1.0d-9             ! �Ȼ�����

 !---- �¸�����ѥ�᥿�� ----
    real(8), parameter :: Rho0   = 1.0d3          ! �����̩������
    real(8), parameter :: DRho0dz=-1.0d0          ! �����̩������
    real(8), parameter :: A0=1.0d-5               ! ���ٸ��ο���
    real(8), parameter :: Sigma=2.0d-2            ! ���ٸ����礭��
    real(8), parameter :: X0 = (xmin+xmax)/2      ! ���ٸ��ΰ���(X)
    real(8), parameter :: Y0 = (ymin+ymax)/2      ! ���ٸ��ΰ���(Y)

   ! real(8), parameter :: Omega=0.3               ! ���ٸ��ο�ư��(BV �Ȥ���)
    real(8)            :: Omega=0.3               ! ���ٸ��ο�ư��(BV �Ȥ���)
    real(8)            :: Brunt                   ! BV ��ư��

    integer            :: it                      ! DO �ѿ�

    Brunt=sqrt(Grav*abs(Drho0Dz)/Rho0)            ! BV ��ư��

    write(6,*) 'Freq. of forcing/Brunt Vaisala freq. ?'
    read(5,*) Omega

 !---------------- ��ɸ�ͤ����� ---------------------
    call esc_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)      ! ISPACK�����

 !------------------- ��������� ----------------------
    !yx_Rho  = Rho0 + DRho0dz * yx_Y                        ! ̩������

    yx_Rho  = 0.0
    yx_Psi  = 0.0
    yx_Zeta = 0.0

    ec_RhoA  = ec_yx(yx_Rho)  ; ec_RhoB  = ec_RhoA
    es_ZetaA = es_yx(yx_Zeta) ; es_ZetaB = es_ZetaA
    es_Psi   = es_yx(yx_Psi) 


    call output_gtool4_init                              ! �ҥ��ȥ꡼�����
    call output_gtool4                                   ! ����ͽ���

 !------------------- ������ʬ ----------------------
    do it=1,nt
       time = it*dt

       yx_ZetaSrc = yx_es(es_dx_es(es_yx( &
                     A0 * exp(-((yx_X-X0)**2 + (yx_Y-Y0)**2)/Sigma**2) &
                        * sin(Omega*Brunt*time))))

       ec_RhoA = ec_RhoB + &
            dt*( - ec_Jacobian_es_ec(es_Psi,ec_RhoB) &
                 - Drho0dz * ec_yx(yx_es(es_dx_es(es_Psi))) &
                 + Diff * ec_Lapla_ec(ec_RhoB) )

       es_ZetaA = es_ZetaB + &
            dt*( - es_Jacobian_es_es(es_Psi,es_ZetaB) &
                 - Grav/Rho0 * es_yx(yx_ec(ec_Dx_ec(ec_RhoB))) &
                 + Visc * es_Lapla_es(es_ZetaB) &
                 + es_yx(yx_ZetaSrc) )

       es_Psi = es_LaplaInv_es(es_ZetaA)

       ec_RhoB  = ec_RhoA
       es_ZetaB = es_ZetaA

       if(mod(it,ndisp) .eq. 0)then                        ! ����
          call output_gtool4
       endif
    enddo

    call output_gtool4_close
    stop

  contains
    subroutine output_gtool4_init
      call HistoryCreate( &                               ! �ҥ��ȥ꡼����
           file='igwave1.nc', title='Internal gravity waves', &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club davis/spmodel project',&
           dims=(/'x','y','t'/), dimsizes=(/im,jm+1,0/),    &
           longnames=(/'X-coordinate','Y-coordinate','time        '/),&
           units=(/'m','m','s'/),                           &
           origin=0.0, interval=real(ndisp*dt) )

      call HistoryPut('x',x_X)                                   ! �ѿ�����
      call HistoryAddattr('x','topology','circular')             ! ����°��
      call HistoryAddattr('x','modulo',xmax-xmin)                ! ����°��
      call HistoryPut('y',y_Y)                                   ! �ѿ�����

      call HistoryAddVariable( &                          ! �ѿ����
           varname='psi', dims=(/'x','y','t'/), & 
           longname='stream function', units='m2/s', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
           varname='zeta', dims=(/'x','y','t'/), & 
           longname='vorticity', units='1/s', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
           varname='rho', dims=(/'x','y','t'/), & 
           longname='density', units='kg/m3', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
           varname='zetasrc', dims=(/'x','y','t'/), & 
           longname='vorticity forcing', units='1/s2', xtype='double')
    end subroutine output_gtool4_init

    subroutine output_gtool4
      yx_Rho = yx_ec(ec_RhoA)
      yx_Psi  = yx_es(es_Psi)
      yx_Zeta = yx_es(es_ZetaA)

      write(6,*) 'it = ',it
      call HistoryPut('t',real(time))
      call HistoryPut('psi',transpose(yx_Psi))
      call HistoryPut('zeta',transpose(yx_Zeta))
      call HistoryPut('rho',transpose(yx_Rho))
      call HistoryPut('zetasrc',transpose(yx_ZetaSrc))
    end subroutine output_gtool4

    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

  end program igwave1
