!--------------------------------------------------------------------------
!  Copyright (C) 2002--2005 SPMODEL Development Group. All rights reserved.
!--------------------------------------------------------------------------
! Sample program for SPML(based on gt4f90io and ISPACK)
!
!    2002/09/24 S.Takehiro
!    2004/01/26 M.Odaka
!    2004/06/17 M.Odaka
!    2005/03/16 S.Takehiro
! 
! Solving 2-D Boussinesq fluid system 
!     d\zeta/dt 
!        + Ud\zeta/dx - dU^2/dy^2 dpsi/dx + J(\psi,\zeta) 
!        + g/\rho_0 d\rho/dx - nu\lapla\zeta = 0,
!
!     \nabla\psi = \zeta
!
!     d\rho/dt + Ud\rho/dx + J(\psi,\rho) = D\nabla\rho
!
!     psi = zeta = 0 at y=0,1
!
! where U(y) is basic flow.
!
  program kh1

    use esc_module
    use gt4_history
    implicit none

 !---- ���ֲ��������� ----
    integer, parameter :: km=42 , lm=42         ! �����ȿ�������(X,Y)
    integer, parameter :: im=128, jm=64         ! �ʻ���������(X,Y)

 !---- �ѿ� ----
    real(8)            :: yx_Psi(0:jm,0:im-1)     ! �ʻҥǡ���(ή��)
    real(8)            :: yx_Zeta(0:jm,0:im-1)    ! �ʻҥǡ���(����)
    real(8)            :: yx_Rho(0:jm,0:im-1)     ! �ʻҥǡ���(̩��)
    real(8)            :: yx_U(0:jm,0:im-1)       ! �ʻҥǡ���(����ή)
    real(8)            :: yx_dU2dy2(0:jm,0:im-1)  ! �ʻҥǡ���(����ή)

    real(8)            :: es_PsiA(-km:km,lm)      ! ���ڥ��ȥ�ǡ���(ή��,t+1)
    real(8)            :: es_ZetaA(-km:km,lm)     ! ���ڥ��ȥ�ǡ���(����,t+1)
    real(8)            :: ec_RhoA(-km:km,0:lm)    ! ���ڥ��ȥ�ǡ���(̩��,t+1)

    real(8)            :: es_PsiB(-km:km,lm)      ! ���ڥ��ȥ�ǡ���(ή��,t)
    real(8)            :: es_ZetaB(-km:km,lm)     ! ���ڥ��ȥ�ǡ���(����,t)
    real(8)            :: ec_RhoB(-km:km,0:lm)    ! ���ڥ��ȥ�ǡ���(̩��,t)

 !---- ��ɸ�ѿ��ʤ� ----
    real(8), parameter :: xmin=0.0, xmax=18.0     ! �ΰ��ϰ�(X ����) 
    real(8), parameter :: ymin=0.0, ymax=6.0      ! �ΰ��ϰ�(Y ����) 

 !---- ������ʬ�ѥ�᥿�� ----
    real(8), parameter :: dt=0.25d-4               ! ���֥��ƥå״ֳ�
    integer, parameter :: nt=40000, ndisp=800     ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
    real(8), parameter :: Grav=9.8d2               ! ���ϲ�®��
    real(8), parameter :: Visc=1.0d-1              ! ưǴ������
    real(8), parameter :: Diff=1.0d-5              ! �Ȼ�����

    real(8), parameter :: U0 = 3.0d1               ! ����ή��ʬ��(®�ٺ�)
    real(8), parameter :: A0 = 0.2                 ! ����ή��ʬ��(�������ؤ���)
    real(8), parameter :: EPS= 1e-6                ! ��������®�ٿ���

    real(8), parameter :: deltaRho = 0.05          ! ̩�ٺ�
    real(8), parameter :: Rho0     = 1.0d0         ! ���ؤ�̩��

 !--- ĶǴ�� ---
    real(8), parameter :: nu=1e-10                 ! Ǵ��������factor
    integer, parameter :: nv=5                     ! Ǵ���γ���
    real(8)            :: es_sVisc(-km:km,lm)      ! ���ڥ��ȥ�ǡ���(Ǵ������)
    real(8)            :: ec_sDiff(-km:km,0:lm)    ! ���ڥ��ȥ�ǡ���(Ǵ������)

    real(8), parameter :: pi = 3.141592653589793   ! �߼�Ψ

    integer            :: i, j, k, l, it           ! DO �ѿ�
    real               :: rnum

 !---------------- ��ɸ�ͤ����� ---------------------
    call esc_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)      ! ISPACK�����

 !------------------- ��������� ----------------------

   ! ή®ʬ��
    yx_U      = U0 * tanh((yx_Y-(ymin+ymax)/2)/A0)
    !yx_dU2dY2 = yx_ec(ec_dy_es(es_dy_ec(ec_yx(yx_U))))     ! ������ dU0/dy=0
    yx_dU2dY2 = -2*U0/A0**2 * sinh((yx_Y-(ymin+ymax)/2)/A0) &
	                    / cosh((yx_Y-(ymin+ymax)/2)/A0)**3 

    !yx_Psi = EPS * cos(2*pi/(xmax-xmin)*yx_X) &
    !             * cos(2*pi/(ymax-ymin)*yx_Y)
    !yx_Psi = 0.0 ; yx_Psi(jm/2,im/2) = EPS
    do j=0,jm
	do i=0,im-1
	   call RANDOM_NUMBER(rnum)
	   yx_Psi(j,i)=EPS * rnum
	enddo
    enddo

    es_PsiA  = es_yx(yx_Psi)        ;  yx_Psi = yx_es(es_PsiA) 
    es_ZetaA = es_Lapla_es(es_PsiA) ;  yx_Zeta  = yx_es(es_ZetaA)


   ! ̩��ʬ��
    yx_Rho    = Rho0 - deltaRho * (tanh((yx_Y-(ymin+ymax)/2)/A0) + 1)/2
    ec_RhoA    = ec_yx(yx_Rho)

    es_ZetaB = es_ZetaA
    es_PsiB  = es_PsiA
    ec_RhoB  = ec_RhoA

    call output_gtool4_init                              ! �ҥ��ȥ꡼�����
    call output_gtool4                                   ! ����ͽ���

 !---------------------- ĶǴ������ ----------------------
    do l=1,lm
       do k=-km,km
          es_sVisc(k,l)= Visc * nu &
                    * ((2*pi*k/(xmax-xmin))**2+(2*pi*l/(ymax-ymin))**2)**nv
       enddo
    enddo

    do l=0,lm
       do k=-km,km
          ec_sDIff(k,l)= Diff * nu &
                     * ((2*pi*k/(xmax-xmin))**2+(2*pi*l/(ymax-ymin))**2)**nv
       enddo
    enddo

 !---------------------- ������ʬ ----------------------
    do it=1,nt
       ec_RhoA = ( ec_RhoB + &
                  dt*( - ec_Jacobian_es_ec(es_PsiB,ec_RhoB)       &
                       - ec_yx(yx_U*yx_ec(ec_dx_ec(ec_RhoB)))    &
                       + Diff * ec_Lapla_ec(ec_RhoB) )           &
                 )/(1+ec_sDiff*dt)

       es_ZetaA = ( es_ZetaB +                                 &
                    dt*( - es_Jacobian_es_es(es_PsiB,es_ZetaB)       &
                         - es_yx(yx_U*yx_es(es_dx_es(es_ZetaB)))    &
                         + es_yx(yx_dU2dy2*yx_es(es_dx_es(es_PsiB)))&
                         - Grav/Rho0 * es_yx(yx_ec(ec_Dx_ec(ec_RhoB))) &
                         + Visc * es_lapla_es(es_ZetaB) )           &
                  )/(1+es_sVisc*dt)

       es_PsiA = es_LaplaInv_es(es_ZetaA)

       es_ZetaB = es_ZetaA
       es_PsiB  = es_PsiA
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
           file='kh1.nc', title='K-H instability', &
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

      call HistoryAddVariable( &                          ! �ѿ����
           varname='u0', dims=(/'y'/), & 
           longname='basic flow', units='1', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
           varname='d2u0dy2', dims=(/'y'/), & 
           longname='d2U0/dy', units='1', xtype='double')
      call HistoryPut('u0',yx_U(:,1))
      call HistoryPut('d2u0dy2',yx_dU2dy2(:,1))
    end subroutine output_gtool4_init

    subroutine output_gtool4
       yx_Psi  = yx_es(es_PsiA)
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

  end program kh1
