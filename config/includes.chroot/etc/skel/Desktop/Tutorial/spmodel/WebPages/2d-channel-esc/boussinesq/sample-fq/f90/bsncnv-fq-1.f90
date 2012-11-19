!-------------------------------------------------------------------------
!  Copyright (C) 2002--2005 SPMODEL Development Group. All rights reserved.
!-------------------------------------------------------------------------
! Sample program for SPML(based on gt4f90io and ISPACK)
!
!   2002/08/19 S.Takehiro
!   2004/01/26 M.Odaka
!   2004/06/17 M.Odaka
!   2002/03/16 S.Takehiro
! 
! Solving 2-D Boussinesq fluid system (FQ problem)
!     d\zeta/dt + J(\psi,\zeta) = PrRa dT/dx + Pr\nabla\zeta
!     dT/dt + J(\psi,T) - y d\psi/dx = \nabla T
!     \nabla\psi = \zeta
!     psi = zeta = dT/dy = 0 at y=0,1
!
  program bsncnv3

    use esc_module
    use gt4_history
    implicit none

 !---- ���ֲ��������� ----
    integer, parameter :: km=32,  lm=10         ! �����ȿ�������(X,Y)
    integer, parameter :: im=128, jm=16         ! �ʻ���������(X,Y)

 !---- �ѿ� ----
    real(8)            :: yx_Psi(0:jm,0:im-1)   ! �ʻҥǡ���(ή��)          
    real(8)            :: yx_Temp(0:jm,0:im-1)  ! �ʻҥǡ���(����/���پ���) 
    real(8)            :: yx_Zeta(0:jm,0:im-1)  ! �ʻҥǡ���(����)          
                                                                            
    real(8)            :: es_PsiB(-km:km,lm)    ! ���ڥ��ȥ�ǡ���(ή��,t)    
    real(8)            :: ec_TempB(-km:km,0:lm) ! ���ڥ��ȥ�ǡ���(���پ���,t)
    real(8)            :: es_ZetaB(-km:km,lm)   ! ���ڥ��ȥ�ǡ���(����,t)

    real(8)            :: es_PsiA(-km:km,lm)    ! ���ڥ��ȥ�ǡ���(ή��,t+1) 
    real(8)            :: ec_TempA(-km:km,0:lm) !���ڥ��ȥ�ǡ���(���پ���,t+1)
    real(8)            :: es_ZetaA(-km:km,lm)   ! ���ڥ��ȥ�ǡ���(����,t+1)

 !---- ��ɸ�ѿ��ʤ� ----
    real(8), parameter :: xmin=0.0, xmax=8.0    ! �ΰ��ϰ�(X ����) 
    real(8), parameter :: ymin=0.0, ymax=1.0    ! �ΰ��ϰ�(Y ����) 

 !---- ������ʬ�ѥ�᥿�� ----
    real(8), parameter :: dt=1e-3               ! ���֥��ƥå״ֳ�        
    integer, parameter :: nt=20000, ndisp=500  ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
    real(8), parameter :: Ra=1.0e4              ! �쥤�꡼��
    real(8), parameter :: Pr=1.0                ! �ץ��ɥ��

    integer            :: it                    ! DO �ѿ�

 !---------------- ��ɸ�ͤ����� ---------------------
    call esc_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)      ! ISPACK�����

 !------------------- ��������� ----------------------
    yx_Temp = 0.0 ;  yx_Temp(jm/2,im/2) = 0.01             ! ���پ����
    yx_Psi  = 0.0
    yx_Zeta = 0.0

    ec_TempA = ec_yx(yx_Temp) ; ec_TempB = ec_TempA
    es_PsiA  = es_yx(yx_Psi)  ; es_PsiB  = es_PsiA
    es_ZetaA = es_yx(yx_Zeta) ; es_ZetaB = es_ZetaA

    call output_gtool4_init                              ! �ҥ��ȥ꡼�����
    call output_gtool4                                   ! ����ͽ���

 !------------------- ������ʬ ----------------------
    do it=1,nt
       ec_TempA = ec_TempB + &
            dt*( - ec_Jacobian_es_ec(es_PsiB,ec_TempB) &
                 + ec_yx(yx_Y*yx_es(es_Dx_es(es_PsiB))) &
                 + ec_Lapla_ec(ec_TempB) )

       es_ZetaA = es_ZetaB + &
            dt*( - es_Jacobian_es_es(es_PsiB,es_ZetaB) &
                 + Pr*Ra*es_yx(yx_ec(ec_Dx_ec(ec_TempB))) &
                 + Pr*es_Lapla_es(es_ZetaB)    )

       es_PsiA = es_LaplaInv_es(es_ZetaA)

       ec_TempB = ec_TempA 
       es_PsiB  = es_PsiA  
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
           file='bsncnv-fq-1.nc', title='Boussinesq convection (FQ)', &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club davis/spmodel project',&
           dims=(/'x','y','t'/), dimsizes=(/im,jm+1,0/),    &
           longnames=(/'X-coordinate','Y-coordinate','time        '/),&
           units=(/'1','1','1'/),                           &
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
           longname='vorticity', units='1', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
           varname='temp', dims=(/'x','y','t'/), & 
           longname='temperature', units='1', xtype='double')

    end subroutine output_gtool4_init

    subroutine output_gtool4
      yx_Temp = yx_ec(ec_TempA) + (1 - yx_Y**2)/2          ! ���Ϥ������پ�
      yx_Psi  = yx_es(es_PsiA)
      yx_Zeta = yx_es(es_ZetaA)

      write(6,*) 'it = ',it
      call HistoryPut('t',real(it*dt))
      call HistoryPut('psi',transpose(yx_Psi))
      call HistoryPut('zeta',transpose(yx_Zeta))
      call HistoryPut('temp',transpose(yx_Temp))
    end subroutine output_gtool4

    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

  end program bsncnv3
