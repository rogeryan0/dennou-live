!----------------------------------------------------------------------
!  Copyright (c) 2002--2005 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
! Sample program for SPML(based on gt4f90io and ISPACK)
!
!   2002/08/19 S.Takehiro
!   2004/01/26 M.Odaka
!   2004/06/03 M.Odaka
!   2004/06/17 M.Odaka
!   2005/03/16 S.Takehiro
! 
! Solving 2-D Boussinesq fluid system (TT problem)
!     d\zeta/dt + J(\psi,\zeta) = PrRt dT/dx + PrRc dC/dx + Pr\nabla\zeta
!     dT/dt + J(\psi,T) - d\psi/dx = \nabla T
!     dC/dt + J(\psi,C) - d\psi/dx = Tau \nabla C
!     \nabla\psi = \zeta
!     psi = zeta = T = C = 0 at y=0,1
!
  program ddfcnv0

    use esc_module
    use gt4_history
    implicit none

 !---- ���ֲ��������� ----
    integer, parameter :: km=21, lm=21           ! �����ȿ�������(X,Y)
    integer, parameter :: im=64, jm=32           ! �ʻ���������(X,Y)

 !---- �ѿ� ----
    real(8)            :: yx_Psi(0:jm,0:im-1)    ! �ʻҥǡ���(ή��)
    real(8)            :: yx_Temp(0:jm,0:im-1)   ! �ʻҥǡ���(����/���پ���)
    real(8)            :: yx_Comp(0:jm,0:im-1)   ! �ʻҥǡ���(����/��������)
    real(8)            :: yx_Zeta(0:jm,0:im-1)   ! �ʻҥǡ���(����)

    real(8)            :: es_Psi(-km:km,lm)      ! ���ڥ��ȥ�ǡ���(ή��)
    real(8)            :: es_TempA(-km:km,lm)  ! ���ڥ��ȥ�ǡ���(���پ���,t+1)
    real(8)            :: es_CompA(-km:km,lm)  ! ���ڥ��ȥ�ǡ���(��������,t+1)
    real(8)            :: es_ZetaA(-km:km,lm)  ! ���ڥ��ȥ�ǡ���(����,t+1)

    real(8)            :: es_TempB(-km:km,lm)  ! ���ڥ��ȥ�ǡ���(���پ���,t)
    real(8)            :: es_CompB(-km:km,lm)  ! ���ڥ��ȥ�ǡ���(��������,t)
    real(8)            :: es_ZetaB(-km:km,lm)  ! ���ڥ��ȥ�ǡ���(����,t)

 !---- ��ɸ�ѿ��ʤ� ----
    real(8), parameter :: xmin=0.0, xmax=2.0     ! �ΰ��ϰ�(X ����) 
    real(8), parameter :: ymin=0.0, ymax=1.0     ! �ΰ��ϰ�(Y ����) 

 !---- ������ʬ�ѥ�᥿�� ----
    real(8), parameter :: dt=1e-4                ! ���֥��ƥå״ֳ�
    integer, parameter :: nt=5000, ndisp=250    ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
    real(8), parameter :: Ra=-1.2e4              ! ���٥쥤�꡼��
    real(8), parameter :: Rc= 1.0e4              ! �����쥤�꡼��
    real(8), parameter :: Pr=1.0                 ! �ץ��ɥ��
    real(8), parameter :: Tau=0.1                ! �����Ȼ�/Ǯ�Ȼ�����

    integer            :: it                     ! DO �ѿ�

 !---------------- ��ɸ�ͤ����� ---------------------
    call esc_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! ISPACK�����

 !------------------- ��������� ----------------------
    yx_Temp = 0.0 ;  yx_Temp(jm/2,im/2) = 0.01           ! ���پ����
    yx_Comp = 0.0
    yx_Psi  = 0.0
    yx_Zeta = 0.0

    es_Psi  = es_yx(yx_Psi)
    es_TempA = es_yx(yx_Temp) ; es_TempB = es_TempA
    es_CompA = es_yx(yx_Comp) ; es_CompB = es_CompA
    es_ZetaA = es_yx(yx_Zeta) ; es_ZetaB = es_ZetaA

    call output_gtool4_init                              ! �ҥ��ȥ꡼�����
    call output_gtool4                                   ! ����ͽ���

 !------------------- ������ʬ ----------------------
    do it=1,nt
       es_TempA = es_TempB + &
            dt*( -es_Jacobian_es_es(es_Psi,es_TempB) &
                 + es_Dx_es(es_Psi) &
                 + es_Lapla_es(es_TempB) )
       es_CompA = es_CompB + &
            dt*( -es_Jacobian_es_es(es_Psi,es_CompB) &
                 + es_Dx_es(es_Psi) &
                 + Tau*es_Lapla_es(es_CompB) )
       es_ZetaA = es_ZetaB + &
            dt*( - es_Jacobian_es_es(es_Psi,es_ZetaB) &
                 + Pr*Ra*es_Dx_es(es_TempB) &
                 + Pr*Rc*es_Dx_es(es_CompB) &
                 + Pr*es_Lapla_es(es_ZetaB)    )
       es_Psi = es_LaplaInv_es(es_ZetaA)

       es_TempB = es_TempA
       es_CompB = es_CompA
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
           file='ddfcnv_0.nc', title='Double diffusive convection', &
           source='Sample program of spmodel', &
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
           longname='voticity', units='1', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
           varname='temp', dims=(/'x','y','t'/), & 
           longname='temperature', units='1', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
           varname='comp', dims=(/'x','y','t'/), & 
           longname='composition', units='1', xtype='double')

    end subroutine output_gtool4_init

    subroutine output_gtool4
      if ( Ra .ge. 0 )then
         yx_Temp = yx_es(es_TempA) + 1-yx_Y       ! ���Ϥ������پ�
      else
         yx_Temp = yx_es(es_TempA) + yx_Y         ! ���Ϥ������پ�
      endif
      if ( Rc .ge. 0 ) then
         yx_Comp = yx_es(es_CompA) + 1-yx_Y       ! ���Ϥ���������
      else
         yx_Comp = yx_es(es_CompA) -yx_Y          ! ���Ϥ���������
      endif
      yx_Psi  = yx_es(es_Psi)
      yx_Zeta = yx_es(es_ZetaA)

      write(6,*) 'it = ',it
      call HistoryPut('psi',transpose(yx_Psi))
      call HistoryPut('zeta',transpose(yx_Zeta))
      call HistoryPut('temp',transpose(yx_Temp))
      call HistoryPut('comp',transpose(yx_Comp))
    end subroutine output_gtool4

    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

  end program ddfcnv0
