!----------------------------------------------------------------------
!     Copyright (c) 2004--2005 YAMADA YUKIKO. All rights reserved.
!----------------------------------------------------------------------
! Sample program for SPML(based on gt4f90io and ISPACK)
!
! 
! Solving equatorial beta plane non-linear shallow water equations
!
! history: 2003/06/02 YAMADA, Y. 
!          2004/08/26 YAMADA, Y. 
!          2005/03/16 Takehiro, S.
!

  program equatorial_beta_nonlinear_shallow_water
    use esc_module
    use gt4_history
    use gt4f90io
    implicit none

 !---- ���ֲ��������� ----
    integer, parameter :: km=16,   lm=16          ! �����ȿ�������(X,Y)
    integer, parameter :: im=64,   jm=32          ! �ʻ���������(X,Y)

 !---- �ѿ� ----
    real(8)            :: yx_U(0:jm,0:im-1)        ! �ʻҥǡ���(U(t+��t))
    real(8)            :: yx_V(0:jm,0:im-1)        ! �ʻҥǡ���(V(t+��t))
    real(8)            :: yx_H(0:jm,0:im-1)        ! �ʻҥǡ���(H(t+��t))
    real(8)            :: ec_U_A(-km:km,0:lm)      ! ���ڥ��ȥ�(U(t+��t))
    real(8)            :: es_V_A(-km:km,lm)        ! ���ڥ��ȥ�(V(t+��t))
    real(8)            :: ec_H_A(-km:km,0:lm)      ! ���ڥ��ȥ�(H(t+��t))
    real(8)            :: ec_U(-km:km,0:lm)     ! ���ڥ��ȥ�(U(t))
    real(8)            :: es_V(-km:km,lm)       ! ���ڥ��ȥ�(V(t))
    real(8)            :: ec_H(-km:km,0:lm)     ! ���ڥ��ȥ�(H(t))
    real(8)            :: ec_U_B(-km:km,0:lm)   ! ���ڥ��ȥ�(U(t-��t))
    real(8)            :: es_V_B(-km:km,lm)     ! ���ڥ��ȥ�(V(t-��t))
    real(8)            :: ec_H_B(-km:km,0:lm)   ! ���ڥ��ȥ�(H(t-��t))
    real(8)            :: yx_QH(0:jm,0:im-1)     ! �ʻҥǡ���(Ǯ��ʬ��)
    real(8)            :: ec_QH(-km:km,0:lm)     ! ���ڥ��ȥ�(Ǯ��ʬ��)

 !---- ��ɸ�ѿ��ʤ� ----
    real(8), parameter :: xmin=-6.0, xmax=12.0  ! �ΰ��ϰ�(X ����)
    real(8), parameter :: ymin=-4.5, ymax=4.5   ! �ΰ��ϰ�(Y ����) 

 !---- ʪ���ѥ�᥿�� ----
    real(8), parameter :: Grav=1.0              ! ���ϲ�®��
    real(8), parameter :: Beta=1.0              ! ��ƻ�¥ѥ�᥿
    real(8), parameter :: H0 = 1.0              ! ʿ�Ѥο���
    real(8), parameter :: Rm = 0.1              ! �쥤�꡼�໤����
    real(8), parameter :: Rh = 0.1              ! ʿ�ѥ˥塼�ȥ���ѷ���
    real(8), parameter :: Num = 0.02            ! Ǵ���Ȼ�����
    real(8), parameter :: Nuh = 0.02            ! Ǯ�Ȼ�����
    real(8), parameter :: Q0=0.01               ! Ǯ��ʬ�ۤο���
    real(8), parameter :: A0=2.0                ! Ǯ��ʬ�ۤ� Y ��������
    real(8), parameter :: Xrange=2.0            ! Ǯ��ʬ�ۤ� X ��������

 !---- ������ʬ�ѥ�᥿�� ----
    real(8), parameter :: dt=2e-2               ! ���֥��ƥå״ֳ�  
    real(8), parameter :: tfil=0.05             ! ���֥ե��륿������
    integer, parameter :: nt=1000, ndisp=10     ! ������ʬ��, ɽ�����ƥå�

 !---- ������� ----
    integer :: i, j, it

    real(8) :: pi                                 ! �߼�Ψ
    pi = atan(1.0D0)*4.0D0

    call esc_initial(im,jm,km,lm,xmin,xmax,ymin,ymax) ! ISPACK ����� 


 !--------------- Ǯ����/����Ѱ� ʬ�� -----------------------

    do i=0,im-1
       do j=0,jm
          if ( abs(yx_X(j,i)) .le. Xrange ) then
             yx_QH(j,i) = Q0 * exp( -yx_Y(j,i)**2/A0 )   &
                             * cos(pi*yx_X(j,i)/(2*Xrange))
          else
             yx_QH(j,i) = 0.0
          endif
       enddo
    enddo

 !------------------- ��������� ----------------------------

    ! ����ͤ��Ż�ή��, ��������
    yx_H = 0.0   ! ʿ�Ѥο���
    yx_U = 0.0
    yx_V = 0.0

    ! ����ͤ˥�����ʬ��, �����ʤ�
!    yx_H = yx_QH + H0
!    yx_U = 0.0
!    yx_V = 0.0
!    yx_QH = 0.0
    
 !---------- ���ڥ��ȥ��Ѵ� / 1 ���ƥå������� ---------------

    ec_U_A = ec_yx(yx_U)
    es_V_A = es_yx(yx_V)
    ec_H_A = ec_yx(yx_H)
    ec_QH  = ec_yx(yx_QH)

    ! 1 ���ƥå��ܤϥ����顼ˡ�ǻ�����ʬ
    ec_H_B = ec_H_A ; ec_H = ec_H_A
    ec_U_B = ec_U_A ; ec_U = ec_U_A
    es_V_B = es_V_A ; es_V = es_V_A

 !------------------------------------------------------------

    call output_gtool4_init                       ! �ҥ��ȥ꡼�����
    call output_gtool4                            ! ����ͽ���

 !----------- ������ʬ[leapflog and timefilter] ------------

    do it=1,nt

       ec_U_A = ec_U_B                                         &
&           + 2.0 * dt * (                                     &
&                  - ec_yx(yx_ec(ec_U)*yx_ec(ec_Dx_ec(ec_U)))  &
&                  - ec_yx(yx_es(es_V)*yx_es(es_Dy_ec(ec_U)))  &
&                  + ec_yx( Beta * yx_Y * (yx_es(es_V)))       &
&                  - Grav * ec_dx_ec(ec_H)                     &
&                  + Num * ( ec_Dx_ec(ec_Dx_ec(ec_U_B))        &
&                          + ec_Dy_es(es_Dy_ec(ec_U_B)) )      &
&                  - Rm * ec_U_B                               &
&                    )

       es_V_A = es_V_B                                         &
&           + 2.0 * dt * (                                     &
&                  - es_yx(yx_ec(ec_U)*yx_es(es_Dx_es(es_V)))  & 
&                  - es_yx(yx_es(es_V)*yx_ec(ec_Dy_es(es_V)))  &
&                  - es_yx( Beta * yx_Y * (yx_ec(ec_U)))       &
&                  - Grav * es_dy_ec(ec_H)                     &
&                  + Num * ( es_Dx_es(es_Dx_es(es_V_B))        &
&                          + es_Dy_ec(ec_Dy_es(es_V_B)) )      &
&                  -Rm * es_V_B                                &
&                   )

       ec_H_A = ec_H_B                                         &
&           + 2.0 * dt * (                                     &
!                  - (ec_dx_ec(ec_U) + ec_dy_es(es_V))  &
&                  - ec_dx_ec(ec_yx((yx_ec(ec_H)+H0)*yx_ec(ec_U)))  &
&                  - ec_dy_es(es_yx((yx_ec(ec_H)+H0)*yx_es(es_V)))  &
&                  - ec_QH                                     &
&                  + Nuh * ( ec_Dx_ec(ec_Dx_ec(ec_H_B))        &
&                          + ec_Dy_es(es_Dy_ec(ec_H_B)) )      &
&                  - Rh * ec_H_B                               &
&                    )


       ! ���֥ե��륿��
       ec_U_B = (1.0 - 2.0 * tfil) * ec_U + tfil * (ec_U_B + ec_U_A)
       ec_U   = ec_U_A
       
       ec_H_B = (1.0 - 2.0 * tfil) * ec_H + tfil * (ec_H_B + ec_H_A)
       ec_H   = ec_H_A
       
       es_V_B = (1.0 - 2.0 * tfil) * es_V + tfil * (es_V_B + es_V_A)
       es_V   = es_V_A
       

       ! ����
       if(mod(it,ndisp) .eq. 0)then  
          call output_gtool4
       endif

    enddo

    call output_gtool4_close
    stop

  contains

 !----------------------- gtool4 ���� ----------------------------
    subroutine output_gtool4_init
      call HistoryCreate( &                             ! �ҥ��ȥ꡼����
           file='thermal_responce.nc', &
           title='Equatorial beta-plane non-linear shallow water model ', &
           source='A sample program of ISPACK and gtool_history/gtool4',  &
           institution='GFD_Dennou Club spmodel project',                 &
           dims=(/'x','y','t'/), dimsizes=(/im,jm+1,0/),                  &
           longnames=(/'X-coordinate','Y-coordinate','time        '/),    &
           units=(/'-','-','-'/),                                         &
           origin=0.0, interval=real( (ndisp)*dt  )      &
                         ) 

      call HistoryPut('x',x_X)                                   ! �ѿ�����
      call HistoryAddattr('x','topology','circular')             ! ����°��
      call HistoryAddattr('x','modulo',xmax-xmin)                ! ����°��
      call HistoryPut('y',y_Y)                                   ! �ѿ�����

      call HistoryAddVariable( &                                 ! �ѿ����
           varname='uvel', dims=(/'x','y','t'/), & 
           longname='X-velocity', units='1', xtype='double')
      call HistoryAddVariable( &                                 ! �ѿ����
           varname='vvel', dims=(/'x','y','t'/), & 
           longname='Y-velocity', units='1', xtype='double')
      call HistoryAddVariable( &                                 ! �ѿ����
           varname='height', dims=(/'x','y','t'/), & 
           longname='height', units='1', xtype='double')
      call HistoryAddVariable( &                                 ! �ѿ����
           varname='wvel', dims=(/'x','y','t'/), & 
           longname='Z-velocity', units='1', xtype='double')
      call HistoryAddVariable( &                                 ! �ѿ����
           varname='hdiv', dims=(/'x','y','t'/), & 
           longname='Horizontal divergence', units='1', xtype='double')
    end subroutine output_gtool4_init

    subroutine output_gtool4
      write(6,*) 'it = ',it
      yx_U   = yx_ec(ec_U_A)
      yx_V   = yx_es(es_V_A)
      yx_H   = yx_ec(ec_H_A)

      call HistoryPut('t',real( it*dt ) )
      call HistoryPut('uvel',transpose(yx_U))
      call HistoryPut('vvel',transpose(yx_V))
      call HistoryPut('height',transpose(yx_H))
      call HistoryPut('wvel', &
           transpose(yx_ec(-H0*(ec_dx_ec(ec_U_A)+ec_dy_es(es_V_A))-ec_QH)))
      call HistoryPut('hdiv', &
           transpose(yx_ec(ec_dx_ec(ec_U_A)+ec_dy_es(es_V_A))))
    end subroutine output_gtool4

    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

  end program equatorial_beta_nonlinear_shallow_water
