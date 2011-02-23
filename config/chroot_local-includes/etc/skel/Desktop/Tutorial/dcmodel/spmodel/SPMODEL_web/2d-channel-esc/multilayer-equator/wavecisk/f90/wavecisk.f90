!----------------------------------------------------------------------
!     Copyright (c) 2004--2005 YAMADA YUKIKO. All rights reserved.
!----------------------------------------------------------------------
! Sample program for SPML(based on gt4f90io and ISPACK)   
!
! 
! Solving equatorial beta plane wave-CISK equations
!
! history: 2004/06/23 YAMADA, Y. ����
!          2004/06/27 YAMADA, Y. ��¤���
!

  program equatorial_beta_wavecisk
    use esc_module
    use gt4_history
    use gt4f90io
    implicit none

 !---- ���ֲ��������� ----
    integer, parameter :: km=16,   lm=16, mm = 3     ! �����ȿ�������(X,Y,Z)
    integer, parameter :: im=64,   jm=32, vm = mm+1  ! �ʻ���������(X,Y,Z)

!    integer, parameter :: km=64,   lm=64, mm = 3  ! �����ȿ�������(X,Y,Z)
!    integer, parameter :: im=200,   jm=100, vm = mm+1  ! �ʻ���������(X,Y,Z)

 !---- ��Ǯ�Υѥ�᥿�ꥼ����������� ----
    
    ! wave-CISK �ѥ�᥿�ꥼ�������: TRUE=�徺��Ǯ��, FALSE=������Ǯ��
    logical            :: positive_only_heat_parameta = .FALSE.

 !---- �ѿ� ----
    real(8)            :: xyz_U(0:jm,0:im-1,vm)     ! �ʻҥǡ���(U(t+��t))
    real(8)            :: xyz_V(0:jm,0:im-1,vm)     ! �ʻҥǡ���(V(t+��t))
    real(8)            :: xyz_H(0:jm,0:im-1,vm)     ! �ʻҥǡ���(H(t+��t))
    real(8)            :: xym_U(0:jm,0:im-1,mm)     ! �ʻҥǡ���(U(t+��t))
    real(8)            :: xym_V(0:jm,0:im-1,mm)     ! �ʻҥǡ���(V(t+��t))
    real(8)            :: xym_H(0:jm,0:im-1,mm)     ! �ʻҥǡ���(H(t+��t))
    real(8)            :: ecm_U(-km:km,0:lm,mm)   ! ���ڥ��ȥ�(U(t))
    real(8)            :: esm_V(-km:km,lm,mm)     ! ���ڥ��ȥ�(V(t))
    real(8)            :: ecm_H(-km:km,0:lm,mm)   ! ���ڥ��ȥ�(H(t))
    real(8)            :: ecm_U_B(-km:km,0:lm,mm) ! ���ڥ��ȥ�(U(t-��t))
    real(8)            :: esm_V_B(-km:km,lm,mm)   ! ���ڥ��ȥ�(V(t-��t))
    real(8)            :: ecm_H_B(-km:km,0:lm,mm) ! ���ڥ��ȥ�(H(t-��t))
    real(8)            :: ec_QH(-km:km,0:lm)      ! ���ڥ��ȥ�(Ǯ��ʬ��)
    real(8)            :: ec_QH_A(-km:km,0:lm)    ! ���ڥ��ȥ�(Ǯ��ʬ��)
    real(8)            :: gamma(mm)               ! ��ľ�⡼�ɸ�ͭ��^0.5 
    real(8)            :: eig(mm,0:vm)            ! ��ľ�⡼�ɸ�ͭ�ؿ�
    real(8)            :: eta(mm)               ! ��Ǯ�α�ľʬ��

 !---- ��ɸ�ѿ��ʤ� ----
    real(8), parameter :: xmin=-10.0, xmax=10.0  ! �ΰ��ϰ�(X ����)
!    real(8), parameter :: xmin=-25.0, xmax=25.0  ! �ΰ��ϰ�(X ����)
    real(8), parameter :: ymin=-5.0, ymax=5.0   ! �ΰ��ϰ�(Y ����) 

 !---- ʪ���ѥ�᥿�� ----
    real(8), parameter :: Grav=1.0              ! ���ϲ�®��
    real(8), parameter :: Beta=1.0              ! ��ƻ�¥ѥ�᥿
    real(8), parameter :: H0 = 1.0              ! ʿ�Ѥο���
    real(8), parameter :: Rm = 0.0              ! �쥤�꡼�໤����
    real(8), parameter :: Rt = 0.0              ! ʿ�ѥ˥塼�ȥ���ѷ���
    real(8), parameter :: Num = 0.02            ! Ǵ���Ȼ�����
    real(8), parameter :: Nut = 0.02            ! Ǯ�Ȼ�����
    real(8), parameter :: Q0=1.0                ! ���Ǯ��ʬ�ۤο���

    real(8), parameter :: AX0=0.5                ! ���Ǯ��ʬ�ۤ�Ⱦ����
!    real(8), parameter :: AX0=0.5*4*2.5              ! ���Ǯ��ʬ�ۤ�Ⱦ����

    real(8), parameter :: AY0=0.5              ! ���Ǯ��ʬ�ۤ�Ⱦ����

 !---- ������ʬ�ѥ�᥿ ----
    real(8), parameter :: dt=2e-2               ! ���֥��ƥå״ֳ�  
    real(8), parameter :: tfil=0.05             ! ���֥ե��륿������
    integer, parameter :: nt=1500, ndisp=20     ! ������ʬ��, ɽ�����ƥå�
!    integer, parameter :: nt=3000, ndisp=20     ! ������ʬ��, ɽ�����ƥå�

 !---- ������� ----
    integer :: i, j, it, m

 !---- ���ڥ��ȥ����� ----
    
    call esc_initial(im,jm,km,lm,xmin,xmax,ymin,ymax) ! ISPACK ����� 
    call vmode_initial( eig, gamma ) ! ��ľ�⡼�ɸ�ͭ�͸�ͭ�ؿ�������
    
 !-------- ��������� -----------

    ! ����ͤ��Ż�ή��
!    xyz_H = H0   ! ʿ�Ѥο���
    xyz_H = 0.0   ! ʿ�Ѥο���
    xyz_U = 0.0
    xyz_V = 0.0

!    xym_H = H0   ! ʿ�Ѥο���
    xym_H = 0.0   ! ʿ�Ѥο���
    xym_U = 0.0
    xym_V = 0.0


    ! ��Ǯ�α�ľ�⡼�ɤ�����
    eta(1) = 1.5 ;  eta(2) = -1.5               
    do m=1,mm  
       eta(m) = eta(m) / eig(m,1) * gamma(m) / vm  * (2**0.5)  
    enddo

    ! ���Ǯ��ʬ�� (���⡼�ɤΤ߲�Ǯ��Ϳ����)
    do i=0,im-1
       do j=0,jm
          xym_H(j,i,1) = - Q0          &
               * exp( -xy_Y(j,i)**2/AY0 )   &
               * exp( -xy_X(j,i)**2/AX0 )   
       enddo
    enddo

    ! ��ʿ���ڥ��ȥ��Ѵ�
    do m = 1, mm 
       ecm_U(:,:,m) = ec_xy(xym_U(:,:,m) )
       esm_V(:,:,m) = es_xy(xym_V(:,:,m) )
       ecm_H(:,:,m) = ec_xy(xym_H(:,:,m) )
    enddo

 !------------------------------------------------------------

    call output_gtool4_init                 ! �ҥ��ȥ꡼�����
    call output_gtool4                      ! ����ͽ���

 !----------------------- ��ʬ main ----------------------------

    ! 1 ���ƥå��ܤϥ����顼ˡ�ǻ�����ʬ
    ecm_H_B = ecm_H
    ecm_U_B = ecm_U
    esm_V_B = esm_V

    do it=1,nt

       ! ��ľή (��Ǯ��) �����
       ec_QH_A = 0.0

       do m = 1, mm 

          ! ������ʬ
          call time_integral( gamma(m), eta(m), ec_QH,         &
               ecm_U(:,:,m),   esm_V(:,:,m),   ecm_H(:,:,m),   &
               ecm_U_B(:,:,m), esm_V_B(:,:,m), ecm_H_B(:,:,m)  )

          ! ��ľή (��Ǯ��) �η׻�
          ec_QH_A = ec_QH_A                                    &
               - (ec_Dx_ec(ecm_U(:,:,m)) + ec_Dy_es(esm_V(:,:,m)))

       enddo

       ec_QH   = ec_QH_A

       ! �徺ή��Τ߲�Ǯ�ξ��
       if (positive_only_heat_parameta .AND. .TRUE. )  then 
          call positive_only_heat(ec_QH)
       endif

       ! ����
       if(mod(it,ndisp) .eq. 0) then 
          call output_gtool4
       endif

    enddo

    call output_gtool4_close
    stop

  contains

 !----------- ������ʬ[leapflog and timefilter] ------------

    subroutine time_integral(  gamma_m, eta_m, ec_QH, &
         ec_U,es_V,ec_H,ec_U_B,es_V_B,ec_H_B)
      
      real(8)              :: ec_U_A(-km:km,0:lm)   ! ���ڥ��ȥ�(U(t+��t))
      real(8)              :: es_V_A(-km:km,lm)     ! ���ڥ��ȥ�(V(t+��t))
      real(8)              :: ec_H_A(-km:km,0:lm)   ! ���ڥ��ȥ�(H(t+��t))
      real(8),intent(inout):: ec_U(-km:km,0:lm)     ! ���ڥ��ȥ�(U(t))
      real(8),intent(inout):: es_V(-km:km,lm)       ! ���ڥ��ȥ�(V(t))
      real(8),intent(inout):: ec_H(-km:km,0:lm)     ! ���ڥ��ȥ�(H(t))
      real(8),intent(inout):: ec_U_B(-km:km,0:lm)   ! ���ڥ��ȥ�(U(t-��t))
      real(8),intent(inout):: es_V_B(-km:km,lm)     ! ���ڥ��ȥ�(V(t-��t))
      real(8),intent(inout):: ec_H_B(-km:km,0:lm)   ! ���ڥ��ȥ�(H(t-��t))
      real(8),intent(in)   :: gamma_m                 ! ��ľ�⡼�ɸ�ͭ��^0.5
      real(8),intent(in)   :: eta_m                 ! ��ľ��Ǯʬ��
      real(8),intent(in)   :: ec_QH(-km:km,0:lm)     ! ���ڥ��ȥ�(Ǯ��ʬ��)


       ec_U_A = ec_U_B                                         &
&           + dt * 2.0 * (                                     &
!&                  - ec_xy(xy_ec(ec_U)*xy_ec(ec_Dx_ec(ec_U)))  &
!&                  - ec_xy(xy_es(es_V)*xy_es(es_Dy_ec(ec_U)))  &
&                  + ec_xy( Beta * xy_Y * (xy_es(es_V)))       &
&                  - Grav * ec_dx_ec(ec_H)                     &
&                  + Num * ( ec_Dx_ec(ec_Dx_ec(ec_U_B))        &
&                          + ec_Dy_es(es_Dy_ec(ec_U_B)) )      &
&                  - Rm * ec_U                                 &
&                    )

       es_V_A = es_V_B                                         &
&           + dt * 2.0 * (                                     &
!&                  - es_xy(xy_ec(ec_U)*xy_es(es_Dx_es(es_V)))  & 
!&                  - es_xy(xy_es(es_V)*xy_ec(ec_Dy_es(es_V)))  &
&                  - es_xy( Beta * xy_Y * (xy_ec(ec_U)))       &
&                  - Grav * es_dy_ec(ec_H)                     &
&                  + Num * ( es_Dx_es(es_Dx_es(es_V_B))        &
&                          + es_Dy_ec(ec_Dy_es(es_V_B)) )      &
&                  -Rm * es_V                                  &
&                   )

       ec_H_A = ec_H_B                                         &
&           + dt * 2.0 * (                                     &
&                  + 1.0 / gamma_m **2 * (                     &
&                      - H0*(ec_dx_ec(ec_U) + ec_dy_es(es_V))  &
!&                    - ec_dx_ec(ec_xy(xy_ec(ec_H)*xy_ec(ec_U))) &
!&                    - ec_dy_es(es_xy(xy_ec(ec_H)*xy_es(es_V))) &
&                    - eta_m * ec_QH                           &
&                                    )                         &
&                  + Nut  ( ec_Dx_ec(ec_Dx_ec(ec_H_B))         &
&                         + ec_Dy_es(es_Dy_ec(ec_H_B)) )       &
&                  - Rt * ec_H                                 &
&                    )


       ! ���֥ե��륿��
       ec_U_B = (1.0 - 2.0 * tfil) * ec_U + tfil * (ec_U_B + ec_U_A)
       ec_U = ec_U_A
       
       ec_H_B = (1.0 - 2.0 * tfil) * ec_H + tfil * (ec_H_B + ec_H_A)
       ec_H = ec_H_A
       
       es_V_B = (1.0 - 2.0 * tfil) * es_V + tfil * (es_V_B + es_V_A)
       es_V = es_V_A
       

     end subroutine time_integral


     ! �徺ή��Τ߲�Ǯ�ξ��
     subroutine positive_only_heat(ec_QH)
       
       real(8),intent(inout):: ec_QH(-km:km,0:lm)      ! ���ڥ��ȥ�(Ǯ��ʬ��)
       real(8)              :: xy_QH(0:jm,0:im-1)      ! �ʻҥǡ���(Ǯ��ʬ��)
       
       xy_QH = xy_ec(ec_QH)
       
       do i=0,im-1
          do j=0,jm
             xy_QH(j,i) = max(0,xy_QH(j,i))
          enddo
       enddo
       
       ec_QH   = ec_xy(xy_QH)
     end subroutine positive_only_heat


     ! ��ľ�⡼�ɤθ�ͭ��, ��ͭ�ؿ�������
     subroutine vmode_initial( eig, gamma )
       
       real(8),intent(out):: gamma(mm)         ! ��ľ�⡼�ɸ�ͭ��^0.5 
       real(8),intent(out):: eig(mm,0:vm)      ! ��ľ�⡼�ɸ�ͭ�ؿ�
       
       integer :: j, m
       real(8) :: pi                                 ! �߼�Ψ
       pi = atan(1.0D0)*4.0D0
       
       ! ��ͭ�ؿ� �岼ü�� 0
       eig(:,0) = 0.0 ; eig(:,vm) = 0.0 
       do m=1,mm 
          ! ��ͭ��^0.5 �η׻�
          gamma(m) = vm * ( 2 - 2 * cos( m*pi/(mm+1) ) )**0.5
          
          ! ��ͭ�ؿ� (���ʲ����Ƥʤ�) �׻�
          do j=1,vm-1
             eig(m,j)   = sin ( (m*j*pi)/(mm+1) )
          enddo
          
       enddo
       
     end subroutine vmode_initial
     
     ! ��ľ�⡼�ɵ��Ѵ� (u,v,phi)
     subroutine inv_vmode_transpote(xym_data, xyz_data)
       
       real(8),intent(in) :: xym_data(0:jm,0:im-1,mm)  ! ��ľ�⡼�ɥǡ���
       real(8),intent(out):: xyz_data(0:jm,0:im-1,vm)  ! ��ľ�ʻҥǡ���
      
       xyz_data = 0.0 
       do j=1,vm
          do m=1,mm   
             xyz_data(:,:,j) = xyz_data(:,:,j)                  &
                  + ( eig(m,j) - eig(m,j-1) )                   &
                  * xym_data(:,:,m) * vm / gamma(m) / (2**0.5)       
          enddo
       enddo
      
     end subroutine inv_vmode_transpote


 !----------------------- gtool4 ���� ----------------------------
    subroutine output_gtool4_init

      real(8)  :: vm_ary(vm)  ! ��ľ�ؤ��ֹ���
      
      do j = 1,vm
         vm_ary(j) = j * 1.0
      enddo

      ! �ҥ��ȥ꡼����
      call HistoryCreate(                                                 &
           file='wavecisk_test.nc',                                       & 
           title='Equatorial beta-plane wave-CISK model ',                &
           source='A sample program of ISPACK and gtool_history/gtool4',  &
           institution='GFD_Dennou Club spmodel project',                 &
           dims=(/'x','y','z','t'/), dimsizes=(/im,jm+1,vm,0/),           &
           longnames=(/'X-coordinate','Y-coordinate',                     &
           'Z-level     ','time        '/),                               &
           units=(/'1','1','1','1'/),                                     &
           origin=0.0, interval=real( (ndisp)*dt  )                       &
           ) 

      call HistoryPut('x',x_X)                                   ! �ѿ�����
      call HistoryAddattr('x','topology','circular')             ! ����°��
      call HistoryAddattr('x','modulo',xmax-xmin)                ! ����°��
      call HistoryPut('y',y_Y)                                   ! �ѿ�����
      call HistoryPut('z',vm_ary)                           ! �ѿ�����

      call HistoryAddVariable( &                                 ! �ѿ����
           varname='uvel', dims=(/'x','y','z','t'/), & 
           longname='X-velocity', units='1', xtype='double')
      call HistoryAddVariable( &                                 ! �ѿ����
           varname='vvel', dims=(/'x','y','z','t'/), & 
           longname='Y-velocity', units='1', xtype='double')
      call HistoryAddVariable( &                                 ! �ѿ����
           varname='height', dims=(/'x','y','z','t'/), & 
           longname='height', units='1', xtype='double')
      call HistoryAddVariable( &                                 ! �ѿ����
           varname='wvel', dims=(/'x','y','t'/), & 
           longname='Z-velocity_(3/2)', units='1', xtype='double')
    end subroutine output_gtool4_init

    subroutine output_gtool4

      real(8)  :: xyz_tr_U(0:im-1,0:jm,vm) ! �ʻҥǡ��� (X-Y �����촹��)
      real(8)  :: xyz_tr_V(0:im-1,0:jm,vm) ! �ʻҥǡ��� (X-Y �����촹��)
      real(8)  :: xyz_tr_H(0:im-1,0:jm,vm) ! �ʻҥǡ��� (X-Y �����촹��)
      
      write(6,*) 'it = ',it

      ! ��ʿ���ڥ��ȥ���Ѵ�
      do m = 1, mm 
         xym_U(:,:,m) = xy_ec(ecm_U(:,:,m)) 
         xym_V(:,:,m) = xy_es(esm_V(:,:,m)) 
         xym_H(:,:,m) = xy_ec(ecm_H(:,:,m)) 
      enddo

      ! ��ľ�⡼�ɵ��Ѵ�
      call inv_vmode_transpote(xym_U, xyz_U)
      call inv_vmode_transpote(xym_V, xyz_V)
      call inv_vmode_transpote(xym_H, xyz_H)

      ! X-Y �����촹��
      do m = 1, vm 
         xyz_tr_U(:,:,m) = transpose(xyz_U(:,:,m))
         xyz_tr_V(:,:,m) = transpose(xyz_V(:,:,m))
         xyz_tr_H(:,:,m) = transpose(xyz_H(:,:,m))
      enddo

      call HistoryPut('t',real( it*dt ) )
      call HistoryPut('uvel', xyz_tr_U )
      call HistoryPut('vvel', xyz_tr_V )
      call HistoryPut('height', xyz_tr_H )
      call HistoryPut('wvel', transpose( xy_ec(ec_QH_A)) )

    end subroutine output_gtool4

    subroutine output_gtool4_close
      call HistoryClose
    end subroutine output_gtool4_close

  end program equatorial_beta_wavecisk
