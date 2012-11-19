!--------------------------------------------------------------------------
!  Copyright (C) 2005 SPMODEL Development Group. All rights reserved.
!--------------------------------------------------------------------------
!
!Title(ɽ��): Two-dimensional barotropic model on a double cyclic region
!             (2 �ż��������ΰ���2�����簵��ǥ�)
!
!       The time integration is performed with the 4th order 
!       Runge-Kutta scheme. 
!       The beta parameter is constant. 
!       The viscous linear term and the beta term are transformed 
!         by the exp., cos, sin functions.  
!       Program is organized for test1 (test for linear terms). 
!
!History(����):
!   2005/10/27  �ݹ�����, ��������
!   2005/10/31  �ݹ�����, k,l �������� bug fix
!
! The governing equation(����������): 
!
!      d\nabla^2\psi/dt 
!          = -J(\psi,\nabla^2\psi) - beta d\psi/dx
!            +(-1)^{p+1}\nu_{2p}\nabla^2p \nabla^2\psi
!
program plbaro_beta_rn4expcs_test1

!== �⥸�塼�������� ================================================
  use ee_module 
  use gt4_history, only : GT_HISTORY, HistoryCreate, HistoryPut, HistoryGet, &
                          HistoryAddVariable, HistoryClose, HistoryAddAttr
  use dc_trace,    only : SetDebug, BeginSub, EndSub, DbgMessage
  use dc_message,  only : MessageNotify
  implicit none

!== ����� ============================================================

 !---- �ѿ�(�ʻ����ǡ���) ----
  real(8), allocatable :: yx_VelX(:,:)        ! ®�ٷ�����ʬ
  real(8), allocatable :: yx_VelY(:,:)        ! ®�ٰ�����ʬ
  real(8), allocatable :: yx_Vor(:,:)         ! ����(��ľ��ʬ)
  real(8), allocatable :: yx_StrFunc(:,:)     ! ή���ؿ�

  real(8), allocatable :: yx_VorAnalytic(:,:) ! ����(��ľ��ʬ)���ϲ�

 !---- �ѿ�(���ڥ��ȥ�ǡ���) ----
  real(8), allocatable :: ee_Vor(:,:)         ! ����(��ľ��ʬ)
  real(8), allocatable :: ee_StrFunc(:,:)     ! ή���ؿ�

  real(8), allocatable :: ee_Xi(:,:)          ! �� : ��=��exp[-(i��+��)t]
  real(8), allocatable :: ee_Vortmp(:,:)      ! ����(��󥲥��å��׻������)
  real(8), allocatable :: ee_k1(:,:)          ! �Ȼ��ʳ��α��ٻ����Ѳ�(1����)
  real(8), allocatable :: ee_k2(:,:)          ! �Ȼ��ʳ��α��ٻ����Ѳ�(2����)
  real(8), allocatable :: ee_k3(:,:)          ! �Ȼ��ʳ��α��ٻ����Ѳ�(3����)
  real(8), allocatable :: ee_k4(:,:)          ! �Ȼ��ʳ��α��ٻ����Ѳ�(4����)

  real(8), allocatable :: ee_OmegaR(:,:)      ! �����ӡ��Ȥο�ư��
                                              ! (- Beta * abs(m)/(n*(n+1))

  real(8), allocatable :: ee_VorAnalytic(:,:) ! ����(��ľ��ʬ)���ϲ�
  real(8), allocatable :: ee_VorInit(:,:)     ! ����(��ľ��ʬ)�����



 !---- ����ѥ�᥿ -----
  real(8), parameter  :: pi = 3.141592653589793D0 ! �߼�Ψ
  character(len=20)   :: DbgMessageFmt='*** DbgMESSAGE ***'
  real(8), parameter  :: vmiss = -999.0       ! ��»��

 !---- NAMELIST �ѿ� ----
  ! NAMELIST �����ѥƥ�ݥ��ե�����̾(̾���ѹ��ػ�)
  character(len=30),parameter  :: nmlfile='plbaro-beta_rn4expcs_test1.nml'

  logical            :: Verbose=.false.          ! ��Ĺ�ʥ�å��������� On/Off
  logical            :: DebugOn=.false.          ! �ǥХå���å����� On/Off
  namelist /message/  Verbose, DebugOn           !
                                                 !-- �ʻ��������ڥ��ȥ� --
  integer :: km=21                               ! X �����������ȿ�
  integer :: lm=21                               ! Y �����������ȿ�
  integer :: im=64                               ! X �����ʻ����� (>3*km)
  integer :: jm=64                               ! Y �����ʻ����� (>3*lm)
  namelist /gridset/ km, lm, im, jm
                                                 !-- ʪ���ѥ�᥿�� --
  real(8)            :: XLength=1.0D0            ! �ΰ���礭��(X ����)
  real(8)            :: YLength=1.0D0            ! �ΰ���礭��(Y ����)
  real(8)            :: Beta=0.0D0               ! �¥ѥ�᥿��
  integer            :: HVOrder=1                ! ĶǴ���μ���(1 �����̤�Ǵ��,
                                                 ! ��ʿ��ץ饷����γ���)
  real(8)            :: HVisc=1.0D0              ! ĶǴ������
  namelist /physics/  XLength, YLength, Beta, HVOrder, HVisc
                                                 ! -- ����� --
  character(len=100) :: initial_file=''          ! ����ͥǡ����ե�����̾
                                                 ! (���ʤ������ǽ���ͤ�׻�)
  real               :: initial_time=0.0         ! �������
  namelist /initial/ initial_file, initial_time  ! 
                                                 ! -- ������ʬ --
  real(8) :: delta_t=1.0e-7                      ! ������ʬ���
  integer :: nstep=2000                          ! ������ʬ���ƥå׿�
  namelist /tint/    delta_t, nstep              ! 
                                                 ! -- �ҥ��ȥ꡼���� --
  character(len=100) :: hst_file=   ''           ! �ҥ��ȥ꡼�ե�����̾
  character(len=100) :: title = &                ! �����ȥ�
   'Test of linear terms of 2-dim barotropic model on a double-cyclic domain'

  integer :: hst_intstep=200                     ! �ҥ��ȥ꡼���ϴֳ֥��ƥå׿�
  namelist /history/    hst_file, title, hst_intstep

  character(len=100) :: rst_file=''              ! �ꥹ�����Ƚ��ϥե�����̾
  integer :: rst_intstep=200                     ! �ꥹ�����Ƚ��ϴֳ֥��ƥå׿�
  namelist /restart/   rst_file, rst_intstep 

 !---- ����ѿ� ----
  real(8), allocatable :: ee_HVisc(:,:)      ! ĶǴ������

  integer :: it=0                            ! ���֥��ƥå�
  real(8) :: time                            ! ��ǥ������
  integer :: k, l                            ! �ȿ�
  real(8) :: OmegaR                          ! �����ӡ��Ȥο�ư��
  real(8) :: NuP                             ! ĶǴ������

  type(GT_HISTORY) :: hst_rst                ! �ꥹ������ GT_HISTORY �ѿ�

 !---------------- NAMELIST �ɤ߹��� ---------------------

  write(6,nml=message) 
  open(10,file=nmlfile,status='OLD')
  read(10,nml=message) ; write(6,nml=message) ; close(10)

  if (verbose) write(6,nml=gridset) 
  open(10,file=nmlfile,status='OLD')
  read(10,nml=gridset) ; write(6,nml=gridset) ; close(10)

  if (verbose) write(6,nml=physics) 
  open(10,file=nmlfile,status='OLD')
  read(10,nml=physics) ; write(6,nml=physics) ; close(10)

  if (verbose) write(6,nml=initial) 
  open(10,file=nmlfile,status='OLD')
  read(10,nml=initial) ; write(6,nml=initial) ; close(10)

  if (verbose) write(6,nml=tint) 
  open(10,file=nmlfile,status='OLD')
  read(10,nml=tint) ; write(6,nml=tint) ; close(10)

  if (verbose) write(6,nml=history) 
  open(10,file=nmlfile,status='OLD')
  read(10,nml=history) ; write(6,nml=history) ; close(10)

  if (verbose) write(6,nml=restart) 
  open(10,file=nmlfile,status='OLD')
  read(10,nml=restart) ; write(6,nml=restart) ; close(10)

 !---------------- �ǥХå������������� -----------------
  if (DebugOn) then
    call SetDebug
  end if

 !------------------ �ѿ��γ���դ� ---------------------
  allocate(yx_VelX(0:jm-1,0:im-1),yx_VelY(0:jm-1,0:im-1))
  allocate(yx_Vor(0:jm-1,0:im-1),yx_StrFunc(0:jm-1,0:im-1))
  allocate(yx_VorAnalytic(0:jm-1,0:im-1))

  allocate(ee_Vor(-lm:lm,-km:km),ee_StrFunc(-lm:lm,-km:km))

  allocate(ee_Vortmp(-lm:lm,-km:km))
  allocate(ee_Xi(-lm:lm,-km:km))
  allocate(ee_k1(-lm:lm,-km:km),ee_k2(-lm:lm,-km:km))
  allocate(ee_k3(-lm:lm,-km:km),ee_k4(-lm:lm,-km:km))

  allocate(ee_HVisc(-lm:lm,-km:km))
  allocate(ee_OmegaR(-lm:lm,-km:km))

  allocate(ee_VorAnalytic(-lm:lm,-km:km),ee_VorInit(-lm:lm,-km:km))

 !------------------ ��ɸ�ͤ����� -----------------------
  call DbgMessage(fmt='call %c', c1='ee_initial') 
  call ee_Initial(im,jm,km,lm,0.0D0,XLength,0.0D0,YLength)

 !------------------ ʪ������������ -----------------------
  ee_Vor = 1.0D0
  ee_HVisc = (-1)**HVOrder * HVisc*(ee_Lapla_ee(ee_Vor))**HVOrder


  do k=-km,km
     do l=-lm,lm
        if ( k**2+l**2 .NE. 0 ) then
           ee_OmegaR(l,k) = - Beta * (2*PI*k/XLength) &
                        / ( (2*PI*k/XLength)**2 + (2*PI*l/YLength)**2 )
        else
           ee_OmegaR(l,k) = vmiss
        endif
     enddo
  enddo

 !------------------- ��������� ----------------------
  time = initial_time

  if ( initial_file == "") then
     ! �ꥹ�����ȥե��������ꤷ�ʤ���������� ee_Vor ��Ϳ����. 
     call set_initial_values
  else
     !  ���������(�ꥹ�����ȥե����뤫����ɤߤ���)
     call HistoryGet( trim(initial_file), 'ee_vor', ee_Vor,  time )
  endif

  ee_Strfunc = ee_LaplaInv_ee(ee_Vor)

 !---------------- ������ʬ(Runge-Kutta + exp �Ѵ�ˡ) -----------------
  call output_restart_init
  call output_history_init

  if ( initial_file == '' ) call output_history    ! ������Ϳ��������ͤϽ���

  call DbgMessage(fmt='%c %c', &
&                 c1=DbgMessageFmt, &
&                 c2='Time integration starts.') 

  do it=1,nstep
     time = initial_time + it * delta_t

     !---- 1 ���� [ k1 = f( x_n, t_n) ] ----
     ee_k1 = ee_DXiDt_ee_ee(ee_Vor,ee_Strfunc,0.0D0)

     !---- 2 ���� [ k2 = f( x_n+k1*dt/2, t_n+dt/2 ) ] ----
     ee_Xi = ee_Vor + ee_k1*delta_t/2.0D0
     ee_Vortmp = ee_Xi2Vor_ee(ee_Xi,delta_t/2.0D0)
     ee_StrFunc = ee_LaplaInv_ee(ee_Vortmp)
     ee_k2 = ee_DXiDt_ee_ee(ee_Vortmp,ee_Strfunc,delta_t/2.0D0)

     !---- 3 ���� [ k3 = f( x_n+k2*dt/2, t_n+dt/2 ) ] ----
     ee_Xi = ee_Vor + ee_k2*delta_t/2.0D0
     ee_Vortmp = ee_Xi2Vor_ee(ee_Xi,delta_t/2.0D0)
     ee_StrFunc = ee_LaplaInv_ee(ee_Vortmp)
     ee_k3 = ee_DXiDt_ee_ee(ee_Vortmp,ee_Strfunc,delta_t/2.0D0)

     !---- 4 ���� [ k4 = f( x_n+k3*dt, t_n+dt ) ] ----
     ee_Xi = ee_Vor + ee_k3*delta_t
     ee_Vortmp = ee_Xi2Vor_ee(ee_Xi,delta_t)
     ee_StrFunc = ee_LaplaInv_ee(ee_Vortmp)
     ee_k4 = ee_DXiDt_ee_ee(ee_Vortmp,ee_Strfunc,delta_t)

     !---- ��ʬ ----
     ee_Xi =ee_Vor + delta_t * (   ee_k1/6.0D0 + ee_k2/3.0D0 &
                                 + ee_k3/3.0D0 + ee_k4/6.0D0    )
     ee_Vor = ee_Xi2Vor_ee(ee_Xi,delta_t)

     ! ���٤���ή���ؿ���׻�
     ee_StrFunc = ee_LaplaInv_ee(ee_Vor)

     if(mod(it,hst_intstep) .eq. 0)then                    ! �ҥ��ȥ꡼����
        call output_history
     endif

     if(mod(it,rst_intstep) .eq. 0)then                    ! �ꥹ�����Ƚ���
        call output_restart
     endif
  enddo

  call DbgMessage(fmt='%c %c',      &
&                 c1=DbgMessageFmt, &
&                 c2='Time integration end.') 

  if(.not. mod(it-1,rst_intstep) .eq. 0)then                    ! �ǽ�����
     call output_restart
  endif

  call output_restart_close
  call output_history_close

! �ʾ� �ᥤ��ץ������ 
!-----------------------------------------------------------------------------
! �ʲ� ���֥롼����

contains
 !========================== �����Ѳ��׻� ==========================
 !
 ! �����Ѳ���(��������Τ�)
 !
  function ee_DVorDtNonlinear_ee_ee(ee_Vor,ee_Strfunc)

    real(8) :: ee_Vor(:,:)                             ! ����(��ľ��ʬ)
    real(8) :: ee_StrFunc(:,:)                         ! ή���ؿ�

    ! ���ٻ����Ѳ�(��������Τ�)
    real(8) :: ee_DVorDtNonlinear_ee_ee(size(ee_Vor,1),size(ee_Vor,2))

    ee_DVorDtNonlinear_ee_ee = - ee_Jacobian_ee_ee(ee_StrFunc,ee_Vor)

  end function ee_DVorDtNonlinear_ee_ee

 !
 ! �� ���� �� �����
 !
  function ee_Xi2Vor_ee(ee_Xi,dt)
    real(8), intent(IN)  :: ee_Xi(-lm:,-km:)        ! �� : ��=��exp[-(i��+��)t]
    real(8), intent(IN)  :: dt                      ! ���ֹ�

    ! ���٦�
    real(8)  :: ee_Xi2Vor_ee( &
                  -size(ee_Vor,1)/2:size(ee_Vor,1)/2, &
                  -size(ee_Vor,2)/2:size(ee_Vor,2)/2     )

    ! ����ΰ�
    integer :: k,l

    do k=0,km
       do l=-lm,lm
          ee_Xi2Vor_ee(l,k) &
               =   ee_Xi(l,k) &
                 * exp(-ee_HVisc(l,k)*dt)*cos(ee_OmegaR(l,k)*dt) &
                 + ee_Xi(-l,-k) &
                 * exp(-ee_HVisc(l,k)*dt)*sin(ee_OmegaR(l,k)*dt)
          ee_Xi2Vor_ee(-l,-k) &
               = ee_Xi(-l,-k) &
               * exp(-ee_HVisc(l,k)*dt)*cos(ee_OmegaR(l,k)*dt) &
               - ee_Xi(l,k) &
               * exp(-ee_HVisc(l,k)*dt)*sin(ee_OmegaR(l,k)*dt)
       enddo
    enddo
    ee_Xi2Vor_ee(0,0) = ee_Xi(0,0)

  end function ee_Xi2Vor_ee

 !
 !  ��(��=��exp[-(i��+��)t])�λ����Ѳ�
 !
  function ee_DXiDt_ee_ee(ee_Vor,ee_Strfunc,dt)

    real(8), intent(IN)  :: ee_Vor(-lm:,-km:)                 ! ���٦�
    real(8), intent(IN)  :: ee_Strfunc(-lm:,-km:)             ! ή���ؿ�
    real(8), intent(IN)  :: dt                          ! ���ֹ�

    ! �λ����Ѳ�
    real(8) :: ee_DXiDt_ee_ee( &
                  -size(ee_Vor,1)/2:size(ee_Vor,1)/2, &
                  -size(ee_Vor,2)/2:size(ee_Vor,2)/2     )

    ! ����ΰ�
    real(8) :: ee_G( &
                  -size(ee_Vor,1)/2:size(ee_Vor,1)/2, &
                  -size(ee_Vor,2)/2:size(ee_Vor,2)/2     )

    integer :: l,k

    ee_G = ee_DVorDtNonlinear_ee_ee(ee_Vor,ee_Strfunc)

    do k=0,km
       do l=-lm,lm
          ee_DXiDt_ee_ee(l,k)   &
            =   exp(ee_HVisc(l,k)*dt) * cos(ee_OmegaR(l,k)*dt) &
              * ee_G(l,k) &
              - exp(ee_HVisc(l,k)*dt) * sin(ee_OmegaR(l,k)*dt) &
              * ee_G(-l,-k)
          ee_DXiDt_ee_ee(-l,-k) &
            =   exp(ee_HVisc(l,k)*dt) * cos(ee_OmegaR(l,k)*dt) &
              * ee_G(-l,-k)           &
              + exp(ee_HVisc(l,k)*dt) * sin(ee_OmegaR(l,k)*dt) &
              * ee_G(l,k)
       enddo
    enddo
    ee_DXiDt_ee_ee(0,0) = 0.0D0

  end function ee_DXiDt_ee_ee


!=========================== ��������� ============================
 !
 !  ���������(�ꥹ�����ȥե�����ʤ����Υǥե��������)
 !
  subroutine set_initial_values    ! ee_Vor ������

   !---- ����� 1 �ȿ���ʬ�Τ�
    integer :: KInitial =1             ! �ȿ� X ��ʬ
    integer :: LInitial =1             ! �ȿ� Y ��ʬ
    real(8) :: Amplitude =1.0D0         ! ����

    namelist /initwave/ KInitial, LInitial, Amplitude

    if (verbose) write(6,nml=initwave) 
    open(10,file=nmlfile,status='OLD')
    read(10,nml=initwave) ; write(6,nml=initwave) ; close(10)

    if (abs(KInitial) .gt. km ) then
       call MessageNotify('E','set_initial_values',&
            'abs(KInitial) should be smaller than km' ) 
    endif
    if (abs(LInitial) .gt. lm ) then
       call MessageNotify('E','set_initial_values',&
            'abs(LInitial) should be smaller than lm' ) 
    endif

    ee_Vor = 0.0D0
    ee_Vor(LInitial, KInitial) = Amplitude

    ee_VorInit = ee_Vor

  end subroutine set_initial_values

!=========================== �ꥹ�����Ƚ��� ============================
 !
 ! �ꥹ�����Ƚ��Ͻ����
 !
  subroutine output_restart_init
    call HistoryCreate( &
           file=trim(rst_file), &
           title=trim(title), &
           source='plbaro-beta_rn4expcs_test1.f90 (2005/10/31)', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'x','y','k','l','t'/), &
           dimsizes=(/im,jm,2*km+1,2*lm+1,0/),&
           longnames=(/'X           ','Y           ',&
                       'X-wavenumber','Y-wavenumber',&
                       'time        '/),&
           units=(/'1','1','1','1','1'/),   &
           origin=real(time), interval=real(rst_intstep*delta_t), &
           xtypes=(/'real'/), history=hst_rst)

   !---- ��ɸ�ѿ����, ���� ----
    call HistoryPut('x',x_X, hst_rst)                       ! �ѿ�����
    call HistoryAddattr('x','topology','circular', hst_rst) ! ����°��
    call HistoryAddattr('x','modulo',XLength, hst_rst)      ! ����°��
    call HistoryPut('y',y_Y, hst_rst)                       ! �ѿ�����
    call HistoryAddattr('y','topology','circular', hst_rst) ! ����°��
    call HistoryAddattr('y','modulo',YLength, hst_rst)      ! ����°��

    call HistoryPut('k',(/(dble(k),k=-km,km)/), hst_rst)    ! �ѿ�����
    call HistoryPut('l',(/(dble(l),l=-lm,lm)/), hst_rst)    ! �ѿ�����

    call HistoryAddVariable( &                              ! �ѿ����
           varname='x_weight', dims=(/'x'/), & 
           longname='Weight for integration in X', &
           units='1', xtype='double',history=hst_rst)
    call HistoryPut('x_weight',x_X_Weight,hst_rst)          ! �ѿ�����

    call HistoryAddVariable( &                              ! �ѿ����
           varname='y_weight', dims=(/'y'/), & 
           longname='Weight for integration in Y', &
           units='1', xtype='double',history=hst_rst)
    call HistoryPut('y_weight',y_Y_Weight,hst_rst)          ! �ѿ�����


   !---- ʪ���ѿ���� ----
    call HistoryAddVariable( &                              ! �ѿ����
           varname='ee_vor', dims=(/'l','k','t'/), & 
           longname='Vorticity', &
           units='1', xtype='double', history=hst_rst)

   !---- �¸��ѥ�᥿����°���Ȥ������, ����(���� Global °��) ----
    call HistoryAddAttr('x','+delta_t', delta_t ,hst_rst)
    call HistoryAddAttr('x','+Beta',    Beta    ,hst_rst)
    call HistoryAddAttr('x','+HVOrder', HVOrder ,hst_rst)
    call HistoryAddAttr('x','+HVisc',   HVisc   ,hst_rst)

  end subroutine output_restart_init

 !
 ! �ꥹ�����Ƚ���
 !
  subroutine output_restart
    write(6,*) ' Restart file output at it = ',it, '  time = ', time
    call HistoryPut('t',real(time),hst_rst)

   !---- ʪ���ѿ����� ----
    call HistoryPut('ee_vor', ee_Vor, hst_rst)
  end subroutine output_restart

 !
 ! �ꥹ�����Ƚ��Ͻ�λ
 !
  subroutine output_restart_close
    call HistoryClose(hst_rst)
  end subroutine output_restart_close


!=========================== �ҥ��ȥ꡼���� ============================
 !
 ! �ҥ��ȥ꡼���Ͻ����
 !
  subroutine output_history_init

    call HistoryCreate( &
           file=trim(hst_file), &
           title=trim(title), &
           source='plbaro-beta_rn4expcs_test1.f90 (2005/10/31)', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'x','y','k','l','t'/), &
           dimsizes=(/im,jm,2*km+1,2*lm+1,0/),&
           longnames=(/'X           ','Y           ',&
                       'X-wavenumber','Y-wavenumber',&
                       'time        '/),&
           units=(/'1','1','1','1','1'/),   &
           origin=real(time), interval=real(rst_intstep*delta_t), &
           xtypes=(/'real'/))

   !---- ��ɸ�ѿ����, ���� ----
    call HistoryPut('x',x_X )                               ! �ѿ�����
    call HistoryAddattr('x','topology','circular')          ! ����°��
    call HistoryAddattr('x','modulo',XLength )              ! ����°��
    call HistoryPut('y',y_Y )                               ! �ѿ�����
    call HistoryAddattr('y','topology','circular')          ! ����°��
    call HistoryAddattr('y','modulo',YLength )              ! ����°��

    call HistoryPut('k',(/(dble(k),k=-km,km)/) )            ! �ѿ�����
    call HistoryPut('l',(/(dble(l),l=-lm,lm)/) )            ! �ѿ�����

    call HistoryAddVariable( &                              ! �ѿ����
           varname='x_weight', dims=(/'x'/), & 
           longname='Weight for integration in X', &
           units='1', xtype='double')
    call HistoryPut('x_weight',x_X_Weight)                  ! �ѿ�����

    call HistoryAddVariable( &                              ! �ѿ����
           varname='y_weight', dims=(/'y'/), & 
           longname='Weight for integration in Y', &
           units='1', xtype='double')
    call HistoryPut('y_weight',y_Y_Weight)                  ! �ѿ�����


   !---- �ѥ�᥿�����, ���� ----
    call HistoryAddVariable( &                                ! �ѿ����
           varname='hvisc', dims=(/'l','k'/), & 
           longname='hyper diffusivity', units='1', xtype='double')

    call HistoryPut('hvisc',ee_HVisc)  ! �ѿ�����

   !---- ʪ���ѿ���� ----
    call HistoryAddVariable( &                                ! �ѿ����
           varname='vor', dims=(/'x','y','t'/), & 
           longname='Vorticity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='strfunc', dims=(/'x','y','t'/), & 
           longname='Stream function', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='velx', dims=(/'x','y','t'/), & 
           longname='x-velocity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='vely', dims=(/'x','y','t'/), & 
           longname='y-velocity', units='1', xtype='double')

    call HistoryAddVariable( &                                ! �ѿ����
           varname='vor_analytic', dims=(/'x','y','t'/), & 
           longname='Vorticity(analytic)', units='1', xtype='double')

   !---- ��������� ----
    call HistoryAddVariable( &                                ! �ѿ����
           varname='ek', dims=(/'t'/), & 
           longname='mean kinetic energy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='ens', dims=(/'t'/), & 
           longname='mean enstrophy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='ektot', dims=(/'t'/), & 
           longname='total kinetic energy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='enstot', dims=(/'t'/), & 
           longname='total enstrophy', units='1', xtype='double')

    call HistoryAddVariable( &                                ! �ѿ����
           varname='l2vor', dims=(/'t'/), & 
           longname='vorrticity error (type2)', &
           units='1', xtype='double')

  !---- �¸��ѥ�᥿����°���Ȥ������, ����(���� Global °��) ----
    call HistoryAddAttr('x','+delta_t', delta_t )
    call HistoryAddAttr('x','+Beta',    Beta    )
    call HistoryAddAttr('x','+HVOrder', HVOrder )
    call HistoryAddAttr('x','+HVisc',   HVisc   )

  end subroutine output_history_init

 !
 ! �ҥ��ȥ꡼����
 !
  subroutine output_history
    write(6,*) ' History file output at it = ',it, '  time = ', time
    call HistoryPut('t',real(time))

   !---- ʪ���ѿ����� ----
    yx_Vor = yx_ee(ee_Vor)
    yx_StrFunc = yx_ee(ee_StrFunc)
    yx_VelX = -yx_ee(ee_Dy_ee(ee_StrFunc))
    yx_VelY = yx_ee(ee_Dx_ee(ee_StrFunc))

    ee_VorAnalytic = 0.0D0
    do l=-lm,lm
       do k=1,km
          OmegaR = - Beta * (2*PI*k/XLength) &
                   / ( (2*PI*k/XLength)**2 + (2*PI*l/YLength)**2 )

          NuP = HVisc *( (2*PI*k/XLength)**2 + (2*PI*l/YLength)**2 )**HVOrder
          ee_VorAnalytic(l, k)   &
               = ee_VorInit(l,k) * cos(OmegaR*time) * exp(-NuP*time) &
                +ee_VorInit(-l,-k) * sin(OmegaR*time) * exp(-NuP*time) 
          ee_VorAnalytic(-l, -k) &
               = -ee_VorInit(l,k) * sin(OmegaR*time) * exp(-NuP*time) &
                 +ee_VorInit(-l,-k) * cos(OmegaR*time) * exp(-NuP*time)
       enddo
       NuP = HVisc *( (2*PI*l/YLength)**2 )**HVOrder
       ee_VorAnalytic(l, 0)  = ee_VorInit(l,0) * exp(-NuP*time)
    enddo
    yx_VorAnalytic = yx_ee(ee_VorAnalytic)

    call HistoryPut('velx',transpose(yx_VelX))
    call HistoryPut('vely',transpose(yx_VelY))
    call HistoryPut('vor',transpose(yx_Vor))
    call HistoryPut('strfunc',transpose(yx_StrFunc))
    call HistoryPut('vor_analytic',transpose(yx_VorAnalytic))

   !---- �����̽��� ----
    call HistoryPut('ek',  AvrYX_yx((yx_VelX**2+yx_VelY**2)/2.0d0))
    call HistoryPut('ens', AvrYX_yx(yx_Vor**2/2.0d0))
    call HistoryPut('ektot', IntYX_yx((yx_VelX**2+yx_VelY**2)/2.0d0))
    call HistoryPut('enstot', IntYX_yx(yx_Vor**2/2.0d0))

    call HistoryPut('l2vor', &
                    sqrt(AvrYX_yx((yx_Vor-yx_VorAnalytic)**2)&
                         /AvrYX_yx(yx_VorAnalytic**2)))

  end subroutine output_history

 !
 ! �ҥ��ȥ꡼���Ͻ�λ
 !
  subroutine output_history_close
    call HistoryClose
  end subroutine output_history_close

end program plbaro_beta_rn4expcs_test1