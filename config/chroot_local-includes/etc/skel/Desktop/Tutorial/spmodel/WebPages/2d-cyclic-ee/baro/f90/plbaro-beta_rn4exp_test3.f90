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
!       The viscous linear term is transformed by the exponential function.  
!       The Program is organized for test3 (test for modon solution). 
!       This program should be complied with Fujitsu Fortran90 compiler 
!    and linked with SSL2 library since the subroutines for calculation 
!    of Bessel functions are used. 
!
!History(����):
!   2005/10/28  �ݹ�����, ��������
!   2005/10/31  �ݹ�����, k,l �������� bug fix
!
! The governing equation(����������): 
!
!      d\nabla^2\psi/dt 
!          = -J(\psi,\nabla^2\psi) - beta d\psi/dx
!            +(-1)^{p+1}\nu_{2p}\nabla^2p \nabla^2\psi
!
program plbaro_beta_rn4exp_test3

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

  real(8), allocatable :: ee_Vortmp(:,:)      ! ����(��󥲥��å��׻������)
  real(8), allocatable :: ee_k1(:,:)          ! �Ȼ��ʳ��α��ٻ����Ѳ�(1����)
  real(8), allocatable :: ee_k2(:,:)          ! �Ȼ��ʳ��α��ٻ����Ѳ�(2����)
  real(8), allocatable :: ee_k3(:,:)          ! �Ȼ��ʳ��α��ٻ����Ѳ�(3����)
  real(8), allocatable :: ee_k4(:,:)          ! �Ȼ��ʳ��α��ٻ����Ѳ�(4����)

  real(8), allocatable :: ee_VorAnalytic(:,:) ! ����(��ľ��ʬ)���ϲ�

 !---- ����ѥ�᥿ -----
  real(8), parameter  :: pi = 3.141592653589793D0 ! �߼�Ψ
  character(len=20)   :: DbgMessageFmt='*** DbgMESSAGE ***'
  real(8), parameter  :: vmiss = -999.0       ! ��»��

 !---- NAMELIST �ѿ� ----
  ! NAMELIST �����ѥƥ�ݥ��ե�����̾(̾���ѹ��ػ�)
  character(len=30),parameter  :: nmlfile='plbaro-beta_rn4exp_test3.nml'

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

                                                 !---- ����� 1 ��ɥ�
  real(8) :: Radius = 1.0                        ! ��ɥ�Ⱦ��
  real(8) :: Speed  = 1.0                        ! ��ɥ�����®��
  real(8) :: XCenter = 0.5                       ! ����濴���� X ��ɸ
  real(8) :: YCenter = 0.5                       ! ����濴���� Y ��ɸ

  namelist /modon/ Radius, Speed, XCenter, YCenter

 !---- ����ѿ� ----
  real(8), allocatable :: ee_HVisc(:,:)      ! ĶǴ������

  integer :: it=0                            ! ���֥��ƥå�
  real(8) :: time                            ! ��ǥ������
  integer :: k, l                            ! �ȿ�
  integer :: i, j                            ! ��ɸ�ѿ�����ǥå���

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

  if (verbose) write(6,nml=modon) 
  open(10,file=nmlfile,status='OLD')
  read(10,nml=modon) ; write(6,nml=modon) ; close(10)

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

  allocate(ee_k1(-lm:lm,-km:km),ee_k2(-lm:lm,-km:km))
  allocate(ee_k3(-lm:lm,-km:km),ee_k4(-lm:lm,-km:km))

  allocate(ee_HVisc(-lm:lm,-km:km))
  allocate(ee_VorAnalytic(-lm:lm,-km:km))

 !------------------ ��ɸ�ͤ����� -----------------------
  call DbgMessage(fmt='call %c', c1='ee_initial') 
  call ee_Initial(im,jm,km,lm,0.0D0,XLength,0.0D0,YLength)

 !------------------ ʪ������������ -----------------------
  ee_Vor = 1.0D0
  ee_HVisc = (-1)**HVOrder * HVisc*(ee_Lapla_ee(ee_Vor))**HVOrder

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
     ee_k1 = ee_DVorDt_ee_ee(ee_Vor,ee_Strfunc)

     !---- 2 ���� [ k2 = f( x_n+k1*dt/2, t_n+dt/2 ) ] ----
     ee_Vortmp = exp(-ee_HVisc*delta_t/2) * ( ee_Vor + ee_k1 * delta_t/2.0D0 )
     ee_StrFunc = ee_LaplaInv_ee(ee_Vortmp)
     ee_k2 = exp(ee_HVisc*delta_t/2) * ee_DVorDt_ee_ee(ee_Vortmp,ee_Strfunc)

     !---- 3 ���� [ k3 = f( x_n+k2*dt/2, t_n+dt/2 ) ] ----
     ee_Vortmp = exp(-ee_HVisc*delta_t/2) * ( ee_Vor + ee_k2 * delta_t/2.0D0 )
     ee_StrFunc = ee_LaplaInv_ee(ee_Vortmp)
     ee_k3 = exp(ee_HVisc*delta_t/2) * ee_DVorDt_ee_ee(ee_Vortmp,ee_Strfunc)

     !---- 4 ���� [ k4 = f( x_n+k3*dt, t_n+dt ) ] ----
     ee_Vortmp = exp(-ee_HVisc*delta_t) * ( ee_Vor + ee_k3 * delta_t )
     ee_StrFunc = ee_LaplaInv_ee(ee_Vortmp)
     ee_k4 = exp(ee_HVisc*delta_t) * ee_DVorDt_ee_ee(ee_Vortmp,ee_Strfunc)

     !---- ��ʬ ----
     ee_Vor = exp(-ee_HVisc*delta_t) &
          * ( ee_Vor + delta_t * (   ee_k1/6.0D0 + ee_k2/3.0D0 &
                                   + ee_k3/3.0D0 + ee_k4/6.0D0 )  )

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

! �ʾ� �ᥤ��ץ���� 
!-----------------------------------------------------------------------------
! �ʲ� ���֥롼����

contains
 !======================= �����Ѳ�(�����ʤ�) ====================

  function ee_DVorDt_ee_ee(ee_Vor,ee_Strfunc)

    real(8) :: ee_Vor(:,:)                             ! ����(��ľ��ʬ)
    real(8) :: ee_StrFunc(:,:)                         ! ή���ؿ�

    ! ���ٻ����Ѳ�(����ʤ�)
    real(8) :: ee_DVorDt_ee_ee(size(ee_Vor,1),size(ee_Vor,2))

    ee_DVorDt_ee_ee = - ee_Jacobian_ee_ee(ee_StrFunc,ee_Vor)  &
                      - Beta * ee_Dx_ee(ee_StrFunc)           

  end function ee_DVorDt_ee_ee

!=========================== ��ɥ�� ============================
 !
 !  ��ɥ��������
 !
  subroutine set_VorMod( &
                    yx_VorMod,  & !(out) ��ɥ����       
                    Ra,         & !(in)  ��ɥ��Ⱦ��       
                    Cs,         & !(in)  ��ɥ������®��   
                    Xc,         & !(in)  ��ɥ���濴 X ��ɸ
                    Yc          & !(in)  ��ɥ���濴 Y ��ɸ
                   )

    real(8), intent(out):: yx_VorMod(0:,0:)     ! ��ɥ����
    real(8), intent(in) :: Ra                   ! ��ɥ��Ⱦ��
    real(8), intent(in) :: Cs                   ! ��ɥ������®��
    real(8), intent(in) :: Xc                   ! ��ɥ���濴 X ��ɸ
    real(8), intent(in) :: Yc                   ! ��ɥ���濴 Y ��ɸ

    ! �����ѥ�᥿��
    real(8),parameter :: eps=1.0D-10            ! K �����뤿����Ǥ������

    ! ����ѿ�
    real(8) :: Kp                  ! ��ɥ��ѥ�᥿��
    real(8) :: Qp                  ! ��ɥ��ѥ�᥿��
    real(8) :: J1KR, J1K           ! �� 1 ��٥å���ؿ�����
    real(8) :: K1KR, K1K           ! �� 2 ���ѷ��٥å���ؿ�����
    real(8) :: Rad                 ! �濴�����Ⱦ��
    real(8) :: SinTheta            ! �濴�˺�ɸ�� \sin\theta
    integer :: ICON                ! ���֥롼���󥨥顼������
    real(8) :: DX, DY              ! ����ѿ�

    Qp = Ra*sqrt(Beta/Cs)
    Kp = Modon_Kparameter(Qp,eps)

    call dbj1(Kp,J1K,ICON)
    call dbk1(Qp,K1K,ICON)
    do i=0,im-1
       do j=0,jm-1
          if ( yx_X(j,i)-mod(Xc,XLength) .LT. -XLength/2 )then 
             DX=yx_X(j,i)+XLength-mod(Xc,XLength)
          else if ( yx_X(j,i)-mod(Xc,XLength) .GT. XLength/2 )then 
             DX=yx_X(j,i)-XLength-mod(Xc,XLength)
          else
             DX=yx_X(j,i)-mod(Xc,XLength)
          endif
          if ( yx_Y(j,i)-mod(Yc,YLength) .LT. -YLength/2 )then 
             DY=yx_Y(j,i)+YLength-mod(Yc,YLength)
          else if ( yx_Y(j,i)-mod(Yc,YLength) .GT. YLength/2 )then 
             DY=yx_Y(j,i)-YLength-mod(Yc,YLength)
          else
             DY=yx_Y(j,i)-mod(Yc,YLength)
          endif

          Rad   = sqrt( DX**2 + DY**2 )
          if ( Rad .NE. 0.0D0 ) then
             SinTheta = DY/Rad
          else
             SinTheta = 0.0D0
          endif
          if ( Rad .LE. Ra ) then
             call dbj1(Kp*Rad/Ra,J1KR,ICON)
             yx_VorMod(j,i) = - Cs * Qp**2/Ra * SinTheta * J1KR/J1K
          else
             call dbk1(Qp*Rad/Ra,K1KR,ICON)
             yx_VorMod(j,i) = - Cs * Qp**2/Ra * SinTheta * K1KR/K1K
          endif
       enddo
    enddo

  end subroutine set_VorMod
  !
  ! Solving Modon dispersion relation.                S.Takehiro Oct, 21, 2005.
  !
  ! Calculating parameter k by giving the value of parameter q
  ! with the relation,  
  !      -J_2(k)/kJ_1(k) = K_2(q)/qK_1(q). 
  ! The bi-section method is used in the function. 
  !
  function Modon_Kparameter(Qp,eps)

    real(8), intent(in) :: Qp                      ! �ѥ�᥿�� q
    real(8), intent(in) :: eps                     ! �Ǥ�����Ƚ���
    real(8)             :: Modon_Kparameter        ! �ѥ�᥿�� k

    ! ����ѿ�
    real(8) :: k1 = 3.83171       ! ����� J_1 �������
    real(8) :: k2 = 5.13562       ! ����� J_2 �������
    real(8) :: kp                 ! �ѥ�᥿��ͽ����

    real(8) :: J1K, J2K           ! �� 1 ��٥å���ؿ�����
    real(8) :: K1Q, K2Q           ! �� 2 ���ѷ��٥å���ؿ�����
    integer :: ICON               ! ���֥롼���󥨥顼������
    real(8) :: f1, f2, fp         ! ����ѿ�

  1000 continue  
    call dbk1(Qp,K1Q,ICON)
    call dbkn(Qp,2,K2Q,ICON)

    call dbj1(k1,J1K,ICON)
    call dbjn(k1,2,J2K,ICON)

    f1 = J2K + K2Q/(Qp*K1Q)*k1*J1K   ! J_2(k) + K_2(q)/qK_1(q) * kJ_1(k) =0

    call dbj1(k2,J1K,ICON)
    call dbjn(k2,2,J2K,ICON)
    f2 = J2K + K2Q/(Qp*K1Q)*k2*J1K   ! J_2(k) + K_2(q)/qK_1(q) * kJ_1(k) =0

    if ( f1*f2 .gt. 0.0D0 ) then
       write(6,*) 'f1 and f2 are the same sign.'
       stop
    endif

  !  kp = (abs(f1)*k2 + abs(f2)*k1)/abs(f1-f2)
    kp = (k1+k2)/2

    call dbj1(kp,J1K,ICON)
    call dbjn(kp,2,J2K,ICON)
    fp = J2K + K2Q/(Qp*K1Q)*kp*J1K   ! J_2(k) + K_2(q)/qK_1(q) * kJ_1(k) =0

    if ( abs(k1-k2)/abs(k1) .lt. eps ) then
        Modon_Kparameter=kp
        return
    end if

    if ( f1*fp .gt. 0.0D0 ) then
       k1 = kp
    else
       k2 = kp
    endif

    goto 1000

 end function Modon_Kparameter

!=========================== ��������� ============================
 !
 !  ���������(�ꥹ�����ȥե�����ʤ����Υǥե��������)
 !
  subroutine set_initial_values    ! w_Vor ������

    call set_VorMod( &
                    yx_Vor,     & !(out) ��ɥ����       
                    Radius,     & !(in)  ��ɥ��Ⱦ��       
                    Speed,      & !(in)  ��ɥ������®��   
                    XCenter,    & !(in)  ��ɥ���濴 X ��ɸ
                    YCenter     & !(in)  ��ɥ���濴 Y ��ɸ
                   )
    
    ee_Vor = ee_yx(yx_Vor)

  end subroutine set_initial_values

!=========================== �ꥹ�����Ƚ��� ============================
 !
 ! �ꥹ�����Ƚ��Ͻ����
 !
  subroutine output_restart_init
    call HistoryCreate( &
           file=trim(rst_file), &
           title=trim(title), &
           source='plbaro-beta_rn4exp_test3.f90 (2005/10/31)', &
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
           source='plbaro-beta_rn4exp_test3.f90 (2005/10/31)', &
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
    call set_VorMod( &
                    yx_VorAnalytic,     & !(out) ��ɥ����       
                    Radius,             & !(in)  ��ɥ��Ⱦ��       
                    Speed,              & !(in)  ��ɥ������®��   
                    XCenter+Speed*time, & !(in)  ��ɥ���濴 X ��ɸ
                    YCenter             & !(in)  ��ɥ���濴 Y ��ɸ
                   )
    yx_VorAnalytic = yx_ee(ee_yx(yx_VorAnalytic))

    yx_Vor = yx_ee(ee_Vor)
    yx_StrFunc = yx_ee(ee_StrFunc)
    yx_VelX = -yx_ee(ee_Dy_ee(ee_StrFunc))
    yx_VelY = yx_ee(ee_Dx_ee(ee_StrFunc))

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

end program plbaro_beta_rn4exp_test3
