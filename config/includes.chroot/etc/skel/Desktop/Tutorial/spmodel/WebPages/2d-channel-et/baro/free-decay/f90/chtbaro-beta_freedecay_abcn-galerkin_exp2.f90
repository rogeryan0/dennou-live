!--------------------------------------------------------------------------
!  Copyright (C) 2008 SPMODEL Development Group. All rights reserved.
!--------------------------------------------------------------------------
!
!Title(ɽ��): Two-dimensional barotropic model on a channel region
!             (2 ������ϩ�ΰ���2�����簵��ǥ�)
!
!       The time integration is performed with the 2nd order Runge-Kutta
!       and Clank-Nicolson scheme. 
!       The beta parameter depends on y. 
!       Program is organized for free-decay turbulence problem, exp1. 
!       The initial energy is concentrated in a band of total wavenumbers.
!
!History(����):
!   2008/10/16  �ݹ�����, plbaro ����¤
!
! The governing equation(����������): 
!
!      d\nabla^2\psi/dt 
!          = -J(\psi,\nabla^2\psi) - beta d\psi/dx
!            +(-1)^{p+1}\nu_{2p}\nabla^2p \nabla^2\psi
!
!      \psi = d^2\psi/dy^2 = 0 at y=y_0,y_1 (free-slip)
!      \psi = d\psi/dy = 0 at y=y_0,y_1     (no-slip)
!
program chtbaro_beta_freedecay_abcn_galerkin_exp2

!== �⥸�塼�������� ================================================
  use lumatrix
  use et_module
  use et_galerkin_module
  use gt4_history, only : GT_HISTORY, HistoryCreate, HistoryPut, HistoryGet, &
                          HistoryAddVariable, HistoryClose, HistoryAddAttr
  use dc_trace,    only : SetDebug, BeginSub, EndSub, DbgMessage
  use dc_message,  only : MessageNotify
  use dc_types
  use dc_string,   only : StoA
  use dc_args
  implicit none

!== ����� ============================================================

 !---- �ѿ�(�ʻ����ǡ���) ----
  real(8), allocatable :: yx_VelX(:,:)        ! ®�ٷ�����ʬ
  real(8), allocatable :: yx_VelY(:,:)        ! ®�ٰ�����ʬ
  real(8), allocatable :: yx_Vor(:,:)         ! ����(��ľ��ʬ)
  real(8), allocatable :: yx_StrFunc(:,:)     ! ή���ؿ�

  real(8), allocatable :: yx_Beta(:,:)        ! �¥ѥ�᥿��
  real(8), allocatable :: yx_FCor(:,:)        ! ���ꥪ��ѥ�᥿��

 !---- �ѿ�(���ڥ��ȥ�ǡ���) ----
  real(8), allocatable :: et_Vor(:,:)         ! ����(��ľ��ʬ)
  real(8), allocatable :: et_StrFunc(:,:)     ! ή���ؿ�

  real(8), allocatable :: et_DVorDt1(:,:)     ! ���ٻ����Ѳ�(t)
  real(8), allocatable :: et_DVorDt0(:,:)     ! ���ٻ����Ѳ�(t-��t)

  real(8), allocatable :: eq_Strfunc(:,:)      ! ���顼����ǡ���(ή��)

 !--- Crank Nicholson ��Ū�׻��Ѻ���ѿ� ---
  real(8), allocatable :: DifLUMT(:,:,:)      ! Crank Nicholson ��Ū�׻��ѹ���
  integer, allocatable :: kpivot(:,:)         ! Crank Nicholson �ѹ���ԥܥåȾ���

 !---- ����ѥ�᥿ -----
  real(8), parameter  :: pi = 3.141592653589793D0 ! �߼�Ψ
  character(len=20)   :: DbgMessageFmt='*** DbgMESSAGE ***'
  real(8), parameter  :: vmiss = -999.0       ! ��»��

 !---- NAMELIST �ѿ� ----
  ! ���ޥ�ɥ饤��������ѿ�
  type(ARGS) :: arg
  character(STRING), pointer :: argv(:) => null()

  ! NAMELIST �����ѥǥե���ȥե�����̾
  character(len=100)  :: nmlfile='chsbaro-beta_freedecay_abcn-galerkin_exp2.nml'

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
                                                 ! ��ʿ��ץ饷����γ���)
  real(8)            :: Visc=1.0D0               ! Ǵ������
  character(len=2)   :: velBC='RR'               ! ή�춭�����(RR/RS/SR/SS)
  namelist /physics/  XLength, YLength, Beta, Visc, velBC
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
   'Test of linear terms of 2-dim barotropic model on a channel domain'

  integer :: hst_intstep=200                     ! �ҥ��ȥ꡼���ϴֳ֥��ƥå׿�
  namelist /history/    hst_file, title, hst_intstep

  character(len=100) :: rst_file=''              ! �ꥹ�����Ƚ��ϥե�����̾
  integer :: rst_intstep=200                     ! �ꥹ�����Ƚ��ϴֳ֥��ƥå׿�
  namelist /restart/   rst_file, rst_intstep 

 !---- ����ѿ� ----
  integer :: it=0                            ! ���֥��ƥå�
  real(8) :: time                            ! ��ǥ������
  integer :: k, l                            ! �ȿ�

  type(GT_HISTORY) :: hst_rst                ! �ꥹ������ GT_HISTORY �ѿ�

 !---------------- NAMELIST �ɤ߹��� ---------------------
  call Open(arg)
  call Debug(arg) ; call Help(arg) ; call Strict(arg)
  call Get(arg, argv)
  if ( size(argv) .le. 0 ) then
     call MessageNotify('W','chsbaro-beta_abcn_exp2',&
                            'There is no argument. The defalut namlist file, '&
                            //trim(nmlfile)//' is used.')
  else
     nmlfile=argv(1)
     call MessageNotify('M','chsbaro-beta_abcn_exp2',&
                            'Namlist file is '//trim(nmlfile))
  endif
  deallocate(argv)
  call Close(arg)

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
  allocate(yx_VelX(0:jm,0:im-1),yx_VelY(0:jm,0:im-1))
  allocate(yx_Vor(0:jm,0:im-1),yx_StrFunc(0:jm,0:im-1))
  allocate(yx_Beta(0:jm,0:im-1),yx_FCor(0:jm,0:im-1))

  allocate(et_Vor(-km:km,0:lm),et_StrFunc(-km:km,0:lm))
  allocate(et_DVorDt1(-km:km,0:lm),et_DVorDt0(-km:km,0:lm))
  allocate(eq_Strfunc(-km:km,4:lm))

  allocate(DifLUMT(-km:km,4:lm,4:lm),kpivot(-km:km,4:lm))

 !------------------ ��ɸ�ͤ����� -----------------------
  call DbgMessage(fmt='call %c', c1='et_initial') 
  call et_galerkin_Initial(im,jm,km,lm,0.0D0,XLength,0.0D0,YLength,VelBC=VelBC)

  call CNDiffusionMatrixStrm( Visc, delta_t, DifLUMT, kpivot )

 !------------------ ʪ������������ -----------------------
  yx_Beta = Beta * sin(PI*yx_Y/YLength)
  yx_FCor = - Beta * YLength/PI * cos(PI*yx_Y/YLength)

 !------------------- ��������� ----------------------
  time = initial_time

  if ( initial_file == "") then
     ! �ꥹ�����ȥե��������ꤷ�ʤ���������� et_Vor ��Ϳ����. 
     call set_initial_values
  else
     !  ���������(�ꥹ�����ȥե����뤫����ɤߤ���)
     call HistoryGet( trim(initial_file), 'et_vor', et_Vor,  time )
  endif

  eq_Strfunc = eq_vor2strm_et(et_Vor)
  et_Strfunc = et_eq(eq_Strfunc)
  et_Vor = et_Lapla_et(et_eq(eq_Strfunc))

 !-------------- ������ʬ(Runge-Kutta & Crakn Nicolson ˡ) ---------------
  call output_restart_init
  call output_history_init

  if ( initial_file == '' ) call output_history    ! ������Ϳ��������ͤϽ���

  call DbgMessage(fmt='%c %c', &
&                 c1=DbgMessageFmt, &
&                 c2='Time integration starts.') 

  et_DVorDt1 = - et_Jacobian_et_et(et_StrFunc,et_Vor)  &
               - et_yx(yx_Beta*yx_et(et_Dx_et(et_StrFunc)))

  do it=1,nstep
     time = initial_time + it * delta_t

     ! �����Ѳ���θ��Ѥ�
     et_DVorDt0 = et_DVorDt1
     et_DVorDt1 = - et_Jacobian_et_et(et_StrFunc,et_Vor)  &
                  - et_yx(yx_Beta*yx_et(et_Dx_et(et_StrFunc)))

     ! �Ȼ��ʳ��ι�������ʬ(Adams-Bashforth scheme)
     et_Vor = et_Vor + delta_t * &
               (3.0d0/2.0d0 * et_DVorDt1 - 1.0d0/2.0d0 * et_DVorDt0)

     ! �Ȼ���λ�����ʬ�׻�(Clank-Nicolson scheme)
     et_Vor = et_Vor + delta_t/2 *( Visc*et_Lapla_et(et_Vor) )
     eq_Strfunc = eq_Vor2Strm_et(et_Vor)

     eq_Strfunc = LuSolve(DifLUMT,kpivot,eq_Strfunc)
     et_Strfunc = et_eq(eq_Strfunc)
     et_Vor = et_Lapla_et(et_Strfunc)
  
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

!=========================== ��������� ============================
 !
 !  ���������(�ꥹ�����ȥե�����ʤ����Υǥե��������)
 !
  subroutine set_initial_values    ! et_Vor ������

   !---- ��������ȿ����ΰ�����ʬ��
    real(8)              :: Ntotal             ! ���ȿ�
    integer              :: random_seed_size   ! ����μ��Ĺ��
    integer, allocatable :: seedarray(:)       ! ����μ�
    real                 :: harvest            ! �������

    integer    :: Seed=0           ! seed(1)�����ꤹ������
    real(8)    :: Nmin=7.0D0       ! ������ͥ륮��ʬ�ۤ����ȿ��ΰ��Ǿ���
    real(8)    :: Nmax=9.0D0       ! ������ͥ륮��ʬ�ۤ����ȿ��ΰ������
    real(8)    :: Etotal=1.0D0     ! ���ʿ�ѥ��ͥ륮������

    namelist /initvalue/ Seed, Nmin, Nmax, Etotal
    
    real(8)    :: kk, ll

    if (verbose) write(6,nml=initvalue) 
    open(10,file=nmlfile,status='OLD')
    read(10,nml=initvalue) ; write(6,nml=initvalue) ; close(10)

    if ( Nmax .gt. km ) then
       call MessageNotify('E','set_initial_values',&
            'Nmax should be smaller equalt to  sqrt(km^2 + lm^2)' ) 
    endif
    if ( Nmin .lt. 0 .OR. Nmax .lt. 0 ) then
       call MessageNotify('E','set_initial_values',&
            'Nmin and Nmax should be larger equal to zero' ) 
    endif

    ! �������
    call random_seed(size=random_seed_size)
    allocate(seedarray(random_seed_size))
    call random_seed(get=seedarray)
    seedarray(1)=Seed
    call random_seed(put=seedarray)

    ! ���ͥ륮�������ȿ� Ntotal �� Nmin <= Ntotal <= Nmax �ˤФ�ޤ�
    yx_Strfunc = 0.0d0
    do k=-km,km
       do l=0,lm
          Ntotal = sqrt(k**2.0+l**2.0)
          if ( (Nmin .le. Ntotal) .AND. (Ntotal .le. Nmax) ) then
             call random_number(harvest)
             kk = 2*PI/XLength*k ; ll = PI/YLength*l
             yx_Strfunc= yx_Strfunc &
                  + cos(kk*yx_X + 2*PI*harvest ) &
                  * sin(ll*yx_Y)
          endif
       enddo
    enddo

   ! (ʿ��)���ͥ륮���򵬳ʲ�
    et_Strfunc=et_yx(yx_Strfunc)
    yx_VelX = -yx_et(et_Dy_et(et_StrFunc))
    yx_VelY = yx_et(et_Dx_et(et_StrFunc))
    et_StrFunc = et_StrFunc &
         * sqrt(Etotal/AvrYX_yx((yx_VelX**2+yx_VelY**2)/2.0d0))

    et_Vor = et_Lapla_et(et_Strfunc)

  end subroutine set_initial_values

!=========================== �Ȼ���(ή���ؿ���) ============================

  subroutine CNDiffusionMatrixStrm( Diffc, dt, DiffLUMatrix, kpivot )
    ! 
    ! Crank Nicholson �Ȼ���Ū�׻��ѹ��������
    ! (ή���ؿ���, ��^{-2}[��^2-D dt/2��^2��^2], LU ʬ��)
    ! 
    real(8), intent(IN)   :: Diffc    ! �Ȼ�����
    real(8), intent(IN)   :: dt       ! ���ֹ�

    ! Crank Nicholson �Ȼ���Ū�׻��ѹ���(��^{-2}(��^2-D (dt/2)��^4, LU ʬ��)
    real(8), intent(OUT)  :: DiffLUMatrix(-km:km,4:lm,4:lm)
    integer, intent(OUT)  :: kpivot(-km:km,4:lm)             ! �ԥܥåȾ���

    real(8)               :: eq_I(-km:km,4:lm)               ! ������ѿ�
    real(8)               :: et_I(-km:km,0:lm)               ! ������ѿ�

    integer :: l

    DiffLUMatrix = 0.0
    do l=4,lm
       eq_I = 0.0 ; eq_I(:,l) = 1.0             ! ���ȿ���ʬ��Ω
       et_I = et_eq(eq_I)
       et_I = et_Lapla_et(et_I)
       et_I = et_I - Diffc * dt/2.0 * et_Lapla_et(et_I)
       DiffLUMatrix(:,:,l) = eq_Vor2Strm_et(et_I)
    enddo

    call LuDeComp(DiffLUMatrix,kpivot)

  end subroutine CNDiffusionMatrixStrm

!=========================== �ꥹ�����Ƚ��� ============================
 !
 ! �ꥹ�����Ƚ��Ͻ����
 !
  subroutine output_restart_init
    call HistoryCreate( &
           file=trim(rst_file), &
           title=trim(title), &
           source='plbaro-beta_freedecay_abcn_exp2.f90 (2008/10/16)', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'x','y','k','l','t'/), &
           dimsizes=(/im,jm+1,2*km+1,lm+1,0/),&
           longnames=(/'X           ','Y           ',&
                       'X-wavenumber','Y-wavenumber',&
                       'time        '/),&
           units=(/'1','1','1','1','1'/),   &
           origin=real(time), interval=real(rst_intstep*delta_t), &
           history=hst_rst)

   !---- ��ɸ�ѿ����, ���� ----
    call HistoryPut('x',x_X, hst_rst)                       ! �ѿ�����
    call HistoryAddattr('x','topology','circular', hst_rst) ! ����°��
    call HistoryAddattr('x','modulo',XLength, hst_rst)      ! ����°��
    call HistoryPut('y',y_Y, hst_rst)                       ! �ѿ�����
    call HistoryAddattr('y','topology','circular', hst_rst) ! ����°��
    call HistoryAddattr('y','modulo',YLength, hst_rst)      ! ����°��

    call HistoryPut('k',(/(dble(k),k=-km,km)/), hst_rst)    ! �ѿ�����
    call HistoryPut('l',(/(dble(l),l=0,lm)/), hst_rst)    ! �ѿ�����

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
           varname='et_vor', dims=(/'l','k','t'/), & 
           longname='Vorticity', &
           units='1', xtype='double', history=hst_rst)

   !---- �¸��ѥ�᥿����°���Ȥ������, ����(���� Global °��) ----
    call HistoryAddAttr('x','+delta_t', delta_t ,hst_rst)
    call HistoryAddAttr('x','+Beta',    Beta    ,hst_rst)
    call HistoryAddAttr('x','+Visc',    Visc   ,hst_rst)

  end subroutine output_restart_init

 !
 ! �ꥹ�����Ƚ���
 !
  subroutine output_restart
    write(6,*) ' Restart file output at it = ',it, '  time = ', time
    call HistoryPut('t',real(time),hst_rst)

   !---- ʪ���ѿ����� ----
    call HistoryPut('et_vor', et_Vor, hst_rst)
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
           source='chsbaro-beta_freedecay_abcn-galerkin_exp2.f90 (2008/10/16)', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'x','y','k','l','t'/), &
           dimsizes=(/im,jm+1,2*km+1,lm+1,0/),&
           longnames=(/'X           ','Y           ',&
                       'X-wavenumber','Y-wavenumber',&
                       'time        '/),&
           units=(/'1','1','1','1','1'/),   &
           origin=real(time), interval=real(hst_intstep*delta_t))

   !---- ��ɸ�ѿ����, ���� ----
    call HistoryPut('x',x_X )                               ! �ѿ�����
    call HistoryAddattr('x','topology','circular')          ! ����°��
    call HistoryAddattr('x','modulo',XLength )              ! ����°��
    call HistoryPut('y',y_Y )                               ! �ѿ�����
    call HistoryAddattr('y','topology','circular')          ! ����°��
    call HistoryAddattr('y','modulo',YLength )              ! ����°��

    call HistoryPut('k',(/(dble(k),k=-km,km)/) )            ! �ѿ�����
    call HistoryPut('l',(/(dble(l),l=0,lm)/) )            ! �ѿ�����

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
           varname='beta', dims=(/'x','y'/), & 
           longname='Beta parameter', units='1', xtype='double')
    call HistoryPut('beta',transpose(yx_Beta))                ! �ѿ�����

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
           varname='pv', dims=(/'x','y','t'/), & 
           longname='potential vorticity', units='1', xtype='double')

  !---- �¸��ѥ�᥿����°���Ȥ������, ����(���� Global °��) ----
    call HistoryAddAttr('x','+delta_t', delta_t )
    call HistoryAddAttr('x','+Beta',    Beta    )
    call HistoryAddAttr('x','+Visc',   Visc   )

  end subroutine output_history_init

 !
 ! �ҥ��ȥ꡼����
 !
  subroutine output_history
    write(6,*) ' History file output at it = ',it, '  time = ', time
    call HistoryPut('t',real(time))

   !---- ʪ���ѿ����� ----
    yx_Vor = yx_et(et_Vor)
    yx_StrFunc = yx_et(et_StrFunc)
    yx_VelX = -yx_et(et_Dy_et(et_StrFunc))
    yx_VelY = yx_et(et_Dx_et(et_StrFunc))

    call HistoryPut('velx',transpose(yx_VelX))
    call HistoryPut('vely',transpose(yx_VelY))
    call HistoryPut('vor',transpose(yx_Vor))
    call HistoryPut('strfunc',transpose(yx_StrFunc))

   !---- �����̽��� ----
    call HistoryPut('ek',  AvrYX_yx((yx_VelX**2+yx_VelY**2)/2.0d0))
    call HistoryPut('ens', AvrYX_yx(yx_Vor**2/2.0d0))
    call HistoryPut('ektot', IntYX_yx((yx_VelX**2+yx_VelY**2)/2.0d0))
    call HistoryPut('enstot', IntYX_yx(yx_Vor**2/2.0d0))
    call HistoryPut('pv',transpose(yx_FCor+yx_Vor))

  end subroutine output_history

 !
 ! �ҥ��ȥ꡼���Ͻ�λ
 !
  subroutine output_history_close
    call HistoryClose
  end subroutine output_history_close

end program chtbaro_beta_freedecay_abcn_galerkin_exp2
