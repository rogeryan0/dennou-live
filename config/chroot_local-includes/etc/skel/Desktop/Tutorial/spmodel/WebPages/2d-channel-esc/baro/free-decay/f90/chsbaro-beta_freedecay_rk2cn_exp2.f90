!--------------------------------------------------------------------------
!  Copyright (C) 2008 SPMODEL Development Group. All rights reserved.
!--------------------------------------------------------------------------
!
!Title(ɽ��): Two-dimensional barotropic model on a channel region
!             (2 ������ϩ�ΰ���2�����簵��ǥ�)
!
!       The time integration is performed with the 2nd order 
!       2nd order Runge-Kutta scheme and Clank-Nicolson scheme. 
!       The beta parameter depends on y.
!       Program is organized for free-decay turbulence problem, exp2, 
!       The initial energy is concentrated in a band of total wavenumbers.
!
!History(����):
!   2008/10/13  �ݹ�����, plbaro ����¤
!
! The governing equation(����������): 
!
!      d\nabla^2\psi/dt 
!          = -J(\psi,\nabla^2\psi) - beta(y) d\psi/dx
!            +(-1)^{p+1}\nu_{2p}\nabla^2p \nabla^2\psi
!
!      \psi = d^2\psi/dy^2 = 0 at y=y_0,y_1
!
program chsbaro_beta_freedecay_rk2cn_exp2

!== �⥸�塼�������� ================================================
  use esc_module
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
  real(8), allocatable :: es_Vor(:,:)         ! ����(��ľ��ʬ)
  real(8), allocatable :: es_StrFunc(:,:)     ! ή���ؿ�

  real(8), allocatable :: es_Vortmp(:,:)      ! ����(t+��t/2)

 !---- ����ѥ�᥿ -----
  real(8), parameter  :: pi = 3.141592653589793D0 ! �߼�Ψ
  character(len=20)   :: DbgMessageFmt='*** DbgMESSAGE ***'
  real(8), parameter  :: vmiss = -999.0       ! ��»��

 !---- NAMELIST �ѿ� ----
  ! ���ޥ�ɥ饤��������ѿ�
  type(ARGS) :: arg
  character(STRING), pointer :: argv(:) => null()

  ! NAMELIST �����ѥǥե���ȥե�����̾
  character(len=100)  :: nmlfile='chsbaro-beta_freedecay_rk2cn_exp2.nml'

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
   'Test of linear terms of 2-dim barotropic model on a chanel domain'

  integer :: hst_intstep=200                     ! �ҥ��ȥ꡼���ϴֳ֥��ƥå׿�
  namelist /history/    hst_file, title, hst_intstep

  character(len=100) :: rst_file=''              ! �ꥹ�����Ƚ��ϥե�����̾
  integer :: rst_intstep=200                     ! �ꥹ�����Ƚ��ϴֳ֥��ƥå׿�
  namelist /restart/   rst_file, rst_intstep 

 !---- ����ѿ� ----
  real(8), allocatable :: es_HVisc(:,:)      ! ĶǴ������

  integer :: it=0                            ! ���֥��ƥå�
  real(8) :: time                            ! ��ǥ������
  integer :: k, l                            ! �ȿ�

  type(GT_HISTORY) :: hst_rst                ! �ꥹ������ GT_HISTORY �ѿ�

 !---------------- NAMELIST �ɤ߹��� ---------------------
  call Open(arg)
  call Debug(arg) ; call Help(arg) ; call Strict(arg)
  call Get(arg, argv)
  if ( size(argv) .le. 0 ) then
     call MessageNotify('W','chsbaro-beta_rk2cn_exp2',&
                            'There is no argument. The defalut namlist file, '&
                            //trim(nmlfile)//' is used.')
  else
     nmlfile=argv(1)
     call MessageNotify('M','chsbaro-beta_rk2cn_exp2',&
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

  allocate(es_Vor(-km:km,lm),es_StrFunc(-km:km,lm))
  allocate(es_Vortmp(-km:km,lm))
  allocate(es_HVisc(-km:km,lm))

 !------------------ ��ɸ�ͤ����� -----------------------
  call DbgMessage(fmt='call %c', c1='esc_initial') 
  call esc_Initial(im,jm,km,lm,0.0D0,XLength,0.0D0,YLength)

 !------------------ ʪ������������ -----------------------
  es_Vor = 1.0D0
  es_HVisc = (-1)**HVOrder * HVisc*(es_Lapla_es(es_Vor))**HVOrder

!!$  yx_Beta = Beta * (sin(2*PI*yx_Y/YLength)+1)/2.0
  yx_Beta = Beta * sin(PI*yx_Y/YLength)
  yx_FCor = - Beta * YLength/PI * cos(PI*yx_Y/YLength)

 !------------------- ��������� ----------------------
  time = initial_time

  if ( initial_file == "") then
     ! �ꥹ�����ȥե��������ꤷ�ʤ���������� es_Vor ��Ϳ����. 
!!$     call set_initial_values
     call set_initial_values_real
  else
     !  ���������(�ꥹ�����ȥե����뤫����ɤߤ���)
     call HistoryGet( trim(initial_file), 'es_vor', es_Vor,  time )
  endif

  es_Strfunc = es_LaplaInv_es(es_Vor)

 !------------- ������ʬ(Adams-Bashfort and Clank Nicolson ˡ) -----------
  call output_restart_init
  call output_history_init

  if ( initial_file == '' ) call output_history    ! ������Ϳ��������ͤϽ���

  call DbgMessage(fmt='%c %c', &
&                 c1=DbgMessageFmt, &
&                 c2='Time integration starts.') 

  do it=1,nstep
     time = initial_time + it * delta_t

     ! �����Ѳ���θ��Ѥ�

     ! �Ȼ��ʳ��ι�������ʬ(Runge Kutta 1 ����)
     es_Vortmp = es_Vor &
          + delta_t/2 *( -es_Jacobian_es_es(es_StrFunc,es_Vor)  &
                        - es_yx(yx_Beta * yx_es(es_Dx_es(es_StrFunc))) )
     
     es_StrFunc = es_LaplaInv_es(es_Vortmp)

     ! �Ȼ��ʳ��ι�������ʬ(Runge Kutta 2 ����)
     es_Vor = es_Vor &
          + delta_t *( -es_Jacobian_es_es(es_StrFunc,es_Vortmp)  &
                       -es_yx(yx_Beta * yx_es(es_Dx_es(es_StrFunc))) )

     ! �Ȼ���λ�����ʬ�׻�(Clank-Nicolson scheme)
     es_Vor = ( 1.0d0 - 0.5*delta_t * es_HVisc )&
             /( 1.0d0 + 0.5*delta_t * es_HVisc )* es_Vor

     ! ���٤���ή���ؿ���׻�
     es_StrFunc = es_LaplaInv_es(es_Vor)

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
  subroutine set_initial_values    ! es_Vor ������

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
    es_StrFunc = 0.0d0
    do k=-km,km
       do l=1,lm
          Ntotal = sqrt(k**2.0+l**2.0)
          if ( (Nmin .le. Ntotal) .AND. (Ntotal .le. Nmax) ) then
             call random_number(harvest)
             es_StrFunc(k,l) = 2.0 * harvest - 1     ! [-1,1] �ΰ������
          endif
       enddo
    enddo

    yx_VelX = -yx_ec(ec_Dy_es(es_StrFunc))
    yx_VelY = yx_es(es_Dx_es(es_StrFunc))
    es_StrFunc = es_StrFunc &
         * sqrt(Etotal/AvrYX_yx((yx_VelX**2+yx_VelY**2)/2.0d0))

    es_Vor = es_Lapla_es(es_StrFunc)

  end subroutine set_initial_values

  subroutine set_initial_values_real    ! es_Vor ������

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
    es_Strfunc=es_yx(yx_Strfunc)
    yx_VelX = -yx_ec(ec_Dy_es(es_StrFunc))
    yx_VelY = yx_es(es_Dx_es(es_StrFunc))
    es_StrFunc = es_StrFunc &
         * sqrt(Etotal/AvrYX_yx((yx_VelX**2+yx_VelY**2)/2.0d0))

    es_Vor = es_Lapla_es(es_Strfunc)

  end subroutine set_initial_values_real

!=========================== �ꥹ�����Ƚ��� ============================
 !
 ! �ꥹ�����Ƚ��Ͻ����
 !
  subroutine output_restart_init
    call HistoryCreate( &
           file=trim(rst_file), &
           title=trim(title), &
           source='plbaro-beta_freedecay_rk2cn_exp2.f90 (2008/10/13)', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'x','y','k','l','t'/), &
           dimsizes=(/im,jm+1,2*km+1,lm,0/),&
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
    call HistoryPut('l',(/(dble(l),l=1,lm)/), hst_rst)    ! �ѿ�����

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
           varname='es_vor', dims=(/'l','k','t'/), & 
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
    call HistoryPut('es_vor', es_Vor, hst_rst)
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
           source='chsbaro-beta_freedecay_rk2cn_exp2.f90 (2008/10/13)', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'x','y','k','l','t'/), &
           dimsizes=(/im,jm+1,2*km+1,lm,0/),&
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
    call HistoryPut('l',(/(dble(l),l=1,lm)/) )            ! �ѿ�����

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

    call HistoryPut('hvisc',es_HVisc)  ! �ѿ�����

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
    yx_Vor = yx_es(es_Vor)
    yx_StrFunc = yx_es(es_StrFunc)
    yx_VelX = -yx_ec(ec_Dy_es(es_StrFunc))
    yx_VelY = yx_es(es_Dx_es(es_StrFunc))

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

end program chsbaro_beta_freedecay_rk2cn_exp2
