!--------------------------------------------------------------------------
!  Copyright (C) 2005-2007 SPMODEL Development Group. All rights reserved.
!--------------------------------------------------------------------------
!
!Title(ɽ��): Two-dimensional barotropic model on a rotating sphere. 
!             (��ž���̾��2�����簵��ǥ�)
!
!       The time integration is performed with the Euler scheme. 
!       Program is organized for decaying tubulence problem. 
!
!History(����):  2005/03/16  S. Takehiro, created.
!                2007/12/07  S. Takehiro, namelist file is set from 
!                                         the command line argument. 
!                2008/02/11  S. Takehiro, beta coefficient fixed.
!                2008/02/13  S. Takehiro, Energy and Enstrophy output fixed.
!                2008/08/10  S. Takehiro, index changed, im -> 0:im-1
!
! The governing equation(����������): 
!
!      d\nabla^2\psi/dt 
!          = -J(\psi,\nabla^2\psi)/a^2 - 2\Omega/a^2 d\psi/d\lambda 
!            +(-1)^{p+1}\nu_{2p}(\nabla^2 + 2/a^2)^p \nabla^2\psi
!
program spbaro_euler_freedecay

!== �⥸�塼�������� ================================================
  use w_module 
  use gt4_history, only : HistoryCreate, HistoryPut, HistoryGet, &
                          HistoryAddVariable, HistoryClose, HistoryAddAttr
  use dc_trace,    only : SetDebug, BeginSub, EndSub, DbgMessage
  use dc_message,  only : MessageNotify
  use dc_types
  use dc_string,   only : StoA
  use dc_args
  implicit none

!== ����� ============================================================

 !---- �ѿ�(�ʻ����ǡ���) ----
  real(8), allocatable :: xy_VelLon(:,:)      ! ®�ٷ�����ʬ
  real(8), allocatable :: xy_VelLat(:,:)      ! ®�ٰ�����ʬ
  real(8), allocatable :: xy_Vor(:,:)         ! ����(��ľ��ʬ)
  real(8), allocatable :: xy_StrFunc(:,:)     ! ή���ؿ�

 !---- �ѿ�(���ڥ��ȥ�ǡ���) ----
  real(8), allocatable :: w_Vor(:)            ! ����(��ľ��ʬ)
  real(8), allocatable :: w_StrFunc(:)        ! ή���ؿ�

  real(8), allocatable :: n_ESp(:)            ! ���ͥ륮�����ڥ��ȥ�
  real(8), allocatable :: n_EnsSp(:)          ! ���󥹥ȥ�ե������ڥ��ȥ�
 
 !---- ����ѥ�᥿ -----
  real(8), parameter  :: pi = 3.141592653589793D0 ! �߼�Ψ
  character(len=20)   :: DbgMessageFmt='*** DbgMESSAGE ***  '

 !---- NAMELIST �ѿ� ----
  ! ���ޥ�ɥ饤��������ѿ�
  type(ARGS) :: arg
  character(STRING), pointer :: argv(:) => null()

  ! NAMELIST �����ѥե�����̾
  character(STRING)  :: nmlfile

  logical            :: Verbose=.false.          ! ���ϥ�å�������٥�
  logical            :: DebugOn=.false.          ! �ǥХå����ϥ���ȥ���
  namelist /message/  Verbose, DebugOn           !
                                                 !-- �ʻ��������ڥ��ȥ� --
  integer :: nm=21                               ! �������ȿ�
  integer :: im=64                               ! ���������ʻ����� (>3*nm+1)
  integer :: jm=32                               ! ���������ʻ����� (>3*nm/2)
  namelist /gridset/ nm, im, jm
                                                 !-- ʪ���ѥ�᥿�� --
  real(8)            :: Radius=1.0D0             ! ���Ⱦ��
  real(8)            :: Omega=0.0D0              ! ��ž��®��
  integer            :: HVOrder=1                ! ĶǴ���μ���(1 �����̤γȻ�)
  real(8)            :: HVisc=1.0D0              ! ĶǴ������
  namelist /physics/  Radius, Omega, HVOrder, HVisc
                                                 ! -- ����� --
  character(len=100) :: initial_file=''          ! ����ͥǡ����ե�����̾
  real               :: initial_time=0.0         ! �������
  namelist /initial/ initial_file, initial_time  ! 
                                                 ! -- ������ʬ --
  real(8) :: delta_t=1.0e-7                      ! ������ʬ���
  integer :: nstep=2000                          ! ������ʬ���ƥå׿�
  namelist /tint/    delta_t, nstep              ! 
                                                 ! -- ���� --
  character(len=100) :: output_file=''           ! ���ϥե�����̾
  character(len=100) :: title = &                ! �����ȥ�
       'Decaying turbulence of 2-dim barotropic fluid on a rotating sphere'
  character(len=100) :: source = &               ! ������̾
       'spbaro_euler_freedecay.f90 (2008/08/10)' !
  integer :: ndisp=200                           ! ���ϴֳ֥��ƥå׿�
  namelist /output/    output_file, title, ndisp ! 

 !---- ����ѿ� ----
  real(8) :: Beta=0.0d0                      ! �¥ѥ�᥿��(2*Omega/radius)
  real(8), allocatable :: w_HVisc(:)         ! ĶǴ������
  real(8), allocatable :: n_Damptime(:)      ! �������(e-folding time)

  integer :: it=0                            ! ���֥��ƥå�
  real(8) :: time                            ! ��ǥ������
  integer :: n, m                            ! ���ȿ�, �Ӿ��ȿ�

 !---------------- NAMELIST �ɤ߹��� ---------------------
  call MessageNotify('M','main', trim(source))

  call Open(arg)
  call Debug(arg) ; call Help(arg) ; call Strict(arg)
  call Get(arg, argv)
  if ( size(argv) .le. 0 ) then
     call MessageNotify('M','main',&
          'Usage: spbaro_euler_freedecay.out [namelist file]')
     call MessageNotify('E','main','There is no argument. &
          & Please set the namelist file name after the execution.')
  else
     nmlfile=argv(1)
     call MessageNotify('M','main','Namelist file is '//trim(nmlfile))
  endif
  deallocate(argv)
  call Close(arg)

  write(6,nml=message)
  open(10,file=nmlfile,status='OLD')
  read(10,nml=message) ; write(6,nml=message) ; close(10)

  if(verbose)write(6,nml=gridset)
  open(10,file=nmlfile,status='OLD')
  read(10,nml=gridset) ; write(6,nml=gridset) ; close(10)

  if(verbose)write(6,nml=physics)
  open(10,file=nmlfile,status='OLD')
  read(10,nml=physics) ; write(6,nml=physics) ; close(10)

  if(verbose)write(6,nml=initial)
  open(10,file=nmlfile,status='OLD')
  read(10,nml=initial) ; write(6,nml=initial) ; close(10)

  if(verbose)write(6,nml=tint) 
  open(10,file=nmlfile,status='OLD')
  read(10,nml=tint)    ; write(6,nml=tint)     ; close(10)

  if(verbose)write(6,nml=output)
  open(10,file=nmlfile,status='OLD')
  read(10,nml=output)  ; write(6,nml=output)   ; close(10)

 !---------------- �ǥХå������������� -----------------
  if (DebugOn) then
    call SetDebug
  end if

 !------------------ �ѿ��γ���դ� ---------------------
   allocate(xy_VelLon(0:im-1,jm),xy_VelLat(0:im-1,jm))
   allocate(xy_Vor(0:im-1,jm),xy_StrFunc(0:im-1,jm))

   allocate(w_Vor((nm+1)*(nm+1)),w_StrFunc((nm+1)*(nm+1)))
   allocate(w_HVisc((nm+1)*(nm+1)))

   allocate(n_ESp(0:nm),n_EnsSp(0:nm))
   allocate(n_Damptime(0:nm))

 !------------------ ��ɸ�ͤ����� -----------------------
  call DbgMessage(fmt='call %c', c1='w_initial') 
  call w_Initial(nm,im,jm)

 !------------------ ʪ������������ -----------------------
  Beta  = 2 *Omega/Radius
  w_HVisc = (-1)**(HVOrder+1)*HVisc*((rn(:,1)+2.0D0)/Radius**2)**HVOrder
  w_HVisc(l_nm(0,0)) = 0.0d0   ! rn(ln(0,0,1) �������ͤʤΤǽ������Ƥ���.

  do n=0,nm
     if ( w_HVisc(l_nm(n,0)) .NE. 0.0D0 ) then
        n_Damptime(n)=-1.0D0/w_HVisc(l_nm(n,0))
     else
!        n_Damptime(n)=huge(0.0E0)              ! �����礭����������
        n_Damptime(n)=huge(0.0D0)              ! �����礭����������
     endif
  enddo

 !------------------- ��������� ----------------------
  if ( initial_file == "") then
     call set_initial_values
  else
     call initial_read_gtool4
  endif

  time = initial_time
  w_Vor = w_xy(xy_Vor)
  w_StrFunc = w_xy(xy_StrFunc)

 !------------------- ������ʬ(Euler ˡ) --------------------
  call output_gtool4_init
  if ( initial_file == '' ) call output_gtool4    ! ������Ϳ��������ͤϽ���

  call DbgMessage(fmt='%c %c', &
&                 c1=DbgMessageFmt, &
&                 c2='Time integration starts.') 

  do it=1,nstep
     time = initial_time + it * delta_t

     w_Vor = w_Vor + delta_t *                          &
&         ( - w_Jacobian_w_w(w_StrFunc,w_Vor)/Radius**2 &
&           - Beta/Radius * w_DLon_w(w_StrFunc)         &
&           + w_HVisc * w_Vor )

     w_StrFunc = w_LaplaInv_w(w_Vor) * Radius**2

     if(mod(it,ndisp) .eq. 0)then                    ! ����
        call output_gtool4
     endif
  enddo

  call DbgMessage(fmt='%c %c',      &
&                 c1=DbgMessageFmt, &
&                 c2='Time integration end.') 

  call output_gtool4_close

! �ʾ� �ᥤ��ץ���� 
!-----------------------------------------------------------------------------
! �ʲ� ���֥롼����

contains

 !------------------- ���������(�ե�����ʤ�) ----------------------
  subroutine set_initial_values

!!$   !---- ����͹��β�ž��
!!$    real(8), parameter :: Omega0   = 1.0D0
!!$    real(8), parameter :: LatOmega = 90.0D0/180.0D0*pi
!!$    real(8), parameter :: LonOmega = 0.0D0/180.0D0*pi
!!$
!!$    xy_StrFunc = Omega0 * Radius &
!!$                   * ( cos(LatOmega)*cos(xy_Lat)*cos(LonOmega-xy_Lon) &
!!$                       + sin(LatOmega)*sin(xy_Lat) )
!!$    w_StrFunc = w_xy(xy_StrFunc)
!!$    w_Vor = w_Lapla_w(w_StrFunc)/Radius**2
!!$    xy_Vor =  xy_w(w_Vor)
!!$
!!$   !---- ����� 1 �ȿ���ʬ
!!$    integer, parameter :: ninit=4, minit=3
!!$
!!$    w_StrFunc = 0.0d0 ; w_StrFunc(l_nm(ninit,minit))=1.0d0
!!$    xy_StrFunc = xy_w(w_StrFunc)
!!$    w_Vor = w_Lapla_w(w_StrFunc)/Radius**2
!!$    xy_Vor =  xy_w(w_Vor)
   !---- ��������ʬ��
    integer              :: random_seed_size   ! ����μ��Ĺ��
    integer, allocatable :: seedarray(:)       ! ����μ�
    real, allocatable    :: harvest(:)         ! �������

    integer    :: seed=0           ! seed(1)�����ꤹ������
    integer    :: nzero=10         ! ������ͥ륮�����ڥ��ȥ�ʬ�ۤΥѥ�᥿
    integer    :: gamma=100        ! ������ͥ륮�����ڥ��ȥ�ʬ�ۤΥѥ�᥿
    real(8)    :: Etotal=1.0D0     ! ��������ͥ륮������

    namelist /initvalue/ seed, nzero, gamma, Etotal

    if(verbose)write(6,nml=initvalue)
    open(10,file=nmlfile,status='OLD')
    read(10,nml=initvalue) ; write(6,nml=initvalue) ; close(10)

    ! �������
    call random_seed(size=random_seed_size)
    allocate(seedarray(random_seed_size))
    call random_seed(get=seedarray)
    seedarray(1)=seed
    call random_seed(put=seedarray)

    w_StrFunc = 0.0d0
    do n=1,nm
       allocate(harvest(-n:n))
       call random_number(harvest)
       w_StrFunc(l_nm(n,(/(m,m=-n,n)/)))=2.0 * harvest - 1 ! [-1,1] �ΰ������
       n_ESp(n) = 0.5 * n*(n+1)*sum(w_StrFunc(l_nm(n,(/(m,m=-n,n)/)))**2)
       deallocate(harvest)
    enddo

    ! ���ڥ��ȥ�ʬ�ۤη������
    do n=0,nm
       do m=-n,n
          if ( n_ESp(n) .ne. 0.0d0 ) then
             w_StrFunc(l_nm(n,m)) = w_StrFunc(l_nm(n,m)) &
                   * sqrt( (dble(n)**(gamma/2)/dble(n+nzero)**gamma) /n_ESp(n))
          endif
       enddo
    enddo

    ! ���ͥ륮�����ڥ��ȥ�Ʒ׻�
    do n=1,nm
       n_ESp(n) = 0.5 * n*(n+1)* sum( w_StrFunc(l_nm(n,(/(m,m=-n,n)/)))**2 )
    enddo

    ! ���ͥ륮�����礭��������. 
    w_StrFunc = w_StrFunc * sqrt(Etotal/sum(n_ESp))
    xy_StrFunc = xy_w(w_StrFunc)
    w_Vor = w_Lapla_w(w_StrFunc)/Radius**2
    xy_Vor =  xy_w(w_Vor)

  end subroutine set_initial_values

 !------------------- ���������(�ե����뤫��) ----------------------  
  subroutine initial_read_gtool4

    call HistoryGet( trim(initial_file), 'vellon', xy_VelLon,  time )
    call HistoryGet( trim(initial_file), 'vellat', xy_VelLat,  time )
    call HistoryGet( trim(initial_file), 'vor',    xy_Vor,     time )
    call HistoryGet( trim(initial_file), 'strfunc',xy_StrFunc, time )

  end subroutine initial_read_gtool4

 !------------------- ���� ----------------------
  subroutine output_gtool4_init

   !---- �ҥ��ȥ꡼�ե�������� ----
    call HistoryCreate( &
           file=trim(output_file), &
           title=trim(title), &
           source='spbaro_euler_freedecay.f90 (2008/08/10)', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'lon','lat','n  ','t  '/), &
           dimsizes=(/im,jm,nm+1,0/),&
           longnames=(/'Longitude            ','Latitude             ',&
                       'Hor.total wave number','time                 '/),&
           units=(/'1','1','1','1'/),   &
           origin=real(time), interval=real(ndisp*delta_t), &
           xtypes=StoA('real','real','int','real'))

   !---- ��ɸ�ѿ����, ���� ----
    call HistoryPut('lon',x_Lon/pi*180)                       ! �ѿ�����
    call HistoryAddattr('lon','topology','circular')          ! ����°��
    call HistoryAddattr('lon','modulo',360.0)                 ! ����°��
    call HistoryPut('lat',y_Lat/pi*180)                       ! �ѿ�����
    call HistoryPut('n',(/(dble(n),n=0,nm)/))                 ! �ѿ�����

   !---- �ѥ�᥿�����, ���� ----
    call HistoryAddVariable( &                                ! �ѿ����
           varname='hvisc', dims=(/'n'/), & 
           longname='hyper diffusivity', units='1', xtype='double')

    call HistoryAddVariable( &                                ! �ѿ����
           varname='damptime', dims=(/'n'/), & 
           longname='dissipative damping time (e-folding)', &
           units='1', xtype='double')

    call HistoryPut('hvisc',w_HVisc(l_nm((/(n,n=0,nm)/),0)))  ! �ѿ�����
    call HistoryPut('damptime',n_Damptime)                    ! �ѿ�����

   !---- ʪ���ѿ���� ----
    call HistoryAddVariable( &                                ! �ѿ����
           varname='vor', dims=(/'lon','lat','t  '/), & 
           longname='Vorticity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='strfunc', dims=(/'lon','lat','t  '/), & 
           longname='Stream function', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='vellon', dims=(/'lon','lat','t  '/), & 
           longname='lon-velocity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='vellat', dims=(/'lon','lat','t  '/), & 
           longname='lat-velocity', units='1', xtype='double')

   !---- ����¾��������� ----
    call HistoryAddVariable( &                                ! �ѿ����
           varname='te', dims=(/'t  '/), & 
           longname='total energy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='tens', dims=(/'t  '/), & 
           longname='total enstrophy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='mvlon', dims=(/'lat','t  '/), & 
           longname='mean zonal flow', units='1', xtype='double')

    call HistoryAddVariable( &                                ! �ѿ����
           varname='esp', dims=(/'n','t'/), & 
           longname='energy spectrum', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='enssp', dims=(/'n','t'/), & 
           longname='enstrophy spectrum', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='tesp', dims=(/'t'/), & 
           longname='Total energy (by spectrum)', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='tenssp', dims=(/'t'/), & 
           longname='Total enstrophy (by spectrum)', units='1', xtype='double')

   !---- �¸��ѥ�᥿����°���Ȥ������, ����(���� Global °��) ----
    call HistoryAddAttr('lon','+Radius',  Radius  )
    call HistoryAddAttr('lon','+delta_t', delta_t )
    call HistoryAddAttr('lon','+Omega',   Omega   )
    call HistoryAddAttr('lon','+HVOrder', HVOrder )
    call HistoryAddAttr('lon','+HVisc',   HVisc   )

  end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it, '  time = ', time
    call HistoryPut('t',real(time))

   !---- ʪ���ѿ����� ----
    xy_Vor = xy_w(w_Vor)
    xy_StrFunc = xy_w(w_StrFunc)
    xy_VelLon = -xy_GradLat_w(w_StrFunc)/Radius
    xy_VelLat = xy_GradLon_w(w_StrFunc)/Radius

    call HistoryPut('vellon',xy_VelLon)
    call HistoryPut('vellat',xy_VelLat)
    call HistoryPut('vor',xy_Vor)
    call HistoryPut('strfunc',xy_StrFunc)

    call HistoryPut('te', &
         IntLonLat_xy((xy_VelLon**2+xy_VelLat**2)/2.0d0)*Radius**2)
    call HistoryPut('tens',&
         IntLonLat_xy(xy_Vor**2/2.0d0)*Radius**2)
    call HistoryPut('mvlon',y_IntLon_xy(xy_VelLon))

    do n=0,nm
       n_ESp(n) = 0.5 * n*(n+1) * sum(w_StrFunc(l_nm(n,(/(m,m=-n,n)/)))**2 )
       n_EnsSp(n) =0.5 * n**2 * (n+1)**2 &
            * sum(w_StrFunc(l_nm(n,(/(m,m=-n,n)/)))**2)
    enddo
    call HistoryPut('esp',n_Esp/Radius**2)
    call HistoryPut('enssp',n_Enssp/Radius**2)
    call HistoryPut('tesp',(4*pi)*sum(n_Esp))
    call HistoryPut('tenssp',(4*pi/Radius**2)*sum(n_Enssp))

  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program spbaro_euler_freedecay
