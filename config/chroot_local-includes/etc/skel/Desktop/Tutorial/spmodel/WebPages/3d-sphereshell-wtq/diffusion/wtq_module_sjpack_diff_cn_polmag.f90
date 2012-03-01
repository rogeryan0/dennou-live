!----------------------------------------------------------------------
!     Copyright (c) 2010 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wtq_module_sjpack
!      ����Υȥ����뼧��Ȼ�����(Crank-Nicolson scheme)
!
!      �����ӥ����ա�����ˡ�ˤ�붭�����Ŭ��
!
!      �׻������ӤΤ���β��ϲ�ˤĤ��Ƥϲ���ʸ��
!
!         ��3 �������ΰ�ǤγȻ����������β��ϲ��
!
!      (wu_diffusion.pdf)�򻲾ȤΤ���. 
!
!����  2010/04/18  �ݹ�����  wtq_diff_cn_polmag.f90 ����ѹ�
!
program wtq_module_sjapck_diff_cn_polmag

  use dc_message
  use dc_string, only : StoA
  use lumatrix
  use wtq_module_sjpack
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  integer,parameter  :: im=32, jm=16         ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: kmo=16, kmi=8        ! �ʻ���������(���ư��, ��ư��)
  integer,parameter  :: nm=10                ! �����ȿ�������(��ʿ)
  integer,parameter  :: lmo=16, lmi=15       ! �����ȿ�������(���ư��, ��ư��)

 !---- ��ɸ�ѿ��ʤ� ----
  real(8),parameter  :: ri=0.5, ro=1.5       ! �⳰Ⱦ��

 !---- �ѿ� ----
  real(8), dimension(0:im-1,jm,0:kmo) :: xyz_Polmag   ! �ȥ����뼧��
  real(8), dimension((nm+1)**2,0:lmo) :: wt_Polmag    ! �ȥ����뼧��
  real(8), dimension((nm+1)**2,0:kmo) :: wz_Polmag    ! �ȥ����뼧��

  real(8), dimension(0:im-1,jm,0:kmo) :: xyz_PolmagInit ! �ȥ����뼧��(�����)
  real(8), dimension(0:im-1,jm,0:kmo) :: xyz_PolmagSol  ! �ȥ����뼧��(���ϲ�)

  real(8), dimension(0:im-1,jm,kmi)   :: xyr_Polmag   ! �ȥ����뼧��
  real(8), dimension((nm+1)**2,0:lmi) :: wq_Polmag    ! �ȥ����뼧��
  real(8), dimension((nm+1)**2,kmi)   :: wr_Polmag    ! �ȥ����뼧��

  real(8), dimension(0:im-1,jm,kmi)   :: xyr_PolmagInit ! �ȥ����뼧��(�����)
  real(8), dimension(0:im-1,jm,kmi)   :: xyr_PolmagSol  ! �ȥ����뼧��(���ϲ�)

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=1e-4                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=1000, ndisp=100        ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  real(8), parameter :: kappa=1.0                  ! �Ȼ�����

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8) :: alpha                   ! ư���ȿ�
  real(8) :: sigma                   ! ��ĹΨ

 !---- ����¾ ----
  integer :: it=0
  real(8) :: time=0                  ! ����
  integer :: k 
  integer :: n,m,l                   ! �ȿ�

 !---------------- ��ɸ�ͤ����� ---------------------
  call wtq_Initial(im,jm,kmi,kmo,nm,lmi,lmo,ri,ro)

 !------------------- ��������� ----------------------
  write(6,*)'n,m,l?'
  read(5,*)n,m,l

  alpha = CalAlpha(n-1,l)
  sigma = -kappa* alpha **2
  write(6,*) 'Radial wavenumber : ', alpha
  write(6,*) 'Growth rate       : ', sigma

  wz_Polmag = 0.0 ; wr_Polmag = 0.0
  do k=0,kmo
     wz_Polmag(l_nm(n,m),k) = sbj(n,alpha*z_Rad(k))
  enddo

  do k=1,kmi
     wr_Polmag(l_nm(n,m),k) = sbj(n,alpha*r_Rad(k))
  enddo

  xyz_Polmag = xyz_wz(wz_Polmag)
  xyz_PolmagInit = xyz_Polmag
  wt_Polmag = wt_xyz(xyz_Polmag)

  xyr_Polmag = xyr_wr(wr_Polmag)
  xyr_PolmagInit = xyr_Polmag
  wq_Polmag = wq_xyr(xyr_Polmag)

  call output_gtool4_init
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt                               ! Crank-Nicolson ˡ�ˤ�������ʬ
     time = it * dt
     wt_Polmag = wt_Polmag + dt/2 * kappa * wt_Lapla_wt(wt_Polmag)
     wq_Polmag = wq_Polmag + dt/2 * kappa * wq_Lapla_wq(wq_Polmag)
     call wtq_PolmagBoundaries(wt_Polmag,wq_Polmag)
     call CNImplicitPolmag(wt_Polmag,wq_Polmag,kappa,kappa,dt)

     if(mod(it,ndisp) .eq. 0)then                    ! ����
        call output_gtool4
     endif
  enddo
  call output_gtool4_close

contains
  !
  ! Spherical bessel functions
  !
  function a(n,x)
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: a

    select case(n)
    case (0)
       a = 1/x
    case (1)
       a = 1/x**2
    case (2)
       a = (3-x**2)/x**3
    case (3)
       a = (15-6*x**2)/x**4
    case (4)
       a = (105-45*x**2+x**4)/x**5
    case (5)
       a = (945-420*x**2+15*x**4)/x**6
    case default
       write(6,*) 'Index is out of range.'
       stop
    end select
  end function a

  function b(n,x)
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: b

    select case(n)
    case (0)
       b = 0.0D0
    case (1)
       b = -1/x
    case (2)
       b = -3/x**2
    case (3)
       b = -(15-x**2)/x**3
    case (4)
       b = -(105-10*x**2)/x**4
    case (5)
       b = -(945-105*x**2+x**4)/x**4
    case default
       write(6,*) 'Index is out of range.'
       stop
    end select
  end function b

  function sbj(n,x)
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: sbj

    sbj = a(n,x)*sin(x) + b(n,x)*cos(x)
  end function sbj

   !-----------------------------------------
   ! sbj(ra*x)=0 �β�����
   !
    function CalAlpha(l,n)
      integer, intent(IN) :: l                 ! ����
      integer, intent(IN) :: n                 ! �������ֹ�
      real(8)             :: CalAlpha
      real(8), parameter  :: eps = 1.0D-14     ! �������

      real(8) :: PI
      real(8) :: xs, xl, xm
      real(8) :: ValS, ValL, ValM

      real(8) :: dx
      integer :: nx=20
      integer :: nn

      nn = n
      PI = atan(1.0D0)*4.0D0
      dx = PI/ro/nx
      xl = dx ; ValL = sbj(l,ro*xl)

      !
      ! det = 0 �Ȥʤ����� x=0 ����� dx ������õ���Ƥ���
      ! n ���ܤ򶴤� xl, xs �����ꤹ��.
      !
100   continue
      xs =xl ; ValS = ValL
      xl = xl+dx
      ValL = sbj(l,ro*xl)
      if( ValL*ValS .le. 0.0 ) nn=nn-1
      if( nn .lt. 0 ) goto 199
      goto 100
199 continue

1000  xm = (xs + xl)/2.0
      ValM = sbj(l,ro*xm)
      
      if ( ValS * ValM .GT. 0.0D0 ) then
         xs = xm ; ValS=sbj(l,ro*xs)
      else
         xl = xm ; ValL=sbj(l,ro*xl)
      endif

      if ( abs(xl-xs) .lt. eps ) then
         CalAlpha = xm
         goto 99
      endif

      goto 1000

99    continue
    end function CalAlpha

 !------------------- �Ȼ��� ----------------------
  subroutine CNImplicitPolmag(wt_Polmag,wq_Polmag,Diffc_o,Diffc_i,dt)

    ! �ȥ����뼧��(���)
    real(8), dimension((nm+1)*(nm+1),0:lmo), intent(INOUT) :: wt_Polmag
    ! �ȥ����뼧��(��)
    real(8), dimension((nm+1)*(nm+1),0:lmi), intent(INOUT) :: wq_Polmag
    real(8), intent(IN)          :: Diffc_o    ! �Ȼ�����(���)
    real(8), intent(IN)          :: Diffc_i    ! �Ȼ�����(��)
    real(8), intent(IN)          :: dt         ! ���ֹ�

    ! Crank Nicholson �Ȼ���Ū�׻��ѹ���(1-D dt/2��^2, LU ʬ��)
    real(8), dimension((nm+1)*(nm+1),0:lmo+lmi+1,0:lmo+lmi+1) :: DiffLUMatrix
    ! �ԥܥåȾ���
    integer, dimension((nm+1)*(nm+1),0:lmo+lmi+1)             :: kpivot

    ! ������ѿ�
    real(8), dimension((nm+1)*(nm+1),0:lmo)     :: wt_I
    real(8), dimension((nm+1)*(nm+1),0:lmo)     :: wt_DI
    real(8), dimension((nm+1)*(nm+1),0:lmi)     :: wq_I
    real(8), dimension((nm+1)*(nm+1),0:lmi)     :: wq_DI
    real(8), dimension((nm+1)**2,0:lmo+lmi+1)   :: wtq_Polmag  ! �ݥ����뼧��

    integer :: l
    logical :: first=.true.

    save DiffLUMatrix, kpivot, first

    if ( first ) then
       first = .false. 

       DiffLUMatrix = 0.0
       do l=0,lmo
          wq_I = 0.0 ; wq_DI = 0.0
          wt_I = 0.0 ; wt_I(:,l) = 1.0             ! ���ȿ���ʬ��Ω
          wt_DI =  - Diffc_o * dt/2.0 * wt_Lapla_wt(wt_I)
          call wtq_PolmagBoundaries(wt_DI,wq_DI)
          DiffLUMatrix(:,0:lmo,l) = wt_I + wt_DI
          DiffLUMatrix(:,lmo+1:lmo+lmi+1,l) = wq_DI
       enddo

       do l=0,lmi
          wt_I = 0.0 ; wt_DI = 0.0
          wq_I = 0.0 ; wq_I(:,l) = 1.0             ! ���ȿ���ʬ��Ω
          wq_DI =  - Diffc_i * dt/2.0 * wq_Lapla_wq(wq_I)
          call wtq_PolmagBoundaries(wt_DI,wq_DI)
          DiffLUMatrix(:,0:lmo,lmo+1+l) = wt_DI
          DiffLUMatrix(:,lmo+1:lmo+lmi+1,lmo+1+l) = wq_I + wq_DI
       enddo

       call ludecomp(DiffLUMatrix,kpivot)
    endif
    
    wtq_Polmag(:,0:lmo) = wt_Polmag
    wtq_Polmag(:,lmo+1:lmo+lmi+1) = wq_Polmag
    wtq_Polmag = LUSolve(DiffLUMatrix,kpivot,wtq_Polmag)
    wt_Polmag = wtq_Polmag(:,0:lmo) 
    wq_Polmag = wtq_Polmag(:,lmo+1:lmo+lmi+1)

  end subroutine CNImplicitPolmag

 !------------------- ���ϥ롼���� ----------------------
  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! �ҥ��ȥ꡼����
           file='wtq_module_sjpack_diff_cn_polmag.nc', &
           title='Diffusion model in a spherical shell', &
           source='Sample program of spmodel library', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=StoA('lon','lat','rad','crad','t'), &
           dimsizes=(/im,jm,kmo+1,kmi,0/),&
           longnames=StoA('Longitude','Latitude','Radius','Radius','time'),&
           units=(/'1','1','1','1','1'/),   &
           origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('lon',x_Lon/pi*180)                       ! �ѿ�����
    call HistoryPut('lat',y_Lat/pi*180)                       ! �ѿ�����
    call HistoryPut('rad',z_Rad)                              ! �ѿ�����
    call HistoryPut('crad',r_Rad)                             ! �ѿ�����

    call HistoryAddVariable( &                                ! �ѿ����
           varname='polmag_o', dims=(/'lon','lat','rad','t  '/), & 
           longname='Poloidal magnetic potential', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='polmagsol_o', dims=(/'lon','lat','rad','t  '/), & 
           longname='Poloidal magnetic potential(analytic)', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='polmagerror_o', dims=(/'lon','lat','rad','t  '/), & 
           longname='Poloidal magnetic potential(error)', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='polmag_i', dims=(/'lon ','lat ','crad','t   '/), & 
           longname='Poloidal magnetic potential', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='polmagsol_i', dims=(/'lon ','lat ','crad','t   '/), & 
           longname='Poloidal magnetic potential(analytic)', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='polmagerror_i', dims=(/'lon ','lat ','crad','t   '/), & 
           longname='Poloidal magnetic potential(error)', &
           units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it
    xyz_Polmag = xyz_wt(wt_Polmag)
    xyz_PolmagSol = xyz_PolmagInit* exp( sigma * time )

    xyr_Polmag = xyr_wq(wq_Polmag)
    xyr_PolmagSol = xyr_PolmagInit* exp( sigma * time )

    call HistoryPut('t',real(it*dt))
    call HistoryPut('polmag_o',xyz_Polmag)
    call HistoryPut('polmagsol_o',xyz_PolmagSol)
    call HistoryPut('polmagerror_o',xyz_Polmag-xyz_PolmagSol)
    call HistoryPut('polmag_i',xyr_Polmag)
    call HistoryPut('polmagsol_i',xyr_PolmagSol)
    call HistoryPut('polmagerror_i',xyr_Polmag-xyr_PolmagSol)
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program wtq_module_sjapck_diff_cn_polmag
