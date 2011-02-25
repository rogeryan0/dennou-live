!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wt_zonal_galerkin_module ����ץ�ץ������
!      �����β��ٳȻ�����(Crank-Nicolson scheme)
!      ��¦�ǲ��ٸ���(T=0), ��¦�ǥե�å���������(dT/dr=0)
!
!      �׻������ӤΤ���β��ϲ�ˤĤ��Ƥϲ���ʸ��
!
!         ��3 ��������ΰ�ǤγȻ����������β��ϲ��
!
!      (wt_diffusion.pdf)�򻲾ȤΤ���. 
!
!����  2009/09/10  �ݹ����� wt_galerkin_DN_cn_diff.f90 ��꼴�о��Ǥ˲�¤
!
program wt_zonal_galerkin_DN_cn_diff
  !
  !    wt_zonal_galerkin_module ����ץ�ץ������
  !      �����β��ٳȻ�����(Crank-Nicolson scheme)
  !      ��¦�ǲ��ٸ���(T=0), ��¦�ǥե�å���������(dT/dr=0)
  !
  !      �׻������ӤΤ���β��ϲ�ˤĤ��Ƥϲ���ʸ��
  !
  !         ��3 ��������ΰ�ǤγȻ����������β��ϲ��
  !
  !      (wt_diffusion.pdf)�򻲾ȤΤ���. 
  !
  use dc_message
  use lumatrix
  use wt_zonal_galerkin_module
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  integer,parameter  :: im=1,  jm=16, km=32  ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=20         ! �����ȿ�������(��ʿ, ư��)

 !---- ��ɸ�ѿ��ʤ� ----
  real(8),parameter  :: ri=0.5, ro=1.5       ! �⳰Ⱦ��

 !---- �ѿ� ----
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Temp   ! ����
  real(8), dimension(nm+1,2:lm)      :: wp_Temp    ! ����
  real(8), dimension(nm+1,0:km)      :: wz_Temp    ! ����

  real(8), dimension(0:im-1,jm,0:km) :: xyz_TempInit   ! ����(�����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_TempSol    ! ����(���ϲ�)

 ! Crank Nicholson ����
  real(8)             :: DifLUMT_Temp(nm+1,0:lm,0:lm)
                                   ! Crank Nicholson ��Ū�׻��ѳȻ�����(����)
  integer             :: kpivot_Temp(nm+1,0:lm)    
                                   ! Crank Nicholson ��Ū�׻��ѳȻ�����ԥܥå�

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=1e-3                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=200, ndisp=20        ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  real(8), parameter :: kappa=1.0               ! �Ȼ�����

  real(8), parameter :: pi=3.1415926535897932385D0  ! �߼�Ψ
  real(8) :: alpha                   ! ư���ȿ�
  real(8) :: sigma                   ! ��ĹΨ

 !---- ����¾ ----
  integer :: it=0
  real(8) :: time=0                  ! ����
  integer :: k 
  integer :: n,l                     ! �ȿ�

 !---------------- ��ɸ�ͤ����� ---------------------
  call wt_galerkin_Initial(im,jm,km,nm,lm,ri,ro,DN=.true.)

  call CNDiffusionMatrixDN( kappa, dt, DifLUMT_Temp, kpivot_Temp )

 !------------------- ��������� ----------------------
  write(6,*) 'Total horizontal, radial wavenumbers: n,l?'
  read(5,*)n,l

  alpha = CalAlpha(n,l)
  sigma = -kappa* alpha **2
  write(6,*) 'Radial wavenumber : ', alpha
  write(6,*) 'Growth rate       : ', sigma

  wz_Temp = 0.0
  do k=0,km
     wz_Temp(l_nm(n,0),k) = -sbn1(n,alpha*ri)*sbj(n,alpha*z_Rad(k)) &
                            +sbj1(n,alpha*ri)*sbn(n,alpha*z_Rad(k))
  enddo
  xyz_Temp = xyz_wz(wz_Temp)
  xyz_TempInit = xyz_Temp
  wp_Temp = wp_wt(wt_xyz(xyz_Temp))

  call output_gtool4_init
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt                                ! Crank-Nicolson ˡ�ˤ�������ʬ
     time = it * dt
     wp_Temp = wp_Temp + dt/2 * wp_wt( kappa * wt_Lapla_wt(wt_wp(wp_Temp)) )
     wp_Temp = LUSolve(DifLUMT_Temp,kpivot_Temp,wp_Temp)

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
    !
    ! ��٥å���ȡ���η��� a_n(x)
    !   j_n(x) = a_n(x)sin(x) + b_n(x) cos(x)
    !   n_n(x) = b_n(x)sin(x) - a_n(x) cos(x)
    !
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
    !
    ! ��٥å���ȡ���η��� b_n(x)
    !    j_n(x) = a_n(x)sin(x) + b_n(x) cos(x)
    !    n_n(x) = b_n(x)sin(x) - a_n(x) cos(x)
    !
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
       b = -(945-105*x**2+x**4)/x**5
    case default
       write(6,*) 'Index is out of range.'
       stop
    end select
  end function b

  function sbj(n,x)
    !
    ! ��1���٥å���ؿ�
    !
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: sbj

    sbj = a(n,x)*sin(x) + b(n,x)*cos(x)
  end function sbj

  function sbn(n,x)
    !
    ! ��2���٥å���ؿ�
    !
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: sbn

    sbn = b(n,x)*sin(x) - a(n,x)*cos(x)
  end function sbn

  function sbj1(n,x)
    !
    ! �� 1 ���٥å���ؿ��� 1 ����ʬ
    !
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: sbj1

    sbj1 = n*sbj(n,x)/x - sbj(n+1,x)
  end function sbj1

  function sbn1(n,x)
    !
    ! �� 2 ���٥å���ؿ��� 1 ����ʬ
    !
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: sbn1

    sbn1 = n*sbn(n,x)/x - sbn(n+1,x)
  end function sbn1

  function det(l,x)
    !
    ! �������󼰤η׻�
    ! ��=sbj1(ri*x)sbn(ro*x)-sbn1(ri*x)sbj(ro*x)
    !
    integer, intent(IN) :: l                 ! ����
    real(8), intent(IN) :: x                 ! 
    real(8)             :: det               ! ����

    det=x*sbj1(l,ri*x)*sbn(l,ro*x) - x*sbn1(l,ri*x)*sbj(l,ro*x)

  end function det

  function CalAlpha(l,n)
    !
    ! sbj1(ri*x)sbn(ro*x)-sbn1(ri*x)sbj(ro*x) �β�����
    !
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
    dx = PI/(ro-ri)/nx
    xl = dx ; ValL = det(l,xl)

    !
    ! det = 0 �Ȥʤ����� x=0 ����� dx ������õ���Ƥ���
    ! n ���ܤ򶴤� xl, xs �����ꤹ��.
    !
100 continue
      xs =xl ; ValS = ValL
      xl = xl+dx
      ValL = det(l,xl)
      if( ValL*ValS .le. 0.0 ) nn=nn-1
      if( nn .lt. 0 ) goto 199
      goto 100
199 continue

1000 xm = (xs + xl)/2.0
    ValM = det(l,xm)
      
    if ( ValS * ValM .GT. 0.0D0 ) then
       xs = xm ; ValS=det(l,xs)
    else
       xl = xm ; ValL=det(l,xl)
    endif

    if ( abs(xl-xs) .lt. eps ) then
       CalAlpha = xm
       goto 99
    endif

    goto 1000

99  continue
  end function CalAlpha

   !------------------- �Ȼ��� ----------------------
    subroutine CNDiffusionMatrixDN( Diffc, dt, DiffLUMatrix, kpivot )
      ! 
      ! Crank Nicholson �Ȼ���Ū�׻��ѹ��������
      ! (1-D dt/2��^2, LU ʬ��)
      ! 
      real(8), intent(IN)   :: Diffc    ! �Ȼ�����
      real(8), intent(IN)   :: dt       ! ���ֹ�

      ! Crank Nicholson ����
      real(8), intent(OUT)  :: DiffLUMatrix(nm+1,2:lm,2:lm)
                              ! Crank Nicholson �Ȼ���Ū�׻��ѹ���
                              ! (1-D dt/2��^2, LU ʬ��)
      integer, intent(OUT)  :: kpivot(nm+1,2:lm)
                              ! Crank Nicholson �Ȼ���Ū�׻��ѹ���ԥܥåȾ���

      real(8)               :: wp_I(nm+1,2:lm)       ! ������ѿ�

      integer :: l

      DiffLUMatrix = 0.0
      do l=2,lm
         wp_I = 0.0 ; wp_I(:,l) = 1.0             ! ���ȿ���ʬ��Ω
         DiffLUMatrix(:,:,l) &
              = wp_I - Diffc * dt/2.0 * wp_wt(wt_Lapla_wt(wt_wp(wp_I)))
      enddo

      call LuDeComp(DiffLUMatrix,kpivot)

    end subroutine CNDiffusionMatrixDN

 !------------------- ���ϥ롼���� ----------------------
  subroutine output_gtool4_init
    !
    ! �ҥ��ȥ꡼���Ͻ�����롼����
    !
    !   �ե��������
    !   �ѿ����
    !
    call HistoryCreate( &                                  ! �ҥ��ȥ꡼����
           file='wt_zonal_galerkin_DN_cn_diff.nc', &
           title='Diffusion model in a spherical shell', &
           source='Sample program of spmodel library', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'lat','rad','t  '/), dimsizes=(/jm,km+1,0/),&
           longnames=(/'Latitude ','Radius   ','time     '/),&
           units=(/'1','1','1'/),   &
           origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('lat',y_Lat/pi*180)                       ! �ѿ�����
    call HistoryPut('rad',z_Rad)                              ! �ѿ�����

    call HistoryAddVariable( &                                ! �ѿ����
           varname='temp', dims=(/'lat','rad','t  '/), & 
           longname='temperature', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='tempsol', dims=(/'lat','rad','t  '/), & 
           longname='temperature(analytic)', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='temperror', dims=(/'lat','rad','t  '/), & 
           longname='temperature(error)', units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    !
    ! �ҥ��ȥ꡼����
    !
    !   ��ʪ���̤� gtool4 �ե�����ؤν��Ϥ�Ԥ�
    !
    write(6,*) 'it = ',it
    xyz_Temp = xyz_wt(wt_wp(wp_Temp))
    xyz_TempSol = xyz_TempInit* exp( sigma * time )

    call HistoryPut('t',real(it*dt))
    call HistoryPut('temp',xyz_Temp(0,:,:))
    call HistoryPut('tempsol',xyz_TempSol(0,:,:))
    call HistoryPut('temperror',xyz_Temp(0,:,:)-xyz_TempSol(0,:,:))
  end subroutine output_gtool4

  subroutine output_gtool4_close
    !
    ! �ҥ��ȥ꡼���Ͻ�λ����
    !
    call HistoryClose
  end subroutine output_gtool4_close

end program wt_zonal_galerkin_DN_cn_diff