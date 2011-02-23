!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wt_zonal_galerkin_module ����ץ�ץ����
!      �����Υݥ�����®�پ�Ȼ�����(Crank-Nicolson scheme)
!      ξ¦Ǵ����
!
!      �׻������ӤΤ���β��ϲ�ˤĤ��Ƥϲ���ʸ��
!
!         ��3 ��������ΰ�ǤγȻ����������β��ϲ��
!
!      (wt_diffusion.pdf)�򻲾ȤΤ���. 
!
!����  2009/09/10  �ݹ����� wt_galerkin_RR_cn_diff.f90 ��꼴�о��Ǥ˲�¤
!
program wt_zonal_galerkin_RR_cn_diff
  !
  !  wt_zonal_galerkin_module ����ץ�ץ����
  !      �����Υݥ�����®�پ�Ȼ�����(Crank-Nicolson scheme)
  !      ξ¦Ǵ����
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
  integer,parameter  :: im=1,  jm=16, km=16  ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=16         ! �����ȿ�������(��ʿ, ư��)

 !---- ��ɸ�ѿ��ʤ� ----
  real(8),parameter  :: ri=0.5, ro=1.5       ! �⳰Ⱦ��

 !---- �ѿ� ----
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Polvel     ! �ݥ�����®�پ�
  real(8), dimension(nm+1,4:lm)      :: wq_Polvel      ! �ݥ�����®�پ�
  real(8), dimension(nm+1,0:km)      :: wz_Polvel      ! �ݥ�����®�پ�
  real(8), dimension(nm+1,0:lm)      :: wt_LaplaPolvel ! ��^2�ݥ�����

  real(8), dimension(0:im-1,jm,0:km) :: xyz_PolvelInit  ! �ݥ�����®�پ�(�����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_PolvelSol   ! �ݥ�����®�پ�(���ϲ�)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Strfunc     ! ή���ؿ�
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VLat        ! ®�ٰ�����ʬ
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VRad        ! ®��ư����ʬ

 ! Crank Nicholson ����
  real(8)             :: DifLUMT_Polvel(nm+1,0:lm,0:lm)
                         ! Crank Nicholson ��Ū�׻��ѳȻ�����(�ݥ�����®�پ�)
  integer             :: kpivot_Polvel(nm+1,0:lm)
                         ! Crank Nicholson ��Ū�׻��ѳȻ�����ԥܥå�

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=1e-3                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=200, ndisp=20        ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  real(8), parameter :: nu=1.0                  ! Ǵ������

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8) :: alpha                   ! ư���ȿ�
  real(8) :: sigma                   ! ��ĹΨ
  real(8) :: C(4)                    ! ��Ω��η���

 !---- ����¾ ----
  integer :: it=0
  real(8) :: time=0                  ! ����
  integer :: k 
  integer :: n,m,l                   ! �ȿ�

 !---------------- ��ɸ�ͤ����� ---------------------
  call wt_galerkin_Initial(im,jm,km,nm,lm,ri,ro,velBC='RR')

  call CNDiffusionMatrixPolvel( nu, dt, DifLUMT_Polvel, kpivot_Polvel )

 !------------------- ��������� ----------------------
  write(6,*) 'Total horizontal, radial wavenumbers: n,l?'
  read(5,*)n,l

  alpha = CalAlpha(n,l)
  sigma = -nu* alpha **2
  write(6,*) 'Radial wavenumber : ', alpha
  write(6,*) 'Growth rate       : ', sigma

  C = CalcCoeff(n,alpha)

  wz_Polvel = 0.0
  do k=0,km
     wz_Polvel(l_nm(n,0),k) &
        =   C(1)*z_Rad(k)**n           + C(2)*z_Rad(k)**(-(n+1))   &
          + C(3)*sbj(n,alpha*z_Rad(k)) + C(4)*sbn(n,alpha*z_Rad(k))
  enddo

  xyz_Polvel = xyz_wz(wz_Polvel)
  xyz_PolvelInit = xyz_Polvel
  wq_Polvel = wq_wt(wt_xyz(xyz_Polvel))
  wt_LaplaPolvel = wt_Lapla_wt(wt_wq(wq_Polvel))

  call output_gtool4_init
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt                                         ! Euler ˡ�ˤ�������ʬ
     time = it * dt

     wt_LaplaPolvel = wt_LaplaPolvel &
          + dt/2 *( nu * wt_Lapla_wt(wt_LaplaPolvel)  )
     wq_Polvel = wq_LaplaPol2Pol_wt(wt_LaplaPolvel)
     wq_Polvel = LuSolve(DifLUMT_Polvel,kpivot_Polvel,wq_Polvel)
     wt_LaplaPolvel = wt_Lapla_wt(wt_wq(wq_Polvel))

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

  recursive function detNxN(a) result(determinant)
    !
    ! NxN ����ι��󼰤η׻�
    !
    implicit none
    real(8),intent(IN) :: a(:,:)
    real(8) :: determinant
    real(8),allocatable :: b(:,:)
    integer :: i,n

    if ( size(a,1) /= size(a,2) )then
       write(6,*) '**** detNxN **** Matrix is not square. '
       stop
    endif

    n=size(a,1)

    if ( n == 2 ) then
       determinant = a(1,1)*a(2,2)-a(2,1)*a(1,2)
    else
       allocate(b(n-1,n-1))
       b = a(2:n,2:n)
       determinant = a(1,1) * detNxN(b)
       do i=2,n-1
          b(1:i-1,:) = a(1:i-1,2:n)
          b(i:n-1,:) = a(i+1:n,2:n)
          determinant = determinant + (-1)**(i-1)*a(i,1)*detNxN(b)
       enddo
       b = a(1:n-1,2:n)
       determinant = determinant + (-1)**(n-1)*a(n,1)*detNxN(b)
       deallocate(b)
    end if

    return 

  end function detNxN

  function det(l,x)
    !
    ! �������󼰦��η׻�
    !
    integer, intent(IN) :: l                 ! ����
    real(8), intent(IN) :: x                 ! 
    real(8)             :: det               ! ����
    real(8)             :: a(4,4)            ! ����

    a(1,1) = ri**l       ; a(1,2) = ri**(-(l+1))
    a(1,3) = sbj(l,ri*x) ; a(1,4) = sbn(l,ri*x)

    a(2,1) = ro**l       ; a(2,2) = ro**(-(l+1))
    a(2,3) = sbj(l,ro*x) ; a(2,4) = sbn(l,ro*x)

    a(3,1) = l*ri**(l-1)    ; a(3,2) = -(l+1)*ri**(-(l+2))
    a(3,3) = x*sbj1(l,ri*x) ; a(3,4) = x*sbn1(l,ri*x)

    a(4,1) = l*ro**(l-1)    ; a(4,2) = -(l+1)*ro**(-(l+2))
    a(4,3) = x*sbj1(l,ro*x) ; a(4,4) = x*sbn1(l,ro*x)

    det=detNxN(a)

  end function det

  function CalcCoeff(l,alpha)
    !
    ! alpha ���᤿��η����η׻�  Ax=b ���
    !
    integer, intent(IN) :: l                 ! ����
    real(8), intent(IN) :: alpha             ! ư���ȿ�
    real(8)             :: CalcCoeff(4)      ! ����

    real(8)             :: a(3,3)            ! ϢΩ����������
    real(8)             :: b(3)              ! ϢΩ����������
    integer             :: kp(3)             ! �ԥܥå�

    a(1,1) = ri**l           ; a(1,2) = ri**(-(l+1))
    a(1,3) = sbj(l,ri*alpha) ; b(1)   = sbn(l,ri*alpha)

    a(2,1) = ro**l           ; a(2,2) = ro**(-(l+1))
    a(2,3) = sbj(l,ro*alpha) ; b(2)   = sbn(l,ro*alpha)

    a(3,1) = l*ri**(l-1)            ; a(3,2) = -(l+1)*ri**(-(l+2))
    a(3,3) = alpha*sbj1(l,ri*alpha) ; b(3) = alpha*sbn1(l,ri*alpha)

    call LUDecomp(a,kp)
    b =  LUSolve(a,kp,b)
    CalcCoeff(1:3) = b ; CalcCoeff(4) = -1

!!$    write(6,*) b(1)*l*ro**(l-1) - b(2)*(l+1)*ro**(-(l+2)) &
!!$             + b(3) * alpha*sbj1(l,ro*alpha) - alpha*sbn1(l,ro*alpha)

  end function CalcCoeff

  function CalAlpha(l,n)
    !
    ! ��=0 �β�����
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
       xs = xm ; ValS = det(l,xs)
    else
       xl = xm ; ValL = det(l,xl)
    endif
     
    if ( abs(xl-xs) .lt. eps ) then
       CalAlpha = xm
       goto 99
    endif
     
    goto 1000
     
99  continue
  end function CalAlpha
    
    !------------------- �Ȼ��� ----------------------
    subroutine CNDiffusionMatrixPolvel( Diffc, dt, DiffLUMatrix, kpivot )
      ! 
      ! Crank Nicholson �Ȼ���Ū�׻��ѹ��������
      ! (1-D dt/2��^2, LU ʬ��)
      ! 
      real(8), intent(IN)   :: Diffc    ! �Ȼ�����
      real(8), intent(IN)   :: dt       ! ���ֹ�

      ! Crank Nicholson ����
      real(8), intent(OUT)  :: DiffLUMatrix(nm+1,4:lm,4:lm)
                              ! Crank Nicholson �Ȼ���Ū�׻��ѹ���
                              ! (1-D dt/2��^2, LU ʬ��)

      integer, intent(OUT)  :: kpivot(nm+1,4:lm)     ! �ԥܥåȾ���
                              ! Crank Nicholson �Ȼ���Ū�׻��ѹ���ԥܥåȾ���

      real(8)               :: wq_I(nm+1,4:lm)       ! ������ѿ�
      real(8)               :: wt_I(nm+1,0:lm)       ! ������ѿ�

      integer :: l

      DiffLUMatrix = 0.0
      do l=4,lm
         wq_I = 0.0 ; wq_I(:,l) = 1.0             ! ���ȿ���ʬ��Ω
         wt_I = wt_wq(wq_I)
         wt_I = wt_Lapla_wt(wt_I)
         wt_I = wt_I - Diffc * dt/2.0 * wt_Lapla_wt(wt_I)
         DiffLUMatrix(:,:,l) = wq_LaplaPol2Pol_wt(wt_I)
      enddo

      call LuDeComp(DiffLUMatrix,kpivot)

    end subroutine CNDiffusionMatrixPolvel

 !------------------- ���ϥ롼���� ----------------------
  subroutine output_gtool4_init
    !
    ! �ҥ��ȥ꡼���Ͻ�����롼����
    !
    !   �ե��������
    !   �ѿ����
    !
    call HistoryCreate( &                                  ! �ҥ��ȥ꡼����
           file='wt_zonal_galerkin_RR_cn_diff.nc', &
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
           varname='polvel', dims=(/'lat','rad','t  '/), & 
           longname='Poloidal velocity field', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='polvelsol', dims=(/'lat','rad','t  '/), & 
           longname='Poloidal velocity field(analytic)', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='polvelerror', dims=(/'lat','rad','t  '/), & 
           longname='Poloidal velocity field(error)', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='strfunc', dims=(/'lat','rad','t  '/), & 
           longname='Stream function', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='vlat', dims=(/'lat','rad','t  '/), & 
           longname='Lat-velocity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='vrad', dims=(/'lat','rad','t  '/), & 
           longname='Rad-velocity', units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    !
    ! �ҥ��ȥ꡼����
    !
    !   ��ʪ���̤� gtool4 �ե�����ؤν��Ϥ�Ԥ�
    !
    write(6,*) 'it = ',it
    xyz_Polvel = xyz_wt(wt_wq(wq_Polvel))
    xyz_PolvelSol = xyz_PolvelInit* exp( sigma * time )

    xyz_Strfunc = xyz_GradLat_wt(wt_wq(wq_Polvel))*xyz_Rad**2*cos(xyz_Lat)
    xyz_VLat = xyz_GradLat_wt(wt_RotRad_wt(wt_wq(wq_Polvel)))*xyz_Rad
    xyz_VRad = xyz_wt(wt_L2_wt(wt_wq(wq_Polvel)))/xyz_Rad

    call HistoryPut('t',real(it*dt))
    call HistoryPut('polvel',xyz_Polvel(0,:,:))
    call HistoryPut('polvelsol',xyz_PolvelSol(0,:,:))
    call HistoryPut('polvelerror',xyz_Polvel(0,:,:)-xyz_PolvelSol(0,:,:))
    call HistoryPut('strfunc',xyz_Strfunc(0,:,:))
    call HistoryPut('vlat',xyz_VLat(0,:,:))
    call HistoryPut('vrad',xyz_VRad(0,:,:))
  end subroutine output_gtool4

  subroutine output_gtool4_close
    !
    ! �ҥ��ȥ꡼���Ͻ�λ����
    !
    call HistoryClose
  end subroutine output_gtool4_close

end program wt_zonal_galerkin_RR_cn_diff
