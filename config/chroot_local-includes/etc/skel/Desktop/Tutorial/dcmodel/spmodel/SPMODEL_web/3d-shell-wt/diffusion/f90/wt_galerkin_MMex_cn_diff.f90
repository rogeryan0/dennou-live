!----------------------------------------------------------------------
!     Copyright (c) 2006--2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wt_galerkin_module ����ץ�ץ����
!      �����Υݥ����뼧��Ȼ�����(Crank-Nicolson scheme)
!
!      �׻������ӤΤ���β��ϲ�ˤĤ��Ƥϲ���ʸ��
!
!         ��3 ��������ΰ�ǤγȻ����������β��ϲ��
!
!      (wt_diffusion.pdf)�򻲾ȤΤ���. 
!
!����  2006/02/19  �ݹ�����
!      2006/03/02  �ݹ�����  RDoc �Ѥ˥����Ƚ���
!      2008/08/13  �ݹ�����  ����ź�����ѹ� im -> 0:im-1
!
program wt_galerkin_MMex_cn_diff
  !
  !  wt_galerkin_module ����ץ�ץ����
  !      �����Υݥ����뼧��Ȼ�����(Crank-Nicolson scheme)
  !
  !      �׻������ӤΤ���β��ϲ�ˤĤ��Ƥϲ���ʸ��
  !
  !         ��3 ��������ΰ�ǤγȻ����������β��ϲ��
  !
  !      (wt_diffusion.pdf)�򻲾ȤΤ���. 
  !
  use dc_message
  use lumatrix
  use wt_galerkin_module
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  integer,parameter  :: im=32, jm=16, km=32  ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=20         ! �����ȿ�������(��ʿ, ư��)

 !---- ��ɸ�ѿ��ʤ� ----
  real(8),parameter  :: ri=0.5, ro=1.5       ! �⳰Ⱦ��

 !---- �ѿ� ----
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Polmag   ! �ݥ����뼧��
  real(8), dimension((nm+1)**2,2:lm) :: wh_Polmag    ! �ݥ����뼧��
  real(8), dimension((nm+1)**2,0:km) :: wz_Polmag    ! �ݥ����뼧��

  real(8), dimension(0:im-1,jm,0:km) :: xyz_PolmagInit ! �ݥ����뼧��(�����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_PolmagSol  ! �ݥ����뼧��(���ϲ�)

 ! Crank Nicholson ����
  real(8)             :: DifLUMT_Polmag((nm+1)*(nm+1),0:lm,0:lm)
                         ! Crank Nicholson ��Ū�׻��ѳȻ�����(�ݥ����뼧��)
  integer             :: kpivot_Polmag((nm+1)*(nm+1),0:lm)   
                         ! Crank Nicholson ��Ū�׻��ѳȻ�����ԥܥå�
 ! �����������������
  real(8)      :: cfdx0_ri((nm+1)*(nm+1))    ! �����������������(0����ʬ@r=ri)
  real(8)      :: cfdx1_ri((nm+1)*(nm+1))    ! �����������������(1����ʬ@r=ri)
  real(8)      :: cfdx0_ro((nm+1)*(nm+1))    ! �����������������(0����ʬ@r=ro)
  real(8)      :: cfdx1_ro((nm+1)*(nm+1))    ! �����������������(1����ʬ@r=ro)

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=1e-3                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=200, ndisp=20        ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  real(8), parameter :: kappa=1.0                  ! Ǵ������

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8) :: alpha                   ! ư���ȿ�
  real(8) :: sigma                   ! ��ĹΨ

 !---- ����¾ ----
  integer :: it=0
  real(8) :: time=0                  ! ����
  integer :: k 
  integer :: n,m,l                   ! �ȿ�

 !---------------- ��ɸ�ͤ����� ---------------------
  do n=0,nm
     do m=-n,n
        cfdx1_ri(l_nm(n,m))  = 1.0D0
        cfdx0_ri(l_nm(n,m)) = -n/ri
        cfdx1_ro(l_nm(n,m)) = 1.0D0
        cfdx0_ro(l_nm(n,m)) = (n+1)/ro
     enddo
  enddo

  call wt_galerkin_Initial(im,jm,km,nm,lm,ri,ro,             &
         MMex_cfdx0_ri=cfdx0_ri, MMex_cfdx1_ri=cfdx1_ri, &
         MMex_cfdx0_ro=cfdx0_ro, MMex_cfdx1_ro=cfdx1_ro   )

  call CNDiffusionMatrixMMex( kappa, dt, DifLUMT_Polmag, kpivot_Polmag )

 !------------------- ��������� ----------------------
  write(6,*)'n,m,l?'
  read(5,*)n,m,l

  alpha = CalAlpha(n,l)
  sigma = -kappa* alpha **2
  write(6,*) alpha, sigma

  wz_Polmag = 0.0
  do k=0,km
     wz_Polmag(l_nm(n,m),k) = -sbn(n+1,alpha*ri)*sbj(n,alpha*z_Rad(k)) &
                            +sbj(n+1,alpha*ri)*sbn(n,alpha*z_Rad(k))
  enddo
  xyz_Polmag = xyz_wz(wz_Polmag)
  xyz_PolmagInit = xyz_Polmag
  wh_Polmag = wh_wt(wt_xyz(xyz_Polmag))

  call output_gtool4_init
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt                                         ! Euler ˡ�ˤ�������ʬ
     time = it * dt
     wh_Polmag = wh_Polmag + dt/2 * wh_wt( kappa * wt_Lapla_wt(wt_wh(wh_Polmag)) )
     wh_Polmag = LUSolve(DifLUMT_Polmag,kpivot_Polmag,wh_Polmag)

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

  function det(l,x)
    !
    ! �������󼰤η׻�
    ! ��=sbj(l+1,ri*x)sbn(l-1,ro*x)-sbn(l+1,ri*x)sbj(l-1,ro*x)
    !
    integer, intent(IN) :: l                 ! ����
    real(8), intent(IN) :: x                 ! 
    real(8)             :: det               ! ����

    det=sbj(l+1,ri*x)*sbn(l-1,ro*x) - sbn(l+1,ri*x)*sbj(l-1,ro*x)    
  end function det

  function CalAlpha(l,n)
    !
    ! ��=sbj(ri*x)sbn(ro*x)-sbn(ri*x)sbj(ro*x) �β�����
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
    subroutine CNDiffusionMatrixMMex( Diffc, dt, DiffLUMatrix, kpivot )
      ! 
      ! Crank Nicholson �Ȼ���Ū�׻��ѹ��������
      ! (1-D dt/2��^2, LU ʬ��)
      ! 
      real(8), intent(IN)   :: Diffc    ! �Ȼ�����
      real(8), intent(IN)   :: dt       ! ���ֹ�

      ! Crank Nicholson ����
      real(8), intent(OUT)  :: DiffLUMatrix((nm+1)*(nm+1),2:lm,2:lm)
                              ! Crank Nicholson �Ȼ���Ū�׻��ѹ���
                              ! (1-D dt/2��^2, LU ʬ��)
      integer, intent(OUT)  :: kpivot((nm+1)*(nm+1),2:lm)     ! �ԥܥåȾ���
                              ! Crank Nicholson �Ȼ���Ū�׻��ѹ���ԥܥåȾ���

      real(8)               :: wh_I((nm+1)*(nm+1),2:lm)       ! ������ѿ�

      integer :: l

      DiffLUMatrix = 0.0
      do l=2,lm
         wh_I = 0.0 ; wh_I(:,l) = 1.0             ! ���ȿ���ʬ��Ω
         DiffLUMatrix(:,:,l) &
              = wh_I - Diffc * dt/2.0 * wh_wt(wt_Lapla_wt(wt_wh(wh_I)))
      enddo

      call LuDeComp(DiffLUMatrix,kpivot)

    end subroutine CNDiffusionMatrixMMex

 !------------------- ���ϥ롼���� ----------------------
  subroutine output_gtool4_init
    !
    ! �ҥ��ȥ꡼���Ͻ�����롼����
    !
    !   �ե��������
    !   �ѿ����
    !
    call HistoryCreate( &                                  ! �ҥ��ȥ꡼����
           file='wt_galerkin_MMex_cn_diff.nc', &
           title='Diffusion model in a spherical shell', &
           source='Sample program of spmodel library', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'lon','lat','rad','t  '/), dimsizes=(/im,jm,km+1,0/),&
           longnames=(/'Longitude','Latitude ','Radius   ','time     '/),&
           units=(/'1','1','1','1'/),   &
           origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('lon',x_Lon/pi*180)                       ! �ѿ�����
    call HistoryPut('lat',y_Lat/pi*180)                       ! �ѿ�����
    call HistoryPut('rad',z_Rad)                              ! �ѿ�����

    call HistoryAddVariable( &                                ! �ѿ����
           varname='polmag', dims=(/'lon','lat','rad','t  '/), & 
           longname='Poloidal Magnetic field', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='polmagsol', dims=(/'lon','lat','rad','t  '/), & 
           longname='Poloidal Magnetic field(analytic)', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='polmagerror', dims=(/'lon','lat','rad','t  '/), & 
           longname='Poloidal Magnetic field(error)', &
           units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    !
    ! �ҥ��ȥ꡼����
    !
    !   ��ʪ���̤� gtool4 �ե�����ؤν��Ϥ�Ԥ�
    !
    write(6,*) 'it = ',it
    xyz_Polmag = xyz_wt(wt_wh(wh_Polmag))
    xyz_PolmagSol = xyz_PolmagInit* exp( sigma * time )

    call HistoryPut('t',real(it*dt))
    call HistoryPut('polmag',xyz_Polmag)
    call HistoryPut('polmagsol',xyz_PolmagSol)
    call HistoryPut('polmagerror',xyz_Polmag-xyz_PolmagSol)
  end subroutine output_gtool4

  subroutine output_gtool4_close
    !
    ! �ҥ��ȥ꡼���Ͻ�λ����
    !
    call HistoryClose
  end subroutine output_gtool4_close

end program wt_galerkin_MMex_cn_diff
