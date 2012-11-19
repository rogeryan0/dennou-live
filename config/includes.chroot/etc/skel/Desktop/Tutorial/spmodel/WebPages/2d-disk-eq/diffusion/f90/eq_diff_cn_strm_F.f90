!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  eq_module ����ץ�ץ����
!      �������ή���ؿ��Ȼ�����(Crank-Nicolson scheme)
!      ��¦���٤���
!
!      �׻������ӤΤ���β��ϲ�ˤĤ��Ƥϲ���ʸ��
!
!         ��2 ���������ΰ�ǤγȻ����������β��ϲ��
!
!      (eq_diffusion.pdf)�򻲾ȤΤ���. 
!
!����  2008/04/16  �ݹ�����
!
program eq_diff_cn_polmag_F
  !
  !  eq_module ����ץ�ץ����
  !      �������ή���ؿ��Ȼ�����(Crank-Nicolson scheme)
  !      ��¦���٤�Ǵ����
  !
  !      �׻������ӤΤ���β��ϲ�ˤĤ��Ƥϲ���ʸ��
  !
  !         ��2 ���������ΰ�ǤγȻ����������β��ϲ��
  !
  !      (eq_diffusion.pdf)�򻲾ȤΤ���. 
  !
  use dc_message
  use lumatrix
  use eq_module
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  integer,parameter  :: im=32, jm=16         ! �ʻ���������(���̳�, ư��)
  integer,parameter  :: km=10, lm=21         ! �����ȿ�������(���̳�, ư��)

 !---- ��ɸ�ѿ��ʤ� ----
  real(8),parameter  :: ra=1.5               ! ��Ⱦ��

 !---- �ѿ� ----
  real(8), dimension(jm,0:im-1)   :: rp_Strm      ! ή���ؿ�
  real(8), dimension(-km:km,0:lm) :: eq_Strm      ! ή���ؿ�
  real(8), dimension(-km:km,jm)   :: er_Strm      ! ή���ؿ�
  real(8), dimension(-km:km,0:lm) :: eq_Vor ! ����(��^2ή��)

  real(8), dimension(jm,0:im-1)  :: rp_StrmInit  ! �ݥ�����®�پ�(�����)
  real(8), dimension(jm,0:im-1)  :: rp_StrmSol   ! �ݥ�����®�پ�(���ϲ�)

 ! Crank Nicholson ����
  real(8)             :: DifLUMT_Strm(-km:km,0:lm,0:lm)
                         ! Crank Nicholson ��Ū�׻��ѳȻ�����(�ݥ�����®�پ�)
  integer             :: kpivot_Strm(-km:km,0:lm)
                         ! Crank Nicholson ��Ū�׻��ѳȻ�����ԥܥå�

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=1e-3                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=100, ndisp=10        ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  real(8), parameter :: nu=1.0                  ! Ǵ������

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8) :: alpha                   ! ư���ȿ�
  real(8) :: sigma                   ! ��ĹΨ
  real(8) :: C(2)                    ! ��Ω��η���

 !---- ����¾ ----
  integer :: it=0
  real(8) :: time=0                  ! ����
  integer :: j,k,l

 !---------------- ��ɸ�ͤ����� ---------------------
  call eq_Initial(im,jm,km,lm,ra)

  call CNDiffusionMatrixStrm( nu, dt, DifLUMT_Strm, kpivot_Strm )

 !------------------- ��������� ----------------------
  write(6,*) 'Azimuthal, radial wavenumbers: k,l?'
  read(5,*)k,l

  alpha = CalAlpha(k,l)
  sigma = -nu* alpha **2
  write(6,*) 'Radial wavenumber : ', alpha
  write(6,*) 'Groeqh rate       : ', sigma

  C = CalcCoeff(k,alpha)

  er_Strm = 0.0
  do j=1,jm
     er_Strm(k,j) &
        =   C(1)*r_Rad(j)**k  + C(2)*bessj(k,alpha*r_Rad(j))
  enddo

  eq_Strm = eq_er(er_Strm)
  rp_StrmInit = rp_eq(eq_Strm)
  eq_Vor = eq_er(er_Lapla_eq(eq_Strm))

  call output_gtool4_init
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt                                         ! Euler ˡ�ˤ�������ʬ
     time = it * dt

     eq_Vor = eq_Vor &
          + dt/2 *( nu * eq_er(er_Lapla_eq(eq_Vor))  )
     eq_Strm = eq_Vor2Strm_eq(eq_Vor,cond='F')
     eq_Strm = LuSolve(DifLUMT_Strm,kpivot_Strm,eq_Strm)
     eq_Vor = eq_er(er_Lapla_eq(eq_Strm))

     if(mod(it,ndisp) .eq. 0)then                    ! ����
        call output_gtool4
     endif
  enddo
  call output_gtool4_close

contains
  !
  ! Bessel functions
  !
  function bessj(n,x)
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: bessj
    integer             :: icon

    call DBJN(x,n,bessj,icon)

    if ( icon /= 0 ) then
       write(6,*) 'icon=',icon
       call MessageNotify('E','bessj','SSL2 ROUTINE DBJN ERROR')
    endif

  end function bessj

  function dbjdx(n,x)
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: dbjdx
    real(8)             :: bessj, bessj1
    integer             :: icon

    call DBJN(x,n,bessj,icon)
    if ( icon /= 0 ) then
       write(6,*) 'icon=',icon
       call MessageNotify('E','dbjdx','SSL2 ROUTINE DBJN ERROR')
    endif
    call DBJN(x,n+1,bessj1,icon)
    if ( icon /= 0 ) then
       write(6,*) 'icon=',icon
       call MessageNotify('E','dbjdx','SSL2 ROUTINE DBJN ERROR')
    endif

    dbjdx = n/x*bessj - bessj1

  end function dbjdx

  function d2bjdx2(n,x)
    real(8), intent(IN) :: x
    integer, intent(IN) :: n
    real(8)             :: d2bjdx2

    d2bjdx2 = - dbjdx(n,x)/x - (1-n**2/x**2) * bessj(n,x)

  end function d2bjdx2

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
    real(8)             :: a(2,2)            ! ����

    a(1,1) = ra**l             ; a(1,2) = bessj(l,ra*x)
    a(2,1) = l*(l-2)*ra**(l-2) ; a(2,2) = x**2* d2bjdx2(l,ra*x) - x/ra*dbjdx(l,ra*x)

    det=detNxN(a)

  end function det

  function CalcCoeff(l,alpha)
    !
    integer, intent(IN) :: l                 ! ����
    real(8), intent(IN) :: alpha             ! ư���ȿ�
    real(8)             :: CalcCoeff(2)      ! ����

    CalcCoeff(1) = bessj(l,ra*alpha) 
    CalcCoeff(2) = -ra**l

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
    dx = PI/ra/nx
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
    subroutine CNDiffusionMatrixStrm( Diffc, dt, DiffLUMatrix, kpivot )
      ! 
      ! Crank Nicholson �Ȼ���Ū�׻��ѹ��������
      ! (1-D dt/2��^2, LU ʬ��)
      ! 
      real(8), intent(IN)   :: Diffc    ! �Ȼ�����
      real(8), intent(IN)   :: dt       ! ���ֹ�

      ! Crank Nicholson ����
      real(8), intent(OUT)  :: DiffLUMatrix(-km:km,0:lm,0:lm)
                              ! Crank Nicholson �Ȼ���Ū�׻��ѹ���
                              ! (1-D dt/2��^2, LU ʬ��)

      integer, intent(OUT)  :: kpivot(-km:km,0:lm)     ! �ԥܥåȾ���
                              ! Crank Nicholson �Ȼ���Ū�׻��ѹ���ԥܥåȾ���

      real(8)               :: eq_I(-km:km,0:lm)       ! ������ѿ�
      real(8)               :: eq_DI(-km:km,0:lm)       ! ������ѿ�

      integer :: l

      DiffLUMatrix = 0.0
      do l=0,lm
         eq_I = 0.0 ; eq_I(:,l) = 1.0             ! ���ȿ���ʬ��Ω
         eq_DI = - Diffc * dt/2.0 &
                  * eq_er(er_Lapla_eq(eq_er(er_Lapla_eq(eq_I))))
         eq_DI = eq_Vor2Strm_eq(eq_DI,cond='F')
         DiffLUMatrix(:,:,l) = eq_I + eq_DI
      enddo

      call LuDeComp(DiffLUMatrix,kpivot)

    end subroutine CNDiffusionMatrixStrm

 !------------------- ���ϥ롼���� ----------------------
  subroutine output_gtool4_init
    !
    ! �ҥ��ȥ꡼���Ͻ�����롼����
    !
    !   �ե��������
    !   �ѿ����
    !
    call HistoryCreate( &                                  ! �ҥ��ȥ꡼����
           file='eq_diff_cn_strm_F.nc', &
           title='Diffusion model in a circle', &
           source='Sample program of spmodel library', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'phi','rad','t  '/), dimsizes=(/im,jm,0/),&
           longnames=(/'Azimuth','Radius ','time   '/),&
           units=(/'1','1','1'/),   &
           origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('phi',p_Phi/pi*180)                       ! �ѿ�����
    call HistoryPut('rad',r_Rad)                              ! �ѿ�����

    call HistoryAddVariable( &                                ! �ѿ����
           varname='strm', dims=(/'rad','phi','t  '/), & 
           longname='stream function', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='strmsol', dims=(/'rad','phi','t  '/), & 
           longname='stream function (analytic)', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='strmerror', dims=(/'rad','phi','t  '/), & 
           longname='stream function (error)', units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    !
    ! �ҥ��ȥ꡼����
    !
    !   ��ʪ���̤� gtool4 �ե�����ؤν��Ϥ�Ԥ�
    !
    write(6,*) 'it = ',it
    rp_Strm = rp_eq(eq_Strm)
    rp_StrmSol = rp_StrmInit* exp( sigma * time )

    call HistoryPut('t',real(it*dt))
    call HistoryPut('strm',rp_Strm)
    call HistoryPut('strmsol',rp_StrmSol)
    call HistoryPut('strmerror',rp_Strm-rp_StrmSol)
  end subroutine output_gtool4

  subroutine output_gtool4_close
    !
    ! �ҥ��ȥ꡼���Ͻ�λ����
    !
    call HistoryClose
  end subroutine output_gtool4_close

end program eq_diff_cn_polmag_F
