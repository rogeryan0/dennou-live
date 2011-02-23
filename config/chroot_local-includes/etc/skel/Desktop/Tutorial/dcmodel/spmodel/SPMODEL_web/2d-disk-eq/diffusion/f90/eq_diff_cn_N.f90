!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  eq_module
!      ����Υȥ����뼧��Ȼ�����(Crank-Nicolson scheme)
!
!      �����ӥ����ա�����ˡ�ˤ�붭�����Ŭ��
!
!      �׻������ӤΤ���β��ϲ�ˤĤ��Ƥϲ���ʸ��
!
!         ��3 �������ΰ�ǤγȻ����������β��ϲ��
!
!      (eq_diffusion.pdf)�򻲾ȤΤ���. 
!
!����  2008/04/15  �ݹ�����
!
program eq_diff_cn_N

  use dc_message
  use dc_string,   only : StoA
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
  real(8), dimension(jm,0:im-1)      :: rp_Temp   ! �ȥ����뼧��
  real(8), dimension(-km:km,0:lm)    :: eq_Temp    ! �ȥ����뼧��
  real(8), dimension(-km:km,jm)      :: er_Temp    ! �ȥ����뼧��

  real(8), dimension(jm,0:im-1)     :: rp_TempInit ! �ȥ����뼧��(�����)
  real(8), dimension(jm,0:im-1)     :: rp_TempSol  ! �ȥ����뼧��(���ϲ�)

 ! Crank Nicholson ��Ū�׻��ѳȻ�����(�ȥ����뼧��)
  real(8)             :: DifLUMT_Temp(-km:km,0:lm,0:lm)
  integer             :: kpivot_Temp(-km:km,0:lm)    ! �ԥܥå�

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=1e-3                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=100, ndisp=10        ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  real(8), parameter :: kappa=1.0                  ! �Ȼ�����

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8) :: alpha                   ! ư���ȿ�
  real(8) :: sigma                   ! ��ĹΨ

 !---- ����¾ ----
  integer :: it=0
  real(8) :: time=0                  ! ����
  integer :: j,k,n

 !---------------- ��ɸ�ͤ����� ---------------------
  call eq_Initial(im,jm,km,lm,ra)

  call CNDiffusionMatrix( kappa, dt, DifLUMT_Temp, kpivot_Temp )

 !------------------- ��������� ----------------------
  write(6,*)'k,n?'
  read(5,*)k,n

  alpha = CalAlpha(k,n)
  sigma = -kappa* alpha **2
  write(6,*) 'Radial wavenumber : ', alpha
  write(6,*) 'Growth rate       : ', sigma

  er_Temp = 0.0
  do j=1,jm
     er_Temp(k,j) = bessj(k,alpha*r_Rad(j))
  enddo
  rp_Temp = rp_er(er_Temp)
  rp_TempInit = rp_Temp
  eq_Temp = eq_rp(rp_Temp)

  call output_gtool4_init
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt                                         ! Euler ˡ�ˤ�������ʬ
     time = it * dt
     eq_Temp = eq_Temp + dt/2 * eq_er( kappa * er_Lapla_eq(eq_Temp) )
     call eq_Boundary(eq_Temp,cond='N')
     eq_Temp = LUSolve(DifLUMT_Temp,kpivot_Temp,eq_Temp)

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
   !-----------------------------------------
   ! dbjdx(ra*x)=0 �β�����
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
      dx = PI/ra/nx
      xl = dx ; ValL = dbjdx(l,ra*xl)

      !
      ! det = 0 �Ȥʤ����� x=0 ����� dx ������õ���Ƥ���
      ! n ���ܤ򶴤� xl, xs �����ꤹ��.
      !
100   continue
      xs =xl ; ValS = ValL
      xl = xl+dx
      ValL = dbjdx(l,ra*xl)
      if( ValL*ValS .le. 0.0 ) nn=nn-1
      if( nn .lt. 0 ) goto 199
      goto 100
199 continue

1000  xm = (xs + xl)/2.0
      ValM = dbjdx(l,ra*xm)
      
      if ( ValS * ValM .GT. 0.0D0 ) then
         xs = xm ; ValS=dbjdx(l,ra*xs)
      else
         xl = xm ; ValL=dbjdx(l,ra*xl)
      endif

      if ( abs(xl-xs) .lt. eps ) then
         CalAlpha = xm
         goto 99
      endif

      goto 1000

99    continue
    end function CalAlpha

 !------------------- �Ȼ��� ----------------------
  subroutine CNDiffusionMatrix( Diffc, dt, DiffLUMatrix, kpivot )

    real(8), intent(IN)          :: Diffc    ! �Ȼ�����
    real(8), intent(IN)          :: dt       ! ���ֹ�

    ! Crank Nicholson �Ȼ���Ū�׻��ѹ���(1-D dt/2��^2, LU ʬ��)
    real(8), dimension(-km:km,0:lm,0:lm), intent(OUT)  :: DiffLUMatrix
    ! �ԥܥåȾ���
    integer, dimension(-km:km,0:lm), intent(OUT)       :: kpivot

    ! ������ѿ�
    real(8), dimension(-km:km,0:lm)           :: eq_I
    real(8), dimension(-km:km,0:lm)           :: eq_DI

    integer :: l

    DiffLUMatrix = 0.0
    do l=0,lm
       eq_I = 0.0 ; eq_I(:,l) = 1.0             ! ���ȿ���ʬ��Ω
       eq_DI =  - Diffc * dt/2.0 * eq_er(er_Lapla_eq(eq_I))
       call eq_Boundary(eq_DI,cond='N')
       DiffLUMatrix(:,:,l) = eq_I + eq_DI
    enddo

    call ludecomp(DiffLUMatrix,kpivot)

  end subroutine CNDiffusionMatrix

 !------------------- ���ϥ롼���� ----------------------
  subroutine output_gtool4_init
    call HistoryCreate( &                                  ! �ҥ��ȥ꡼����
           file='eq_diff_cn_N.nc', &
           title='Diffusion model in a circle', &
           source='Sample program of spmodel library', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'phi','rad','t  '/), dimsizes=(/im,jm,0/),&
           longnames=StoA('Azimuth','Radius','time'),&
           units=(/'1','1','1'/),   &
           origin=0.0, interval=real(ndisp*dt) )

    call HistoryPut('phi',p_Phi/pi*180)                       ! �ѿ�����
    call HistoryPut('rad',r_Rad)                              ! �ѿ�����

    call HistoryAddVariable( &                                ! �ѿ����
           varname='temp', dims=(/'rad','phi','t  '/), & 
           longname='temperature', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='tempsol', dims=(/'rad','phi','t  '/), & 
           longname='temperature(analytic)', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='temperror', dims=(/'rad','phi','t  '/), & 
           longname='temperature(error)', &
           units='1', xtype='double')
  end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it
    rp_Temp = rp_eq(eq_Temp)
    rp_TempSol = rp_TempInit* exp( sigma * time )

    call HistoryPut('t',real(it*dt))
    call HistoryPut('temp',rp_Temp)
    call HistoryPut('tempsol',rp_TempSol)
    call HistoryPut('temperror',rp_Temp-rp_TempSol)
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

end program eq_diff_cn_N
