!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  SPMODEL 2 ������ϩ�ΰ�֥��ͥ�����ή��ǥ�
!      �����ӥ����ե��顼����ˡ�ˤ��׻�(���ٸ�����)
!
!      ������ʬ : Crank Nicolson ˡ
!
! Solving 2-D Diffusioon equation (MMex problem)
!     d��/dt = \nabla ��
!     d��+|k|�� = 0 at y=d/2, ��-|k|�� = 0 at y=-d/2
!
!      �׻������ӤΤ���β��ϲ�ˤĤ��Ƥϲ���ʸ��
!
!         ��2 ������ϩ�ΰ�ǤγȻ����������β��ϲ��
!
!      (et_diffusion.pdf)�򻲾ȤΤ���. 
!
!����  2009/09/16  �ݹ�����  et_galerkin_MMex_cn_diff.f90 �� fftj �Ѥ˲�¤
!      
!
program et_galerkin_module_fftj_MMex_cn_diff
  !
  ! SPMODEL 2 ������ϩ�ΰ�֥��ͥ�����ή��ǥ�
  !      �����ӥ����ե��顼����ˡ�ˤ��׻�(���ٸ�����)
  !
  !      ������ʬ : Crank Nicolson ˡ
  !
  ! Solving 2-D Diffusioon equation (MMex problem)
  !     d��/dt = \nabla ��
  !     d��+|k|�� = 0 at y=d/2, ��-|k|�� = 0 at y=-d/2
  !
  !      �׻������ӤΤ���β��ϲ�ˤĤ��Ƥϲ���ʸ��
  !
  !         ��2 ������ϩ�ΰ�ǤγȻ����������β��ϲ��
  !
  !      (et_diffusion.pdf)�򻲾ȤΤ���. 
  !
  use lumatrix
  use et_galerkin_module_fftj
  use gt4_history
  use dc_message
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter  :: im=64, jm=32      ! �ʻ���������(X,Y)
  integer, parameter  :: km=21, lm=20      ! �����ȿ�������(X,Y)

 !---- �ѿ� ----
  real(8)             :: yx_Phi(0:jm,0:im-1)     ! �ʻҥǡ���(����)
  real(8)             :: eh_Phi(-km:km,2:lm)     ! ���顼����ǡ���(����)

  real(8)             :: yx_PhiInit(0:jm,0:im-1) ! �ʻҥǡ���(����, �����)
  real(8)             :: yx_PhiSol(0:jm,0:im-1)  ! �ʻҥǡ���(����, ���ϲ�)
  real(8)             :: yx_PhiError(0:jm,0:im-1)! �ʻҥǡ���(����, ��)

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=1e-3                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=100, ndisp=10        ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  real(8), parameter          :: XL=2.0         ! y �����ΰ���礭��
  real(8), parameter          :: YD=1.0         ! y �����ΰ���礭��
  real(8), parameter          :: Kappa=0.5      ! �Ȼ�����

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter  :: xmin=0, xmax=XL        ! X ��ɸ�ϰ�
  real(8), parameter  :: ymin=-YD/2, ymax=YD/2  ! Y ��ɸ�ϰ�

  real(8)             :: cfdy0_ymin(-km:km)     ! (0����ʬ@x=xmin)
  real(8)             :: cfdy1_ymin(-km:km)     ! (1����ʬ@x=xmin)
  real(8)             :: cfdy0_ymax(-km:km)     ! (0����ʬ@x=xmax)
  real(8)             :: cfdy1_ymax(-km:km)     ! (1����ʬ@x=xmax)

 !--- Crank Nicholson ��Ū�׻��Ѻ���ѿ� ---
  real(8) :: DiffLUMT_MMex(-km:km,2:lm,2:lm)
                                      ! Crank Nicholson ��Ū�׻��ѹ���
  integer :: kpivot_MMex(-km:km,2:lm) ! Crank Nicholson �ѹ���ԥܥåȾ���

 !---- ����¾ ----
  real(8), parameter  :: pi=3.1415926535897932385D0
  integer :: it=0
  real(8) :: time=0.0D0
  real(8) :: rk, rl                              ! x, y �����ȿ�
  integer :: k, l

 !---------------- ��ɸ�ͤ����� ---------------------
  do k=-km,km
     cfdy1_ymax(k) = 1.0D0
     cfdy0_ymax(k) = abs(2*k*Pi/XL)
     cfdy1_ymin(k) = 1.0D0
     cfdy0_ymin(k) = -abs(2*k*Pi/XL)
  enddo

  call et_galerkin_Initial(im,jm,km,lm,xmin,xmax,ymin,ymax,     &
         MMex_cfdy0_ymin=cfdy0_ymin, MMex_cfdy1_ymin=cfdy1_ymin,&
         MMex_cfdy0_ymax=cfdy0_ymax, MMex_cfdy1_ymax=cfdy1_ymax   )

  call CNDiffusionMatrixMMex( kappa, dt, DiffLUMT_MMex, kpivot_MMex )

 !------------------- ��������� ----------------------
  write(6,*) 'Positive k:cos(kx), negative k: sin(kx)'
  write(6,*) 'Positvie l:cos(lx), negative l: sin(lx)'
  write(6,*) 'k,l?'
  read(5,*)  k,l

  rk=2*abs(k)*Pi/XL
  if ( k .ge. 0 .AND. l .ge. 0) then
     write(6,*) 'Type of the initial value : cos(kx)*cos(ly)'
     rl = InvXtanX(rk*YD/2,l)*2/YD
     yx_Phi = cos(rk*yx_X) * cos(rl*yx_Y)
  else if ( k .lt. 0 .AND. l .ge. 0) then
     write(6,*) 'Type of the initial value : sin(kx)*cos(ly)'
     rl = InvXtanX(rk*YD/2,l)*2/YD
     yx_Phi = sin(rk*yx_X) * cos(rl*yx_Y)
  else if ( k .ge. 0 .AND. l .lt. 0) then
     write(6,*) 'Type of the initial value : cos(kx)*sin(ly)'
     rl = InvXcotX(-rk*YD/2,abs(l))*2/YD
     yx_Phi = cos(rk*yx_X) * sin(rl*yx_Y)
  else 
     write(6,*) 'Type of the initial value : sin(kx)*sin(ly)'
     rl = InvXcotX(-rk*YD/2,abs(l))*2/YD
     yx_Phi = sin(rk*yx_X) * sin(rl*yx_Y)
  endif
  yx_PhiInit = yx_Phi
  
  eh_Phi = eh_et(et_yx(yx_Phi))

  call output_gtool4_init                            ! �ҥ��ȥ꡼�����
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt               ! Adams-Bashfort & Crank-Nicolson ˡ�ˤ�������ʬ
     time =it*dt

     eh_Phi = eh_Phi + dt/2 * eh_et(kappa * et_Lapla_et(et_eh(eh_Phi)))
     eh_Phi = LuSolve(DiffLUMT_MMex,kpivot_MMex,eh_Phi)

     if(mod(it,ndisp) .eq. 0)then                    ! ����
        call output_gtool4
     endif
  enddo

  call output_gtool4_close                           ! �ҥ��ȥ꡼�����
  stop

  contains

    function InvXtanX(val,n)
      !
      ! x*tan(x)=val(>0) �β�����
      !
      real(8), intent(IN) :: val                ! x*tan(x)=val > 0
      integer, intent(IN) :: n                  ! �������ֹ�
      real(8)             :: InvXtanX
      real(8), parameter  :: eps = 1.0D-14     ! �������

      real(8) :: PI
      real(8) :: xs, xl, xm
      real(8) :: ValS, ValL, ValM

      PI = atan(1.0D0)*4.0D0

      xs=n*PI
      xl=PI/2.0D0  + n*PI - eps

      ValS = xs*tan(xs)-val ; ValL = xl*tan(xl)-val
      if ( ValS * ValL .GT. 0.0D0 ) &
           call MessageNotify('E','InvXtanX',&
           'Initial values of ValS and ValL are the same sign.')
!!$          write(6,*) 'vals, vall',vals, vall
1000  xm = (xs + xl)/2.0
      ValM = xm*tan(xm) - val
      
      if ( ValS * ValM .GT. 0.0D0 ) then
         xs = xm ; ValS=xs*tan(xs)-val
      else
         xl = xm ; ValL=xl*tan(xl)-val
      endif

      if ( abs(xl-xs) .lt. eps ) then
         InvXtanX = xm
         goto 99
      endif

      goto 1000

99    continue
    end function InvXtanX

    function InvXcotX(val,n)
      !
      ! x*cot(x)=val(<0) �β�����
      !
      real(8), intent(IN) :: val                ! x*cot(x)=val < 0
      integer, intent(IN) :: n                  ! �������ֹ�
      real(8)             :: InvXcotX
      real(8), parameter  :: eps = 1.0D-14     ! �������

      real(8) :: PI
      real(8) :: xs, xl, xm
      real(8) :: ValS, ValL, ValM

      PI = atan(1.0D0)*4.0D0

      xs=PI/2.0D0  + n*PI + eps
      xl=(n+1)*PI  - eps

      ValS = xs/tan(xs)-val ; ValL = xl/tan(xl)-val
      if ( ValS * ValL .GT. 0.0D0 ) &
           call MessageNotify('E','InvXcotX',&
           'Initial values of ValS and ValL are the same sign.')
!!$          write(6,*) 'vals, vall',vals, vall
1000  xm = (xs + xl)/2.0
      ValM = xm/tan(xm) - val
      
      if ( ValS * ValM .GT. 0.0D0 ) then
         xs = xm ; ValS=xs/tan(xs)-val
      else
         xl = xm ; ValL=xl/tan(xl)-val
      endif

      if ( abs(xl-xs) .lt. eps ) then
         InvXcotX = xm
         goto 99
      endif

      goto 1000

99    continue
    end function InvXcotX

   !------------------- �Ȼ��� ----------------------
    subroutine CNDiffusionMatrixMMex( Diffc, dt, DiffLUMatrix, kpivot )
      ! 
      ! Crank Nicholson �Ȼ���Ū�׻��ѹ��������
      ! (1-D dt/2��^2, LU ʬ��)
      ! 
      real(8), intent(IN)   :: Diffc    ! �Ȼ�����
      real(8), intent(IN)   :: dt       ! ���ֹ�

      ! Crank Nicholson �Ȼ���Ū�׻��ѹ���(1-D dt/2��^2, LU ʬ��)
      real(8), intent(OUT)  :: DiffLUMatrix(-km:km,2:lm,2:lm)
      integer, intent(OUT)  :: kpivot(-km:km,2:lm)            ! �ԥܥåȾ���

      real(8)               :: eh_I(-km:km,2:lm)              ! ������ѿ�

      integer :: l

      DiffLUMatrix = 0.0
      do l=2,lm
         eh_I = 0.0 ; eh_I(:,l) = 1.0             ! ���ȿ���ʬ��Ω
         DiffLUMatrix(:,:,l) &
              = eh_I - Diffc * dt/2.0 * eh_et(et_Lapla_et(et_eh(eh_I)))
      enddo

      call LuDeComp(DiffLUMatrix,kpivot)

    end subroutine CNDiffusionMatrixMMex

   !------------------- ���� ----------------------
    subroutine output_gtool4_init
      !
      ! �ҥ��ȥ꡼���Ͻ�����롼����
      !
      !   �ե��������
      !   �ѿ����
      !
      call HistoryCreate( &                               ! �ҥ��ȥ꡼����
           file='et_galerkin_module_fftj_MMex_cn_diff.nc', &
           title='Diffusion equation (MMex)', &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club davis/spmodel project',&
           dims=(/'x','y','t'/), dimsizes=(/im,jm+1,0/),    &
           longnames=(/'X-coordinate','Y-coordinate','time        '/),&
           units=(/'1','1','1'/),                           &
           origin=0.0, interval=real(ndisp*dt) )

      call HistoryPut('x',x_X)                            ! �ѿ�����
      call HistoryAddattr('x','topology','circular')      ! ����°��
      call HistoryAddattr('x','modulo',xmax-xmin)         ! ����°��
      call HistoryPut('y',y_Y)                            ! �ѿ�����

      call HistoryAddVariable( &                          ! �ѿ����
           varname='phi', dims=(/'x','y','t'/), & 
           longname='diffusive element', units='1', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
           varname='phisol', dims=(/'x','y','t'/), & 
           longname='diffusive element(analytic)', units='1', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
           varname='phierror', dims=(/'x','y','t'/), & 
           longname='diffusive element(Error)', units='1', xtype='double')

     !---- �¸��ѥ�᥿����°���Ȥ������, ����(Global °��) ----
      call HistoryAddAttr('x','+kappa', kappa )

    end subroutine output_gtool4_init

    subroutine output_gtool4
      !
      ! �ҥ��ȥ꡼����
      !
      !   ��ʪ���̤� gtool4 �ե�����ؤν��Ϥ�Ԥ�
      !
      yx_Phi = yx_et(et_eh(eh_Phi))

      yx_PhiSol = yx_PhiInit &
              * exp( - kappa * ( rk**2 + rl**2 )*time )

      yx_PhiError = yx_Phi-yx_PhiSol

      write(6,*) 'it = ',it
      call HistoryPut('t',real(time))
      call HistoryPut('phi',transpose(yx_Phi))
      call HistoryPut('phisol',transpose(yx_PhiSol))
      call HistoryPut('phierror',transpose(yx_PhiError))
    end subroutine output_gtool4

    subroutine output_gtool4_close
      !
      ! �ҥ��ȥ꡼���Ͻ�λ����
      !
      call HistoryClose
    end subroutine output_gtool4_close

end program et_galerkin_module_fftj_MMex_cn_diff
