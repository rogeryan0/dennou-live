!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  SPMODEL 3 ����ʿ���ΰ��󰵽�ή�Υ�ǥ�
!      �����ӥ���������ˡ�ˤ��׻�
!
!      ������ʬ : Crank-Nicolson ˡ
!      ��¦Ǵ����, ��¦��ͳ���٤���
!
! Solving 3-D impompressible fluid system
!     d\nabla^2\phi/dt =  Pr\nabla^2\nabla^2\phi
!     phi = phi' = 0 at z=d/2
!     phi = phi''= 0 at z=-d/2
!
!      �׻������ӤΤ���β��ϲ�ˤĤ��Ƥϲ���ʸ��
!
!         ��2 ������ϩ�ΰ�ǤγȻ����������β��ϲ��
!
!      (et_diffusion.pdf)�򻲾ȤΤ���. 
!
!����  2009/12/29  �ݹ�����: et_RF_cn_diff.f90 ����¤
!      
!
program tee_module_polvel_RF_cn_diff
  !
  ! SPMODEL 3 ����ʿ���ΰ��󰵽�ή�Υ�ǥ�
  !      �����ӥ���������ˡ�ˤ��׻�
  !
  !      ������ʬ : Crank-Nicolson ˡ
  !      ��¦Ǵ����, ��¦��ͳ���٤���
  !
  ! Solving 3-D impompressible fluid system
  !     d\nabla^2\phi/dt =  Pr\nabla^2\nabla^2\phi
  !     phi = phi' = 0 at z=d/2
  !     phi = phi''= 0 at z=-d/2
  !
  !      �׻������ӤΤ���β��ϲ�ˤĤ��Ƥϲ���ʸ��
  !
  !         ��2 ������ϩ�ΰ�ǤγȻ����������β��ϲ��
  !
  !      (et_diffusion.pdf)�򻲾ȤΤ���. 
  !
  use lumatrix
  use tee_module
  use gt4_history
  use dc_message
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter  :: im=32, jm=32, km=16      ! �ʻ���������(X,Y,Z)
  integer, parameter  :: lm=10, mm=10, nm=16      ! �����ȿ�������(X,Y,Z)

 !---- ʪ���ѥ�᥿�� ----
  real(8), parameter          :: XD=2.0         ! x �����ΰ���礭��
  real(8), parameter          :: YD=2.0         ! y �����ΰ���礭��
  real(8), parameter          :: ZD=1.0         ! y �����ΰ���礭��

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter  :: xmin=0, xmax=XD        ! X ��ɸ�ϰ�
  real(8), parameter  :: ymin=0, ymax=YD        ! Y ��ɸ�ϰ�
  real(8), parameter  :: zmin=-ZD/2, zmax=ZD/2  ! Z ��ɸ�ϰ�

 !---- �ѿ� ----
  real(8)             :: zyx_Phi(0:km,0:jm-1,0:im-1)  ! �ݥ�����ݥƥ󥷥��
  real(8)             :: zyx_LaplaPhi(0:km,0:jm-1,0:im-1) ! 

  real(8)             :: tee_Phi(0:nm,-mm:mm,-lm:lm)      ! ���ڥ��ȥ�ǡ���
  real(8)             :: tee_LaplaPhi(0:nm,-mm:mm,-lm:lm) ! ���ڥ��ȥ�ǡ���

  real(8)             :: zyx_PhiInit(0:km,0:jm-1,0:im-1)  ! �ʻҥǡ���(�����)
  real(8)             :: zyx_PhiSol(0:km,0:jm-1,0:im-1)   ! �ʻҥǡ���(���ϲ�)
  real(8)             :: zyx_PhiError(0:km,0:jm-1,0:im-1) ! �ʻҥǡ���(��)

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=1e-3                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=100, ndisp=10   ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  real(8), parameter          :: Pr=1.0         ! �ץ��ɥ��
  character(len=2), parameter :: VelBC='RF'     ! ®�پ춭�����(RR/FF/RF/FR)

 !--- Crank Nicholson ��Ū�׻��Ѻ���ѿ� ---
  real(8) :: DifLUMT_Phi((2*lm+1)*(2*mm+1),0:nm,0:nm)
                                     ! Crank Nicholson ��Ū�׻��ѹ���
  integer :: kpivot_Phi((2*lm+1)*(2*mm+1),0:nm) 
                                     ! Crank Nicholson �ѹ���ԥܥåȾ���

 !---- ����¾ ----
  real(8), parameter  :: pi=3.1415926535897932385D0
  integer :: it=0
  real(8) :: time=0.0D0
  real(8) :: rk, rn
  integer :: l,m,n

 !---------------- ��ɸ�ͤ����� ---------------------
  call tee_Initial(im,jm,km,lm,mm,nm,xmin,xmax,ymin,ymax,zmin,zmax)

  call CNDiffusionMatrixPol( Pr, dt, DifLUMT_Phi, kpivot_Phi )

 !------------------- ��������� ----------------------
  write(6,*) 'Positive l:cos(lx), negative l: sin(lx)'
  write(6,*) 'Positive m:cos(my), negative m: sin(my)'
  write(6,*) 'n: sinh(nz)+sin(nz)'
  write(6,*) 'l,m,n?'
  read(5,*)  l,m,n

  if ( l .ge. 0 .AND. m .ge. 0 ) then
     write(6,*) 'Type of the initial value : cos(lx)*cos(my)*[sinh(nz)+sin(nz)]'
     rk = sqrt((2*l*Pi/XD)**2+(2*m*Pi/YD)**2)
     rn = InvXcotX(rk*ZD/tanh(rk*ZD),abs(n))/ZD
     zyx_Phi = cos(2*l*PI/XD*zyx_X) * cos(2*m*PI/YD*zyx_Y) &
          * (   sin(rn*ZD)  * sinh(rk*(zyx_Z+ZD/2)) &
              - sinh(rk*ZD) * sin(rn*(zyx_Z+ZD/2))  )
  else if ( l .lt. 0 .AND. m .ge. 0 ) then
     write(6,*) 'Type of the initial value : sin(lx)*cos(my)*[sinh(nz)+sin(nz)]'
     rk = sqrt((2*l*Pi/XD)**2+(2*m*Pi/YD)**2)
     rn = InvXcotX(rk*ZD/tanh(rk*ZD),abs(n))/ZD
     zyx_Phi = sin(2*l*PI/XD*zyx_X) * cos(2*m*PI/YD*zyx_Y) &
          * (   sin(rn*ZD)  * sinh(rk*(zyx_Z+ZD/2)) &
              - sinh(rk*ZD) * sin(rn*(zyx_Z+ZD/2))  )
  else if ( l .ge. 0 .AND. m .lt. 0 ) then
     write(6,*) 'Type of the initial value : cos(lx)*sin(my)*[sinh(nz)+sin(nz)]'
     rk = sqrt((2*l*Pi/XD)**2+(2*m*Pi/YD)**2)
     rn = InvXcotX(rk*ZD/tanh(rk*ZD),abs(n))/ZD
     zyx_Phi = cos(2*l*PI/XD*zyx_X) * sin(2*m*PI/YD*zyx_Y) &
          * (   sin(rn*ZD)  * sinh(rk*(zyx_Z+ZD/2)) &
              - sinh(rk*ZD) * sin(rn*(zyx_Z+ZD/2))  )
  else if ( l .lt. 0 .AND. m .lt. 0 ) then
     write(6,*) 'Type of the initial value : sin(lx)*sin(my)*[sinh(nz)+sin(nz)]'
     rk = sqrt((2*l*Pi/XD)**2+(2*m*Pi/YD)**2)
     rn = InvXcotX(rk*ZD/tanh(rk*ZD),abs(n))/ZD
     zyx_Phi = sin(2*l*PI/XD*zyx_X) * sin(2*m*PI/YD*zyx_Y) &
          * (   sin(rn*ZD)  * sinh(rk*(zyx_Z+ZD/2)) &
              - sinh(rk*ZD) * sin(rn*(zyx_Z+ZD/2))  )
  endif

  zyx_PhiInit = zyx_Phi
  
  tee_Phi  = tee_zyx(zyx_Phi)
  tee_LaplaPhi = tee_Lapla_tee(tee_Phi)

  call output_gtool4_init                            ! �ҥ��ȥ꡼�����
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt                                         ! Cn ˡ�ˤ�������ʬ
     time =it*dt

     tee_LaplaPhi = tee_LaplaPhi + dt/2 *( Pr*tee_Lapla_tee(tee_LaplaPhi)  )
     tee_Phi = tee_LaplaPol2PolGrid_tee(tee_LaplaPhi,cond=VelBC)
     tee_Phi = aee_e2a(LUSolve(DifLUMT_Phi,kpivot_Phi,e2a_aee(tee_Phi)))
     tee_LaplaPhi = tee_Lapla_tee(tee_Phi)

     if(mod(it,ndisp) .eq. 0)then                    ! ����
        call output_gtool4
     endif
  enddo

  call output_gtool4_close                           ! �ҥ��ȥ꡼�����
  stop

  contains

    function InvXcotX(val,n)
      !
      ! x*cot(x)=val(>0) �β�����
      !
      real(8), intent(IN) :: val                ! x*cot(x)=val > 0
      integer, intent(IN) :: n                  ! �������ֹ�
      real(8)             :: InvXcotX
      real(8), parameter  :: eps = 1.0D-14     ! �������

      real(8) :: PI
      real(8) :: xs, xl, xm
      real(8) :: ValS, ValL, ValM

      PI = atan(1.0D0)*4.0D0

      xs=(n+1)*PI  + eps
      xl=PI/2.0D0  + (n+1)*PI

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

   !------------------- �Ȼ���(ή���ؿ���) ----------------------
    subroutine CNDiffusionMatrixPol( Diffc, dt, DiffLUMatrix, kpivot )
      ! 
      ! Crank Nicholson �Ȼ���Ū�׻��ѹ��������
      ! (ή���ؿ���, ��^{-2}[��^2-D dt/2��^2��^2], LU ʬ��)
      ! 
      real(8), intent(IN)   :: Diffc    ! �Ȼ�����
      real(8), intent(IN)   :: dt       ! ���ֹ�

      ! Crank Nicholson �Ȼ���Ū�׻��ѹ���(��^{-2}(��^2-D (dt/2)��^4, LU ʬ��)
      real(8), intent(OUT) :: DiffLUMatrix((2*lm+1)*(2*mm+1),0:nm,0:nm)     
      integer, intent(OUt) :: kpivot((2*lm+1)*(2*mm+1),0:nm)

      real(8)               :: e2t_I((2*lm+1)*(2*mm+1),0:nm)   ! ������ѿ�
      real(8)               :: tee_DI(0:nm,-mm:mm,-lm:lm)      ! ������ѿ�

      integer :: n

      DiffLUMatrix = 0.0
      do n=0,nm
         e2t_I = 0.0 ; e2t_I(:,n) = 1.0             ! ���ȿ���ʬ��Ω
         tee_DI = - Diffc * dt/2.0 * tee_Lapla_tee(tee_Lapla_tee(aee_e2a(e2t_I)))
         tee_DI = tee_LaplaPol2PolGrid_tee(tee_DI,cond=VelBC)
         DiffLUMatrix(:,:,n) = e2t_I + e2a_aee(tee_DI)
      enddo

      call LuDeComp(DiffLUMatrix,kpivot)

    end subroutine CNDiffusionMatrixPol

   !------------------- ���� ----------------------
    subroutine output_gtool4_init
      !
      ! �ҥ��ȥ꡼���Ͻ�����롼����
      !
      !   �ե��������
      !   �ѿ����
      !
      call HistoryCreate( &                               ! �ҥ��ȥ꡼����
           file='tee_module_polvel_RF_cn_diff.nc', &
           title='3dim. imcompressible fluid', &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club davis/spmodel project',&
           dims=(/'x','y','z','t'/), dimsizes=(/im,jm,km+1,0/),    &
           longnames=(/'X-coordinate','Y-coordinate',  &
                       'Z-coordinate','time        '/),&
           units=(/'1','1','1','1'/),                  &
           origin=0.0, interval=real(ndisp*dt) )

      call HistoryPut('x',x_X)                            ! �ѿ�����
      call HistoryAddattr('x','topology','circular')      ! ����°��
      call HistoryAddattr('x','modulo',xmax-xmin)         ! ����°��
      call HistoryPut('y',y_Y)                            ! �ѿ�����
      call HistoryAddattr('y','topology','circular')      ! ����°��
      call HistoryAddattr('y','modulo',xmax-xmin)         ! ����°��
      call HistoryPut('z',z_Z)                            ! �ѿ�����

      call HistoryAddVariable( &                          ! �ѿ����
           varname='phi', dims=(/'z','y','x','t'/), & 
           longname='Poloidal potential', units='1', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
           varname='laplaphi', dims=(/'z','y','x','t'/), & 
           longname='Laplacian of Poloidal potential', &
           units='1', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
           varname='phisol', dims=(/'z','y','x','t'/), & 
           longname='Poloidal potential(analytic)', units='1', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
           varname='phierror', dims=(/'z','y','x','t'/), & 
           longname='Poloidal potential(Error)', units='1', xtype='double')

     !---- �¸��ѥ�᥿����°���Ȥ������, ����(Global °��) ----
      call HistoryAddAttr('x','+VelBC', VelBC )

    end subroutine output_gtool4_init

    subroutine output_gtool4
      !
      ! �ҥ��ȥ꡼����
      !
      !   ��ʪ���̤� gtool4 �ե�����ؤν��Ϥ�Ԥ�
      !
      zyx_Phi  = zyx_tee(tee_Phi)
      zyx_LaplaPhi = zyx_tee(tee_LaplaPhi)

      zyx_PhiSol = zyx_PhiInit &
           * exp( - Pr * ( rk**2 + rn**2 )*time )

      zyx_PhiError = zyx_Phi-zyx_PhiSol

      write(6,*) 'it = ',it
      call HistoryPut('t',real(it*dt))
      call HistoryPut('phi',zyx_Phi)
      call HistoryPut('laplaphi',zyx_LaplaPhi)
      call HistoryPut('phisol',zyx_PhiSol)
      call HistoryPut('phierror',zyx_PhiError)
    end subroutine output_gtool4

    subroutine output_gtool4_close
      !
      ! �ҥ��ȥ꡼���Ͻ�λ����
      !
      call HistoryClose
    end subroutine output_gtool4_close
    
end program tee_module_polvel_RF_cn_diff
