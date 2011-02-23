!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  SPMODEL 3 ����ʿ���ΰ�֥��ͥ�����ή��ǥ�
!      �����ӥ�����ˡ�ˤ��׻�(���ٸ�����)
!
!      ������ʬ : Crank Nicolson ˡ
!
!      �׻������ӤΤ���β��ϲ�ˤĤ��Ƥϲ���ʸ��
!
!         ��2 ������ϩ�ΰ�ǤγȻ����������β��ϲ��
!
!      (et_diffusion.pdf)�򻲾ȤΤ���. 
!
! Solving 3-D Diffusioon equation (DD problem)
!     d��/dt = \nabla ��
!     �� = 0 at y=-d/2,d/2
!
!����  2009/12/25  �ݹ�����: et_DD_cn_diff.f90 ����¤
!
program tee_module_DD_cn_diff
  !
  ! SPMODEL 3 ����ʿ���ΰ�֥��ͥ�����ή��ǥ�
  !      �����ӥ����ե��顼����ˡ�ˤ��׻�(���ٸ�����)
  !
  !      ������ʬ : Crank Nicolson ˡ
  !
  ! Solving 3-D Diffusioon equation (DD problem)
  !     d��/dt = \nabla ��
  !     �� = 0 at y=-d/2,d/2
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
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter  :: im=32, jm=32, km=16      ! �ʻ���������(X,Y,Z)
  integer, parameter  :: lm=10, mm=10, nm=10      ! �����ȿ�������(X,Y,Z)

 !---- �ѿ� ----
  real(8)             :: zyx_Phi(0:km,0:jm-1,0:im-1)   ! �ʻҥǡ���
  real(8)             :: tee_Phi(0:nm,-mm:mm,-lm:lm)   ! ���ڥ��ȥ�ǡ���(����)
  
  real(8)             :: zyx_PhiInit(0:km,0:jm-1,0:im-1)  ! �ʻҥǡ���(�����)
  real(8)             :: zyx_PhiSol(0:km,0:jm-1,0:im-1)   ! �ʻҥǡ���(���ϲ�)
  real(8)             :: zyx_PhiError(0:km,0:jm-1,0:im-1) ! �ʻҥǡ���(��)

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=1e-3                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=100, ndisp=10        ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  real(8), parameter          :: XD=2.0         ! x �����ΰ���礭��
  real(8), parameter          :: YD=2.0         ! y �����ΰ���礭��
  real(8), parameter          :: ZD=1.0         ! y �����ΰ���礭��
  real(8), parameter          :: Kappa=1.0      ! �Ȼ�����
  character(len=2)            :: BC='DD'        ! �������(DD/DN/ND/NN)

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter  :: xmin=0, xmax=XD        ! X ��ɸ�ϰ�
  real(8), parameter  :: ymin=0, ymax=YD        ! Y ��ɸ�ϰ�
  real(8), parameter  :: zmin=-ZD/2, zmax=ZD/2  ! Z ��ɸ�ϰ�


 !--- Crank Nicholson ��Ū�׻��Ѻ���ѿ� ---
  real(8) :: DiffLUMT((2*lm+1)*(2*mm+1),0:nm,0:nm)     
                                    ! Crank Nicholson ��Ū�׻��ѹ���
  integer :: kpivot((2*lm+1)*(2*mm+1),0:nm)

 !---- ����¾ ----
  real(8), parameter  :: pi=3.1415926535897932385D0
  integer :: it=0
  real(8) :: time=0.0D0
  integer :: l,m,n

 !---------------- ��ɸ�ͤ����� ---------------------
  call tee_Initial(im,jm,km,lm,mm,nm,xmin,xmax,ymin,ymax,zmin,zmax)

  call CNDiffusionMatrix( kappa, dt, DiffLUMT, kpivot )

 !------------------- ��������� ----------------------
  write(6,*) 'Positive l:cos(kx), negative l: sin(kx)'
  write(6,*) 'Positvie m:cos(my), negative m: sin(my)'
  write(6,*) 'Positvie n:cos(nz), negative n: sin(nz)'
  write(6,*) 'l,m,n?'
  read(5,*)  l,m,n

  if ( l .ge. 0 .AND. m .ge. 0 .AND. n .ge. 0) then
     write(6,*) 'Type of the initial value : cos(lx)*cos(my)cos(nz)'
     zyx_Phi=cos(2*l*Pi*zyx_X/XD)*cos(2*m*Pi*zyx_Y/YD)*cos((2*n+1)*Pi*zyx_Z/ZD)
  else   if ( l .lt. 0 .AND. m .ge. 0 .AND. n .ge. 0) then
     write(6,*) 'Type of the initial value : sin(lx)*cos(my)cos(nz)'
     zyx_Phi=sin(2*l*Pi*zyx_X/XD)*cos(2*m*Pi*zyx_Y/YD)*cos((2*n+1)*Pi*zyx_Z/ZD)
  else   if ( l .ge. 0 .AND. m .lt. 0 .AND. n .ge. 0) then
     write(6,*) 'Type of the initial value : cos(lx)*sin(my)cos(nz)'
     zyx_Phi=cos(2*l*Pi*zyx_X/XD)*sin(2*m*Pi*zyx_Y/YD)*cos((2*n+1)*Pi*zyx_Z/ZD)
  else   if ( l .lt. 0 .AND. m .lt. 0 .AND. n .ge. 0) then
     write(6,*) 'Type of the initial value : sin(lx)*sin(my)cos(nz)'
     zyx_Phi=sin(2*l*Pi*zyx_X/XD)*sin(2*m*Pi*zyx_Y/YD)*cos((2*n+1)*Pi*zyx_Z/ZD)
  else if ( l .ge. 0 .AND. m .ge. 0 .AND. n .lt. 0) then
     write(6,*) 'Type of the initial value : cos(lx)*cos(my)cos(nz)'
     zyx_Phi=cos(2*l*Pi*zyx_X/XD)*cos(2*m*Pi*zyx_Y/YD)*sin(2*n*Pi*zyx_Z/ZD)
  else   if ( l .lt. 0 .AND. m .ge. 0 .AND. n .lt. 0) then
     write(6,*) 'Type of the initial value : sin(lx)*cos(my)cos(nz)'
     zyx_Phi=sin(2*l*Pi*zyx_X/XD)*cos(2*m*Pi*zyx_Y/YD)*sin(2*Pi*zyx_Z/ZD)
  else   if ( l .ge. 0 .AND. m .lt. 0 .AND. n .lt. 0) then
     write(6,*) 'Type of the initial value : cos(lx)*sin(my)cos(nz)'
     zyx_Phi=cos(2*l*Pi*zyx_X/XD)*sin(2*m*Pi*zyx_Y/YD)*sin(2*n*Pi*zyx_Z/ZD)
  else   if ( l .lt. 0 .AND. m .lt. 0 .AND. n .lt. 0) then
     write(6,*) 'Type of the initial value : sin(lx)*sin(my)cos(nz)'
     zyx_Phi=sin(2*l*Pi*zyx_X/XD)*sin(2*m*Pi*zyx_Y/YD)*sin(2*n*Pi*zyx_Z/ZD)
  end if

  zyx_PhiInit = zyx_Phi
  
  tee_Phi = tee_zyx(zyx_Phi)

  call output_gtool4_init                            ! �ҥ��ȥ꡼�����
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt               ! Adams-Bashfort & Crank-Nicolson ˡ�ˤ�������ʬ
     time =it*dt

     tee_Phi = tee_Phi + dt/2 * kappa * tee_Lapla_tee(tee_Phi)
     tee_Phi = aee_e2a(LuSolve(DiffLUMT,kpivot,e2a_aee(tee_Phi)))

     if(mod(it,ndisp) .eq. 0)then                    ! ����
        call output_gtool4
     endif
  enddo

  call output_gtool4_close                           ! �ҥ��ȥ꡼�����
  stop

  contains

   !------------------- �Ȼ��� ----------------------
    subroutine CNDiffusionMatrix( Diffc, dt, DiffLUMatrix, kpivot )
      ! 
      ! Crank Nicholson �Ȼ���Ū�׻��ѹ��������
      ! (1-D dt/2��^2, LU ʬ��)
      ! 
      real(8), intent(IN)   :: Diffc    ! �Ȼ�����
      real(8), intent(IN)   :: dt       ! ���ֹ�

      ! Crank Nicholson �Ȼ���Ū�׻��ѹ���(1-D dt/2��^2, LU ʬ��)
      real(8), intent(OUT) :: DiffLUMatrix((2*lm+1)*(2*mm+1),0:nm,0:nm)     
      integer, intent(OUt) :: kpivot((2*lm+1)*(2*mm+1),0:nm)

      real(8)               :: e2t_I((2*lm+1)*(2*mm+1),0:nm)   ! ������ѿ�
      real(8)               :: tee_DI(0:nm,-mm:mm,-lm:lm)      ! ������ѿ�

      integer :: n

      DiffLUMatrix = 0.0
      do n=0,nm
         e2t_I = 0.0 ; e2t_I(:,n) = 1.0d0             ! ���ȿ���ʬ��Ω
         tee_DI =  - Diffc * dt/2.0 * tee_Lapla_tee(aee_e2a(e2t_I))
         call tee_Boundaries(tee_DI,cond=BC)
         DiffLUMatrix(:,:,n) = e2t_I + e2a_aee(tee_DI)
      enddo

      call LuDeComp(DiffLUMatrix,kpivot)

    end subroutine CNDiffusionMatrix

   !------------------- ���� ----------------------
    subroutine output_gtool4_init
      !
      ! �ҥ��ȥ꡼���Ͻ�����롼����
      !
      !   �ե��������
      !   �ѿ����
      !
      call HistoryCreate( &                               ! �ҥ��ȥ꡼����
           file='tee_module_DD_cn_diff.nc', &
           title='Diffusion equation', &
           source='Sample program of gtool_history/gtool4', &
           institution='GFD_Dennou Club davis/spmodel project',&
           dims=(/'x','y','z','t'/), dimsizes=(/im,jm,km+1,0/),    &
           longnames=(/'X-coordinate','Y-coordinate',&
                       'Z-coordinate','time        '/),&
           units=(/'1','1','1','1'/),                           &
           origin=0.0, interval=real(ndisp*dt) )

      call HistoryPut('x',x_X)                            ! �ѿ�����
      call HistoryAddattr('x','topology','circular')      ! ����°��
      call HistoryAddattr('x','modulo',xmax-xmin)         ! ����°��
      call HistoryPut('y',y_Y)                            ! �ѿ�����
      call HistoryAddattr('y','topology','circular')      ! ����°��
      call HistoryAddattr('y','modulo',ymax-ymin)         ! ����°��
      call HistoryPut('z',z_Z)                            ! �ѿ�����

      call HistoryAddVariable( &                          ! �ѿ����
           varname='phi', dims=(/'z','y','x','t'/), & 
           longname='diffusive element', units='1', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
           varname='phisol', dims=(/'z','y','x','t'/), & 
           longname='diffusive element(analytic)', units='1', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
           varname='phierror', dims=(/'z','y','x','t'/), & 
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
      zyx_Phi = zyx_tee(tee_Phi)

      if ( n .ge. 0 ) then
         zyx_PhiSol = zyx_PhiInit &
              * exp( - kappa * (   (2*PI*l/XD)**2 + (2*PI*m/YD)**2 &
                                 +((2*n+1)*PI/ZD)**2   )*time )
      else
         zyx_PhiSol = zyx_PhiInit &
              * exp( - kappa * (  (2*PI*l/XD)**2 + (2*PI*m/YD)**2 &
                                 +(2*PI*n/ZD)**2     )*time )
      endif

      zyx_PhiError = zyx_Phi-zyx_PhiSol

      write(6,*) 'it = ',it
      call HistoryPut('t',real(time))
      call HistoryPut('phi',zyx_Phi)
      call HistoryPut('phisol',zyx_PhiSol)
      call HistoryPut('phierror',zyx_PhiError)
    end subroutine output_gtool4

    subroutine output_gtool4_close
      !
      ! �ҥ��ȥ꡼���Ͻ�λ����
      !
      call HistoryClose
    end subroutine output_gtool4_close

end program tee_module_DD_cn_diff
