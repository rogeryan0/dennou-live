!----------------------------------------------------------------------
!     Copyright (c) 2006 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  SPMODEL 2 ������ϩ�ΰ�֥��ͥ�����ή��ǥ�
!      �����ӥ����ե��顼����ˡ�ˤ��׻�(���ٸ�����)
!
!      ������ʬ : Crank Nicolson ˡ
!
!      �׻������ӤΤ���β��ϲ�ˤĤ��Ƥϲ���ʸ��
!
!         ��2 ������ϩ�ΰ�ǤγȻ����������β��ϲ��
!
!      (et_diffusion.pdf)�򻲾ȤΤ���. 
!
! Solving 2-D Diffusioon equation (DD problem)
!     d��/dt = \nabla ��
!     �� = 0 at y=-d/2,d/2
!
!����  2002/01/31  �ݹ�����
!      2002/02/12  �ݹ�����  y ������ؿ�, ���ؿ��ɲ�
!      2006/03/03  �ݹ����� RDoc �Ѥ˥����Ƚ���
!
program et_galerkin_DD_cn_diff
  !
  ! SPMODEL 2 ������ϩ�ΰ�֥��ͥ�����ή��ǥ�
  !      �����ӥ����ե��顼����ˡ�ˤ��׻�(���ٸ�����)
  !
  !      ������ʬ : Crank Nicolson ˡ
  !
  ! Solving 2-D Diffusioon equation (DD problem)
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
  use et_module
  use et_galerkin_module
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter  :: im=64, jm=16      ! �ʻ���������(X,Y)
  integer, parameter  :: km=21, lm=10      ! �����ȿ�������(X,Y)

 !---- �ѿ� ----
  real(8)             :: yx_Phi(0:jm,0:im-1)     ! �ʻҥǡ���(����)
  real(8)             :: ed_Phi(-km:km,2:lm)     ! ���顼����ǡ���(����)

  real(8)             :: yx_PhiInit(0:jm,0:im-1) ! �ʻҥǡ���(����, �����)
  real(8)             :: yx_PhiSol(0:jm,0:im-1)  ! �ʻҥǡ���(����, ���ϲ�)
  real(8)             :: yx_PhiError(0:jm,0:im-1)! �ʻҥǡ���(����, ��)

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=1e-3                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=500, ndisp=50        ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  real(8), parameter          :: XD=2.0         ! x �����ΰ���礭��
  real(8), parameter          :: YD=1.0         ! y �����ΰ���礭��
  real(8), parameter          :: Kappa=2.0      ! �Ȼ�����

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter  :: xmin=0, xmax=XD        ! X ��ɸ�ϰ�
  real(8), parameter  :: ymin=-YD/2, ymax=YD/2  ! Y ��ɸ�ϰ�


 !--- Crank Nicholson ��Ū�׻��Ѻ���ѿ� ---
  real(8) :: DiffLUMT_DD(-km:km,2:lm,2:lm)     
                                    ! Crank Nicholson ��Ū�׻��ѹ���
  integer :: kpivot_DD(-km:km,2:lm) ! Crank Nicholson �ѹ���ԥܥåȾ���

 !---- ����¾ ----
  real(8), parameter  :: pi=3.1415926535897932385D0
  integer :: it=0
  real(8) :: time=0.0D0
  integer :: k, l

 !---------------- ��ɸ�ͤ����� ---------------------
  call et_galerkin_Initial(im,jm,km,lm,xmin,xmax,ymin,ymax,DD=.true.)

  call CNDiffusionMatrixDD( kappa, dt, DiffLUMT_DD, kpivot_DD )

 !------------------- ��������� ----------------------
  write(6,*) 'Positive k:cos(kx), negative k: sin(kx)'
  write(6,*) 'Positvie l:cos(lx), negative l: sin(lx)'
  write(6,*) 'k,l?'
  read(5,*)  k,l

  if ( k .ge. 0 .AND. l .ge. 0) then
     write(6,*) 'Type of the initial value : cos(kx)*cos(ly)'
     yx_Phi = cos(2*k*Pi*yx_X/XD) * cos((2*l+1)*Pi*yx_Y/YD)
  else if ( k .lt. 0 .AND. l .ge. 0) then
     write(6,*) 'Type of the initial value : sin(kx)*cos(ly)'
     yx_Phi = sin(2*k*Pi*yx_X/XD) * cos((2*l+1)*Pi*yx_Y/YD)
  else if ( k .ge. 0 .AND. l .lt. 0) then
     write(6,*) 'Type of the initial value : cos(kx)*cos(ly)'
     yx_Phi = cos(2*k*Pi*yx_X/XD) * sin(2*l*Pi*yx_Y/YD)
  else 
     write(6,*) 'Type of the initial value : sin(kx)*sin(ly)'
     yx_Phi = sin(2*k*Pi*yx_X/XD) * sin(2*l*Pi*yx_Y/YD)
  endif
  yx_PhiInit = yx_Phi
  
  ed_Phi = ed_et(et_yx(yx_Phi))

  call output_gtool4_init                            ! �ҥ��ȥ꡼�����
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt               ! Adams-Bashfort & Crank-Nicolson ˡ�ˤ�������ʬ
     time =it*dt

     ed_Phi = ed_Phi + dt/2 * ed_et(kappa * et_Lapla_et(et_ed(ed_Phi)))
     ed_Phi = LuSolve(DiffLUMT_DD,kpivot_DD,ed_Phi)

     if(mod(it,ndisp) .eq. 0)then                    ! ����
        call output_gtool4
     endif
  enddo

  call output_gtool4_close                           ! �ҥ��ȥ꡼�����
  stop

  contains

   !------------------- �Ȼ��� ----------------------
    subroutine CNDiffusionMatrixDD( Diffc, dt, DiffLUMatrix, kpivot )
      ! 
      ! Crank Nicholson �Ȼ���Ū�׻��ѹ��������
      ! (1-D dt/2��^2, LU ʬ��)
      ! 
      real(8), intent(IN)   :: Diffc    ! �Ȼ�����
      real(8), intent(IN)   :: dt       ! ���ֹ�

      ! Crank Nicholson �Ȼ���Ū�׻��ѹ���(1-D dt/2��^2, LU ʬ��)
      real(8), intent(OUT)  :: DiffLUMatrix(-km:km,2:lm,2:lm)
      integer, intent(OUT)  :: kpivot(-km:km,2:lm)            ! �ԥܥåȾ���

      real(8)               :: ed_I(-km:km,2:lm)              ! ������ѿ�

      integer :: l

      DiffLUMatrix = 0.0
      do l=2,lm
         ed_I = 0.0 ; ed_I(:,l) = 1.0             ! ���ȿ���ʬ��Ω
         DiffLUMatrix(:,:,l) &
              = ed_I - Diffc * dt/2.0 * ed_et(et_Lapla_et(et_ed(ed_I)))
      enddo

      call LuDeComp(DiffLUMatrix,kpivot)

    end subroutine CNDiffusionMatrixDD

   !------------------- ���� ----------------------
    subroutine output_gtool4_init
      !
      ! �ҥ��ȥ꡼���Ͻ�����롼����
      !
      !   �ե��������
      !   �ѿ����
      !
      call HistoryCreate( &                               ! �ҥ��ȥ꡼����
           file='et_galerkin_DD_cn_diff.nc', &
           title='Diffusion equation (DD)', &
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
      yx_Phi = yx_et(et_ed(ed_Phi))

      if ( l .ge. 0 ) then
         yx_PhiSol = yx_PhiInit &
              * exp( - kappa * (   (2*PI*k/XD)**2 &
                                 +((2*l+1)*PI/YD)**2   )*time )
      else
         yx_PhiSol = yx_PhiInit &
              * exp( - kappa * (  (2*PI*k/XD)**2 &
                                 +(2*PI*l/YD)**2     )*time )
      endif

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

end program et_galerkin_DD_cn_diff
