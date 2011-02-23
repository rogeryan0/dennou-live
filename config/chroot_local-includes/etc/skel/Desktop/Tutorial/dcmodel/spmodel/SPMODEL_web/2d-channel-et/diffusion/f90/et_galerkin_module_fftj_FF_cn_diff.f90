!----------------------------------------------------------------------
!     Copyright (c) 2009  Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  SPMODEL 2 ������ϩ�ΰ��󰵽�ή�Υ�ǥ�
!      �����ӥ����ե��顼����ˡ�ˤ��׻�
!
!      ������ʬ : Crank-Nicolson ˡ
!      ξü��ͳ���٤���
!
! Solving 2-D impompressible fluid system
!     d\zeta/dt =  Pr\nabla\zeta
!     \nabla\psi = \zeta
!     psi = psi'' = 0 at y=-d/2,d/2
!
!      �׻������ӤΤ���β��ϲ�ˤĤ��Ƥϲ���ʸ��
!
!         ��2 ������ϩ�ΰ�ǤγȻ����������β��ϲ��
!
!      (et_diffusion.pdf)�򻲾ȤΤ���. 
!
!����  2009/09/16  �ݹ�����  et_galerkin_SS_cn_diff.f90 �� fftj �Ѥ˲�¤
!
program et_galerkin_module_fftj_FF_cn_diff
  !
  ! SPMODEL 2 ������ϩ�ΰ��󰵽�ή�Υ�ǥ�
  !      �����ӥ����ե��顼����ˡ�ˤ��׻�
  !
  !      ������ʬ : Crank-Nicolson ˡ
  !      ξü��ͳ���٤���
  !
  ! Solving 2-D impompressible fluid system
  !     d\zeta/dt =  Pr\nabla\zeta
  !     \nabla\psi = \zeta
  !     psi = psi'' = 0 at y=-d/2,d/2
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
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter  :: im=32, jm=32      ! �ʻ���������(X,Y)
  integer, parameter  :: km=10, lm=20      ! �����ȿ�������(X,Y)

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter  :: XD=8.0                   ! x �����ΰ���礭��
  real(8), parameter  :: YD=1.0                   ! y �����ΰ���礭��
  real(8), parameter  :: xmin=0,     xmax=XD      ! X ��ɸ�ϰ�
  real(8), parameter  :: ymin=-YD/2, ymax=YD/2    ! Y ��ɸ�ϰ�

 !---- �ѿ� ----
  real(8)             :: yx_Psi(0:jm,0:im-1)      ! �ʻҥǡ���(ή��)
  real(8)             :: yx_Zeta(0:jm,0:im-1)     ! �ʻҥǡ���(����)

  real(8)             :: et_Psi(-km:km,0:lm)      ! ���ڥ��ȥ�ǡ���(ή��)
  real(8)             :: et_Zeta(-km:km,0:lm)     ! ���ڥ��ȥ�ǡ���(����)

  real(8)             :: eq_Psi(-km:km,4:lm)      ! ���顼����ǡ���(ή��)

  real(8)             :: yx_PsiInit(0:jm,0:im-1)  ! �ʻҥǡ���(����, �����)
  real(8)             :: yx_PsiSol(0:jm,0:im-1)   ! �ʻҥǡ���(����, ���ϲ�)
  real(8)             :: yx_PsiError(0:jm,0:im-1) ! �ʻҥǡ���(����, ��)

 !---- ������ʬ�ѥ�᥿�� ----
  real(8), parameter :: dt=1e-3                 ! ���֥��ƥå״ֳ�
  integer, parameter :: nt=100, ndisp=10   ! ������ʬ��, ɽ�����ƥå�

 !---- ʪ���ѥ�᥿�� ----
  real(8), parameter          :: Pr=1.0         ! �ץ��ɥ��
  character(len=2), parameter :: VelBC='FF'     ! ®�پ춭�����(RR/FF/RF/FR)

 !--- Crank Nicholson ��Ū�׻��Ѻ���ѿ� ---
  real(8) :: DifLUMT_Psi(-km:km,4:lm,4:lm)
                                     ! Crank Nicholson ��Ū�׻��ѹ���
  integer :: kpivot_Psi(-km:km,4:lm) ! Crank Nicholson �ѹ���ԥܥåȾ���

 !---- ����¾ ----
  real(8), parameter  :: pi=3.1415926535897932385D0
  integer :: it=0
  real(8) :: time=0.0D0
  integer :: k, l

 !---------------- ��ɸ�ͤ����� ---------------------
  call et_galerkin_Initial(im,jm,km,lm,xmin,xmax,ymin,ymax,VelBC=VelBC)

  call CNDiffusionMatrixStrm( Pr, dt, DifLUMT_Psi, kpivot_Psi )

 !------------------- ��������� ----------------------
  write(6,*) 'Positive k:cos(kx), negative k: sin(kx)'
  write(6,*) 'Positvie l:cos(lx), negative l: sin(lx)'
  write(6,*) 'k,l?'
  read(5,*)  k,l

  if ( k .ge. 0 .AND. l .ge. 0) then
     write(6,*) 'Type of the initial value : cos(kx)*cos(ly)'
     yx_Psi = cos(2*k*Pi*yx_X/XD) * cos((2*l+1)*Pi*yx_Y/YD)
  else if ( k .lt. 0 .AND. l .ge. 0) then
     write(6,*) 'Type of the initial value : sin(kx)*cos(ly)'
     yx_Psi = sin(2*k*Pi*yx_X/XD) * cos((2*l+1)*Pi*yx_Y/YD)
  else if ( k .ge. 0 .AND. l .lt. 0) then
     write(6,*) 'Type of the initial value : cos(kx)*cos(ly)'
     yx_Psi = cos(2*k*Pi*yx_X/XD) * sin(2*l*Pi*yx_Y/YD)
  else 
     write(6,*) 'Type of the initial value : sin(kx)*sin(ly)'
     yx_Psi = sin(2*k*Pi*yx_X/XD) * sin(2*l*Pi*yx_Y/YD)
  endif
  yx_PsiInit = yx_Psi
  
  eq_Psi = eq_et(et_yx(yx_Psi))

  et_Psi  = et_yx(yx_Psi)
  et_Zeta = et_Lapla_et(et_eq(eq_Psi))

  call output_gtool4_init                            ! �ҥ��ȥ꡼�����
  call output_gtool4

 !------------------- ������ʬ ----------------------
  do it=1,nt                                         ! Cn ˡ�ˤ�������ʬ
     time =it*dt
     et_Zeta = et_Zeta + dt/2 *( Pr*et_Lapla_et(et_Zeta)  )
     eq_Psi = eq_Vor2Strm_et(et_Zeta)
     eq_Psi = LuSolve(DifLUMT_Psi,kpivot_Psi,eq_Psi)
     et_Zeta = et_Lapla_et(et_eq(eq_Psi))

     if(mod(it,ndisp) .eq. 0)then                    ! ����
        call output_gtool4
     endif
  enddo

  call output_gtool4_close                           ! �ҥ��ȥ꡼�����
  stop

  contains

   !------------------- �Ȼ���(ή���ؿ���) ----------------------
    subroutine CNDiffusionMatrixStrm( Diffc, dt, DiffLUMatrix, kpivot )
      ! 
      ! Crank Nicholson �Ȼ���Ū�׻��ѹ��������
      ! (ή���ؿ���, ��^{-2}[��^2-D dt/2��^2��^2], LU ʬ��)
      ! 
      real(8), intent(IN)   :: Diffc    ! �Ȼ�����
      real(8), intent(IN)   :: dt       ! ���ֹ�

      ! Crank Nicholson �Ȼ���Ū�׻��ѹ���(��^{-2}(��^2-D (dt/2)��^4, LU ʬ��)
      real(8), intent(OUT)  :: DiffLUMatrix(-km:km,4:lm,4:lm)
      integer, intent(OUT)  :: kpivot(-km:km,4:lm)             ! �ԥܥåȾ���

      real(8)               :: eq_I(-km:km,4:lm)               ! ������ѿ�
      real(8)               :: et_I(-km:km,0:lm)               ! ������ѿ�

      integer :: l

      DiffLUMatrix = 0.0
      do l=4,lm
         eq_I = 0.0 ; eq_I(:,l) = 1.0             ! ���ȿ���ʬ��Ω
         et_I = et_eq(eq_I)
         et_I = et_Lapla_et(et_I)
         et_I = et_I - Diffc * dt/2.0 * et_Lapla_et(et_I)
         DiffLUMatrix(:,:,l) = eq_Vor2Strm_et(et_I)
      enddo

      call LuDeComp(DiffLUMatrix,kpivot)

    end subroutine CNDiffusionMatrixStrm

   !------------------- ���� ----------------------
    subroutine output_gtool4_init
      !
      ! �ҥ��ȥ꡼���Ͻ�����롼����
      !
      !   �ե��������
      !   �ѿ����
      !
      call HistoryCreate( &                               ! �ҥ��ȥ꡼����
           file='et_galerkin_module_fftj_FF_cn_diff.nc', &
           title='2dim. imcompressible fluid', &
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
           varname='psi', dims=(/'x','y','t'/), & 
           longname='stream function', units='1', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
           varname='zeta', dims=(/'x','y','t'/), & 
           longname='vorticity', units='1', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
           varname='psisol', dims=(/'x','y','t'/), & 
           longname='stream function(analytic)', units='1', xtype='double')
      call HistoryAddVariable( &                          ! �ѿ����
           varname='psierror', dims=(/'x','y','t'/), & 
           longname='stream function(Error)', units='1', xtype='double')

     !---- �¸��ѥ�᥿����°���Ȥ������, ����(Global °��) ----
      call HistoryAddAttr('x','+VelBC', VelBC )

    end subroutine output_gtool4_init

    subroutine output_gtool4
      !
      ! �ҥ��ȥ꡼����
      !
      !   ��ʪ���̤� gtool4 �ե�����ؤν��Ϥ�Ԥ�
      !
      yx_Psi  = yx_et(et_eq(eq_Psi))
      yx_Zeta = yx_et(et_Zeta)

      if ( l .ge. 0 ) then
         yx_PsiSol = yx_PsiInit &
              * exp( - Pr * (   (2*PI*k/XD)**2 &
                                 +((2*l+1)*PI/YD)**2   )*time )
      else
         yx_PsiSol = yx_PsiInit &
              * exp( - Pr * (  (2*PI*k/XD)**2 &
                                 +(2*PI*l/YD)**2     )*time )
      endif

      yx_PsiError = yx_Psi-yx_PsiSol

      write(6,*) 'it = ',it
      call HistoryPut('t',real(it*dt))
      call HistoryPut('psi',transpose(yx_Psi))
      call HistoryPut('zeta',transpose(yx_Zeta))
      call HistoryPut('psisol',transpose(yx_PsiSol))
      call HistoryPut('psierror',transpose(yx_PsiError))
    end subroutine output_gtool4

    subroutine output_gtool4_close
      !
      ! �ҥ��ȥ꡼���Ͻ�λ����
      !
      call HistoryClose
    end subroutine output_gtool4_close

end program et_galerkin_module_fftj_FF_cn_diff
