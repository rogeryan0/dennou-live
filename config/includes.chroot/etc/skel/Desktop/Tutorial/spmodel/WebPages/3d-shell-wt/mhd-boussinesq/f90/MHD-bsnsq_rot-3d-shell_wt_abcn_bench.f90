!--------------------------------------------------------------------------
!  Copyright (C) 2002--2008 SPMODEL Development Group. All rights reserved.
!--------------------------------------------------------------------------
!
!ɽ��  ��ž��̥֥��ͥ�������ή�Υ�ǥ�
!
!      * ����Ĵ��ȡ���Ѵ� + �����ӥ���������ˡ
!      * ������ʬ Adams-Bashforth + Crank Nicholson ��������
!      * �����ʥ�٥���ޡ���������
!
!����  2002/11/01  ��������ʿ  �٥���ޡ��� Case 1 �ꥹ�����Ȳ�ǽ�С������
!      2002/11/02  ��������ʿ  spmodel ��ˡ�˽�������, �ɤ߹��߽���ͤΥݥƥ󥷥�벽
!      2002/11/19  ��������ʿ  ���������ɤ߹��ߤν���
!      2002/11/19  ��������ʿ  �������򥿥�ˡ�����ѹ� 
!      2004/02/16  ������ ��ʿ gt4f90io �λ��Ѥ�ȼ���饤�֥��̾�ѹ�
!      2004/11/26  �ݹ� ����   Adams-Bashforth scheme �Ѥγƻ����Ѳ��̤�
!                              �ꥹ�������ѿ����ɲ�
!      2004/11/27  �ݹ� ����   ���������������롼�����ɲ�.
!      2004/12/30  �ݹ� ����   HistoryCreate �� xtype ���ɲ�
!                              (Alpha machine �Ǥν��ϻ����顼�ؤλ����н�).
!                              ��̤���Ϥ������֤�ɽ��. 
!      2005/01/03  �ݹ� ����   �������ɲ�, �����ե������Ϳ��������ͤϽ��Ϥ���
!      2005/01/04  �ݹ� ����   HistoryCreate �Υ��ץ���� xtypes typo ����
!
!      2005/01/06  �ݹ� ����   �٥���ޡ����ץ��������̲�
!                              ���Ⱦ��, ʪ���ѥ�᥿��, �������� namelist ��
!                              �ѿ��ɤ߹��ߤ� HistoryGet �ǹԤ�. 
!      2005/01/07  �ݹ� ����   Namelist �ѿ�����°���Ȥ��ƽ���. 
!      2005/01/19  �ݹ� ����   Ⱦ����ǵ���⳰Ⱦ�¤������ǽ�ˤ���. 
!      2005/01/20  �ݹ� ����   ����, ��ή, �إꥷ�ƥ����η׻��ɲ�. 
!      2005/01/25  �ݹ� ����   �����ȥ�� Namelist �������ǽ�ˤ���. 
!      2005/02/04  �ݹ� ����   ����Ȼ���η��� 1/Pm ��ȴ���Ƥ����Τ���. 
!      2005/02/11  �ݹ� ����   ���� 1/Pm �ν�������. 
!      2005/04/17  �ݹ� ����   �����٤� Namelist �ǻ���
!                              ���ͥ륮�����ڥ��ȥ�ν����ɲ�
!      2005/04/29  �ݹ� ����   ��������� V����V ���� (��xV)xV ���ѹ�
!                              ���٤η׻���ݥƥ󥷥�뤫��ľ�ܹԤʤ�. 
!      2005/05/09  �ݹ� ����   ����ͤΥ�����(case0/case1)�Υ����å�Ƴ��
!      2005/05/19  �ݹ� ����   Clank Nicolson �� 1 �ʸ�˶������׻����ɲ�
!      2006/02/19  �ݹ� ����   Crank-Nicolson ���󡦷׻��ѹ�
!      2006/02/21  �ݹ� ����   ������̾(����)����˽���
!      2006/02/28  �ݹ� ����   RDoc �Ѥ˥������ѹ�, �ե������̾
!      2007/06/26  �ݹ� ����   lon_weight, lat_weight ����
!      2007/09/13  �ݹ� ����   ������ʬ���������ѹ�
!      2007/12/19  �ݹ� ����   ���� Crank-Nicolson �������
!      2007/12/20  �ݹ�����    ���ޥ�ɥ饤���������Ϥ�, 
!                              NAMELIST �ե�����̾�����
!      2008/01/25  �ݹ� ����   ���쥨�ͥ륮���׻� bug fix
!      2008/06/11  �ݹ� ����   OPENMP �����å�Ƴ��
!      2008/08/09  �ݹ� ����   ����ź������ im->0:im-1 ���ѹ�
!
program MHD_bsnsq_rot_3d_shell_wt_abcn_bench
  !
  !  ��ž��̥֥��ͥ�������ή�Υ�ǥ�
  !
  !   * ����Ĵ��ȡ���Ѵ� + �����ӥ����ե��顼����ˡ
  !   * ������ʬ Adams-Bashforth + Crank Nicholson ��������
  !   * �����ʥ�٥���ޡ���������
  !
  use lumatrix
  use at_module,  only : a_Int_ag
  use wt_module
  use dc_trace,   only : SetDebug, BeginSub, EndSub, DbgMessage
  use dc_message, only : MessageNotify
  use dc_types
  use dc_string,   only : StoA
  use dc_args
  use gt4_history
  implicit none

!== ����� ============================================================

 !---- �ѿ� ----
  real(8), allocatable :: xyz_Torvel(:,:,:)      ! �ȥ�����®��
  real(8), allocatable :: wt_Torvel(:,:)         ! �ȥ�����®��
  real(8), allocatable :: xyz_Polvel(:,:,:)      ! �ݥ�����®��
  real(8), allocatable :: wt_Polvel(:,:)         ! �ݥ�����®��
  real(8), allocatable :: xyz_LaplaPolvel(:,:,:) ! �ݥ�����®��
  real(8), allocatable :: wt_LaplaPolvel(:,:)    ! ��^2�ݥ�����®��

  real(8), allocatable :: xyz_Tormag(:,:,:)      ! �ȥ����뼧��
  real(8), allocatable :: wt_Tormag(:,:)         ! �ȥ����뼧��
  real(8), allocatable :: xyz_Polmag(:,:,:)      ! �ݥ����뼧��
  real(8), allocatable :: wt_Polmag(:,:)         ! �ݥ����뼧��

  real(8), allocatable :: xyz_Temp(:,:,:)       ! ����
  real(8), allocatable :: wt_Temp(:,:)          ! ����

  real(8), allocatable :: xyz_Vlon(:,:,:)       ! ®��(����)
  real(8), allocatable :: xyz_Vlat(:,:,:)       ! ®��(����)
  real(8), allocatable :: xyz_Vrad(:,:,:)       ! ®��(ư��)

  real(8), allocatable :: xyz_Blon(:,:,:)       ! ����(����)
  real(8), allocatable :: xyz_Blat(:,:,:)       ! ����(����)
  real(8), allocatable :: xyz_Brad(:,:,:)       ! ����(ư��)

  real(8), allocatable :: xyz_Zlon(:,:,:)       ! ����(����)
  real(8), allocatable :: xyz_Zlat(:,:,:)       ! ����(����)
  real(8), allocatable :: xyz_Zrad(:,:,:)       ! ����(ư��)

  real(8), allocatable :: xyz_Jlon(:,:,:)       ! ��ή(����)
  real(8), allocatable :: xyz_Jlat(:,:,:)       ! ��ή(����)
  real(8), allocatable :: xyz_Jrad(:,:,:)       ! ��ή(ư��)

  real(8), allocatable :: xy_TempBndry(:,:,:)   ! ���ٶ�����
  real(8), allocatable :: w_TempBndry(:,:)      ! ���ٶ����ͥ��ڥ��ȥ�

  real(8), allocatable :: nz_EkTor(:,:)   ! ��ư���ͥ륮��(�ȥ�������ʬ)
  real(8), allocatable :: nz_EkPol(:,:)   ! ��ư���ͥ륮��(�ݥ�������ʬ)

  real(8), allocatable :: nz_EmTor(:,:)   ! ���쥨�ͥ륮��(�ȥ�������ʬ)
  real(8), allocatable :: nz_EmPol(:,:)   ! ���쥨�ͥ륮��(�ݥ�������ʬ)

  real(8), allocatable :: nz_EnsTor(:,:)  ! ���󥹥ȥ�ե���(�ȥ�������ʬ)
  real(8), allocatable :: nz_EnsPol(:,:)  ! ���󥹥ȥ�ե���(�ݥ�������ʬ)

 !---- ������ʬ���ѿ� ----

  real(8), allocatable :: wt_DtDTorvel1(:,:), wt_DtDTorvel0(:,:)
                          ! �ȥ�����®�ٻ����Ѳ�
  real(8), allocatable :: wt_DtDLaplaPolvel1(:,:), wt_DtDLaplaPolvel0(:,:)
                          ! ��^2�ݥ�����®�ٻ����Ѳ�
  real(8), allocatable :: wt_DtDTormag1(:,:), wt_DtDTormag0(:,:)
                          ! �ȥ����뼧������Ѳ�
  real(8), allocatable :: wt_DtDPolmag1(:,:), wt_DtDPolmag0(:,:)
                          ! �ݥ����뼧������Ѳ�
  real(8), allocatable :: wt_DtDTemp1(:,:), wt_DtDTemp0(:,:)
                          ! ���ٻ����Ѳ�

  real(8), allocatable :: DifLUMT_Torvel(:,:,:)
                          ! Crank Nicholson ��Ū�׻�����(�ȥ�����®�پ�)
  integer, allocatable :: kpivot_Torvel(:,:)
                          ! Crank Nicholson ����ԥܥå�(�ȥ�����®�پ�)

  real(8), allocatable :: DifLUMT_Polvel(:,:,:)
                          ! Crank Nicholson ��Ū�׻�����
                          ! (�ݥ�����®�پ�)
  integer, allocatable :: kpivot_Polvel(:,:)
                          ! Crank Nicholson ����ԥܥå�(�ݥ�����®�پ�)

  real(8), allocatable :: DifLUMT_Tormag(:,:,:)
                          ! Crank Nicholson ��Ū�׻�����(�ȥ����뼧��)
  integer, allocatable :: kpivot_Tormag(:,:)
                          ! Crank Nicholson ����ԥܥå�(�ȥ����뼧��)

  real(8), allocatable :: DifLUMT_Polmag(:,:,:)
                          ! Crank Nicholson ��Ū�׻�����(�ݥ����뼧��)
  integer, allocatable :: kpivot_Polmag(:,:)
                          ! Crank Nicholson ����ԥܥå�(�ݥ����뼧��)

  real(8), allocatable :: DifLUMT_Temp(:,:,:)
                          ! Crank Nicholson ��Ū�׻�����(���پ�)
  integer, allocatable :: kpivot_Temp(:,:)    ! �ԥܥå�
                          ! Crank Nicholson ��Ū�׻�����ԥܥå�(���پ�)

 !---- ����¾ ----
  real(8), parameter :: pi=3.1415926535897932385D0
  character(len=20)  :: DbgMessageFmt='*** DbgMESSAGE ***  '
  real(8), parameter  :: vmiss = -999.0       ! ��»��
  integer :: it=0
  real(8) :: time
  integer :: n, m, l

 !---- NAMELIST �ѿ� ----
  ! ���ޥ�ɥ饤��������ѿ�
  type(ARGS) :: arg
  character(STRING), pointer :: argv(:) => null()

  ! NAMELIST �����ѥǥե���ȥե�����̾
  character(STRING)  :: nmlfile='MHD-bsnsq_rot-3d-shell_wt_abcn_bench.nml'

  logical            :: verbose=.false.          ! ���ϥ�å�������٥�
  logical            :: DebugOn=.false.          ! �ǥХå����ϥ���ȥ���
  namelist /message/  Verbose, DebugOn           !

  integer  :: im=64, jm=32, km=16                ! �ʻ���(����, ����, ư��)
  integer  :: nm=21, lm=16                       ! �����ȿ�(��ʿ, ư��)
  integer  :: np=1                               ! OPENMP �ץ�����
  namelist /gridset/ im, jm, km, nm, lm, np      !-- �ʻ��������ڥ��ȥ� --

  real(8)            :: ri=0.0D0                 ! ��Ⱦ��
  real(8)            :: ro=0.0D00                ! ��Ⱦ��
  real(8)            :: eta=0.0D0                ! �⳰Ⱦ����
  namelist /radius/  ri, ro, eta                 !-- ��ɸ�ѿ��ʤ� --

  character(STRING)  :: initial_file=''          ! ����ͥǡ����ե�����̾
  real               :: initial_time=0.0         ! �������
  namelist /initial/ initial_file, initial_time  ! -- ����� --

  real(8) :: delta_t=1.0e-7                      ! ������ʬ���
  integer :: nstep=2000                          ! ������ʬ���ƥå׿�
  namelist /tint/    delta_t, nstep              ! -- ������ʬ --

  character(STRING)  :: output_file=''           ! ���ϥե�����̾
  character(STRING)  :: title = &                ! �����ȥ�
            'MHD Dynamo in 3-dim spherical shell'!
  character(STRING)  :: source = &               ! ������̾
            'MHD-bsnsq_rot-3d-shell_bench_wt_abcn.f90 (2008/08/09)'!
  integer :: ndisp=200                           ! ���ϴֳ֥��ƥå׿�
  namelist /output/    output_file, title, ndisp ! -- ���� --

  real(8)            :: Ra=100.0D0               ! �쥤�꡼��
  real(8)            :: Pr=1.0D0                 ! �ץ��ɥ��
  real(8)            :: Ekman=1.0d-3             ! �����ޥ��
  real(8)            :: Pm=5.0D0                 ! �����ץ��ɥ��
  namelist /physics/  Ra, Pr, Ekman, Pm          ! -- ʪ���ѥ�᥿�� --

  character(len=2)   :: VelBC='RR'               ! ®�ٶ������(RR/RF/FR/FF)
  character(len=2)   :: TempBC='DD'              ! ���ٶ������(DD/DN/ND/NN)
  real(8)            :: Temptop=0.0D0            ! ��������(��ü)
  real(8)            :: Tempbottom=1.0D0         ! ��������(��ü)
  namelist /boundary/ VelBC, TempBC, Temptop, Tempbottom
                                                 ! -- ������� --

 !---------------- NAMELIST �ɤ߹��� ---------------------
  call Open(arg)
  call Debug(arg) ; call Help(arg) ; call Strict(arg)
  call Get(arg, argv)
  if ( size(argv) .le. 0 ) then
     call MessageNotify('W','main',&
          'Usage: MHD_bsnsq_rot_3d_shell_wt_abcn_bench.out [nmlfilename]')
     call MessageNotify('E','main','There is no argument. ')
  else
     nmlfile=argv(1)
     call MessageNotify('M','main','Namelist file is '//trim(nmlfile))
  endif
  deallocate(argv)
  call Close(arg)

  call MessageNotify('M','main', trim(source))

  write(6,nml=message) 
  open(10,file=nmlfile,status='OLD')
  read(10,nml=message) ; write(6,nml=message) ; close(10)

  if(verbose) write(6,nml=gridset)
  open(10,file=nmlfile,status='OLD')
  read(10,nml=gridset)  ; write(6,nml=gridset) ; close(10)

  if (verbose) write(6,nml=radius)
  open(10,file=nmlfile,status='OLD')
  read(10,nml=radius)
   !
   ! ri,ro �� eta �Τɤ��餫�� NAMELIST radius ��Ϳ����. eta �����꤬ͥ�褵���
   !
  if ( eta /= 0.0D0 ) then
     ri=eta/(1.0D0-eta) ; ro=1.0D0/(1.0D0-eta)
     call MessageNotify('M','main', &
             'The inner/outer radii were calculatied with their ratio, eta.')
  else if ( ro /= 0.0D0 .and. ro /= 0.0D0 ) then
     eta = ri/ro
     call MessageNotify('M','main', &
             'The ratio of inner/outer radii were calculatied with ro and ri.')
  else
     call MessageNotify('E','main', &
                 'The inner/outer radii or their ratio should be given')
  endif
  write(6,nml=radius)  ; close(10)

  if(verbose)write(6,nml=initial)
  open(10,file=nmlfile,status='OLD')
  read(10,nml=initial) ; write(6,nml=initial) ; close(10)

  if(verbose)write(6,nml=tint)
  open(10,file=nmlfile,status='OLD')
  read(10,nml=tint)     ; write(6,nml=tint)    ; close(10)

  if(verbose)write(6,nml=output)
  open(10,file=nmlfile,status='OLD')
  read(10,nml=output)   ; write(6,nml=output)  ; close(10)

  if(verbose)write(6,nml=physics)
  open(10,file=nmlfile,status='OLD')
  read(10,nml=physics)  ; write(6,nml=physics) ; close(10)

  if(verbose)write(6,nml=boundary)
  open(10,file=nmlfile,status='OLD')
  read(10,nml=boundary) ; write(6,nml=boundary); close(10)

 !---------------- �ǥХå������������� -----------------
  if (DebugOn) then
    call SetDebug
  end if

 !------------------ �ѿ��γ���դ� ---------------------
  allocate(xyz_Torvel(0:im-1,jm,0:km),wt_Torvel((nm+1)**2,0:lm))
  allocate(xyz_Polvel(0:im-1,jm,0:km),wt_Polvel((nm+1)**2,0:lm))
  allocate(xyz_LaplaPolvel(0:im-1,jm,0:km),wt_LaplaPolvel((nm+1)**2,0:lm))

  allocate(xyz_Tormag(0:im-1,jm,0:km),wt_Tormag((nm+1)**2,0:lm))
  allocate(xyz_Polmag(0:im-1,jm,0:km),wt_Polmag((nm+1)**2,0:lm))

  allocate(xyz_Temp(0:im-1,jm,0:km),wt_Temp((nm+1)**2,0:lm))

  allocate(xyz_Vlon(0:im-1,jm,0:km))        ! ®��(����)
  allocate(xyz_Vlat(0:im-1,jm,0:km))        ! ®��(����)
  allocate(xyz_Vrad(0:im-1,jm,0:km))        ! ®��(ư��)

  allocate(xyz_Zlon(0:im-1,jm,0:km))        ! ����(����)
  allocate(xyz_Zlat(0:im-1,jm,0:km))        ! ����(����)
  allocate(xyz_Zrad(0:im-1,jm,0:km))        ! ����(ư��)

  allocate(xyz_Blon(0:im-1,jm,0:km))        ! ����(����)
  allocate(xyz_Blat(0:im-1,jm,0:km))        ! ����(����)
  allocate(xyz_Brad(0:im-1,jm,0:km))        ! ����(ư��)

  allocate(xyz_Jlon(0:im-1,jm,0:km))        ! ��ή(����)
  allocate(xyz_Jlat(0:im-1,jm,0:km))        ! ��ή(����)
  allocate(xyz_Jrad(0:im-1,jm,0:km))        ! ��ή(ư��)

  allocate(xy_TempBndry(0:im-1,jm,2))       ! ���ٶ�����
  allocate(w_TempBndry((nm+1)**2,2))    ! ���ٶ����ͥ��ڥ��ȥ�

  allocate(nz_EkTor(0:nm,0:km))         ! ��ư���ͥ륮��(�ȥ�������ʬ)
  allocate(nz_EkPol(0:nm,0:km))         ! ��ư���ͥ륮��(�ݥ�������ʬ)

  allocate(nz_EmTor(0:nm,0:km))         ! ���쥨�ͥ륮��(�ȥ�������ʬ)
  allocate(nz_EmPol(0:nm,0:km))         ! ���쥨�ͥ륮��(�ݥ�������ʬ)

  allocate(nz_EnsTor(0:nm,0:km))        ! ���󥹥ȥ�ե���(�ȥ�������ʬ)
  allocate(nz_EnsPol(0:nm,0:km))        ! ���󥹥ȥ�ե���(�ݥ�������ʬ)

 ! �ȥ�����®�ٻ����Ѳ�
  allocate(wt_DtDTorvel1((nm+1)**2,0:lm),wt_DtDTorvel0((nm+1)**2,0:lm))

 ! ��^2�ݥ�����®�ٻ����Ѳ�
  allocate(wt_DtDLaplaPolvel1((nm+1)**2,0:lm),wt_DtDLaplaPolvel0((nm+1)**2,0:lm))
 ! �ȥ����뼧������Ѳ�
  allocate(wt_DtDTormag1((nm+1)**2,0:lm),wt_DtDTormag0((nm+1)**2,0:lm))

 ! �ݥ����뼧������Ѳ�
  allocate(wt_DtDPolmag1((nm+1)**2,0:lm),wt_DtDPolmag0((nm+1)**2,0:lm))

 ! ���ٻ����Ѳ�
  allocate(wt_DtDTemp1((nm+1)**2,0:lm),wt_DtDTemp0((nm+1)**2,0:lm))

 ! Crank Nicholson ��Ū�׻��ѳȻ�����(�ȥ�����®�پ�)
  allocate(DifLUMT_Torvel((nm+1)*(nm+1),0:lm,0:lm))
  allocate(kpivot_Torvel((nm+1)*(nm+1),0:lm))          ! �ԥܥå�

 ! Crank Nicholson ��Ū�׻��ѳȻ�����(�ݥ�����®�پ�)
  allocate(DifLUMT_Polvel((nm+1)*(nm+1),0:lm,0:lm))
  allocate(kpivot_Polvel((nm+1)*(nm+1),0:lm))          ! �ԥܥå�

 ! Crank Nicholson ��Ū�׻��ѳȻ�����(�ȥ����뼧��)
  allocate(DifLUMT_Tormag((nm+1)*(nm+1),0:lm,0:lm))
  allocate(kpivot_Tormag((nm+1)*(nm+1),0:lm))          ! �ԥܥå�

 ! Crank Nicholson ��Ū�׻��ѳȻ�����(�ݥ����뼧��)
  allocate(DifLUMT_Polmag((nm+1)*(nm+1),0:lm,0:lm))
  allocate(kpivot_Polmag((nm+1)*(nm+1),0:lm))          ! �ԥܥå�

 ! Crank Nicholson ��Ū�׻��ѳȻ�����(���پ�)
  allocate(DifLUMT_Temp((nm+1)*(nm+1),0:lm,0:lm))
  allocate(kpivot_Temp((nm+1)*(nm+1),0:lm))          ! �ԥܥå�

 !---------------- ��ɸ�ͤ����� ---------------------
  call DbgMessage(fmt='call %c', c1='wt_Initial') 

  if ( np .gt. 1 ) then
     call wt_Initial(im,jm,km,nm,lm,ri,ro,np=np)
  else
     call wt_Initial(im,jm,km,nm,lm,ri,ro)
  endif

  wt_VMiss = vmiss

 !------------------- ��������� ----------------------
  if ( initial_file == "") then
     call set_initial_values 
       ! xyz_Torvel,xyz_Polvel,xyz_Tormag,xyz_Polmag,xyz_Temp �����ꤹ��
  else
     call initial_read_gtool4 
       ! xyz_Torvel,xyz_Polvel,xyz_Tormag,xyz_Polmag,xyz_Temp,
       ! wt_DtDTorvel1, wt_DtDLaplaPolvel1, 
       ! wt_DtDTormag1, wt_DtDPolmag1, wt_DtDTemp1 ���ɤ߹���
  endif

 !------------------- ���������� ----------------------
  xy_Tempbndry(:,:,1) = Temptop
  xy_Tempbndry(:,:,2) = Tempbottom

  w_TempBndry(:,1) = w_xy(xy_TempBndry(:,:,1))
  w_TempBndry(:,2) = w_xy(xy_TempBndry(:,:,2))

 !------------------- Crank-Nicoslon �ѹ������� ----------------------
  call CNDiffusionMatrixTorvel( 1.0D0, delta_t, DifLUMT_Torvel, kpivot_Torvel )
  call CNDiffusionMatrixPolvel( 1.0D0, delta_t, DifLUMT_Polvel, kpivot_Polvel )
  call CNDiffusionMatrixTemp( 1/Pr, delta_t, DifLUMT_Temp, kpivot_Temp )
  call CNDiffusionMatrixTormag( 1/Pm, delta_t, DifLUMT_Tormag, kpivot_Tormag )
  call CNDiffusionMatrixPolmag( 1/Pm, delta_t, DifLUMT_Polmag, kpivot_Polmag )


 !------------------- ������ʬ���Ϥ������� --------------------
  time = initial_time
  
  wt_Temp = wt_xyz(xyz_Temp)
  wt_Torvel = wt_xyz(xyz_Torvel) 
  wt_Polvel = wt_xyz(xyz_Polvel)
  wt_LaplaPolvel = wt_Lapla_wt(wt_Polvel)
  xyz_LaplaPolvel = xyz_wt(wt_LaplaPolvel)
  wt_Tormag = wt_xyz(xyz_Tormag)
  wt_Polmag = wt_xyz(xyz_Polmag)

  if ( initial_file == "") then
     call wt_TorBoundariesGrid(wt_Torvel,cond=velBC)
     wt_Polvel = wt_wz(wz_LaplaPol2Pol_wz(wz_wt(wt_LaplaPolvel),cond=velBC))
     wt_LaplaPolvel = wt_Lapla_wt(wt_Polvel)
     call wt_TormagBoundariesGrid(wt_Tormag)
     call wt_PolmagBoundariesGrid(wt_Polmag)
     call wt_BoundariesGrid(wt_Temp,w_TempBndry,cond=TempBC)

     xyz_Temp = xyz_wt(wt_Temp)
     xyz_Torvel = xyz_wt(wt_Torvel) 
     xyz_Polvel = xyz_wt(wt_Polvel)
     xyz_LaplaPolvel = xyz_wt(wt_LaplaPolvel)
     xyz_Tormag = xyz_wt(wt_Tormag)
     xyz_Polmag = xyz_wt(wt_Polmag)
  endif

  call wt_Potential2Vector(xyz_Vlon,xyz_Vlat,xyz_Vrad,wt_Torvel,wt_Polvel)
  call wt_Potential2Vector &
       (xyz_ZLon,xyz_ZLat,xyz_ZRad,-wt_LaplaPolvel,wt_Torvel)

  call wt_Potential2Vector(xyz_Blon,xyz_Blat,xyz_Brad,wt_Tormag,wt_Polmag)
  call wt_Potential2Rotation(xyz_JLon,xyz_JLat,xyz_JRad,wt_Tormag,wt_Polmag)

  if ( initial_file == "") then
     call TimeDerivetives_noDiffusion( &
          wt_Torvel, wt_Polvel, wt_LaplaPolvel, wt_Temp, &
          xyz_Vlon, xyz_Vlat, xyz_Vrad, &
          xyz_Zlon, xyz_Zlat, xyz_Zrad, &
          xyz_Blon, xyz_Blat, xyz_Brad, &
          xyz_Jlon, xyz_Jlat, xyz_Jrad, &
          xyz_Temp, &
          wt_DtDTorvel1, wt_DtDLaplaPolvel1, &
          wt_DtDTormag1, wt_DtDPolmag1, wt_DtDTemp1 )
  endif

  call output_gtool4_init
  if ( initial_file == '' ) call output_gtool4    ! ������Ϳ��������ͤϽ���

 !------------------- ������ʬ(Adams-Bashforth ˡ) --------------------
  do it=1,nstep
     time = initial_time + it * delta_t

     ! �Τλ����Ѳ������¸
     wt_DtDTorvel0 = wt_DtDTorvel1
     wt_DtDLaplaPolvel0 = wt_DtDLaplaPolvel1
     wt_DtDTormag0 = wt_DtDTormag1
     wt_DtDPolmag0 = wt_DtDPolmag1
     wt_DtDTemp0   = wt_DtDTemp1

     call TimeDerivetives_noDiffusion( &
          wt_Torvel, wt_Polvel, wt_LaplaPolvel, wt_Temp, &
          xyz_Vlon, xyz_Vlat, xyz_Vrad, &
          xyz_Zlon, xyz_Zlat, xyz_Zrad, &
          xyz_Blon, xyz_Blat, xyz_Brad, &
          xyz_Jlon, xyz_Jlat, xyz_Jrad, &
          xyz_Temp, &
          wt_DtDTorvel1, wt_DtDLaplaPolvel1, &
          wt_DtDTormag1, wt_DtDPolmag1, wt_DtDTemp1 )

     ! �ȥ�����®�پ�
     wt_Torvel = wt_Torvel &
          + delta_t *( 3/2.0*wt_DtDTorvel1 - 1/2.0*wt_DtDTorvel0 ) &
          + delta_t/2.0 * wt_Lapla_wt(wt_Torvel) 
     call wt_TorBoundariesGrid(wt_Torvel,cond=velBC)
     wt_Torvel = LUSolve(DifLUMT_Torvel,kpivot_Torvel,wt_Torvel)

     ! �ݥ�����®�پ�
     wt_LaplaPolvel = wt_LaplaPolvel &
           + delta_t *(3/2.0*wt_DtDLaplaPolvel1 - 1/2.0*wt_DtDLaplaPolvel0 ) &
           + delta_t/2.0 * wt_Lapla_wt(wt_LaplaPolvel) 
     wt_Polvel = wt_wz(wz_LaplaPol2Pol_wz(wz_wt(wt_LaplaPolvel),cond=velBC))
     wt_Polvel = LUSolve(DifLUMT_Polvel,kpivot_Polvel,wt_Polvel)
     wt_LaplaPolvel = wt_Lapla_wt(wt_Polvel)

     ! �ȥ����뼧��
     wt_Tormag = wt_Tormag &
          + delta_t *( 3/2.0*wt_DtDTormag1 - 1/2.0*wt_DtDTormag0 ) &
          + delta_t/2.0 * (1/Pm) * wt_Lapla_wt(wt_Tormag) 
     call wt_TormagBoundariesGrid(wt_Tormag)
     wt_Tormag = LUSolve(DifLUMT_Tormag,kpivot_Tormag,wt_Tormag)

     ! �ݥ����뼧��
     wt_Polmag = wt_Polmag &
           + delta_t *(3/2.0*wt_DtDPolmag1 - 1/2.0*wt_DtDPolmag0 ) &
           + delta_t/2.0* (1/Pm) * wt_Lapla_wt(wt_Polmag) 
     call wt_PolmagBoundariesGrid(wt_Polmag)
     wt_Polmag = LUSolve(DifLUMT_Polmag,kpivot_Polmag,wt_Polmag)

     ! ���پ�
     wt_Temp = wt_Temp &
          + delta_t *( 3/2.0*wt_DtDTemp1 - 1/2.0*wt_DtDTemp0 ) &
          + delta_t/2.0 * (1/Pr) * wt_Lapla_wt(wt_Temp) 
     call wt_BoundariesGrid(wt_Temp,w_TempBndry,cond=TempBC)
     wt_Temp = LUSolve(DifLUMT_Temp,kpivot_Temp,wt_Temp)

     ! ����å��ͷ׻�
     xyz_Torvel = xyz_wt(wt_Torvel)
     xyz_Polvel = xyz_wt(wt_Polvel)
     xyz_LaplaPolvel = xyz_wt(wt_LaplaPolvel)
     xyz_Tormag = xyz_wt(wt_Tormag)
     xyz_Polmag = xyz_wt(wt_Polmag)
     xyz_Temp = xyz_wt(wt_Temp)

     call wt_Potential2Vector(xyz_Vlon,xyz_Vlat,xyz_Vrad,wt_Torvel,wt_Polvel)
     call wt_Potential2Vector &
          (xyz_ZLon,xyz_ZLat,xyz_ZRad,-wt_LaplaPolvel,wt_Torvel)

     call wt_Potential2Vector(xyz_Blon,xyz_Blat,xyz_Brad,wt_Tormag,wt_Polmag)
     call wt_Potential2Rotation(xyz_JLon,xyz_JLat,xyz_JRad,wt_Tormag,wt_Polmag)

     if(mod(it,ndisp) .eq. 0)then                    ! ����
        call output_gtool4
     endif
  enddo
  call output_gtool4_close

contains

! �ʾ� �ᥤ��ץ���� 
!-----------------------------------------------------------------------------
! �ʲ� ���֥롼����
 !------------------- �����Ѳ���(�Ȼ���ʳ�) ----------------------
  subroutine TimeDerivetives_noDiffusion( &
          wt_Torvel, wt_Polvel, wt_LaplaPolvel, wt_Temp, &
          xyz_Vlon, xyz_Vlat, xyz_Vrad, &
          xyz_Zlon, xyz_Zlat, xyz_Zrad, &
          xyz_Blon, xyz_Blat, xyz_Brad, &
          xyz_Jlon, xyz_Jlat, xyz_Jrad, &
          xyz_Temp, &
          wt_DtDTorvel, wt_DtDLaplaPolvel, &
          wt_DtDTormag, wt_DtDPolmag, wt_DtDTemp )
    !
    ! �����Ѳ���(�Ȼ���ʳ�)�η׻���Ԥ�. 
    !
    real(8), intent(IN) :: wt_Torvel(:,:)        ! �ȥ�����®��
    real(8), intent(IN) :: wt_Polvel(:,:)        ! �ݥ�����®��
    real(8), intent(IN) :: wt_LaplaPolvel(:,:)   ! ��^2 �ݥ�����®��
    real(8), intent(IN) :: wt_Temp(:,:)          ! ����

    real(8), intent(IN)     :: xyz_Vlon(:,:,:)   ! ®��(����)
    real(8), intent(IN)     :: xyz_Vlat(:,:,:)   ! ®��(����)
    real(8), intent(IN)     :: xyz_Vrad(:,:,:)   ! ®��(ư��)

    real(8), intent(IN)     :: xyz_Zlon(:,:,:)   ! ����(����)
    real(8), intent(IN)     :: xyz_Zlat(:,:,:)   ! ����(����)
    real(8), intent(IN)     :: xyz_Zrad(:,:,:)   ! ����(ư��)

    real(8), intent(IN)     :: xyz_Blon(:,:,:)   ! ����(����)
    real(8), intent(IN)     :: xyz_Blat(:,:,:)   ! ����(����)
    real(8), intent(IN)     :: xyz_Brad(:,:,:)   ! ����(ư��)

    real(8), intent(IN)     :: xyz_Jlon(:,:,:)   ! ��ή(����)
    real(8), intent(IN)     :: xyz_Jlat(:,:,:)   ! ��ή(����)
    real(8), intent(IN)     :: xyz_Jrad(:,:,:)   ! ��ή(ư��)

    real(8), intent(IN)     :: xyz_Temp(:,:,:)   ! ����

    real(8), intent(OUT) :: wt_DtDTorvel(:,:)   ! �ȥ�����®�ٻ����Ѳ���
    real(8), intent(OUT) :: wt_DtDLaplaPolvel(:,:)
                                                ! ��^2 �ݥ�����®�ٻ����Ѳ���
    real(8), intent(OUT) :: wt_DtDTormag(:,:)   ! �ȥ����뼧������Ѳ���
    real(8), intent(OUT) :: wt_DtDPolmag(:,:)   ! �ݥ����뼧������Ѳ���
    real(8), intent(OUT) :: wt_DtDTemp(:,:)     ! ���ٻ����Ѳ���

    !--- ����ѿ� ---
    real(8), dimension(size(xyz_Vlon,1),size(xyz_Vlon,2),size(xyz_Vlon,3)) &
         :: xyz_ZxVLon   ! (��xV)xV (����)
    real(8), dimension(size(xyz_Vlat,1),size(xyz_Vlat,2),size(xyz_Vlat,3)) &
         :: xyz_ZxVLat   ! (��xV)xV(����)
    real(8), dimension(size(xyz_Vrad,1),size(xyz_Vrad,2),size(xyz_Vrad,3)) &
         :: xyz_ZxVRad   ! (��xV)xV(ư��)

    real(8), dimension(size(xyz_Blon,1),size(xyz_Blon,2),size(xyz_Blon,3)) &
         :: xyz_JxBLon   ! (��xB)xB (����)
    real(8), dimension(size(xyz_Blat,1),size(xyz_Blat,2),size(xyz_Blat,3)) &
         :: xyz_JxBLat   ! (��xB)xB(����)
    real(8), dimension(size(xyz_Brad,1),size(xyz_Brad,2),size(xyz_Brad,3)) &
         :: xyz_JxBRad   ! (��xB)xB(ư��)

    ! (��xv)xv ��������׻�
    xyz_ZxVLon = xyz_ZLat * xyz_VRad - xyz_ZRad * xyz_VLat 
    xyz_ZxVLat = xyz_ZRad * xyz_VLon - xyz_ZLon * xyz_VRad 
    xyz_ZxVRad = xyz_ZLon * xyz_VLat - xyz_ZLat * xyz_VLon 

   ! (��xB)xv ��������׻�
    xyz_JxBLon = xyz_JLat * xyz_BRad - xyz_JRad * xyz_BLat 
    xyz_JxBLat = xyz_JRad * xyz_BLon - xyz_JLon * xyz_BRad 
    xyz_JxBRad = xyz_JLon * xyz_BLat - xyz_JLat * xyz_BLon 

  ! �ȥ���������Ѳ�
    wt_DtDTorvel = &
           wt_L2Inv_wt(  2/Ekman * wt_KxRGrad_wt(wt_Torvel)   &
                       - 2/Ekman * wt_Qoperator_wt(wt_Polvel) ) &
         - wt_L2Inv_wt(wt_RadRot_xyz_xyz(      &
                       xyz_ZxVLon - xyz_JxBLon/(Ekman*Pm), &
                       xyz_ZxVLat - xyz_JxBLat/(Ekman*Pm)))

  ! �ݥ���������Ѳ�
    wt_DtDLaplaPolvel = &
         - Ra/(Ekman*RO)*wt_Temp                    &
         + wt_L2Inv_wt(  2/Ekman * wt_KxRGrad_wt(wt_LaplaPolvel) &
                       + 2/Ekman * wt_Qoperator_wt(wt_Torvel) )  &
         + wt_L2Inv_wt(wt_RadRotRot_xyz_xyz_xyz(          &
               xyz_ZxVLon - xyz_JxBLon/(Ekman*Pm),&
               xyz_ZxVLat - xyz_JxBLat/(Ekman*Pm),&
               xyz_ZxVRad - xyz_JxBRad/(Ekman*Pm) ))

  ! ���ٻ����Ѳ�
    wt_DtDTemp = &
         - wt_Div_xyz_xyz_xyz(  &
               xyz_Vlon*xyz_Temp, xyz_Vlat*xyz_Temp, xyz_Vrad*xyz_Temp)

  ! �ȥ����뼧��
    wt_DtDTormag = &
           wt_L2Inv_wt(wt_RadRotRot_xyz_xyz_xyz(         &
               xyz_Vlat * xyz_Brad - xyz_Vrad * xyz_Blat,&
               xyz_Vrad * xyz_Blon - xyz_Vlon * xyz_Brad,&
               xyz_Vlon * xyz_Blat - xyz_Vlat * xyz_Blon))

  ! �ݥ����뼧��
    wt_DtDPolmag = &
           wt_L2Inv_wt( wt_RadRot_xyz_xyz(               &
               xyz_Vlat * xyz_Brad - xyz_Vrad * xyz_Blat,&
               xyz_Vrad * xyz_Blon - xyz_Vlon * xyz_Brad))

  end subroutine TimeDerivetives_noDiffusion

 !------------------- �Ȼ���(�ȥ�����®�پ�) ----------------------
  subroutine CNDiffusionMatrixTorvel( Diffc, dt, DiffLUMatrix, kpivot )
    !
    ! Crank Nicolson �ѳȻ��ౢŪ��������(�ȥ�����®�پ�)
    !
    real(8), intent(IN)          :: Diffc    ! �Ȼ�����
    real(8), intent(IN)          :: dt       ! ���ֹ�

    real(8), intent(OUT)   :: DiffLUMatrix(:,0:,0:)
                                ! Crank Nicholson �ѹ���(1-D dt/2��^2, LU ʬ��)
    integer, intent(OUT)   :: kpivot(:,0:)        ! �ԥܥå�
                                ! Crank Nicholson ����ԥܥåȾ���

    real(8), dimension(size(DiffLUMatrix,1),0:size(DiffLUMatrix,2)-1) :: wt_I
    ! ������ѿ�
    real(8), dimension(size(DiffLUMatrix,1),0:size(DiffLUMatrix,2)-1) :: wt_DI
    ! ������ѿ�

    integer :: l

    DiffLUMatrix = 0.0
    do l=0,lm
       wt_I = 0.0 ; wt_I(:,l) = 1.0             ! ���ȿ���ʬ��Ω
       wt_DI = - Diffc * dt/2.0 * wt_Lapla_wt(wt_I) 
       call wt_TorBoundariesGrid(wt_DI,cond=velBC)
       DiffLUMatrix(:,:,l) = wt_I + wt_DI
    enddo

    call ludecomp(DiffLUMatrix,kpivot)

  end subroutine CNDiffusionMatrixTorvel

 !------------------- �Ȼ���(�ݥ�����®�پ�) ----------------------
  subroutine CNDiffusionMatrixPolvel( Diffc, dt, DiffLUMatrix, kpivot )
    !
    ! Crank Nicolson �ѳȻ��ౢŪ��������(�ݥ�����®�پ�)
    !
    real(8), intent(IN)          :: Diffc    ! �Ȼ�����
    real(8), intent(IN)          :: dt       ! ���ֹ�

    real(8), intent(OUT)   :: DiffLUMatrix(:,0:,0:)
                                ! Crank Nicholson �ѹ���(1-D dt/2��^2, LU ʬ��)
    integer, intent(OUT)   :: kpivot(:,0:)        ! �ԥܥå�
                                ! Crank Nicholson ����ԥܥåȾ���

    real(8), dimension(size(DiffLUMatrix,1),0:size(DiffLUMatrix,2)-1) :: wt_I
    ! ������ѿ�
    real(8), dimension(size(DiffLUMatrix,1),0:size(DiffLUMatrix,2)-1) :: wt_DI
    ! ������ѿ�

    integer :: l

    DiffLUMatrix = 0.0
    do l=0,lm
       wt_I = 0.0 ; wt_I(:,l) = 1.0             ! ���ȿ���ʬ��Ω
       wt_DI = - Diffc * dt/2.0 * wt_Lapla_wt(wt_Lapla_wt(wt_I))
       wt_DI = wt_wz(wz_LaplaPol2Pol_wz(wz_wt(wt_DI),cond=velBC))
       DiffLUMatrix(:,:,l) = wt_I + wt_DI
    enddo

    call ludecomp(DiffLUMatrix,kpivot)

  end subroutine CNDiffusionMatrixPolvel

 !------------------- �Ȼ���(���پ�) ----------------------
  subroutine CNDiffusionMatrixTemp( Diffc, dt, DiffLUMatrix, kpivot )
    !
    ! Crank Nicolson �ѳȻ��ౢŪ��������(���پ�)
    !
    real(8), intent(IN)          :: Diffc    ! �Ȼ�����
    real(8), intent(IN)          :: dt       ! ���ֹ�

    real(8), intent(OUT)   :: DiffLUMatrix(:,0:,0:)
                                ! Crank Nicholson �ѹ���(1-D dt/2��^2, LU ʬ��)
    integer, intent(OUT)   :: kpivot(:,0:)        ! �ԥܥå�
                                ! Crank Nicholson ����ԥܥåȾ���

    real(8), dimension(size(DiffLUMatrix,1),0:size(DiffLUMatrix,2)-1) :: wt_I
    ! ������ѿ�
    real(8), dimension(size(DiffLUMatrix,1),0:size(DiffLUMatrix,2)-1) :: wt_DI
    ! ������ѿ�
    real(8), dimension(size(DiffLUMatrix,1),2) :: w_Bndry
    ! ������ѿ�

    integer :: l

    DiffLUMatrix = 0.0
    w_Bndry=0.0D0                               ! �����λ����Ѳ��� 0
    do l=0,lm
       wt_I = 0.0 ; wt_I(:,l) = 1.0             ! ���ȿ���ʬ��Ω
       wt_DI = - Diffc * dt/2.0 * wt_Lapla_wt(wt_I) 
       call wt_BoundariesGrid(wt_DI,w_Bndry,cond=TempBC)
       DiffLUMatrix(:,:,l) = wt_I + wt_DI
    enddo

    call ludecomp(DiffLUMatrix,kpivot)

  end subroutine CNDiffusionMatrixTemp

 !------------------- �Ȼ���(�ȥ����뼧��) ----------------------
  subroutine CNDiffusionMatrixTormag( Diffc, dt, DiffLUMatrix, kpivot )
    !
    ! Crank Nicolson �ѳȻ��ౢŪ��������(�ȥ����뼧��)
    !
    real(8), intent(IN)          :: Diffc    ! �Ȼ�����
    real(8), intent(IN)          :: dt       ! ���ֹ�

    real(8), intent(OUT)   :: DiffLUMatrix(:,0:,0:)
                                ! Crank Nicholson �ѹ���(1-D dt/2��^2, LU ʬ��)
    integer, intent(OUT)   :: kpivot(:,0:)        ! �ԥܥå�
                                ! Crank Nicholson ����ԥܥåȾ���

    real(8), dimension(size(DiffLUMatrix,1),0:size(DiffLUMatrix,2)-1) :: wt_I
    ! ������ѿ�
    real(8), dimension(size(DiffLUMatrix,1),0:size(DiffLUMatrix,2)-1) :: wt_DI
    ! ������ѿ�

    integer :: l

    DiffLUMatrix = 0.0
    do l=0,lm
       wt_I = 0.0 ; wt_I(:,l) = 1.0             ! ���ȿ���ʬ��Ω
       wt_DI = - Diffc * dt/2.0 * wt_Lapla_wt(wt_I) 
       call wt_TormagBoundariesGrid(wt_DI)
       DiffLUMatrix(:,:,l) = wt_I + wt_DI
    enddo

    call ludecomp(DiffLUMatrix,kpivot)

  end subroutine CNDiffusionMatrixTormag

 !------------------- �Ȼ���(�ݥ����뼧��) ----------------------
  subroutine CNDiffusionMatrixPolmag( Diffc, dt, DiffLUMatrix, kpivot )
    !
    ! Crank Nicolson �ѳȻ��ౢŪ��������(�ݥ����뼧��)
    !
    real(8), intent(IN)          :: Diffc    ! �Ȼ�����
    real(8), intent(IN)          :: dt       ! ���ֹ�


    real(8), intent(OUT)   :: DiffLUMatrix(:,0:,0:)
                                ! Crank Nicholson �ѹ���(1-D dt/2��^2, LU ʬ��)
    integer, intent(OUT)   :: kpivot(:,0:)        ! �ԥܥå�
                                ! Crank Nicholson ����ԥܥåȾ���

    real(8), dimension(size(DiffLUMatrix,1),0:size(DiffLUMatrix,2)-1) :: wt_I
    ! ������ѿ�
    real(8), dimension(size(DiffLUMatrix,1),0:size(DiffLUMatrix,2)-1) :: wt_DI
    ! ������ѿ�

    integer :: l

    DiffLUMatrix = 0.0
    do l=0,lm
       wt_I = 0.0 ; wt_I(:,l) = 1.0             ! ���ȿ���ʬ��Ω
       wt_DI = - Diffc * dt/2.0 * wt_Lapla_wt(wt_I) 
       call wt_PolmagBoundariesGrid(wt_DI)
       DiffLUMatrix(:,:,l) = wt_I + wt_DI
    enddo

    call ludecomp(DiffLUMatrix,kpivot)

  end subroutine CNDiffusionMatrixPolmag

 !------------------- ���������(�ե�����ʤ�) ----------------------
  subroutine set_initial_values         ! xyz_* �����ꤹ��
    !
    ! ���������롼����
    !
    !     ����ͥե����뤬���ꤵ��ʤ��ä����������ǽ�������ꤹ��.
    !     xyz_Torvel,xyz_Polvel,xyz_Tormag,xyz_Polmag,xyz_Temp �����ꤹ��
    !
    integer :: case=1                       ! ����ͤΥ�����(0/1)
    namelist /inittype/ case                ! ����ͤΥ���������

    real(8), dimension(size(xyz_Vlon,1),size(xyz_Vlon,2),size(xyz_Vlon,3)) &
         :: xyz_X         ! x=2r-r_i-r_o
    real(8), dimension(size(xyz_Vlon,1),size(xyz_Vlon,2),size(xyz_Vlon,3)) &
         :: xyz_theta     ! ;����

    real(8), parameter  :: A=0.1D0                     ! ������پ�����
    real(8), parameter  :: pi=3.1415926535897932385D0

    if(verbose) write(6,nml=inittype)
    open(10,file=nmlfile,status='OLD')
    read(10,nml=inittype); write(6,nml=inittype) ; close(10)

    xyz_theta = pi/2.0D0 - xyz_Lat
    xyz_X = 2*xyz_Rad - Ri - Ro

    ! ���پ�
    xyz_Temp =  Ro*Ri/xyz_Rad - Ri &
       + 210*A/sqrt(17920*pi) * (1 - 3*xyz_X**2 + 3*xyz_X**4 -xyz_X**6) &
           * sin(pi/2-xyz_Lat)**4 * cos(4*xyz_Lon)

    ! ®�پ� ; �Ż߾���
    xyz_Torvel =  0 ; xyz_Polvel =  0

    ! ���� 
    if ( case == 0 )then
       xyz_Tormag = 0.0D0 ;  xyz_Polmag = 0.0D0    ! ����ʤ�

       call MessageNotify('M','set_initial_values',&
            'Selected initial data type is  "CASE0". ')

    elseif ( case == 1 )then
       xyz_BLon = 5.0D0 * sin(pi*(xyz_Rad-ri)) * sin(2.0D0*xyz_theta)
       xyz_BLat = 5.0D0/8.0D0 &
            * ( 9.0D0*xyz_Rad - 8.0D0*ro - ri**4.0D0/xyz_Rad**3.0D0) &
            * sin(xyz_theta)
       xyz_BRad = 5.0D0/8.0D0 * ( 8.0D0*ro - 6.0D0*xyz_Rad &
            - 2.0D0*ri**4.0D0/xyz_Rad**3.0D0 )* cos(xyz_theta)

       ! ���줫��ݥƥ󥷥��ؤ��Ѵ�
       xyz_Tormag = xyz_wt(wt_L2Inv_wt(wt_RadRot_xyz_xyz(xyz_BLon, xyz_BLat)))
       xyz_Polmag = xyz_wt(wt_L2Inv_wt(wt_xyz(xyz_Rad * xyz_BRad)))

       call MessageNotify('M','set_initial_values',&
            'Selected initial data type is  "CASE1". ')
    else
       call MessageNotify('E','set_initial_values',&
            'The parameter "case" should be 0 or 1.')
    endif

  end subroutine set_initial_values

 !------------------- ���������(�ե����뤫��) ----------------------  
  subroutine initial_read_gtool4
    !
    ! ���������롼����
    !
    !     ����ͥե����뤫���ѿ�
    !     xyz_Torvel,xyz_Polvel,xyz_Tormag,xyz_Polmag,xyz_Temp,
    !     wt_DtDTorvel1, wt_DtDLaplaPolvel1, 
    !     wt_DtDTormag1, wt_DtDPolmag1, wt_DtDTemp1 ���ɤ߹���

    call HistoryGet( trim(initial_file), 'torvel', xyz_Torvel, initial_time )
    call HistoryGet( trim(initial_file), 'polvel', xyz_Polvel, initial_time )
    call HistoryGet( trim(initial_file), 'tormag', xyz_Tormag, initial_time )
    call HistoryGet( trim(initial_file), 'polmag', xyz_Polmag, initial_time )
    call HistoryGet( trim(initial_file), 'temp',   xyz_Temp,   initial_time )

    call HistoryGet( trim(initial_file), 'DtDTorvel', wt_DtDTorvel1, initial_time )
    call HistoryGet( trim(initial_file), 'DtDLaplaPolvel', wt_DtDLaplaPolvel1, initial_time )
      
    call HistoryGet( trim(initial_file), 'DtDTormag', wt_DtDTormag1, initial_time )
      
    call HistoryGet( trim(initial_file), 'DtDPolmag', wt_DtDPolmag1, initial_time )

    call HistoryGet( trim(initial_file), 'DtDTemp', wt_DtDTemp1, initial_time )
      
  end subroutine initial_read_gtool4

 !------------------- ���� ----------------------
  subroutine output_gtool4_init
    !
    ! �ҥ��ȥ꡼���Ͻ�����롼����
    !
    !   �ե��������
    !   �ѿ����
    !

    !---- �ҥ��ȥ꡼�ե�������� ----
    call HistoryCreate( &
           file=trim(output_file), &
           title=trim(title), source=trim(source), &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'lon','lat','rad','nm ','n  ','m  ','l  ','t  '/), &
           dimsizes=(/im,jm,km+1,(nm+1)**2,nm+1,2*nm+1,lm+1,0/),&
           longnames=(/'Longitude             ','Latitude              ',&
                       'Radius                ', &
                       'Hor.wave number index ','Hor.total wave number ',&
                       'zonal wave number     ','Radial chebyshev order',&
                       'time                  '/),&
           units=(/'1','1','1','1','1','1','1','1'/),   &
           origin=real(time), interval=real(ndisp*delta_t))

   !---- ��ɸ�ѿ����, ���� ----
    call HistoryPut('lon',x_Lon/pi*180)                       ! �ѿ�����
    call HistoryAddattr('lon','topology','circular')          ! ����°��
    call HistoryAddattr('lon','modulo',360.0)                 ! ����°��
    call HistoryPut('lat',y_Lat/pi*180)                       ! �ѿ�����
    call HistoryPut('rad',z_Rad)                              ! �ѿ�����
    call HistoryPut('nm',(/(dble(n),n=0,(nm+1)**2)/))         ! �ѿ�����
    call HistoryPut('n',(/(dble(n),n=0,nm)/))                 ! �ѿ�����
    call HistoryPut('m',(/(dble(m),m=-nm,nm)/))               ! �ѿ�����
    call HistoryPut('l',(/(dble(l),l=0,lm)/))                 ! �ѿ�����

    call HistoryAddVariable( &                                ! �ѿ����
           varname='lon_weight', dims=(/'lon'/), & 
           longname='weight function in longitude', &
           units='1', xtype='double')
    call HistoryPut('lon_weight',x_Lon_weight)                ! �ѿ�����
    call HistoryAddVariable( &                                ! �ѿ����
           varname='lat_weight', dims=(/'lat'/), & 
           longname='weight function in latitude', &
           units='1', xtype='double')
    call HistoryPut('lat_weight',y_Lat_weight)                ! �ѿ�����
    call HistoryAddVariable( &                                ! �ѿ����
           varname='rad_weight', dims=(/'rad'/), & 
           longname='weight function in radial', &
           units='1', xtype='double')
    call HistoryPut('rad_weight',z_Rad_weight)                ! �ѿ�����

   !---- ʪ���ѿ���� ----
    !-- ®�٥ݥƥ󥷥�� --
    call HistoryAddVariable( &                                ! �ѿ����
           varname='torvel', dims=(/'lon','lat','rad','t  '/), & 
           longname='toroidal velocity potential', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='polvel', dims=(/'lon','lat','rad','t  '/), & 
           longname='poloidal velocity potential', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='laplapolvel', dims=(/'lon','lat','rad','t  '/), & 
           longname='laplacian poloidal velocity potential', &
           units='1', xtype='double')

    !-- ����ݥƥ󥷥�� --
    call HistoryAddVariable( &                                ! �ѿ����
           varname='tormag', dims=(/'lon','lat','rad','t  '/), & 
           longname='toroidal magnetic field potential', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='polmag', dims=(/'lon','lat','rad','t  '/), & 
           longname='poloidal magnetic field potential', units='1', xtype='double')
    !-- ���� --
    call HistoryAddVariable( &                                ! �ѿ����
           varname='temp', dims=(/'lon','lat','rad','t  '/), & 
           longname='temperature', units='1', xtype='double')
    !-- ®�� --
    call HistoryAddVariable( &                                ! �ѿ����
           varname='vlon', dims=(/'lon','lat','rad','t  '/), & 
           longname='lon-velocity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='vlat', dims=(/'lon','lat','rad','t  '/), & 
           longname='lat-velocity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='vrad', dims=(/'lon','lat','rad','t  '/), & 
           longname='rad-velocity', units='1', xtype='double')
    !-- ���� --
    call HistoryAddVariable( &                                ! �ѿ����
           varname='zlon', dims=(/'lon','lat','rad','t  '/), & 
           longname='lon-vorticity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='zlat', dims=(/'lon','lat','rad','t  '/), & 
           longname='lat-vorticity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='zrad', dims=(/'lon','lat','rad','t  '/), & 
           longname='rad-voticity', units='1', xtype='double')
    !-- ���� --
    call HistoryAddVariable( &                                ! �ѿ����
           varname='blon', dims=(/'lon','lat','rad','t  '/), & 
           longname='lon-mag.field', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='blat', dims=(/'lon','lat','rad','t  '/), & 
           longname='lat-mag.field', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='brad', dims=(/'lon','lat','rad','t  '/), & 
           longname='rad-mag.field', units='1', xtype='double')
    !-- ��ή --
    call HistoryAddVariable( &                                ! �ѿ����
           varname='jlon', dims=(/'lon','lat','rad','t  '/), & 
           longname='lon-current', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='jlat', dims=(/'lon','lat','rad','t  '/), & 
           longname='lat-current', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='jrad', dims=(/'lon','lat','rad','t  '/), & 
           longname='rad-current', units='1', xtype='double')

   !---- �����Ѳ������ ----
    call HistoryAddVariable( &                                ! �ѿ����
           varname='DtDTorvel', dims=(/'nm ','l  ','t  '/), & 
           longname='time variation of velocity toroidal potential', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='DtDLaplaPolvel', dims=(/'nm ','l  ','t  '/), & 
           longname='time variation of laplacian of velocity poloidal potential', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='DtDTemp', dims=(/'nm ','l  ','t  '/), & 
           longname='time variation of temperature', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='DtDTormag', dims=(/'nm ','l  ','t  '/), & 
           longname='time variation of magnetic field toroidal potential', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='DtDPolmag', dims=(/'nm ','l  ','t  '/), & 
           longname='time variation of magnetic field poloidal potential', &
           units='1', xtype='double')

   !---- ����¾��������� ----
    !-- �����ͥ륮�� --
    call HistoryAddVariable( &                                ! �ѿ����
           varname='ek', dims=(/'t  '/), & 
           longname='mean kinetic energy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='em', dims=(/'t  '/), & 
           longname='mean magnetic energy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='ektot', dims=(/'t  '/), & 
           longname='total kinetic energy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='emtot', dims=(/'t  '/), & 
           longname='total magnetic energy', units='1', xtype='double')


    !-- ʿ���Ӿ�ή --
    call HistoryAddVariable( &                                ! �ѿ����
           varname='um', dims=(/'lat','rad','t  '/), & 
           longname='zonal flow', units='1', xtype='double')
    !-- �إꥷ�ƥ��� --
    call HistoryAddVariable( &                                ! �ѿ����
           varname='hel', dims=(/'lon','lat','rad','t  '/), & 
           longname='helicity', units='1', xtype='double')

    !-- ��ư���ͥ륮�����ڥ��ȥ��������� --
    call HistoryAddVariable( &                                ! �ѿ����
           varname='ektor_nmz', dims=(/'n  ','m  ','rad','t  '/), & 
           longname='Toroidal kinetic energy spectrum', units='1', xtype='double')
    call HistoryAddAttr('ektor_nmz','missing_value', vmiss  )

    call HistoryAddVariable( &                                ! �ѿ����
           varname='ekpol_nmz', dims=(/'n  ','m  ','rad','t  '/), & 
           longname='Poloidal kinetic energy spectrum', units='1', xtype='double')
    call HistoryAddAttr('ekpol_nmz','missing_value', vmiss  )

    call HistoryAddVariable( &                                ! �ѿ����
           varname='ektor_nz', dims=(/'n  ','rad','t  '/), & 
           longname='Toroidal kinetic energy spectrum', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='ekpol_nz', dims=(/'n  ','rad','t  '/), & 
           longname='Poloidal kinetic energy spectrum', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='ektor_n', dims=(/'n  ','t  '/), & 
           longname='Toroidal kinetic energy spectrum', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='ekpol_n', dims=(/'n  ','t  '/), & 
           longname='Poloidal kinetic energy spectrum', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='ektor', dims=(/'t  '/), & 
           longname='Toroidal kinetic energy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='ekpol', dims=(/'t  '/), & 
           longname='Poloidal kinetic energy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='ekpot', dims=(/'t'/), & 
           longname='Kinetic energy (by potential)', units='1', xtype='double')

    !-- ���쥨�ͥ륮�����ڥ��ȥ��������� --
    call HistoryAddVariable( &                                ! �ѿ����
           varname='emtor_nmz', dims=(/'n  ','m  ','rad','t  '/), & 
           longname='Toroidal magnetic energy spectrum', units='1', xtype='double')
    call HistoryAddAttr('emtor_nmz','missing_value', vmiss  )

    call HistoryAddVariable( &                                ! �ѿ����
           varname='empol_nmz', dims=(/'n  ','m  ','rad','t  '/), & 
           longname='Poloidal magnetic energy spectrum', units='1', xtype='double')
    call HistoryAddAttr('empol_nmz','missing_value', vmiss  )

    call HistoryAddVariable( &                                ! �ѿ����
           varname='emtor_nz', dims=(/'n  ','rad','t  '/), & 
           longname='Toroidal magnetic energy spectrum', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='empol_nz', dims=(/'n  ','rad','t  '/), & 
           longname='Poloidal magnetic energy spectrum', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='emtor_n', dims=(/'n  ','t  '/), & 
           longname='Toroidal magnetic energy spectrum', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='empol_n', dims=(/'n  ','t  '/), & 
           longname='Poloidal magnetic energy spectrum', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='emtor', dims=(/'t  '/), & 
           longname='Toroidal magnetic energy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='empol', dims=(/'t  '/), & 
           longname='Poloidal magnetic energy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='empot', dims=(/'t'/), & 
           longname='Magnetic energy (by potential)', units='1', xtype='double')
   !---- ���󥹥ȥ�ե������ڥ��ȥ��������� ----
    call HistoryAddVariable( &                                ! �ѿ����
           varname='enstor_nmz', dims=(/'n  ','m  ','rad','t  '/), & 
           longname='Toroidal enstrophy spectrum', units='1', xtype='double')
    call HistoryAddAttr('enstor_nmz','missing_value', vmiss  )

    call HistoryAddVariable( &                                ! �ѿ����
           varname='enspol_nmz', dims=(/'n  ','m  ','rad','t  '/), & 
           longname='Poloidal enstrophy spectrum', units='1', xtype='double')
    call HistoryAddAttr('enspol_nmz','missing_value', vmiss  )

    call HistoryAddVariable( &                                ! �ѿ����
           varname='enstor_nz', dims=(/'n  ','rad','t  '/), & 
           longname='Toroidal enstrophy spectrum', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='enspol_nz', dims=(/'n  ','rad','t  '/), & 
           longname='Poloidal enstrophy spectrum', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='enstor_n', dims=(/'n  ','t  '/), & 
           longname='Toroidal enstrophy spectrum', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='enspol_n', dims=(/'n  ','t  '/), & 
           longname='Poloidal enstrophy spectrum', units='1', xtype='double')

    call HistoryAddVariable( &                                ! �ѿ����
           varname='enstor', dims=(/'t  '/), & 
           longname='Toroidal enstrophy', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='enspol', dims=(/'t  '/), & 
           longname='Poloidal enstrophy', units='1', xtype='double')

    call HistoryAddVariable( &                                ! �ѿ����
           varname='enspot', dims=(/'t'/), & 
           longname='enstrophy (by potential)', units='1', xtype='double')


   !---- �¸��ѥ�᥿����°���Ȥ������, ����(���� Global °��) ----
    call HistoryAddAttr('lon','+Radius', (/ri,ro,eta/) )
    call HistoryAddAttr('lon','+delta_t', delta_t  )

    call HistoryAddAttr('lon', '+Rayleigh',   Ra    )
    call HistoryAddAttr('lon', '+Prandtl',    Pr    )
    call HistoryAddAttr('lon', '+Ekman',      Ekman )
    call HistoryAddAttr('lon', '+MagPrandtl', Pm    )

    call HistoryAddAttr('lon', '+VelBoundary',  VelBC  )
    call HistoryAddAttr('lon', '+TempBoundary', TempBC )

   end subroutine output_gtool4_init

  subroutine output_gtool4
    !
    ! �ҥ��ȥ꡼����
    !
    !   ��ʪ���̤� gtool4 �ե�����ؤν��Ϥ�Ԥ�
    !
    write(6,*) 'it = ',it, '  time = ', time
    call HistoryPut('t',real(time))

   !---- ʪ���ѿ����� ----
    call HistoryPut('torvel',xyz_Torvel)
    call HistoryPut('polvel',xyz_Polvel)
    call HistoryPut('tormag',xyz_Tormag)
    call HistoryPut('polmag',xyz_Polmag)
    call HistoryPut('laplapolvel',xyz_LaplaPolvel)
    call HistoryPut('temp',xyz_Temp)

    call HistoryPut('vlon',xyz_Vlon)
    call HistoryPut('vlat',xyz_Vlat)
    call HistoryPut('vrad',xyz_Vrad)

    call HistoryPut('zlon',xyz_Zlon)
    call HistoryPut('zlat',xyz_Zlat)
    call HistoryPut('zrad',xyz_Zrad)

    call HistoryPut('blon',xyz_Blon)
    call HistoryPut('blat',xyz_Blat)
    call HistoryPut('brad',xyz_Brad)

    call HistoryPut('jlon',xyz_Jlon)
    call HistoryPut('jlat',xyz_Jlat)
    call HistoryPut('jrad',xyz_Jrad)

   !---- �����Ѳ������ ----
    call HistoryPut('DtDTorvel',      wt_DtDTorvel1)
    call HistoryPut('DtDLaplaPolvel', wt_DtDLaplaPolvel1)
    call HistoryPut('DtDTemp',        wt_DtDTemp1)
    call HistoryPut('DtDTormag',      wt_DtDTormag1)
    call HistoryPut('DtDPolmag',      wt_DtDPolmag1)

   !---- �����̽��� ----
    !-- ʿ�ѥ��ͥ륮�� --
    call HistoryPut('ek',AvrLonLatRad_xyz((xyz_Vlon**2+xyz_Vlat**2+xyz_Vrad**2)/2))
    call HistoryPut('em',AvrLonLatRad_xyz((xyz_Blon**2+xyz_Blat**2+xyz_Brad**2)/(2*Ekman*Pm)))

    !-- �����ͥ륮�� --
    call HistoryPut('ektot',IntLonLatRad_xyz((xyz_Vlon**2+xyz_Vlat**2+xyz_Vrad**2)/2))
    call HistoryPut('emtot',IntLonLatRad_xyz((xyz_Blon**2+xyz_Blat**2+xyz_Brad**2)/(2*Ekman*Pm)))
    !-- �Ӿ�ή --
    call HistoryPut('um',yz_avrLon_xyz(xyz_Vlon))
    call HistoryPut('hel',xyz_Vlon*xyz_Zlon + xyz_Vlat*xyz_Zlat + xyz_Vrad*xyz_Zrad)

   !---- ��ư���ͥ륮�����ڥ��ȥ�����̽��� ----
    nz_EkTor = nz_ToroidalEnergySpectrum_wt(wt_Torvel)
    nz_EkPol = nz_PoloidalEnergySpectrum_wt(wt_Polvel)

    call HistoryPut('ektor_nmz',nmz_ToroidalEnergySpectrum_wt(wt_Torvel))
    call HistoryPut('ekpol_nmz',nmz_PoloidalEnergySpectrum_wt(wt_Polvel))
    call HistoryPut('ektor_nz',nz_EkTor)
    call HistoryPut('ekpol_nz',nz_EkPol)
    call HistoryPut('ektor_n',a_Int_ag(nz_EkTor/(spread(z_Rad,1,nm+1)**2)))
    call HistoryPut('ekpol_n',a_Int_ag(nz_EkPol/(spread(z_Rad,1,nm+1)**2)))
    call HistoryPut('ektor',IntRad_z(sum(nz_EkTor,1)/z_Rad**2))
    call HistoryPut('ekpol',IntRad_z(sum(nz_EkPol,1)/z_Rad**2))
    call HistoryPut('ekpot',IntRad_z(sum(nz_EkTor,1)/z_Rad**2)+IntRad_z(sum(nz_EkPol,1)/z_Rad**2))

   !---- ���쥨�ͥ륮�����ڥ��ȥ�����̽��� ----
    nz_EmTor = nz_ToroidalEnergySpectrum_wt(wt_Tormag)/(Ekman*Pm)
    nz_EmPol = nz_PoloidalEnergySpectrum_wt(wt_Polmag)/(Ekman*Pm)

    call HistoryPut('emtor_nmz',nmz_ToroidalEnergySpectrum_wt(wt_Tormag)/(Ekman*Pm))
    call HistoryPut('empol_nmz',nmz_PoloidalEnergySpectrum_wt(wt_Polmag)/(Ekman*Pm))
    call HistoryPut('emtor_nz',nz_EmTor)
    call HistoryPut('empol_nz',nz_EmPol)
    call HistoryPut('emtor_n',a_Int_ag(nz_EmTor/(spread(z_Rad,1,nm+1)**2)))
    call HistoryPut('empol_n',a_Int_ag(nz_EmPol/(spread(z_Rad,1,nm+1)**2)))
    call HistoryPut('emtor',IntRad_z(sum(nz_EmTor,1)/z_Rad**2))
    call HistoryPut('empol',IntRad_z(sum(nz_EmPol,1)/z_Rad**2))
    call HistoryPut('empot',IntRad_z(sum(nz_EmTor,1)/z_Rad**2)+IntRad_z(sum(nz_EmPol,1)/z_Rad**2))

   !---- ���󥹥ȥ�ե������ڥ��ȥ�����̽��� ----
    nz_EnsTor = nz_ToroidalEnergySpectrum_wt(-wt_LaplaPolvel)
    nz_EnsPol = nz_PoloidalEnergySpectrum_wt(wt_Torvel)

    call HistoryPut('enstor_nmz',nmz_ToroidalEnergySpectrum_wt(-wt_LaplaPolvel))
    call HistoryPut('enspol_nmz',nmz_PoloidalEnergySpectrum_wt(wt_Torvel))
    call HistoryPut('enstor_nz',nz_EnsTor)
    call HistoryPut('enspol_nz',nz_EnsPol)
    call HistoryPut('enstor_n',a_Int_ag(nz_EnsTor/(spread(z_Rad,1,nm+1)**2)))
    call HistoryPut('enspol_n',a_Int_ag(nz_EnsPol/(spread(z_Rad,1,nm+1)**2)))
    call HistoryPut('enstor',IntRad_z(sum(nz_EnsTor,1)/z_Rad**2))
    call HistoryPut('enspol',IntRad_z(sum(nz_EnsPol,1)/z_Rad**2))
    call HistoryPut('enspot',IntRad_z(sum(nz_EnsTor,1)/z_Rad**2)+IntRad_z(sum(nz_EnsPol,1)/z_Rad**2))

  end subroutine output_gtool4

  subroutine output_gtool4_close
    !
    ! �ҥ��ȥ꡼���Ͻ�λ����
    !
    call HistoryClose

  end subroutine output_gtool4_close

end program MHD_bsnsq_rot_3d_shell_wt_abcn_bench
