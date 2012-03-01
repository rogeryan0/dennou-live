!----------------------------------------------------------------------
!  Copyright (C) 2004--2008 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  ��ž��̥֥��ͥ���ή�Υ�ǥ�
!      Adams-Bashforth + Crank Nicholson scheme
!      ��ľ�ʻ������֤Ǥ�����ˡ�ˤ�붭�����Ŭ��
!
!����  2002/09/04  �ݹ� ����   �٥���ޡ��� Case 0
!      2002/10/14  �ݹ� ����   �ꥹ�����Ȳ�ǽ�С������
!      2002/11/19  �ݹ� ����   �������򥿥�ˡ�����ѹ�
!      2002/11/24  �ݹ� ����   ��ľ�ʻҶ��֤Ǥ� Crank Nicholson ����. 
!                              ����׻��Ƕ����ͤ��ѹ����ʤ�. 
!      2002/11/26  �ݹ� ����   ����򸺤餷�ѿ�̾�ѹ�
!      2004/02/16  ������ ��ʿ gt4f90io �λ��Ѥ�ȼ���饤�֥��̾�ѹ�
!      2004/10/27  �ݹ� ����   Adams-Bashforth scheme �Ѥγƻ����Ѳ��̤�
!                              �ꥹ�������ѿ����ɲ�
!      2004/12/30  �ݹ� ����   HistoryCreate �� xtype ���ɲ�
!                              (Alpha machine �Ǥν��ϻ����顼�ؤλ����н�).
!      2005/01/03  �ݹ� ����   �������ɲ�
!      2005/01/04  �ݹ� ����   HistoryCreate �Υ��ץ���� xtypes typo ����
!      2008/08/13  �ݹ� ����   ����ź�����ѹ� im -> 0:im-1
!
program gami_abcn_bench0

  use lumatrix
  use wt_module
  use gt4_history
  implicit none

 !---- ���ֲ��������� ----
  ! T10 N8
  integer,parameter :: im=32, jm=16, km=8  ! �ʻ���������(����, ����, ư��)
  integer,parameter :: nm=10, lm=8         ! �����ȿ�������(��ʿ, ư��)
  !  T21 N16
  !integer,parameter :: im=64, jm=32, km=16  ! �ʻ���������(����, ����, ư��)
  !integer,parameter :: nm=21, lm=16         ! �����ȿ�������(��ʿ, ư��)
  !! T21 N32
  !integer,parameter  :: im=64, jm=32, km=32  ! �ʻ���������(����, ����, ư��)
  !integer,parameter  :: nm=21, lm=32         ! �����ȿ�������(��ʿ, ư��)
  !  T42 N32
  !integer,parameter :: im=128, jm=64, km=32 ! �ʻ���������(����, ����, ư��)
  !integer,parameter :: nm=42, lm=32         ! �����ȿ�������(��ʿ, ư��)
  !  T85 N32
  !integer,parameter :: im=256, jm=128, km=48 ! �ʻ���������(����, ����, ư��)
  !integer,parameter :: nm=85, lm=48          ! �����ȿ�������(��ʿ, ư��)
  !  T170 N48
  !integer,parameter :: im=512, jm=256, km=64 ! �ʻ���������(����, ����, ư��)
  !integer,parameter :: nm=170, lm=64         ! �����ȿ�������(��ʿ, ư��)
  !  T341 N80
  !integer,parameter :: im=1024, jm=512, km=80 ! �ʻ���������(����, ����, ư��)
  !integer,parameter :: nm=341, lm=80          ! �����ȿ�������(��ʿ, ư��)

 !---- ��ɸ�ѿ��ʤ� ----
  real(8),parameter  :: Ri=7.0D0/13.0D0     ! �⳰Ⱦ�� \eta=0.35
  real(8),parameter  :: Ro=20.0D0/13.0D0    ! �⳰Ⱦ�� \eta=0.35

 !---- �ѿ� ----
  real(8), dimension((nm+1)**2,0:lm) :: wt_VTor       ! �ȥ�����®��
  real(8), dimension((nm+1)**2,0:lm) :: wt_VPol       ! �ݥ�����®��
  real(8), dimension((nm+1)**2,0:lm) :: wt_LaplaVPol  ! ��^2�ݥ�����®��
  real(8), dimension((nm+1)**2,0:lm) :: wt_Temp       ! ����

  real(8), dimension(0:im-1,jm,0:km) :: xyz_VTor      ! �ȥ�����®��
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VPol      ! �ݥ�����®��
  real(8), dimension(0:im-1,jm,0:km) :: xyz_Temp      ! ����

  real(8), dimension(0:im-1,jm,0:km) :: xyz_VLon      ! ®��(����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VLat      ! ®��(����)
  real(8), dimension(0:im-1,jm,0:km) :: xyz_VRad      ! ®��(ư��)

  real(8), dimension(0:im-1,jm,0:km) :: xyz_Q      ! Ǯ��
  real(8), dimension((nm+1)**2,0:lm) :: wt_Q       ! Ǯ��

  real(8)             :: xy_TempBndry(0:im-1,jm,2) ! ������
  real(8)             :: w_TempBndry((nm+1)**2,2)  ! ������

 !---- ������ʬ���ѿ� ----

 ! �ȥ�����®�ٻ����Ѳ�
  real(8), dimension((nm+1)**2,0:lm) :: wt_DtDVTor1, wt_DtDVTor0
 ! ��^2�ݥ�����®�ٻ����Ѳ�
  real(8), dimension((nm+1)**2,0:lm) :: wt_DtDLaplaVPol1, wt_DtDLaplaVPol0
 ! ���ٻ����Ѳ�
  real(8), dimension((nm+1)**2,0:lm) :: wt_DtDTemp1, wt_DtDTemp0

 ! Crank Nicholson ��Ū�׻��ѳȻ�����(®�پ�)
  real(8), dimension((nm+1)*(nm+1),0:km,0:lm)   :: DifLUMT_Vel
  integer, dimension((nm+1)*(nm+1),0:km)        :: kpivot_Vel     ! �ԥܥå�
 ! Crank Nicholson ��Ū�׻��ѳȻ�����(����)
  real(8), dimension((nm+1)*(nm+1),0:km,0:lm)   :: DifLUMT_Temp
  integer, dimension((nm+1)*(nm+1),0:km)        :: kpivot_Temp    ! �ԥܥå�

 !---- ʪ���ѥ�᥿�� ----
  character(len=2), parameter :: VelBC='RR'     ! ®�ٶ������(RR/RF/FR/FF)
  character(len=2), parameter :: TempBC='DD'    ! ���ٶ������(DD/DN/ND/NN)

  real(8), parameter :: Ra=100.0D0              ! �쥤�꡼��
  real(8), parameter :: Pr=1.0D0                ! �ץ��ɥ��
  real(8), parameter :: Ekman=1.0d-3            ! �����ޥ��

  real(8), parameter :: A=0.1D0                 ! ���ʬ�ۿ���
  real(8), parameter :: Temptop=0.0D0           ! ��������(��ü)
  real(8), parameter :: Tempbottom=1.0D0        ! ��������(��ü)

 !---- ����¾ ----
  real(8), parameter :: pi=3.1415926535897932385D0
  integer :: it=0
  real(8) :: time

 !---- NAMELIST �ѿ� ----
  character(len=100) :: initial_file=''          ! ����ͥǡ����ե�����̾
  real(8)            :: initial_time=0.0         ! �������
  namelist /initial/ initial_file, initial_time  ! ���������ѥ�᥿��

  real(8) :: delta_t=2.0d-4                      ! ������ʬ���
  integer :: nstep=250                           ! ������ʬ���ƥå׿�
  namelist /tint/    delta_t, nstep              ! ������ʬ�ѥ�᥿��

  character(len=100) :: output_file='gami_abcn_bench0.nc'  ! ���ϥե�����̾
  integer :: ndisp=50                            ! ���ϴֳ֥��ƥå׿�
  namelist /output/    output_file, ndisp        ! ��������

 !---------------- ��ɸ�ͤ����� ---------------------
  call wt_Initial(im,jm,km,nm,lm,Ri,Ro)

 !------------------- ������ʬ�ѥ�᥿�� --------------------
  write(6,nml=tint)   ; read(5,nml=tint)   ; write(6,nml=tint)

 !------------------- ��������� ----------------------
  write(6,nml=initial) ; read(5,nml=initial) ; write(6,nml=initial)
  time = initial_time

  if ( initial_file /= '' ) then
     call initial_read_gtool4(initial_file, initial_time, &
                      xyz_VTor, xyz_VPol, xyz_Temp, &
                      wt_DtDVTor1, wt_DtDLaplaVPol1, wt_DtDTemp1 )

  else
     call set_initial_values(xyz_VTor, xyz_VPol, xyz_Temp)
  endif

  wt_Temp = wt_xyz(xyz_Temp)
  wt_VTor = wt_xyz(xyz_VTor) ; wt_VPol = wt_xyz(xyz_VPol)
  
  wt_LaplaVPol = wt_Lapla_wt(wt_VPol)
  call wt_Potential2Vector(xyz_VLon,xyz_VLat,xyz_VRad,wt_VTor,wt_VPol)  

  if ( initial_file == '' ) then
     call TimeDerivetives_noDiffusion( &
          wt_VTor, wt_VPol, wt_LaplaVPol, wt_Temp, wt_Q, &
          xyz_VLon, xyz_VLat, xyz_VRad, xyz_Temp, &
          wt_DtDVTor1, wt_DtDLaplaVPol1, wt_DtDTemp1 )
  endif

  call CNDiffusionMatrixGrid( 1.0D0, delta_t, DifLUMT_Vel, kpivot_Vel )
  call CNDiffusionMatrixGrid( 1/Pr, delta_t, DifLUMT_Temp, kpivot_Temp )

  xyz_Q = 0.0 ; wt_Q = wt_xyz(xyz_Q)    ! Ǯ���ʤ�

 !------------------- ���������� ----------------------
  xy_Tempbndry(:,:,1) = Temptop
  xy_Tempbndry(:,:,2) = Tempbottom

  w_TempBndry(:,1) = w_xy(xy_TempBndry(:,:,1))
  w_TempBndry(:,2) = w_xy(xy_TempBndry(:,:,2))

 !------------------- �������� ----------------------
  write(6,nml=output) ; read(5,nml=output) ; write(6,nml=output)

  call output_gtool4_init
  if ( initial_file == '' ) call output_gtool4    ! ������Ϳ��������ͤϽ���

 !------------------- ������ʬ(Adams-Bashforth ˡ) --------------------
  do it=1,nstep
     time = initial_time + it * delta_t

     ! �Τλ����Ѳ������¸
     wt_DtDVTor0 = wt_DtDVTor1
     wt_DtDLaplaVPol0 = wt_DtDLaplaVPol1
     wt_DtDTemp0   = wt_DtDTemp1

     call TimeDerivetives_noDiffusion( &
         wt_VTor, wt_VPol, wt_LaplaVPol, wt_Temp, wt_Q, &
         xyz_VLon, xyz_VLat, xyz_VRad, xyz_Temp, &
         wt_DtDVTor1, wt_DtDLaplaVPol1, wt_DtDTemp1 )

     ! �ȥ�����®�پ�
     wt_VTor = wt_VTor + delta_t *( 3/2.0*wt_DtDVTor1 - 1/2.0*wt_DtDVTor0 ) &
                           + delta_t/2.0 * wt_Lapla_wt(wt_VTor) 
     call wt_TorBoundariesGrid(wt_VTor,cond=velBC)
     wt_VTor = LUSolve(DifLUMT_Vel,kpivot_Vel,wz_wt(wt_VTor))
     call wt_TorBoundariesGrid(wt_VTor,cond=velBC)

     ! �ݥ�����®�پ�
     wt_LaplaVPol = wt_LaplaVPol &
           + delta_t *(3/2.0*wt_DtDLaplaVPol1 - 1/2.0*wt_DtDLaplaVPol0 ) &
           + delta_t/2.0 * wt_Lapla_wt(wt_LaplaVPol) 
     wt_VPol = wt_LaplaPol2PolGrid_wt(wt_LaplaVPol,cond=velBC)
     wt_LaplaVPol = wt_Lapla_wt(wt_VPol)

     wt_LaplaVPol = LUSolve(DifLUMT_Vel,kpivot_Vel,wz_wt(wt_LaplaVPol))
     wt_VPol = wt_LaplaPol2PolGrid_wt(wt_LaplaVPol,cond=velBC)
     wt_LaplaVPol = wt_Lapla_wt(wt_VPol)

     ! ���پ�
     wt_Temp = wt_Temp + delta_t *( 3/2.0*wt_DtDTemp1 - 1/2.0*wt_DtDTemp0 ) &
                       + delta_t/2.0/Pr * wt_Lapla_wt(wt_Temp) 
     call wt_BoundariesGrid(wt_Temp,w_TempBndry,cond=TempBC)
     wt_Temp = LUSolve(DifLUMT_Temp,kpivot_Temp,wz_wt(wt_Temp))
     call wt_BoundariesGrid(wt_Temp,w_TempBndry,cond=TempBC)

     ! ����å��ͷ׻�
     xyz_Temp = xyz_wt(wt_Temp)
     call wt_Potential2Vector(xyz_VLon,xyz_VLat,xyz_VRad,wt_VTor,wt_VPol)

     if(mod(it,ndisp) .eq. 0)then                    ! ����
        call output_gtool4
     endif
  enddo
  call output_gtool4_close

contains

 !------------------- �����Ѳ��� ----------------------
  subroutine TimeDerivetives_noDiffusion( &
       wt_VTor, wt_VPol, wt_LaplaVPol, wt_Temp, wt_Q, &
       xyz_VLon, xyz_VLat, xyz_VRad, xyz_Temp, &
       wt_DtDVTor, wt_DtDLaplaVPol, wt_DtDTemp )

   ! �ȥ�����®��
    real(8), dimension((nm+1)**2,0:lm), intent(IN) :: wt_VTor
   ! �ݥ�����®��
    real(8), dimension((nm+1)**2,0:lm), intent(IN) :: wt_VPol
   ! ��^2 �ݥ�����®��
    real(8), dimension((nm+1)**2,0:lm), intent(IN) :: wt_LaplaVPol
   ! ����
    real(8), dimension((nm+1)**2,0:lm), intent(IN) :: wt_Temp
   ! Ǯ��
    real(8), dimension((nm+1)**2,0:lm), intent(IN) :: wt_Q

    real(8), dimension(0:im-1,jm,0:km), intent(IN) :: xyz_VLon   ! ®��(����)
    real(8), dimension(0:im-1,jm,0:km), intent(IN) :: xyz_VLat   ! ®��(����)
    real(8), dimension(0:im-1,jm,0:km), intent(IN) :: xyz_VRad   ! ®��(ư��)
    real(8), dimension(0:im-1,jm,0:km), intent(IN) :: xyz_Temp   ! ����

   ! �ȥ�����®�ٻ����Ѳ���
    real(8), dimension((nm+1)**2,0:lm), intent(OUT) :: wt_DtDVTor
   ! �ݥ�����®�ٻ����Ѳ���
    real(8), dimension((nm+1)**2,0:lm), intent(OUT) :: wt_DtDLaplaVPol
   ! ���ٻ����Ѳ���
    real(8), dimension((nm+1)**2,0:lm), intent(OUT) :: wt_DtDTemp

    real(8), dimension(0:im-1,jm,0:km)     :: xyz_VGradV_Lon   ! v����v(����)
    real(8), dimension(0:im-1,jm,0:km)     :: xyz_VGradV_Lat   ! v����v(����)
    real(8), dimension(0:im-1,jm,0:km)     :: xyz_VGradV_Rad   ! v����v(ư��)


   ! v����v ��������׻�
    call wt_VGradV(xyz_VGradV_Lon,xyz_VGradV_Lat,xyz_VGradV_Rad, &
                   xyz_VLon,xyz_VLat,xyz_VRad )

  ! �ȥ���������Ѳ�
    wt_DtDVTor = &
           wt_L2Inv_wt(  2/Ekman * wt_KxRGrad_wt(wt_VTor)   &
                       - 2/Ekman * wt_Qoperator_wt(wt_VPol) ) &
         - wt_L2Inv_wt(wt_RadRot_xyz_xyz(xyz_VGradV_Lon,xyz_VGradV_Lat)) 

  ! �ݥ���������Ѳ�
    wt_DtDLaplaVPol = &
         - Ra/(Ekman*RO)*wt_Temp                    &
         + wt_L2Inv_wt(  2/Ekman * wt_KxRGrad_wt(wt_LaplaVPol) &
                       + 2/Ekman * wt_Qoperator_wt(wt_VTor) )  &
         + wt_L2inv_wt(wt_RadRotRot_xyz_xyz_xyz(      &
               xyz_VGradV_Lon,xyz_VGradV_Lat,xyz_VGradV_Rad ))

  ! ���ٻ����Ѳ�
    wt_DtDTemp = &
         - wt_Div_xyz_xyz_xyz(  &
               xyz_VLon*xyz_Temp,xyz_VLat*xyz_Temp,xyz_VRad*xyz_Temp)

  end subroutine TimeDerivetives_noDiffusion

 !------------------- �Ȼ��� ----------------------
  subroutine CNDiffusionMatrixGrid( Diffc, dt, DiffLUMatrix, kpivot )

    real(8), intent(IN)          :: Diffc    ! �Ȼ�����
    real(8), intent(IN)          :: dt       ! ���ֹ�

    ! Crank Nicholson �Ȼ���Ū�׻��ѹ���(1-D dt/2��^2, LU ʬ��)
    real(8), dimension((nm+1)*(nm+1),0:km,0:lm), intent(OUT)  :: DiffLUMatrix
    ! �ԥܥåȾ���
    integer, dimension((nm+1)*(nm+1),0:km), intent(OUT)       :: kpivot

    ! ������ѿ�
    real(8), dimension((nm+1)*(nm+1),0:lm)           :: wt_I

    integer :: l

    DiffLUMatrix = 0.0
    do l=0,lm
       wt_I = 0.0 ; wt_I(:,l) = 1.0             ! ���ȿ���ʬ��Ω
       DiffLUMatrix(:,:,l) = - Diffc * dt/2.0 * wz_wt(wt_Lapla_wt(wt_I))
    enddo

    DiffLUMatrix(:,0,:) = 0.0D0 ; DiffLUMatrix(:,km,:) = 0.0D0 ! �����ͤ���¸

    do l=0,lm
       wt_I = 0.0 ; wt_I(:,l) = 1.0             ! ���ȿ���ʬ��Ω
       DiffLUMatrix(:,:,l) = wz_wt(wt_I) + DiffLUMatrix(:,:,l)
    enddo
    call ludecomp(DiffLUMatrix,kpivot)

  end subroutine CNDiffusionMatrixGrid

 !------------------- ���������(�ե�����ʤ�) ----------------------
  subroutine set_initial_values(xyz_VTor, xyz_VPol, xyz_Temp)

    real(8), dimension(0:im-1,jm,0:km), intent(out) :: xyz_VTor !�ȥ�����®��
    real(8), dimension(0:im-1,jm,0:km), intent(out) :: xyz_VPol !�ݥ�����®��
    real(8), dimension(0:im-1,jm,0:km), intent(out) :: xyz_Temp !����

    real(8), dimension(0:im-1,jm,0:km)     :: xyz_X             ! x=2r-r_i-r_o

    xyz_X = 2*xyz_Rad - Ri - Ro

    xyz_Temp =  Ro*Ri/xyz_Rad - Ri &
       + 210*A/sqrt(17920*pi) * (1 - 3*xyz_X**2 + 3*xyz_X**4 -xyz_X**6) &
           * sin(pi/2-xyz_Lat)**4 * cos(4*xyz_Lon)

    xyz_VTor =  0     ! �Ż߾���
    xyz_VPol =  0     ! �Ż߾���

  end subroutine set_initial_values

 !------------------- ���������(�ե����뤫��) ----------------------  
  subroutine initial_read_gtool4( filename, time, &
                      xyz_VTor, xyz_VPol, xyz_Temp, &
                      wt_DtDVTor, wt_DtDLaplaVPol, wt_DtDTemp )

    character(len=*), intent(in)   :: filename
    real(8), intent(in)            :: time
    real(8), dimension(0:im-1,jm,0:km), intent(out) :: xyz_VTor !�ȥ�����®��
    real(8), dimension(0:im-1,jm,0:km), intent(out) :: xyz_VPol !�ݥ�����®��
    real(8), dimension(0:im-1,jm,0:km), intent(out) :: xyz_Temp ! ����

 ! �ȥ�����®�ٻ����Ѳ� 
    real(8), dimension((nm+1)**2,0:lm), intent(out) :: wt_DtDVTor
 ! ��^2�ݥ�����®�ٻ����Ѳ�
    real(8), dimension((nm+1)**2,0:lm), intent(out) :: wt_DtDLaplaVPol
 ! ���ٻ����Ѳ�
    real(8), dimension((nm+1)**2,0:lm), intent(out) :: wt_DtDTemp

 ! �ɤ߹���������
    real(8), dimension(0:im-1,jm,0:km) :: xyz_DtDVar

    character(len=100) :: varname, ctime

    write(ctime,*) time

    varname = trim(filename)//'@vtor,t='//trim(adjustl(ctime))
    call LoadVariable(varname, xyz_VTor, im*jm*(km+1))

    varname = trim(filename)//'@vpol,t='//trim(adjustl(ctime))
    call LoadVariable(varname, xyz_VPol, im*jm*(km+1))

    varname = trim(filename)//'@temp,t='//trim(adjustl(ctime))
    call LoadVariable(varname, xyz_Temp,   im*jm*(km+1))

    varname = trim(filename)//'@DtDVTor,t='//trim(adjustl(ctime))
    call LoadVariable(varname, xyz_DtDVar, im*jm*(km+1))
    wt_DtDVTor      = wt_xyz(xyz_DtDVar)

    varname = trim(filename)//'@DtDLaplaVPol,t='//trim(adjustl(ctime))
    call LoadVariable(varname, xyz_DtDVar, im*jm*(km+1))
    wt_DtDLaplaVPol = wt_xyz(xyz_DtDVar)

    varname = trim(filename)//'@DtDTemp,t='//trim(adjustl(ctime))
    call LoadVariable(varname, xyz_DtDVar, im*jm*(km+1))
    wt_DtDTemp      = wt_xyz(xyz_DtDVar)

  end subroutine initial_read_gtool4

 !------------------- ���� ----------------------
  subroutine output_gtool4_init

    call HistoryCreate( &                                  ! �ҥ��ȥ꡼����
           file=trim(output_file), &
           title='Benchmark of dynamo model (case0)', &
           source='Sample program of spmodel library', &
           institution='GFD_Dennou Club SPMODEL project',     &
           dims=(/'lon','lat','rad','t  '/), dimsizes=(/im,jm,km+1,0/),&
           longnames=(/'Longitude','Latitude ','Radius   ','time     '/),&
           units=(/'1','1','1','1'/),   &
           origin=real(time), interval=real(ndisp*delta_t), &
           xtypes=(/'real'/))

    call HistoryPut('lon',x_Lon/pi*180)                       ! �ѿ�����
    call HistoryAddattr('lon','topology','circular')          ! ����°��
    call HistoryAddattr('lon','modulo',360.0)                 ! ����°��
    call HistoryPut('lat',y_Lat/pi*180)                       ! �ѿ�����
    call HistoryPut('rad',z_Rad)                              ! �ѿ�����

    call HistoryAddVariable( &                                ! �ѿ����
           varname='vtor', dims=(/'lon','lat','rad','t  '/), & 
           longname='toroidal velocity potential', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='vpol', dims=(/'lon','lat','rad','t  '/), & 
           longname='poloidal velocity potential', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='temp', dims=(/'lon','lat','rad','t  '/), & 
           longname='temperature', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='vlon', dims=(/'lon','lat','rad','t  '/), & 
           longname='lon-velocity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='vlat', dims=(/'lon','lat','rad','t  '/), & 
           longname='lat-velocity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='vrad', dims=(/'lon','lat','rad','t  '/), & 
           longname='rad-velocity', units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='DtDVTor', dims=(/'lon','lat','rad','t  '/), & 
           longname='time variation of velocity toroidal potential', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='DtDLaplaVPol', dims=(/'lon','lat','rad','t  '/), & 
           longname='time variation of laplacian of velocity poloidal potential', &
           units='1', xtype='double')
    call HistoryAddVariable( &                                ! �ѿ����
           varname='DtDTemp', dims=(/'lon','lat','rad','t  '/), & 
           longname='time variation of temperature', &
           units='1', xtype='double')

   call HistoryAddVariable( &                                ! �ѿ����
           varname='ek', dims=(/'t  '/), & 
           longname='kinetic energy', units='1', xtype='double')
   call HistoryAddVariable( &                                ! �ѿ����
           varname='um', dims=(/'lat','rad','t  '/), & 
           longname='zonal flow', units='1', xtype='double')
   end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it, '  time = ', time
    call HistoryPut('t',real(time))
    call HistoryPut('vtor',xyz_wt(wt_VTor))
    call HistoryPut('vpol',xyz_wt(wt_VPol))
    call HistoryPut('temp',xyz_Temp)
    call HistoryPut('vlon',xyz_VLon)
    call HistoryPut('vlat',xyz_VLat)
    call HistoryPut('vrad',xyz_VRad)
    call HistoryPut('DtDVTor',xyz_wt(wt_DtDVTor1))
    call HistoryPut('DtDLaplaVPol',xyz_wt(wt_DtDLaplaVPol1))
    call HistoryPut('DtDTemp',xyz_wt(wt_DtDTemp1))

    call HistoryPut('ek',AvrLonLatRad_xyz((xyz_VLon**2+xyz_VLat**2+xyz_VRad**2)/2))
    call HistoryPut('um',yz_avrLon_xyz(xyz_VLon))
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose
  end subroutine output_gtool4_close

  subroutine LoadVariable(var_uri, array, length)
    use gt4f90io
    ! �ѿ���ؼ����� URI
    character(len = *), intent(in):: var_uri
    ! ����Ȥ���Ĺ��
    integer, intent(in):: length
    real(8), intent(out):: array(length)
    type(GT_VARIABLE):: var
    continue

    call open(var, var_uri)
    call get(var, array, length)
    call close(var)
  end subroutine LoadVariable

end program gami_abcn_bench0
