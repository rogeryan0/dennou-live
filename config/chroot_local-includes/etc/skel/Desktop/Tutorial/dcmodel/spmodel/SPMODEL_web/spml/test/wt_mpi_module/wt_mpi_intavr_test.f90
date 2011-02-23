!----------------------------------------------------------------------
!     Copyright (c) 2002-2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_module テストプログラム
!      積分・平均計算
!
!履歴  2002/09/05  竹広真一
!      2007/11/09  竹広真一  エラーメッセージ追加
!      2007/11/11  竹広真一  平均チェック追加
!      2008/06/06  竹広真一  MPI 並列
!
program wt_mpi_intavr_test

  use dc_message, only : MessageNotify
  use wt_mpi_module

  implicit none
  include 'mpif.h'

  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ri=0.5, ro=1.5      ! 内外半径
  real(8), allocatable        :: xvz_Data(:,:,:)

  real(8), allocatable        :: xv_Data(:,:)
  real(8), allocatable        :: vz_Data(:,:)
  real(8), allocatable        :: xz_Data(:,:)
  real(8), allocatable        :: x_Data(:)
  real(8), allocatable        :: v_Data(:)
  real(8), allocatable        :: z_Data(:)

  real(8), parameter  :: pi=3.1415926535897932385D0
  real(8), parameter  :: eps=1D-10

  integer :: iproc, np, ierr

 !---------------- MPI スタート ---------------------
  call MPI_INIT(IERR)
  call MPI_COMM_RANK(MPI_COMM_WORLD,IPROC,IERR)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

  call wt_mpi_Initial(im,jm,km,nm,lm,ri,ro)

  allocate(xvz_Data(0:im-1,jc,0:km))
  allocate(xv_Data(0:im-1,jc))
  allocate(vz_Data(jc,0:km))
  allocate(xz_Data(0:im-1,0:km))
  allocate(x_Data(0:im-1))
  allocate(v_Data(jc))
  allocate(z_Data(0:km))

!=============================== 積分 =================================

  call MessageNotify('M','wt_mpi_intavr_test', &
       'wt_mpi_module integration function tests')

  write( 6,* ) 
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

 ! 定数
  write(6,*) '---- Constat field ----'
  write(6,*) 'Results (calculated,exact)'
  xvz_Data = 1.0

  vz_Data = vz_IntLon_xvz(xvz_Data)
  write(6,*) 'IntLon : ', vz_Data(1,1), 2*pi
  if ( maxval(abs(vz_Data - 2*pi )) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Integration error too large.')
  endif

  xz_Data = xz_IntLat_xvz(xvz_Data)
  write(6,*) 'IntLat : ', xz_Data(1,1), 2.0D0
  if ( maxval(abs(xz_Data - 2.0D0 )) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Integration error too large.')
  endif

  xv_Data = xv_IntRad_xvz(xvz_Data)
  write(6,*) 'IntRad : ', xv_Data(1,1), 1/3.0D0*(ro**3-ri**3)
  if ( maxval(abs(xv_Data - 1/3.0D0*(ro**3-ri**3) )) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Integration error too large.')
  endif

  z_Data = z_IntLonLat_xvz(xvz_Data)
  write(6,*) 'IntLonLat : ', z_Data(1), 4*pi
  write(6,*) maxval(abs(z_Data - 4*pi ))
  if ( maxval(abs(z_Data - 4*pi)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Integration error too large.')
  endif

  x_Data = x_IntLatRad_xvz(xvz_Data)
  write(6,*) 'IntLatRad : ', x_Data(1), 2/3.0D0*(ro**3-ri**3)
  if ( maxval(abs(x_Data - 2/3.0D0*(ro**3-ri**3) )) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Integration error too large.')
  endif

  v_Data = v_IntLonRad_xvz(xvz_Data)
  write(6,*) 'IntLonRad : ', v_Data(1), 2*pi/3.0D0*(ro**3-ri**3)
  if ( maxval(abs(v_Data - 2*pi/3.0D0*(ro**3-ri**3) )) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Integration error too large.')
  endif

  write(6,*) 'IntLonLatRad : ', IntLonLatRad_xvz(xvz_Data), 4*pi/3*(ro**3-ri**3)
  if ( abs(IntLonLatRad_xvz(xvz_Data) - 4*pi/3*(ro**3-ri**3)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Integration error too large.')
  endif

 ! 
  write(6,*)
  write(6,*) '---- r ----'
  write(6,*) 'Results (calculated-exact)'

  xvz_Data = xvz_Rad 

  vz_Data = vz_IntLon_xvz(xvz_Data) - 2*pi* xvz_Rad(1,:,:)
  write(6,*) 'IntLon : ', vz_Data(1,1) 
  if ( maxval(abs(vz_Data)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Integration error too large.')
  endif

  xz_Data = xz_IntLat_xvz(xvz_Data) - 2.0D0 * xvz_Data(:,1,:)
  write(6,*) 'IntLat : ', xz_Data(1,1)
  if ( maxval(abs(xz_Data)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Integration error too large.')
  endif

  xv_Data = xv_IntRad_xvz(xvz_Data) - 1/4.0D0*(ro**4-ri**4) 
  write(6,*) 'IntRad : ', xv_Data(1,1)
  if ( maxval(abs(xv_Data)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Integration error too large.')
  endif

  z_Data = z_IntLonLat_xvz(xvz_Data) - 4*pi * xvz_Data(1,1,:)
  write(6,*) 'IntLonLat : ', z_Data(1)
  if ( maxval(abs(z_Data)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Integration error too large.')
  endif

  x_Data = x_IntLatRad_xvz(xvz_Data) - 1/2.0D0*(ro**4-ri**4)
  write(6,*) 'IntLatRad : ', x_Data(1)
  if ( maxval(abs(x_Data)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Integration error too large.')
  endif

  v_Data = v_IntLonRad_xvz(xvz_Data) - pi/2.0D0*(ro**4-ri**4)
  write(6,*) 'IntLonRad : ', v_Data(1)
  if ( maxval(abs(v_Data)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Integration error too large.')
  endif

  write(6,*) 'IntLonLatRad : ', IntLonLatRad_xvz(xvz_Data) - pi*(ro**4-ri**4)
  if ( abs(IntLonLatRad_xvz(xvz_Data) - pi*(ro**4-ri**4)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Integration error too large.')
  endif

  write(6,*)
  write(6,*) '---- sin(phi)**2  ----'
  write(6,*) 'Results (calculated-exact)'

  xvz_Data = sin(xvz_Lat)**2

  vz_Data = vz_IntLon_xvz(xvz_Data) - 2*pi* xvz_Data(1,:,:)
  write(6,*) 'IntLon : ', vz_Data(1,1) 
  if ( maxval(abs(vz_Data)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Integration error too large.')
  endif

  xz_Data = xz_IntLat_xvz(xvz_Data) - 2.0D0/3.0D0
  write(6,*) 'IntLat : ', xz_Data(1,1)
  if ( maxval(abs(xz_Data)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Integration error too large.')
  endif

  xv_Data = xv_IntRad_xvz(xvz_Data) - 1/3.0D0*(ro**3-ri**3) * xvz_Data(:,:,1)
  write(6,*) 'IntRad : ', xv_Data(1,1)
  if ( maxval(abs(xv_Data)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Integration error too large.')
  endif

  z_Data = z_IntLonLat_xvz(xvz_Data) - 4*pi/3.0
  write(6,*) 'IntLonLat : ', z_Data(1)
  if ( maxval(abs(z_Data)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Integration error too large.')
  endif

  x_Data = x_IntLatRad_xvz(xvz_Data) - 2/9.0D0*(ro**3-ri**3)
  write(6,*) 'IntLatRad : ', x_Data(1)
  if ( maxval(abs(x_Data)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Integration error too large.')
  endif

  v_Data = v_IntLonRad_xvz(xvz_Data) - 2*pi/3.0D0*(ro**3-ri**3) * xvz_Data(1,:,1)

  write(6,*) 'IntLonRad : ', v_Data(1)
  if ( maxval(abs(v_Data)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Integration error too large.')
  endif

  write(6,*) 'IntLonLatRad : ', IntLonLatRad_xvz(xvz_Data) - 4*pi/9*(ro**3-ri**3)
  if ( abs(IntLonLatRad_xvz(xvz_Data) - 4*pi/9*(ro**3-ri**3)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Integration error too large.')
  endif

!=============================== 平均 =================================

  write( 6,* )
  call MessageNotify('M','wt_mpi_intavr_test', &
       'wt_module averaging function tests')

  write( 6,* )
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

 ! 定数
  write(6,*) '---- Constat field ----'
  write(6,*) 'Results (calculated,exact)'
  xvz_Data = 1.0

  vz_Data = vz_AvrLon_xvz(xvz_Data)
  write(6,*) 'AvrLon : ', vz_Data(1,1), 1.0D0
  if ( maxval(abs(vz_Data - 1.0D0 )) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Average error too large.')
  endif

  xz_Data = xz_AvrLat_xvz(xvz_Data)
  write(6,*) 'AvrLat : ', xz_Data(1,1), 1.0D0
  if ( maxval(abs(xz_Data - 1.0D0 )) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Average error too large.')
  endif

  xv_Data = xv_AvrRad_xvz(xvz_Data)
  write(6,*) 'AvrRad : ', xv_Data(1,1), 1.0D0
  if ( maxval(abs(xv_Data - 1.0D0))  > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Average error too large.')
  endif

  z_Data = z_AvrLonLat_xvz(xvz_Data)
  write(6,*) 'AvrLonLat : ', z_Data(1), 1.0D0
  write(6,*) maxval(abs(z_Data - 1.0D0 ))
  if ( maxval(abs(z_Data - 1.0D0)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Average error too large.')
  endif

  x_Data = x_AvrLatRad_xvz(xvz_Data)
  write(6,*) 'AvrLatRad : ', x_Data(1), 1.0D0
  if ( maxval(abs(x_Data - 1.0D0))  > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Average error too large.')
  endif

  v_Data = v_AvrLonRad_xvz(xvz_Data)
  write(6,*) 'AvrLonRad : ', v_Data(1), 1.0D0
  if ( maxval(abs(v_Data - 1.0D0)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Average error too large.')
  endif

  write(6,*) 'AvrLonLatRad : ', AvrLonLatRad_xvz(xvz_Data), 1.0D0
  if ( abs(AvrLonLatRad_xvz(xvz_Data) - 1.0D0) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Average error too large.')
  endif

 ! 
  write(6,*)
  write(6,*) '---- r ----'
  write(6,*) 'Results (calculated-exact)'

  xvz_Data = xvz_Rad 

  vz_Data = vz_AvrLon_xvz(xvz_Data) - xvz_Rad(1,:,:)
  write(6,*) 'AvrLon : ', vz_Data(1,1) 
  if ( maxval(abs(vz_Data)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Average error too large.')
  endif

  xz_Data = xz_AvrLat_xvz(xvz_Data) - xvz_Data(:,1,:)
  write(6,*) 'AvrLat : ', xz_Data(1,1)
  if ( maxval(abs(xz_Data)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Average error too large.')
  endif

  xv_Data = xv_AvrRad_xvz(xvz_Data) &
             - 1.0D0/4.0D0*(ro**4-ri**4)/(1/3.0D0*(ro**3-ri**3))
  write(6,*) 'AvrRad : ', xv_Data(1,1)
  if ( maxval(abs(xv_Data)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Average error too large.')
  endif

  z_Data = z_AvrLonLat_xvz(xvz_Data) - xvz_Data(1,1,:)
  write(6,*) 'AvrLonLat : ', z_Data(1)
  write(6,*) maxval(abs(z_Data))
  if ( maxval(abs(z_Data)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Average error too large.')
  endif

  x_Data = x_AvrLatRad_xvz(xvz_Data) &
            - 1/2.0D0*(ro**4-ri**4)/(2/3.0D0*(ro**3-ri**3))
  write(6,*) 'AvrLatRad : ', x_Data(1)
  if ( maxval(abs(x_Data)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Average error too large.')
  endif

  v_Data = v_AvrLonRad_xvz(xvz_Data) &
       - pi/2.0D0*(ro**4-ri**4)/(2*pi/3.0D0*(ro**3-ri**3))
  write(6,*) 'AvrLonRad : ', v_Data(1)
  if ( maxval(abs(v_Data)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Average error too large.')
  endif

  write(6,*) 'AvrLonLatRad : ', &
       AvrLonLatRad_xvz(xvz_Data) - pi*(ro**4-ri**4)/(4*pi/3*(ro**3-ri**3))
  if ( abs(AvrLonLatRad_xvz(xvz_Data) &
            - pi*(ro**4-ri**4)/(4*pi/3*(ro**3-ri**3))) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Average error too large.')
  endif

  write(6,*)
  write(6,*) '---- sin(phi)**2  ----'
  write(6,*) 'Results (calculated-exact)'

  xvz_Data = sin(xvz_Lat)**2

  vz_Data = vz_AvrLon_xvz(xvz_Data) - xvz_Data(1,:,:)
  write(6,*) 'AvrLon : ', vz_Data(1,1) 
  if ( maxval(abs(vz_Data)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Average error too large.')
  endif

  xz_Data = xz_AvrLat_xvz(xvz_Data) - 1.0D0/3.0D0
  write(6,*) 'AvrLat : ', xz_Data(1,1)
  if ( maxval(abs(xz_Data)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Average error too large.')
  endif

  xv_Data = xv_AvrRad_xvz(xvz_Data) - xvz_Data(:,:,1)
  write(6,*) 'AvrRad : ', xv_Data(1,1)
  if ( maxval(abs(xv_Data)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Average error too large.')
  endif

  z_Data = z_AvrLonLat_xvz(xvz_Data) - 1.0D0/3.0D0
  write(6,*) 'AvrLonLat : ', z_Data(1)
  write(6,*) maxval(abs(z_Data))
  if ( maxval(abs(z_Data)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Average error too large.')
  endif

  x_Data = x_AvrLatRad_xvz(xvz_Data) - 1.0D0/3.0D0
  write(6,*) 'AvrLatRad : ', x_Data(1)
  if ( maxval(abs(x_Data)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Average error too large.')
  endif

  v_Data = v_AvrLonRad_xvz(xvz_Data) - xvz_Data(1,:,1)

  write(6,*) 'AvrLonRad : ', v_Data(1)
  if ( maxval(abs(v_Data)) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Average error too large.')
  endif

  write(6,*) 'AvrLonLatRad : ', AvrLonLatRad_xvz(xvz_Data) - 1.0D0/3D0
  if ( abs(AvrLonLatRad_xvz(xvz_Data) - 1.0D0/3.0D0) > eps ) then
     call MessageNotify('E','wt_mpi_intavr_test', &
          'Average error too large.')
  endif

  call MessageNotify('M','wt_mpi_intavr_test', &
       'wt_module integration/averaging function tests succeeded!')

 !------ MPIの終了 ------

  call MPI_FINALIZE(IERR)

end program wt_mpi_intavr_test
