!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!É½Âê  wa_deriv_mpi_module ¥Æ¥¹¥È¥×¥í¥°¥é¥à :: ÈùÊ¬´Ø¿ô¤Î¥Æ¥¹¥È
!
!ÍúÎò  2008/05/28  ÃÝ¹­¿¿°ì
!
program wa_deriv_mpi_test

  use dc_message, only : MessageNotify
  use wa_mpi_module
  implicit none
  include 'mpif.h'

  integer, parameter :: im=32, jm=16, nm=10, km=2

  real(8), allocatable  ::  xva_data1(:,:,:)              ! ¸µ¤Î´Ø¿ô
  real(8), allocatable  ::  xva_data2(:,:,:)              ! ¸µ¤Î´Ø¿ô
  real(8), allocatable  ::  xva_ddata(:,:,:)              ! ÈùÊ¬¤ÎÀµ²ò
  real(8), allocatable  ::  xv_mu(:,:)                    ! ¦Ì=sin¦Õ
  real(8), parameter    ::  eps = 1.0D-10
  integer :: iproc, np, ierr


 !---------------- MPI ¥¹¥¿¡¼¥È ---------------------
  call MPI_INIT(IERR)
  call MPI_COMM_RANK(MPI_COMM_WORLD,IPROC,IERR)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

  call MessageNotify('M','wa_deriv_mpi_test', &
                         'wa_deriv_mpi_module function tests') 

  call wa_mpi_Initial( nm, im, jm, km )

  allocate(xva_data1(0:im-1,jc,km))
  allocate(xva_data2(0:im-1,jc,km))
  allocate(xva_ddata(0:im-1,jc,km))
  allocate(xv_mu(0:im-1,jc))

  !---- Y_1^-1 Y_2^1 ¤Î¥Æ¥¹¥È ----
  xva_data1(:,:,1) = -cos(xv_Lat)*sin(xv_Lon)             ! Y_1^{-1}
  xva_data1(:,:,2)  = sin(xv_Lat)*cos(xv_Lat) * cos(xv_Lon) ! Y_2^1

  xva_ddata(:,:,1) = 2*cos(xv_Lat)*sin(xv_Lon)             ! wa_Lapla_wa
  xva_ddata(:,:,2) = -6*sin(xv_Lat)*cos(xv_Lat) * cos(xv_Lon) ! Y_2^1

  if ( maxval(abs(xva_wa(wa_Lapla_wa(wa_xva(xva_data1)))-xva_ddata)) > eps ) then
     call MessageNotify('E','Test of wa_Lapla_wa',&
                        'Y_1^-1, Y^2_1 Laplacian error too large') 
  endif
  call MessageNotify('M','Test of wa_Lapla_wa', &
                         'Test of Laplacian Y_1^-1,Y_2^1 succeeded!') 

  xva_ddata(:,:,1) = 1.0/2.0*cos(xv_Lat)*sin(xv_Lon)      ! wa_LaplaInv_wa
  xva_ddata(:,:,2) = -1.0D0/6.0*sin(xv_Lat)*cos(xv_Lat) * cos(xv_Lon)

  if ( maxval(abs(xva_wa(wa_LaplaInv_wa(wa_xva(xva_data1)))-xva_ddata)) > eps ) then
     call MessageNotify('E','Test of wa_LaplaInv_wa',&
                        'Y_1^-1 Y_2^1 Inverse Laplacian error too large') 
  endif
  call MessageNotify('M','Test of wa_LaplaInv_wa', &
                         'Test of Inverse Laplacian Y_1^-1 Y_2^1 succeeded!') 

  xva_ddata(:,:,1) = -cos(xv_Lat)*cos(xv_Lon)         ! wa_DLon_wa
  xva_ddata(:,:,2) = -sin(xv_Lat)*cos(xv_Lat) * sin(xv_Lon)

  if ( maxval(abs(xva_wa(wa_DLon_wa(wa_xva(xva_data1)))-xva_ddata)) > eps ) then
     call MessageNotify('E','Test of wa_DLon_wa',&
                        'Y_1^-1 Y_2^1 Longitudinal derivative error too large') 
  endif
  call MessageNotify('M','Test of wa_DLon_wa', &
                         'Test of DLon Y_1^-1 Y_2^1 succeeded!') 

  xva_ddata(:,:,1) = -cos(xv_Lon)                     ! xva_GradLon_wa
  xva_ddata(:,:,2) = -sin(xv_Lat) * sin(xv_Lon)
  if ( maxval(abs(xva_GradLon_wa(wa_xva(xva_data1))-xva_ddata)) > eps ) then
     call MessageNotify('E','Test of xva_GradLon_wa',&
                        'Y_1^-1 Y_2^1 Longitudinal gradient error too large') 
  endif
  call MessageNotify('M','Test of xva_GradLon_wa', &
                         'Test of GradLon Y_1^-1 Y_2^1 succeeded!') 

  xva_ddata(:,:,1) = sin(xv_Lat)*sin(xv_Lon)         ! xva_GradLat_wa
  xva_ddata(:,:,2) = cos(2*xv_Lat) * cos(xv_Lon)
  if ( maxval(abs(xva_GradLat_wa(wa_xva(xva_data1))-xva_ddata)) > eps ) then
     write(6,*) maxval(abs(xva_GradLat_wa(wa_xva(xva_data1))-xva_ddata))
     call MessageNotify('E','Test of xva_GradLat_wa',&
                        'Y_1^-1 Y_2^1 Latitudinal gradient error too large') 
  endif
  call MessageNotify('M','Test of xva_GradLat_wa', &
                         'Test of GradLat Y_1^-1 Y_2^1 succeeded!') 

  !---- Y_1^-1 cos¦Õ, Y_2^1 cos¦Õ ¤Î¥Æ¥¹¥È ----
  xva_data1(:,:,1) = -cos(xv_Lat)**2*sin(xv_Lon)              ! Y_1^-1 cos¦Õ
  xva_data1(:,:,2) = sin(xv_Lat)*cos(xv_Lat)**2 * cos(xv_Lon) ! Y_2^1 cos¦Õ

  xva_ddata(:,:,1) = -cos(xv_Lat)*cos(xv_Lon)                ! wa_DivLon_xv
  xva_ddata(:,:,2) = -sin(xv_Lat)*cos(xv_Lat)*sin(xv_Lon)    ! wa_DivLon_xv
  if ( maxval(abs(xva_wa(wa_DivLon_xva(xva_data1))-xva_ddata)) > eps ) then
     call MessageNotify('E','Test of wa_DivLon_xv',&
          'Y_1^-1 cos¦Õ, Y_2^1 cos¦Õ Longitudinal divergence error too large') 
  endif
  call MessageNotify('M','Test of xva_DivLon_wa', &
                         'Test of DivLon Y_1^-1 cos¦Õ,Y_2^1 cos¦È succeeded!') 

  xva_ddata(:,:,1) = 3*sin(xv_Lat)*cos(xv_Lat)*sin(xv_Lon)        !wa_DivLat_wa
  xva_ddata(:,:,2) = cos(xv_Lat)*(1-4*sin(xv_Lat)**2)*cos(xv_Lon) !wa_DivLat_wa
  if ( maxval(abs(xva_wa(wa_DivLat_xva(xva_data1))-xva_ddata)) > eps ) then
     call MessageNotify('E','Test of wa_DivLat_xv',&
            'Y_1^-1 cos¦Õ, Y_2^1 cos¦Õ Latitudinal divergence error too large') 
  endif
  call MessageNotify('M','Test of wa_DivLat_xv', &
                       'Test of DivLat Y_1^-1 cos¦Õ, Y_2^1 cos¦È succeeded!') 

  !---- Jacobian ¤Î¥Æ¥¹¥È ----
  xva_data1(:,:,1) = -cos(xv_Lat)*sin(xv_Lon)                ! Y_1^{-1}
  xva_data1(:,:,2) = -cos(xv_Lat)*sin(xv_Lon)                ! Y_1^{-1}
  xva_data2(:,:,1) = -cos(xv_Lat)*sin(xv_Lon)                ! Y_1^{-1}
  xva_data2(:,:,2) = sin(xv_Lat)*cos(xv_Lat) * cos(xv_Lon)   ! Y_2^1

  xva_ddata(:,:,1) = 0.0
  xva_ddata(:,:,2) = sin(xv_Lat)**2 - cos(xv_Lat)**2*cos(xv_Lon)**2
  if ( maxval(abs(xva_wa(wa_JacobianMPI_wa_wa(wa_xva(xva_data1),wa_xva(xva_data2))) &
                  -xva_ddata)) > eps ) then
     call MessageNotify('E','Test of wa_JacobianMPI_wa_wa',&
                        'Y_1^-1, Y_2^1 Jacobian error too large') 
  endif
  call MessageNotify('M','Test of wa_JacobianMPI_wa_wa', &
                         'Test of wa_JacobianMPI_wa_wa Y_1^-1, Y_2^1 succeeded!') 


  !============== ÈùÊ¬·×»» (¦Ë,¦ÌºÂÉ¸·ÏÍÑ) ¤Î¥Æ¥¹¥È ==============
  xv_mu = sin(xv_Lat)

  !----- Y_2^0, Y_1^1 ¤Î¥Æ¥¹¥È -----
  xva_data1(:,:,1) = 3*xv_mu**2-1                              ! Y_2^0
  xva_data1(:,:,2) = sqrt(1-xv_mu**2)*cos(xv_Lon)                  ! Y_1^1

  xva_ddata(:,:,1) = 0.0
  xva_ddata(:,:,2) = -sqrt(1-xv_mu**2)*sin(xv_Lon)
  if ( maxval(abs(xva_GradLambda_wa(wa_xva(xva_data1)) -xva_ddata)) > eps ) then
     call MessageNotify('E','Test of GradLambda',&
                        'Y_2^0, Y_1^1 xva_GradLambda_wa error too large') 
  endif
  call MessageNotify('M','Test of xva_GradLambda_wa', &
                         'Test of xva_GradLambda_wa Y_2^0, Y_1^1 succeeded!') 

  xva_ddata(:,:,1) = 6*xv_mu*(1-xv_mu**2)
  xva_ddata(:,:,2) = -xv_mu*sqrt(1-xv_mu**2)*cos(xv_Lon)
  if ( maxval(abs(xva_GradMu_wa(wa_xva(xva_data1)) -xva_ddata)) > eps ) then
     call MessageNotify('E','Test of GradMu',&
                        'Y_2^0, Y_1^1 xva_GradMu_wa error too large') 
  endif
  call MessageNotify('M','Test of xva_GradMu_wa', &
                         'Test of xva_GradMu_wa Y_2^0, Y_1^1 succeeded!') 

  !----- Y_2^0(1-¦Ì^2), Y_1^1 (1-¦Ì^2) ¤Î¥Æ¥¹¥È -----
  xva_data1(:,:,1) = (3*xv_mu**2-1)*(1-xv_mu**2)       ! Y_2^0 (1-¦Ì^2)
  xva_data1(:,:,2) = (1-xv_mu**2)**(3.0/2)*cos(xv_Lon) ! Y_1^1 (1-¦Ì^2)

  xva_ddata(:,:,1) = 0.0
  xva_ddata(:,:,2) = -(1-xv_mu**2)**(1.0d0/2)*sin(xv_Lon)
  if ( maxval(abs(xva_wa(wa_DivLambda_xva(xva_data1)) -xva_ddata)) > eps ) then
     call MessageNotify('E','Test of DivLambda',&
          'Y_2^0 (1-¦Ì^2), Y_1^1 ¢å(1-¦Ì^2) xva_DivLambda_wa error too large') 
  endif
  call MessageNotify('M','Test of xva_DivLambda_wa', &
        'Test of xva_DivLambda_wa Y_2^0 (1-¦Ì^2),Y_1^1 (1-¦Ì^2) succeeded!') 

  xva_ddata(:,:,1) = (2-3*xv_mu**2)*4*xv_mu
  xva_ddata(:,:,2) = -3.0D0*xv_mu*(1-xv_mu**2)**(1.0D0/2)*cos(xv_Lon)
  if ( maxval(abs(xva_wa(wa_DivMu_xva(xva_data1)) -xva_ddata)) > eps ) then
     call MessageNotify('E','Test of DivMu',&
            'Y_2^0 (1-¦Ì^2), Y_1^1 ¢å(1-¦Ì^2) xva_DivMu_wa error too large') 
  endif
  call MessageNotify('M','Test of xva_DivMu_wa', &
         'Test of xva_DivMu_wa Y_2^0 (1-¦Ì^2), Y_1^1 (1-¦Ì^2) succeeded!') 


  call MessageNotify('M','wa_deriv_mpi_test', &
                         'wa_deriv_mpi_module function tests succeeded!') 

 !------ MPI¤Î½ªÎ» ------

  call MPI_FINALIZE(IERR)

end program wa_deriv_mpi_test
