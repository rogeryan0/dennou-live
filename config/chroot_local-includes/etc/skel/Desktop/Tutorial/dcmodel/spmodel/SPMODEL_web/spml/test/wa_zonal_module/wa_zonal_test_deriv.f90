!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!É½Âê  wa_zonal_module ¥Æ¥¹¥È¥×¥í¥°¥é¥à :: ÈùÊ¬´Ø¿ô¤Î¥Æ¥¹¥È
!
!ÍúÎò  2008/12/29  ÃÝ¹­¿¿°ì
!
program wa_zonal_test_deriv

  use dc_message, only : MessageNotify
  use wa_zonal_module
  implicit none

  integer, parameter :: im=1, jm=16, nm=10, km=2

  real(8), dimension(0:im-1,1:jm,km)     ::  xya_data1              ! ¸µ¤Î´Ø¿ô
  real(8), dimension(0:im-1,1:jm,km)     ::  xya_data2              ! ¸µ¤Î´Ø¿ô
  real(8), dimension(0:im-1,1:jm,km)     ::  xya_ddata              ! ÈùÊ¬¤ÎÀµ²ò
  real(8), dimension(0:im-1,1:jm)        ::  xy_mu                  ! ¦Ì=sin¦Õ
  real(8), parameter               ::  eps = 1.0D-10

  call MessageNotify('M','wa_test_deriv', &
                         'wa_deriv_module function tests') 

  call wa_Initial( nm, im, jm, km )

  !---- Y_1^-1 Y_2^1 ¤Î¥Æ¥¹¥È ----
  xya_data1(:,:,1) = sqrt(3.0D0)*sin(xy_Lat)                   ! Y_1^0
  xya_data1(:,:,2) = sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0)  ! Y_2^0

  xya_ddata(:,:,1) = -2*sqrt(3.0D0)*sin(xy_Lat)                   ! wa_Lapla_wa
  xya_ddata(:,:,2) = -6*sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0)  ! 

  if ( maxval(abs(xya_wa(wa_Lapla_wa(wa_xya(xya_data1)))-xya_ddata)) > eps ) then
     call MessageNotify('E','Test of wa_Lapla_wa',&
                        'Y_1^0, Y^2_0 Laplacian error too large') 
  endif
  call MessageNotify('M','Test of wa_Lapla_wa', &
                         'Test of Laplacian Y_1^-1,Y_2^1 succeeded!') 

  xya_ddata(:,:,1) = -1.0D0/2.0D0*sqrt(3.0D0)*sin(xy_Lat)        ! wa_Lapla_wa
  xya_ddata(:,:,2) = -1.0D0/6.0D0*sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0)

  if ( maxval(abs(xya_wa(wa_LaplaInv_wa(wa_xya(xya_data1)))-xya_ddata)) > eps ) then
     call MessageNotify('E','Test of wa_LaplaInv_wa',&
                        'Y_1^0 Y_2^0 Inverse Laplacian error too large') 
  endif
  call MessageNotify('M','Test of wa_LaplaInv_wa', &
                         'Test of Inverse Laplacian Y_1^-1 Y_2^1 succeeded!') 

  xya_ddata(:,:,1) = 0.0D0         ! wa_DLon_wa
  xya_ddata(:,:,2) = 0.0D0

  if ( maxval(abs(xya_wa(wa_DLon_wa(wa_xya(xya_data1)))-xya_ddata)) > eps ) then
     call MessageNotify('E','Test of wa_DLon_wa',&
                        'Y_1^0 Y_2^0 Longitudinal derivative error too large') 
  endif
  call MessageNotify('M','Test of wa_DLon_wa', &
                         'Test of DLon Y_1^0 Y_2^0 succeeded!') 

  xya_ddata(:,:,1) = 0.0D0                     ! xya_GradLon_wa
  xya_ddata(:,:,2) = 0.0D0
  if ( maxval(abs(xya_GradLon_wa(wa_xya(xya_data1))-xya_ddata)) > eps ) then
     call MessageNotify('E','Test of xya_GradLon_wa',&
                        'Y_1^0 Y_2^0 Longitudinal gradient error too large') 
  endif
  call MessageNotify('M','Test of xya_GradLon_wa', &
                         'Test of GradLon Y_1^-1 Y_2^1 succeeded!') 

  xya_ddata(:,:,1) = sqrt(3.0D0)*cos(xy_Lat)         ! xya_GradLat_wa
  xya_ddata(:,:,2) = sqrt(5.0D0)*3.0*sin(xy_Lat)*cos(xy_Lat)

  if ( maxval(abs(xya_GradLat_wa(wa_xya(xya_data1))-xya_ddata)) > eps ) then
     write(6,*) maxval(abs(xya_GradLat_wa(wa_xya(xya_data1))-xya_ddata))
     call MessageNotify('E','Test of xya_GradLat_wa',&
                        'Y_1^0 Y_2^0 Latitudinal gradient error too large') 
  endif
  call MessageNotify('M','Test of xya_GradLat_wa', &
                         'Test of GradLat Y_1^0 Y_2^0 succeeded!') 

  !---- Y_1^0 cos¦Õ, Y_2^0 cos¦Õ ¤Î¥Æ¥¹¥È ----
  xya_data1(:,:,1) = sqrt(3.0D0)*sin(xy_Lat)*cos(xy_Lat)        
  ! Y_1^0 cos¦Õ
  xya_data1(:,:,2) = sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0)*cos(xy_Lat)
  ! Y_2^0 cos¦Õ

  xya_ddata(:,:,1) = 0.0D0   ! wa_DivLon_xya
  xya_ddata(:,:,2) = 0.0D0   ! wa_DivLon_xya

  if ( maxval(abs(xya_wa(wa_DivLon_xya(xya_data1))-xya_ddata)) > eps ) then
     call MessageNotify('E','Test of wa_DivLon_xy',&
          'Y_1^0 cos¦Õ, Y_2^0 cos¦Õ Longitudinal divergence error too large') 
  endif
  call MessageNotify('M','Test of xya_DivLon_wa', &
                         'Test of DivLon Y_1^0 cos¦Õ,Y_2^0 cos¦Õ succeeded!') 

  xya_ddata(:,:,1) = sqrt(3.0D0)*(cos(xy_Lat)**2-2*sin(xy_Lat)**2)
  ! wa_DivLat_xy
  xya_ddata(:,:,2) = sqrt(5.0D0) &
       *(3.0*(sin(xy_Lat)*cos(xy_Lat)**2-sin(xy_Lat)**3)+sin(xy_Lat)) 
  ! wa_DivLat_xy

  if ( maxval(abs(xya_wa(wa_DivLat_xya(xya_data1))-xya_ddata)) > eps ) then
     call MessageNotify('E','Test of wa_DivLat_xy',&
            'Y_1^0 cos¦Õ, Y_2^0 cos¦Õ Latitudinal divergence error too large') 
  endif
  call MessageNotify('M','Test of wa_DivLat_xy', &
                       'Test of DivLat Y_1^0 cos¦Õ, Y_2^0 cos¦Õ succeeded!') 

  !---- Jacobian ¤Î¥Æ¥¹¥È ----
  xya_data1(:,:,1) = sin(xy_Lat)                             ! Y_1^0
  xya_data1(:,:,2) = 3.0D0/2.0D0*sin(xy_Lat)**2-1.0D0/2.0D0  ! Y_2^0
  xya_data2(:,:,1) = sin(xy_Lat)                             ! Y_1^0
  xya_data2(:,:,2) = 3.0D0/2.0D0*sin(xy_Lat)**2-1.0D0/2.0D0  ! Y_2^0

  xya_ddata(:,:,1) = 0.0D0
  xya_ddata(:,:,2) = 0.0D0
  if ( maxval(abs(xya_wa(wa_Jacobian_wa_wa(wa_xya(xya_data1),wa_xya(xya_data2))) &
                  -xya_ddata)) > eps ) then
     call MessageNotify('E','Test of wa_Jacobian_wa_wa',&
                        'Y_1^0, Y_2^0 Jacobian error too large') 
  endif
  call MessageNotify('M','Test of wa_Jacobian_wa_wa', &
                         'Test of wa_Jacobian_wa_wa Y_1^0, Y_2^0 succeeded!') 


  !============== ÈùÊ¬·×»» (¦Ë,¦ÌºÂÉ¸·ÏÍÑ) ¤Î¥Æ¥¹¥È ==============
  xy_mu = sin(xy_Lat)

  !----- Y_2^0, Y_1^0 ¤Î¥Æ¥¹¥È -----
  xya_data1(:,:,1) = 3*xy_mu**2-1                                 ! Y_2^0
  xya_data1(:,:,2) = xy_mu                                        ! Y_1^1

  xya_ddata(:,:,1) = 0.0
  xya_ddata(:,:,2) = 0.0D0
  if ( maxval(abs(xya_GradLambda_wa(wa_xya(xya_data1)) -xya_ddata)) > eps ) then
     call MessageNotify('E','Test of GradLambda',&
                        'Y_2^0, Y_1^0 xya_GradLambda_wa error too large') 
  endif
  call MessageNotify('M','Test of xya_GradLambda_wa', &
                         'Test of xya_GradLambda_wa Y_2^0, Y_1^0 succeeded!') 

  xya_ddata(:,:,1) = 6*xy_mu*(1-xy_mu**2)
  xya_ddata(:,:,2) = 1-xy_mu**2
  if ( maxval(abs(xya_GradMu_wa(wa_xya(xya_data1)) -xya_ddata)) > eps ) then
     call MessageNotify('E','Test of GradMu',&
                        'Y_2^0, Y_1^1 xya_GradMu_wa error too large') 
  endif
  call MessageNotify('M','Test of xya_GradMu_wa', &
                         'Test of xya_GradMu_wa Y_2^0, Y_1^1 succeeded!') 

  !----- Y_2^0(1-¦Ì^2), Y_1^0 (1-¦Ì^2) ¤Î¥Æ¥¹¥È -----
  xya_data1(:,:,1) = (3*xy_mu**2-1)*(1-xy_mu**2)       ! Y_2^0 (1-¦Ì^2)
  xya_data1(:,:,2) = xy_mu*(1-xy_mu**2)                ! Y_1^0 (1-¦Ì^2)

  xya_ddata(:,:,1) = 0.0D0
  xya_ddata(:,:,2) = 0.0D0
  if ( maxval(abs(xya_wa(wa_DivLambda_xya(xya_data1)) -xya_ddata)) > eps ) then
     call MessageNotify('E','Test of DivLambda',&
          'Y_2^0 (1-¦Ì^2), Y_1^0 (1-¦Ì^2) xya_DivLambda_wa error too large') 
  endif
  call MessageNotify('M','Test of xya_DivLambda_wa', &
        'Test of xya_DivLambda_wa Y_2^0 (1-¦Ì^2),Y_1^0 (1-¦Ì^2) succeeded!') 

  xya_ddata(:,:,1) = (2-3*xy_mu**2)*4*xy_mu
  xya_ddata(:,:,2) = (1-xy_mu**2) - 2*xy_mu**2
  if ( maxval(abs(xya_wa(wa_DivMu_xya(xya_data1)) -xya_ddata)) > eps ) then
     call MessageNotify('E','Test of DivMu',&
            'Y_2^0 (1-¦Ì^2), Y_1^0 (1-¦Ì^2) xya_DivMu_wa error too large') 
  endif
  call MessageNotify('M','Test of xya_DivMu_wa', &
         'Test of xya_DivMu_wa Y_2^0 (1-¦Ì^2), Y_1^0 (1-¦Ì^2) succeeded!') 


  call MessageNotify('M','wa_zonal_test_deriv', &
       'wa_zonal_module derivative function tests succeeded!') 

end program wa_zonal_test_deriv
