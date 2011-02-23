!----------------------------------------------------------------------
!     Copyright (c) 2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  w_deriv_module �ƥ��ȥץ���� :: ��ʬ�ؿ��Υƥ���
!
!����  2007/11/01  �ݹ�����
!      2008/06/28  ��������ʿ  �����������ѹ�
!
program wa_test_deriv

  use dc_message, only : MessageNotify
  use wa_module
  implicit none

  integer, parameter :: im=32, jm=16, nm=10, km=2

  real(8), dimension(0:im-1,1:jm,km)     ::  xya_data1              ! ���δؿ�
  real(8), dimension(0:im-1,1:jm,km)     ::  xya_data2              ! ���δؿ�
  real(8), dimension(0:im-1,1:jm,km)     ::  xya_ddata              ! ��ʬ������
  real(8), dimension(0:im-1,1:jm)        ::  xy_mu                  ! ��=sin��
  real(8), parameter               ::  eps = 1.0D-10

  call MessageNotify('M','wa_test_deriv', &
                         'wa_deriv_module function tests') 

  call wa_Initial( nm, im, jm, km )

  !---- Y_1^-1 Y_2^1 �Υƥ��� ----
  xya_data1(:,:,1) = -cos(xy_Lat)*sin(xy_Lon)             ! Y_1^{-1}
  xya_data1(:,:,2)  = sin(xy_Lat)*cos(xy_Lat) * cos(xy_Lon) ! Y_2^1

  xya_ddata(:,:,1) = 2*cos(xy_Lat)*sin(xy_Lon)             ! wa_Lapla_wa
  xya_ddata(:,:,2) = -6*sin(xy_Lat)*cos(xy_Lat) * cos(xy_Lon) ! Y_2^1

  if ( maxval(abs(xya_wa(wa_Lapla_wa(wa_xya(xya_data1)))-xya_ddata)) > eps ) then
     call MessageNotify('E','Test of wa_Lapla_wa',&
                        'Y_1^-1, Y^2_1 Laplacian error too large') 
  endif
  call MessageNotify('M','Test of wa_Lapla_wa', &
                         'Test of Laplacian Y_1^-1,Y_2^1 succeeded!') 

  xya_ddata(:,:,1) = 1.0/2.0*cos(xy_Lat)*sin(xy_Lon)      ! wa_LaplaInv_wa
  xya_ddata(:,:,2) = -1.0D0/6.0*sin(xy_Lat)*cos(xy_Lat) * cos(xy_Lon)

  if ( maxval(abs(xya_wa(wa_LaplaInv_wa(wa_xya(xya_data1)))-xya_ddata)) > eps ) then
     call MessageNotify('E','Test of wa_LaplaInv_wa',&
                        'Y_1^-1 Y_2^1 Inverse Laplacian error too large') 
  endif
  call MessageNotify('M','Test of wa_LaplaInv_wa', &
                         'Test of Inverse Laplacian Y_1^-1 Y_2^1 succeeded!') 

  xya_ddata(:,:,1) = -cos(xy_Lat)*cos(xy_Lon)         ! wa_DLon_wa
  xya_ddata(:,:,2) = -sin(xy_Lat)*cos(xy_Lat) * sin(xy_Lon)

  if ( maxval(abs(xya_wa(wa_DLon_wa(wa_xya(xya_data1)))-xya_ddata)) > eps ) then
     call MessageNotify('E','Test of wa_DLon_wa',&
                        'Y_1^-1 Y_2^1 Longitudinal derivative error too large') 
  endif
  call MessageNotify('M','Test of wa_DLon_wa', &
                         'Test of DLon Y_1^-1 Y_2^1 succeeded!') 

  xya_ddata(:,:,1) = -cos(xy_Lon)                     ! xya_GradLon_wa
  xya_ddata(:,:,2) = -sin(xy_Lat) * sin(xy_Lon)
  if ( maxval(abs(xya_GradLon_wa(wa_xya(xya_data1))-xya_ddata)) > eps ) then
     call MessageNotify('E','Test of xya_GradLon_wa',&
                        'Y_1^-1 Y_2^1 Longitudinal gradient error too large') 
  endif
  call MessageNotify('M','Test of xya_GradLon_wa', &
                         'Test of GradLon Y_1^-1 Y_2^1 succeeded!') 

  xya_ddata(:,:,1) = sin(xy_Lat)*sin(xy_Lon)         ! xya_GradLat_wa
  xya_ddata(:,:,2) = cos(2*xy_Lat) * cos(xy_Lon)
  if ( maxval(abs(xya_GradLat_wa(wa_xya(xya_data1))-xya_ddata)) > eps ) then
     write(6,*) maxval(abs(xya_GradLat_wa(wa_xya(xya_data1))-xya_ddata))
     call MessageNotify('E','Test of xya_GradLat_wa',&
                        'Y_1^-1 Y_2^1 Latitudinal gradient error too large') 
  endif
  call MessageNotify('M','Test of xya_GradLat_wa', &
                         'Test of GradLat Y_1^-1 Y_2^1 succeeded!') 

  !---- Y_1^-1 cos��, Y_2^1 cos�� �Υƥ��� ----
  xya_data1(:,:,1) = -cos(xy_Lat)**2*sin(xy_Lon)              ! Y_1^-1 cos��
  xya_data1(:,:,2) = sin(xy_Lat)*cos(xy_Lat)**2 * cos(xy_Lon) ! Y_2^1 cos��

  xya_ddata(:,:,1) = -cos(xy_Lat)*cos(xy_Lon)                ! wa_DivLon_xy
  xya_ddata(:,:,2) = -sin(xy_Lat)*cos(xy_Lat)*sin(xy_Lon)    ! wa_DivLon_xy
  if ( maxval(abs(xya_wa(wa_DivLon_xya(xya_data1))-xya_ddata)) > eps ) then
     call MessageNotify('E','Test of wa_DivLon_xy',&
          'Y_1^-1 cos��, Y_2^1 cos�� Longitudinal divergence error too large') 
  endif
  call MessageNotify('M','Test of xya_DivLon_wa', &
                         'Test of DivLon Y_1^-1 cos��,Y_2^1 cos�� succeeded!') 

  xya_ddata(:,:,1) = 3*sin(xy_Lat)*cos(xy_Lat)*sin(xy_Lon)        !wa_DivLat_wa
  xya_ddata(:,:,2) = cos(xy_Lat)*(1-4*sin(xy_Lat)**2)*cos(xy_Lon) !wa_DivLat_wa
  if ( maxval(abs(xya_wa(wa_DivLat_xya(xya_data1))-xya_ddata)) > eps ) then
     call MessageNotify('E','Test of wa_DivLat_xy',&
            'Y_1^-1 cos��, Y_2^1 cos�� Latitudinal divergence error too large') 
  endif
  call MessageNotify('M','Test of wa_DivLat_xy', &
                       'Test of DivLat Y_1^-1 cos��, Y_2^1 cos�� succeeded!') 

  !---- Jacobian �Υƥ��� ----
  xya_data1(:,:,1) = -cos(xy_Lat)*sin(xy_Lon)                ! Y_1^{-1}
  xya_data1(:,:,2) = -cos(xy_Lat)*sin(xy_Lon)                ! Y_1^{-1}
  xya_data2(:,:,1) = -cos(xy_Lat)*sin(xy_Lon)                ! Y_1^{-1}
  xya_data2(:,:,2) = sin(xy_Lat)*cos(xy_Lat) * cos(xy_Lon)   ! Y_2^1

  xya_ddata(:,:,1) = 0.0
  xya_ddata(:,:,2) = sin(xy_Lat)**2 - cos(xy_Lat)**2*cos(xy_Lon)**2
  if ( maxval(abs(xya_wa(wa_Jacobian_wa_wa(wa_xya(xya_data1),wa_xya(xya_data2))) &
                  -xya_ddata)) > eps ) then
     call MessageNotify('E','Test of wa_Jacobian_wa_wa',&
                        'Y_1^-1, Y_2^1 Jacobian error too large') 
  endif
  call MessageNotify('M','Test of wa_Jacobian_wa_wa', &
                         'Test of wa_Jacobian_wa_wa Y_1^-1, Y_2^1 succeeded!') 


  !============== ��ʬ�׻� (��,�̺�ɸ����) �Υƥ��� ==============
  xy_mu = sin(xy_Lat)

  !----- Y_2^0, Y_1^1 �Υƥ��� -----
  xya_data1(:,:,1) = 3*xy_mu**2-1                              ! Y_2^0
  xya_data1(:,:,2) = sqrt(1-xy_mu**2)*cos(xy_Lon)                  ! Y_1^1

  xya_ddata(:,:,1) = 0.0
  xya_ddata(:,:,2) = -sqrt(1-xy_mu**2)*sin(xy_Lon)
  if ( maxval(abs(xya_GradLambda_wa(wa_xya(xya_data1)) -xya_ddata)) > eps ) then
     call MessageNotify('E','Test of GradLambda',&
                        'Y_2^0, Y_1^1 xya_GradLambda_wa error too large') 
  endif
  call MessageNotify('M','Test of xya_GradLambda_wa', &
                         'Test of xya_GradLambda_wa Y_2^0, Y_1^1 succeeded!') 

  xya_ddata(:,:,1) = 6*xy_mu*(1-xy_mu**2)
  xya_ddata(:,:,2) = -xy_mu*sqrt(1-xy_mu**2)*cos(xy_Lon)
  if ( maxval(abs(xya_GradMu_wa(wa_xya(xya_data1)) -xya_ddata)) > eps ) then
     call MessageNotify('E','Test of GradMu',&
                        'Y_2^0, Y_1^1 xya_GradMu_wa error too large') 
  endif
  call MessageNotify('M','Test of xya_GradMu_wa', &
                         'Test of xya_GradMu_wa Y_2^0, Y_1^1 succeeded!') 

  !----- Y_2^0(1-��^2), Y_1^1 (1-��^2) �Υƥ��� -----
  xya_data1(:,:,1) = (3*xy_mu**2-1)*(1-xy_mu**2)       ! Y_2^0 (1-��^2)
  xya_data1(:,:,2) = (1-xy_mu**2)**(3.0/2)*cos(xy_Lon) ! Y_1^1 (1-��^2)

  xya_ddata(:,:,1) = 0.0
  xya_ddata(:,:,2) = -(1-xy_mu**2)**(1.0d0/2)*sin(xy_Lon)
  if ( maxval(abs(xya_wa(wa_DivLambda_xya(xya_data1)) -xya_ddata)) > eps ) then
     call MessageNotify('E','Test of DivLambda',&
          'Y_2^0 (1-��^2), Y_1^1 ��(1-��^2) xya_DivLambda_wa error too large') 
  endif
  call MessageNotify('M','Test of xya_DivLambda_wa', &
        'Test of xya_DivLambda_wa Y_2^0 (1-��^2),Y_1^1 (1-��^2) succeeded!') 

  xya_ddata(:,:,1) = (2-3*xy_mu**2)*4*xy_mu
  xya_ddata(:,:,2) = -3.0D0*xy_mu*(1-xy_mu**2)**(1.0D0/2)*cos(xy_Lon)
  if ( maxval(abs(xya_wa(wa_DivMu_xya(xya_data1)) -xya_ddata)) > eps ) then
     call MessageNotify('E','Test of DivMu',&
            'Y_2^0 (1-��^2), Y_1^1 ��(1-��^2) xya_DivMu_wa error too large') 
  endif
  call MessageNotify('M','Test of xya_DivMu_wa', &
         'Test of xya_DivMu_wa Y_2^0 (1-��^2), Y_1^1 (1-��^2) succeeded!') 


  call MessageNotify('M','wa_test_deriv', &
                         'wa_deriv_module function tests succeeded!') 

end program wa_test_deriv
