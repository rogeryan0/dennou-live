!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  l_module �ƥ��ȥץ���� :: ��ʬ�ؿ��Υƥ���
!
!����  2008/12/23  �ݹ�����
!
program l_test_deriv

  use dc_message, only : MessageNotify
  use l_module
  implicit none

  integer, parameter :: jm=16, nm=10

  real(8), dimension(1:jm)  ::  y_data1              ! ���δؿ�
  real(8), dimension(1:jm)  ::  y_ddata              ! ��ʬ������
  real(8), dimension(1:jm)  ::  mu                   ! ��=sin��
  real(8), parameter        ::  eps = 1.0D-10

  call MessageNotify('M','l_test_deriv', &
                         'l_module tests of derivative functions') 

  call l_Initial( nm, jm )

  !============== ��ʬ�׻� (�պ�ɸ����) �Υƥ��� ==============

  !---- P_1 �Υƥ��� ----
  y_data1 = sqrt(3.0D0)*sin(y_Lat)         ! P_1

  y_ddata = -2*sqrt(3.0D0)*sin(y_Lat)      ! l_Lapla_l
  if ( maxval(abs(y_l(l_Lapla_l(l_y(y_data1)))-y_ddata)) > eps ) then
     call MessageNotify('E','Test of l_Lapla_l',&
                        'P_1 Laplacian error too large') 
  endif
  call MessageNotify('M','Test of l_Lapla_l', &
                         'Test of Laplacian P_1 succeeded!') 

  y_ddata = -0.5*sqrt(3.0D0)*sin(y_Lat)      ! l_Lapla_l
  if ( maxval(abs(y_l(l_LaplaInv_l(l_y(y_data1)))-y_ddata)) > eps ) then
     call MessageNotify('E','Test of l_LaplaInv_l',&
                        'P_1 Laplacian error too large') 
  endif
  call MessageNotify('M','Test of l_LaplaInv_l', &
                         'Test of Inverse Laplacian P_1 succeeded!') 

  y_ddata = sqrt(3.0D0)*cos(y_Lat)         ! y_GradLat_l
  if ( maxval(abs(y_GradLat_l(l_y(y_data1))-y_ddata)) > eps ) then
     write(6,*) maxval(abs(y_GradLat_l(l_y(y_data1))-y_ddata))
     call MessageNotify('E','Test of y_GradLat_l',&
                        'P_1 Latitudinal gradient error too large') 
  endif
  call MessageNotify('M','Test of y_GradLat_l', &
                         'Test of GradLat P_1 succeeded!') 

  !---- P_1 cos�� �Υƥ��� ----
  y_data1 = sqrt(3.0D0)*sin(y_Lat)*cos(y_Lat)               ! P_1

  y_ddata = sqrt(3.0D0)*(cos(y_Lat)**2-2*sin(y_Lat)**2)     ! p_DivLat_y
  if ( maxval(abs(y_l(l_DivLat_y(y_data1))-y_ddata)) > eps ) then
     call MessageNotify('E','Test of l_DivLat_y',&
                        'P_1 cos phi Latitudinal divergence error too large') 
  endif
  call MessageNotify('M','Test of l_DivLat_y', &
                         'Test of DivLat P_1 cos phi succeeded!') 

  !---- P_2 �Υƥ��� ----
  y_data1 = sqrt(5.0D0)*(3.0/2*sin(y_Lat)**2-1/2.0)       ! P_2

  y_ddata = -6*y_data1                                    ! l_Lapla_l
  if ( maxval(abs(y_l(l_Lapla_l(l_y(y_data1)))-y_ddata)) > eps ) then
     call MessageNotify('E','Test of l_Lapla_l',&
                        'P_2 Laplacian error too large') 
  endif
  call MessageNotify('M','Test of l_Lapla_l', &
                         'Test of Laplacian P_2 succeeded!') 

  y_ddata = -y_data1/6.0D0                                ! l_Lapla_l
  if ( maxval(abs(y_l(l_LaplaInv_l(l_y(y_data1)))-y_ddata)) > eps ) then
     call MessageNotify('E','Test of l_LaplaInv_l',&
                        'P_2 Laplacian error too large') 
  endif
  call MessageNotify('M','Test of l_LaplaInv_l', &
                         'Test of Inverse Laplacian P_2 succeeded!') 

  y_ddata = sqrt(5.0D0)*3.0*sin(y_Lat)*cos(y_Lat)         ! y_GradLat_l
  if ( maxval(abs(y_GradLat_l(l_y(y_data1))-y_ddata)) > eps ) then
     write(6,*) maxval(abs(y_GradLat_l(l_y(y_data1))-y_ddata))
     call MessageNotify('E','Test of y_GradLat_l',&
                        'P_2 Latitudinal gradient error too large') 
  endif
  call MessageNotify('M','Test of y_GradLat_l', &
                         'Test of GradLat P_2 succeeded!') 

  !---- P_2 cos�� �Υƥ��� ----
  y_data1 = sqrt(5.0D0)*(3.0/2*sin(y_Lat)**2-1/2.0)*cos(y_Lat)    ! P_2 cos��

  y_ddata = sqrt(5.0D0)*(3.0*(sin(y_Lat)*cos(y_Lat)**2-sin(y_Lat)**3) &
                         +sin(y_Lat))                             ! l_DivLat_y

  if ( maxval(abs(y_l(l_DivLat_y(y_data1))-y_ddata)) > eps ) then
     call MessageNotify('E','Test of l_DivLat_y',&
                        'P_2 cos phi Latitudinal divergence error too large') 
  endif
  call MessageNotify('M','Test of l_DivLat_y', &
                         'Test of DivLat P_2 cos phi succeeded!') 

  call MessageNotify('M','l_test_deriv', &
                         'l_module tests of derivative functions succeeded.') 

  !============== ��ʬ�׻� (�̺�ɸ����) �Υƥ��� ==============
  mu = sin(y_Lat)

  !----- P_2 �Υƥ��� -----
  y_data1 = 3*mu**2-1                              ! P_2
  y_ddata = 6*mu*(1-mu**2)                         ! (1-��^2)��/�ߦ� 

  if ( maxval(abs(y_GradMu_l(l_y(y_data1)) - y_ddata)) > eps ) then
     call MessageNotify('E','Test of GradMu',&
                        'P_2 y_GradMu_l error too large') 
  endif
  call MessageNotify('M','Test of y_GradMu_l', &
                         'Test of y_GradMu_l P_2 succeeded!') 

  !----- Y_2^0 (1-��^2) �Υƥ��� -----
  y_data1 = (3*mu**2-1)*(1-mu**2)                ! P_2 (1-��^2)
  y_ddata = (2-3*mu**2)*4*mu                     ! ��/�ߦ�

  if ( maxval(abs(y_l(l_DivMu_y(y_data1)) - y_ddata)) > eps ) then
     call MessageNotify('E','Test of DivMu',&
                        'P_2 (1-��^2) y_DivMu_l error too large') 
  endif
  call MessageNotify('M','Test of y_DivMu_l', &
                         'Test of y_DivMu_l P_2 (1-��^2)succeeded!') 


  call MessageNotify('M','w_test_deriv', &
                         'w_deriv_module function tests succeeded!') 

end program l_test_deriv
