!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  w_zonal_module �ƥ��ȥץ���� :: �����Ѵ��ؿ��Υƥ���
!
!����  2008/12/28  �ݹ�����
!      2008/06/28  ��������ʿ  �����������ѹ�
!
program w_zonal_test_base

  use dc_message, only : MessageNotify
  use w_zonal_module
  implicit none

  integer, parameter :: im=1, jm=64, nm=42

  real(8), dimension(nm+1)          ::  w_data
  real(8), dimension(0:im-1,1:jm)   ::  xy_data
  real(8), parameter                ::  eps = 1.0D-10
  real(8) :: pi

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','w_zonal_test_base', &
                         'w_zonal_module basic functions tests') 

  call w_Initial( nm, im, jm )

  !---- Y_1^0 �Υƥ��� ----
  xy_data = sqrt(3.0D0)*sin(xy_Lat)                      ! 
  w_data= 0.0D0 ; w_data(l_nm(1,0))=1.0D0

  if ( maxval(abs(w_xy(xy_data)-w_data)) > eps ) then
     call MessageNotify('E','Test of Y_1^0','Spectral transform error too large') 
  endif
  if ( maxval(abs(xy_w(w_xy(xy_data))-xy_data)) > eps ) then
     call MessageNotify('E','Test of Y_1^0',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of Y_1^0','Test of Y_1^0 succeeded!') 

  !---- Y_2^0 �Υƥ��� ----
  xy_data = sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0)                 ! Y_2^0
  w_data= 0.0D0 ; w_data(l_nm(2,0))=1.0D0

  if ( maxval(abs(w_xy(xy_data)-w_data)) > eps ) then
     call MessageNotify('E','Test of Y_2^0','Spectral transform error too large') 
  endif
  if ( maxval(abs(xy_w(w_xy(xy_data))-xy_data)) > eps ) then
     call MessageNotify('E','Test of Y_2^0',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of Y_2^0','Test of Y_2^0 succeeded!') 

  !---- ����Ū�ؿ��Υƥ��� ----
  xy_data = (sin(xy_Lat)-1)**2*(sin(xy_Lat)-0.5)*(sin(xy_Lat)+1)

  if ( maxval(abs(xy_w(w_xy(xy_data))-xy_data)) > eps ) then
     write(6,*) maxval(abs(xy_w(w_xy(xy_data))-xy_data))
     call MessageNotify('E','Test of general function',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of genral function', &
       'Test of general function succeeded!') 

end program w_zonal_test_base
