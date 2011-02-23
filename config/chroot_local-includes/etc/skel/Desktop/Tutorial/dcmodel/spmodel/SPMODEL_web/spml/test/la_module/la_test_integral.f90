!----------------------------------------------------------------------
!     Copyright (c) 2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  la_module �ƥ��ȥץ���� :: ��ʬ��ʿ�Ѵؿ��Υƥ���
!
!����  2008/12/25  �ݹ�����
!
program la_test_integral

  use dc_message, only : MessageNotify
  use la_module
  use dc_trace, only : SetDebug
  implicit none

  integer, parameter :: jm=64, nm=42, km=2

  real(8), dimension(1:jm,km)        ::  ya_data            ! ���δؿ�
  real(8), dimension(km)             ::  a_idata            ! ��ʬ��ʿ�Ѥ�����
  real(8), parameter                 ::  eps = 1.0D-10
  real(8) :: pi

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','la_test_integral', &
                         'la_module integral/average function tests') 

  call la_Initial( nm, jm )

  !---- ��ʬ�Υƥ��� ----
  ya_data(:,1) = pi*sin(y_Lat)**2
  ya_data(:,2) = pi*sin(y_Lat)**4

  a_idata(1) = 2*pi/3       ! a_IntLat_ya
  a_idata(2) = 2*pi/5       ! a_IntLat_ya

  if ( maxval(abs(a_IntLat_ya(ya_data)-a_idata)) > eps ) then
     call MessageNotify('E','Test of a_IntLat_ya','Error too large') 
  endif
  call MessageNotify('M','Test of a_IntLat_ya','Test succeeded!') 

  !---- ʿ�ѤΥƥ��� ----
  ya_data(:,1) = 0.5*sin(y_Lat)**2
  ya_data(:,2) = 0.5*sin(y_Lat)**4

  a_idata(1) = 1.0D0/6       ! a_AvrLonLat_ya
  a_idata(2) = 1.0D0/10      ! a_AvrLonLat_ya

  if ( maxval(abs(a_AvrLat_ya(ya_data)-a_idata)) > eps ) then
     call MessageNotify('E','Test of a_AvrLat_ya','Error too large') 
  endif
  call MessageNotify('M','Test of a_AvrLat_ya','Test succeeded!') 

  call MessageNotify('M','la_test_integral', &
                     'la_module integral/average function tests succeded!') 

end program la_test_integral
