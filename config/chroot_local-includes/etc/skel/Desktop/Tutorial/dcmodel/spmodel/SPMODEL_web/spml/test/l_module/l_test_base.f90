!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  l_module テストプログラム :: 基本変換関数のテスト
!
!履歴  2008/12/23  竹広真一
!
program l_test_base

  use dc_message, only : MessageNotify
  use l_module
  implicit none

  integer, parameter :: jm=16, nm=10

  real(8), dimension(0:nm) :: l_data
  real(8), dimension(1:jm) :: y_data
  real(8), parameter       :: eps = 1.0D-10

  call MessageNotify('M','l_test_base', &
                         'l_module basic functions tests') 

  call l_Initial( nm, jm )

  !---- P_1 のテスト ----
  y_data = sqrt(3.0D0)*sin(y_Lat)
  l_data= 0.0D0 ; l_data(1)=1.0D0

  if ( maxval(abs(l_y(y_data)-l_data)) > eps ) then
     call MessageNotify('E','Test of P_1','Spectral transform error too large') 
  endif
  if ( maxval(abs(y_l(l_y(y_data))-y_data)) > eps ) then
     call MessageNotify('E','Test of P_1',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of P_1','Test of P_1 succeeded!') 

  !---- Y_2^0 のテスト ----
  y_data = sqrt(5.0D0)*(3.0/2*sin(y_Lat)**2-1/2.0)                 ! P_2
  l_data= 0.0D0 ; l_data(2)=1.0D0

  if ( maxval(abs(l_y(y_data)-l_data)) > eps ) then
     call MessageNotify('E','Test of P_2','Spectral transform error too large') 
  endif
  if ( maxval(abs(y_l(l_y(y_data))-y_data)) > eps ) then
     call MessageNotify('E','Test of P_2^0',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of P_2','Test of P_2 succeeded!') 

  !---- 一般的関数のテスト ----
  y_data = (sin(y_Lat)-1)**2*(sin(y_Lat)-0.5)*(sin(y_Lat)+1)

  if ( maxval(abs(y_l(l_y(y_data))-y_data)) > eps ) then
     write(6,*) maxval(abs(y_l(l_y(y_data))-y_data))
     call MessageNotify('E','Test of general function',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of genral function', &
       'Test of general function succeeded!') 

end program l_test_base
