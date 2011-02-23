!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  l_module テストプログラム :: 積分・平均関数のテスト
!
!履歴  2008/12/23  竹広真一
!
program l_test_integral

  use dc_message, only : MessageNotify
  use l_module
  implicit none

  integer, parameter :: jm=16, nm=10

  real(8), dimension(1:jm)        ::  y_idata            ! 積分・平均の正解
  real(8)                         ::  idata              ! 積分・平均の正解
  real(8), parameter              ::  eps = 1.0D-10
  real(8) :: pi

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','l_test_integral', &
                         'l_module tests of integral/average functions') 

  call l_Initial( nm, jm )

  !---- 積分のテスト ----
  y_idata = pi*sin(y_Lat)
  idata   = 0.0D0                   ! IntLat_y

  if ( abs(IntLat_y(y_idata)-idata) > eps ) then
     call MessageNotify('E','Test of IntLat_y for an odd function','Error too large') 
  endif
  call MessageNotify('M','Test of IntLat_y for an odd function','Test succeeded!') 

  y_idata = pi*sin(y_Lat)**2      
  idata   = 2*pi/3                  ! IntLat_y

  if ( abs(IntLat_y(y_idata)-idata) > eps ) then
     call MessageNotify('E','Test of IntLat_y for an even function','Error too large') 
  endif
  call MessageNotify('M','Test of IntLat_y for an even function','Test succeeded!') 

  !---- 平均のテスト ----
  y_idata = 0.5*sin(y_Lat)
  idata   = 0.0D0                   ! AvrLat_y

  if ( abs(AvrLat_y(y_idata)-idata) > eps ) then
     call MessageNotify('E','Test of AvrLat_y for an odd function','Error too large') 
  endif
  call MessageNotify('M','Test of AvrLat_y for an odd function','Test succeeded!') 

  y_idata = 0.5*sin(y_Lat)**2
  idata   = 1.0D0/6                 ! AvrLat_y

  if ( abs(AvrLat_y(y_idata)-idata) > eps ) then
     call MessageNotify('E','Test of AvrLat_y for an even function','Error too large') 
  endif
  call MessageNotify('M','Test of AvrLat_y for an even function','Test succeeded!') 

  call MessageNotify('M','l_test_integral', &
                         'l_module tests of integral/average functions succeeded') 

end program l_test_integral
