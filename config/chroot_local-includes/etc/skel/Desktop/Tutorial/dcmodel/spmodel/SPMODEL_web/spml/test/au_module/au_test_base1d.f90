!----------------------------------------------------------------------
!     COPYRIGHT (c) 2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  au_module テストプログラム
!
!履歴  2007/12/28  竹広真一
!      2008/01/05  竹広真一  g_Dr2_u のテスト追加
!
program au_test_base1d

  use dc_message, only : MessageNotify
  use au_module
  implicit none
  integer, parameter :: im=32, km=32
  real(8), parameter :: ra=2.0
  real(8), parameter :: eps=1.0D-9

  real(8), dimension(0:im) :: g_z
  real(8), dimension(0:km) :: u_z
  real(8), dimension(0:km) :: u_z_sol

  write(6,*)'++++++ Testing basic functions u_g, g_u, t_Dx_u +++++'

  call au_initial(im,km,ra,(/0/))

  g_z = 2*(g_R/ra)**2 - 1
  write(6,*) 'z = T_1(2*g_R**2/a**2 - 1)'
  u_z = u_g(g_z)
  u_z_sol = 0.0 ; u_z_sol(1) = 1.0d0
  write(6,*) 'Max.val of error values is ', maxval(abs(u_z_sol- u_z))
  if ( maxval(abs(u_z_sol-u_z)) > eps )then
     call MessageNotify('E','au_test_base1d', &
          'Transform error too large.')
  endif

  write(6,*) 'g_u(u_g(z)) - z = 0'
  write(6,*) 'Max.val of error values is ', maxval(abs(g_u(u_z)- g_z))
  if ( maxval(abs(g_u(u_z)- g_z)) > eps )then
     call MessageNotify('E','au_test_base1d', &
          'Transform error too large.')
  endif

  write(6,*) 'g_dr_u(u_g(z))) - 4*g_R/a**2 = 0'
  write(6,*) 'Max.val of calculated error are ',&
       maxval(abs(g_Dr_u(u_z)) - 4*g_R/ra**2)
  if ( maxval(abs(g_Dr_u(u_z)) - 4*g_R/ra**2) > eps )then
     call MessageNotify('E','au_test_base1d', &
          'Differentiation error too large.')
  endif

  g_z = 2*(2*g_R**2/ra**2 - 1)**2 - 1 
  write(6,*) 
  write(6,*) 'z = T_2(2*g_R**2/a**2 - 1)'
  u_z = u_g(g_z)
  u_z_sol = 0.0 ;  u_z_sol(2) = 1.0d0
!!$  write(6,*) 'Max.val of error values is ', maxval(abs(u_z_sol- u_z))
  if ( maxval(abs(u_z_sol-u_z)) > eps )then
     call MessageNotify('E','au_test_base1d', &
          'Transform error too large.')
  endif

  write(6,*) 'g_u(u_g(z)) - z = 0'
  write(6,*) 'Max.val of calculated error are ', maxval(abs(g_u(u_z)- g_z))
  if ( maxval(abs(g_u(u_z)- g_z)) > eps )then
     call MessageNotify('E','au_test_base1d', &
          'Transform error too large.')
  endif

  write(6,*) 'g_Dr_u(u_g(z))) - ( 16r/a**2(2*r**2/a**2-1) ) = 0'
  write(6,*) 'Max.val of calculated error are ', &
       maxval(abs(g_Dr_u(u_z) - ( 16*g_R/ra**2*(2*g_R**2/ra**2-1))))
  if ( maxval(abs(g_Dr_u(u_z) - ( 16*g_R/ra**2*(2*g_R**2/ra**2-1)))) > eps )then
     call MessageNotify('E','au_test_base1d', &
          'Differentiation g_Dr_u error too large.')
  endif

  write(6,*) 'g_Dr2_u(u_g(z))) - ( 96r**2/a**4-16/a**2) ) = 0'
  write(6,*) 'Max.val of calculated error are ', &
       maxval(abs(g_Dr2_u(u_z) - ( 96*g_R**2/ra**4-16/ra**2 )))
  if ( maxval(abs(g_Dr2_u(u_z) - ( 96*g_R**2/ra**4-16/ra**2 ))) > eps )then
     call MessageNotify('E','au_test_base1d', &
          'Differentiation error g_Dr2_u too large.')
  endif

  call MessageNotify('M','au_test_base1d', &
       'Tests of basic functions g_u, u_g, g_Dr_u g_Dr2_u suceeded!')

end program au_test_base1d
