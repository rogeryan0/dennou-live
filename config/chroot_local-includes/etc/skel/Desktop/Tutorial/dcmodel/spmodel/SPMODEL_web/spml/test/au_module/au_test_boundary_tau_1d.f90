!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  au_module テストプログラム
!      1 次元境界値問題
!
!履歴  2007/12/28  竹広真一
!
program au_test_boundary_tau_1d

  use dc_message, only : MessageNotify
  use au_module
  implicit none

  integer, parameter :: im=8, km=6
  real(8), parameter :: ra=1.0
  real(8), parameter :: amin=2.0D0

  real(8), dimension(0:im) :: g_a
  real(8), dimension(0:im) :: g_dadx
  real(8), dimension(0:km) :: u_a
  real(8)                  :: value=amin

  real(8), parameter    :: eps=1.0D-10                 ! 許容誤差

  call au_initial(im,km,ra,(/0/))

  g_a = (g_R-ra)**2 + 1
  u_a = u_g(g_a)

!----- au_BoundaryTau_D ------
  write(6,*) 'Test of au_BoundaryTau_D...'
  call au_BoundaryTau_D(u_a,value)
  g_a = g_u(u_a)
  if ( abs(g_a(0)-amin) > eps )then
     call MessageNotify('E','Test of au_BoundaryTau_D', &
          'B.C. adoption error too large.')
  endif
  call MessageNotify('M','Test of au_BoundaryTau_D', &
       'Test of au_BoundaryTau_D suceeded!')
  

!----- au_BoundaryTau_N ------
  write(6,*) 'Test of au_BoundaryTau_N...'
  call au_BoundaryTau_N(u_a,value)
  g_a = g_u(u_a)
  g_dadx = g_Dr_u(u_g(g_a))
  if ( abs(g_dadx(0)-amin)  > eps )then
     call MessageNotify('E','Test of au_BoundaryTau_N', &
          'B.C. adoption error too large.')
  endif
  call MessageNotify('M','Test of au_BoundaryTau_N', &
       'Test of au_BoundaryTau_N suceeded!')

end program au_test_boundary_tau_1d
