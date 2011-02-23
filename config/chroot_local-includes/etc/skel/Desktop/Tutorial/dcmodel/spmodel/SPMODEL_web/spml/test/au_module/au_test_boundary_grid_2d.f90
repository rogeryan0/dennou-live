!----------------------------------------------------------------------
!     Copyright (c) 2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  au_module テストプログラム
!      1 次元境界値問題
!
!履歴  2007/12/28  竹広真一
!
program au_test_boundary_grid_2d

  use dc_message, only : MessageNotify
  use au_module
  implicit none

  integer, parameter :: im=8, km=8, nm=2
  real(8), parameter :: ra=2.0

  real(8), dimension(nm,0:im) :: ag_a
  real(8), dimension(nm,0:im) :: ag_dadx
  real(8), dimension(nm,0:km) :: au_a
  real(8), dimension(nm)      :: value

  real(8), parameter    :: eps=1.0D-10                 ! 許容誤差

  value(:) = (/0,1/)

  call au_initial(im,km,ra,(/1,2/))

  ag_a(1,:) = sin(g_R)
  ag_a(2,:) = cos(g_R)
  ag_a(1,:) = g_R
  ag_a(2,:) = g_R**2
  au_a = au_ag(ag_a)

!----- au_BoundaryGrid_D ------
  write(6,*) 'Test of au_BoundaryGrid_D...'
  call au_BoundaryGrid_D(au_a,value)
  ag_a = ag_au(au_a)
  if ( maxval(abs(ag_a(:,0)-value(:))) > eps )then
     call MessageNotify('E','Test of au_BoundaryGrid_D', &
          'B.C. adoption error too large.')
  endif
  call MessageNotify('M','Test of au_BoundaryGrid_D', &
       'Test of au_BoundaryGrid_D suceeded!')
  

!----- au_BoundaryGrid_N ------
  write(6,*) 'Test of au_BoundaryGrid_N...'
  call au_BoundaryGrid_N(au_a,value)
  ag_a = ag_au(au_a)
  ag_dadx = ag_Dr_au(au_ag(ag_a))
  if ( maxval(abs(ag_dadx(:,0)-value(:))) > eps )then
     call MessageNotify('E','Test of au_BoundaryGrid_N', &
          'B.C. adoption error too large.')
  endif
  call MessageNotify('M','Test of au_BoundaryGrid_N', &
       'Test of au_BoundaryGrid_N suceeded!')

!----- au_BoundaryTau_D ------
  write(6,*) 'Test of au_BoundaryTau_D...'
  call au_BoundaryTau_D(au_a,value)
  ag_a = ag_au(au_a)
  if ( maxval(abs(ag_a(:,0)-value(:))) > eps ) then
     call MessageNotify('E','Test of au_BoundaryTau_D', &
          'B.C. adoption error too large.')
  endif
  call MessageNotify('M','Test of au_BoundaryTau_D', &
       'Test of au_BoundaryTau_D suceeded!')
  

!----- au_BoundaryTau_N ------
  write(6,*) 'Test of au_BoundaryTau_N...'
  call au_BoundaryTau_N(au_a,value)
  ag_a = ag_au(au_a)
  ag_dadx = ag_Dr_au(au_ag(ag_a))
  if ( maxval(abs(ag_dadx(:,0)-value(:)))> eps )then
     call MessageNotify('E','Test of au_BoundaryTau_N', &
          'B.C. adoption error too large.')
  endif
  call MessageNotify('M','Test of au_BoundaryTau_N', &
       'Test of au_BoundaryTau_N suceeded!')

end program au_test_boundary_grid_2d
