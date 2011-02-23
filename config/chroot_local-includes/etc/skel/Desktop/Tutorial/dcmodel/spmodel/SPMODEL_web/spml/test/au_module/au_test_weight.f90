!----------------------------------------------------------------------
!     Copyright (c) 2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  au_module テストプログラム
!
!履歴  2007/12/28  竹広真一  
!      
!
program au_test_weight

  use dc_message, only : MessageNotify
  use au_module
  implicit none
  integer, parameter :: im=32, km=32
  real(8), parameter :: ra=2.0
  real(8), parameter :: eps=1.0D-6

  real(8), dimension(0:im) :: g_z

  integer :: i

  write(6,*)'++++++ Testing g_R_Weight +++++'

do i=1,2
  call au_initial(im,km/i,ra,(/1/))

!!$  write(6,*) 'R'
!!$  write(6,*) g_R
!!$  write(6,*) 
!!$  write(6,*) 'R weight'
!!$  write(6,*) g_R_weight

  g_z(:) = 1
  write(6,*) 
  write(6,*) 'z = 1, int_0^a z dr = ', ra
  write(6,*) 'Calculated value is   ', sum(g_z*g_R_weight)
  if ( abs(sum(g_z*g_R_weight)-2) > eps )then
     call MessageNotify('E','au_test_weight', &
          'Integration error too large.')
  endif

  g_z(:) = g_R**2
  write(6,*) 
  write(6,*) 'z = R**2, int_0^a z dx = ', ra**3/3d0
  write(6,*) 'Calculated value is      ',sum(g_z*g_R_weight)
  if ( abs(sum(g_z*g_R_weight)-ra**3/3.0d0) > eps )then
     call MessageNotify('E','au_test_weight', &
          'Integration error too large.')
  endif

  write(6,*) 
  write(6,*) 'z = R**3, int_0^2 z dx = ', ra**4/4.0
  g_z(:) = g_R**3
  write(6,*) 'Calculated value is             ',sum(g_z*g_R_weight)
  if ( abs(sum(g_z*g_R_weight)-ra**4/4.0) > eps )then
     call MessageNotify('E','au_test_weight', &
          'Integration error too large.')
  endif

end do

  call MessageNotify('M','au_test_weight', &
       'Test of g_R_Weight suceeded!')

end program au_test_weight



