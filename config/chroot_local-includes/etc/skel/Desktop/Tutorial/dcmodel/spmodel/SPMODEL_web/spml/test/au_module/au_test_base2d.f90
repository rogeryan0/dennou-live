!----------------------------------------------------------------------
!     Copyright (c) 2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  au_module テストプログラム
!
!履歴  2007/12/28  竹広真一
!      2008/01/05  竹広真一  ag_Dr2_au のテスト追加
!      
!
program au_test_base2d

  use dc_message, only : MessageNotify
  use au_module
  implicit none
  integer, parameter :: im=32, km=32, nm=4
  real(8), parameter :: ra=0.5
  real(8), parameter :: eps=1.0D-8

  real(8), dimension(0:nm,0:im) :: ag_y
  real(8), dimension(0:nm,0:im) :: ag_y_orig
  real(8), dimension(0:nm,0:im) :: ag_y_deriv
  real(8), dimension(0:nm,0:im) :: ag_y_deriv2

  real(8), dimension(0:nm,0:km) :: au_y
  real(8), dimension(0:nm,0:km) :: au_y_sol

  integer, dimension(0:nm)      :: nd=(/0,1,2,3,4/)

  real(8), dimension(0:im) :: g_x

  integer :: i, m, k

  write(6,*)'++++++ Testing basic functions au_ag, ag_au, ag_Dx_au +++++'

  call au_initial(im,km,ra,nd)

  g_x = 2*g_R**2/ra**2-1
  
  ag_y(0,:) = 1                                  ! T_0
  ag_y(1,:) = g_R*g_x                            ! T_1
  ag_y(2,:) = g_R**2*(2*g_x**2 - 1)              ! T_2
  ag_y(3,:) = g_R**3*(4*g_x**3 - 3*g_x)          ! T_3
  ag_y(4,:) = g_R**4*(8*g_x**4 - 8*g_x**2 + 1)   ! T_4

  ag_y_deriv(0,:) = 0.0D0                
  ag_y_deriv(1,:) = g_x + 4*g_R**2/ra**2 
  ag_y_deriv(2,:) = 2*g_R*(2*g_x**2 - 1)+ g_R**2*4*g_x*4*g_R/ra**2
  ag_y_deriv(3,:) = 3*g_R**2*(4*g_x**3 - 3*g_x)+(12*g_x**2 - 3)*4*g_R**4/ra**2
  ag_y_deriv(4,:) = 4*g_R**3*(8*g_x**4 - 8*g_x**2 + 1) &
                    +g_R**4 * (32*g_x**3 - 16*g_x)*4*g_R/ra**2

  ag_y_deriv2(0,:) = 0.0D0                
  ag_y_deriv2(1,:) = 12*g_R/ra**2
  ag_y_deriv2(2,:) &
           = 2*(2*g_x**2 - 1) + 80*g_R**2*g_x/ra**2 +64*g_R**4/ra**4
  ag_y_deriv2(3,:) = 6*g_R*(4*g_x**3 - 3*g_x) &
                   + 28*g_R**3/ra**2*(12*g_x**2-3)+ (24*g_x)*16*g_R**5/ra**4
  ag_y_deriv2(4,:) = 12*g_R**2*(8*g_x**4 - 8*g_x**2 + 1) &
       + 36*g_R**4/ra**2*(32*g_x**3 - 16*g_x) &
       + 16*g_R**6/ra**4 * (96*g_x**2-16)

  au_y_sol(0,:) = 0; au_y_sol(0,0) =1.0          ! T_0
  au_y_sol(1,:) = 0; au_y_sol(1,1) =1.0          ! T_1
  au_y_sol(2,:) = 0; au_y_sol(2,2) =1.0          ! T_2
  au_y_sol(3,:) = 0; au_y_sol(3,3) =1.0          ! T_3
  au_y_sol(4,:) = 0; au_y_sol(4,4) =1.0          ! T_4

  au_y = au_ag(ag_y)

  write(6,*) 'y(n)=T_n(x), n=0...4'
  write(6,*) 'Test of au_ag'
  write(6,*) 'wrong values are displayed below...'
  do m=0,nm
     do k=0,km
        if ( abs(au_y(m,k)-au_y_sol(m,k)) .gt. eps ) then
           write(6,*) m,i, au_y(m,k), au_y_sol(m,k), au_y(m,k)-au_y_sol(m,k)
           call MessageNotify('E','au_test_base2d', &
                'Transformation error too large.')
        endif
     enddo
  enddo

  ag_y_orig = ag_y
  ag_y = ag_au(au_ag(ag_y))

  write(6,*) 'y(n)=T_n(x), n=0...4'
  write(6,*) 'Test of au_ag, ag_au'
  write(6,*) 'wrong values are displayed below...'
  do m=0,nm
     do i=0,im
        if ( abs(ag_y(m,i)-ag_y_orig(m,i)) .gt. eps ) then
           write(6,*) m,i, ag_y(m,i), ag_y_orig(m,i), ag_y(m,i)-ag_y_orig(m,i)
           call MessageNotify('E','au_test_base2d', &
                'Transformation error too large.')
        endif
     enddo
  enddo
  
  write(6,*) 'Test of ag_Dr_au'
  ag_y = ag_Dr_au(au_ag(ag_y_orig))
  write(6,*) 'wrong values are displayed below...'
  do m=0,nm
     do i=0,im
        if ( abs(ag_y(m,i)-ag_y_deriv(m,i)) .gt. eps ) then
           write(6,*) m,i, ag_y(m,i), ag_y_deriv(m,i), ag_y(m,i)-ag_y_deriv(m,i)
           call MessageNotify('E','au_test_base2d', &
                'Differentiation error ag_Dr_au too large.')
        endif
     enddo
  enddo

  write(6,*) 'Test of ag_Dr2_au'
  ag_y = ag_Dr2_au(au_ag(ag_y_orig))
  write(6,*) 'wrong values are displayed below...'
  do m=0,nm
     do i=0,im
        if ( abs(ag_y(m,i)-ag_y_deriv2(m,i)) .gt. eps ) then
           write(6,*) m,i, ag_y(m,i), ag_y_deriv2(m,i), ag_y(m,i)-ag_y_deriv2(m,i)
           call MessageNotify('E','au_test_base2d', &
                'Differentiation error ag_Dr2_au too large.')
        endif
     enddo
  enddo

  call MessageNotify('M','au_test_base2d', &
       'Tests of basic functions ag_au, au_ag, ag_Dr_au suceeded!')

end program au_test_base2d

