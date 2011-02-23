!----------------------------------------------------------------------
! Copyright (c) 2002-2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_module テストプログラム
!
!履歴  2002/04/10  竹広真一 
!      2007/10/25  竹広真一  エラーメッセージ追加
!      2010/01/01  佐々木洋平 dc_test を使うように修正
!
program at_test_weight

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use at_module
  implicit none
  integer, parameter :: im=32, km=32
  real(8), parameter :: xmin=0.0D0, xmax=2.0D0
  real(8), dimension(0:im) :: g_z
  real(8), parameter       :: pi=3.1415926535897932385D0

  ! 判定誤差設定
  integer, parameter :: check_digits = 9
  integer, parameter :: ignore = -10
  integer :: i

  call MessageNotify('M','at_test_weight', &
    & 'Test of g_X_Weight')

  do i=1,2
    call at_initial(im,km/i,xmin,xmax)
    g_z(:) = 1.0d0
    call check0d( sum(g_z*g_x_weight), 2.0d0, &
      & 'z=1, int_0^2 z dx = 2')
    g_z(:) = g_x**2
    call check0d( sum(g_z*g_x_weight), 8.0d0/3.0d0, &
      & 'z=x**2, int_0^2 z dx = 8/3')
    g_z(:) = sin(pi*g_x/2)
    call check0d( sum(g_z*g_x_weight), 4.0d0/pi, &
      & 'z=sin(pi*x/2), int_0^2 z dx = 4/pi')
  end do
  
  call MessageNotify('M','Test of g_X_Weight', &
    'Test of g_X_Weight suceeded!')
  
contains
  subroutine check1d(var,true,funcname)
    real(8) :: var(:)                  
    real(8) :: true(:)
    character(len=*), optional :: funcname
    
    call AssertEqual(&
      & message = funcname,   &
      & answer  = true,       &
      & check   = var,        &
      significant_digits = check_digits, ignore_digits = ignore   &
      )
  end subroutine check1d
  
  subroutine check0d(var0,true0,funcname)
    real(8) :: var0                       
    real(8) :: true0                       
    real(8) :: vartmp(1)
    real(8) :: truetmp(1)
    character(len=*), optional :: funcname

    vartmp(1) = var0
    truetmp(1) = true0
    call check1d(vartmp,truetmp, funcname)
  end subroutine check0d

end program at_test_weight



