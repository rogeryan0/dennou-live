!----------------------------------------------------------------------
! Copyright (c) 2002-2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_module テストプログラム
!      1 次元境界値問題
!
!履歴  2002/11/19  竹広真一
!      2007/10/25  竹広真一  エラーメッセージ追加
!      2010/01/01  佐々木洋平 dc_test を使うように修正
!
program at_test_boundaries2d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use at_module
  implicit none

  integer, parameter :: im=8, km=8, nm=2
  real(8), parameter :: xmin=-1.0D0, xmax=1.0D0

  real(8), dimension(nm,0:im) :: ag_a
  real(8), dimension(nm,0:im) :: ag_dadx
  real(8), dimension(nm,0:km) :: at_a
  real(8), dimension(nm,2)    :: values

  ! 判定誤差設定
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11

  values(1,:) = (/0.0d0,0.0d0/)
  values(2,:) = (/1.0d0,-2.0d0/)

  call MessageNotify('M','at_test_boundaries2d', &
    & '2D Boundary value problem')

  call at_initial(im,km,xmin,xmax)

  ag_a(1,:) = sin(g_X)
  ag_a(2,:) = cos(g_X)
  at_a = at_ag(ag_a)

!----- at_BoundariesGrid_DD ------
  call at_BoundariesGrid_DD(at_a,values)
  ag_a = ag_at(at_a)
  call check1d(ag_a(:,0), values(:,1), 'test of at_Boundaries_DD(0)')
  call check1d(ag_a(:,im), values(:,2), 'test of at_Boundaries_DD(im)')
  call MessageNotify('M','test at_BoundariesGrid_DD', &
       'succeeded!')
  
!----- at_BoundariesGrid_DN ------
  call at_BoundariesGrid_DN(at_a,values)
  ag_a = ag_at(at_a)
  ag_dadx = ag_at(at_Dx_at(at_ag(ag_a)))
  call check1d(ag_a(:,0), values(:,1), 'test of at_Boundaries_DN(0)')
  call check1d(ag_dadx(:,im), values(:,2), 'test of at_Boundaries_DN(im)')
  call MessageNotify('M','test at_BoundariesGrid_DN', &
       'succeeded!')

!----- at_BoundariesGrid_ND ------
  call at_BoundariesGrid_ND(at_a,values)
  ag_a = ag_at(at_a)
  ag_dadx = ag_at(at_Dx_at(at_ag(ag_a)))
  call check1d(ag_dadx(:,0), values(:,1), 'test of at_Boundaries_ND(0)')
  call check1d(ag_a(:,im), values(:,2), 'test of at_Boundaries_ND(im)')
  call MessageNotify('M','test at_BoundariesGrid_ND', &
       'succeeded!')

!----- at_BoundariesGrid_NN ------
  call at_BoundariesGrid_NN(at_a,values)
  ag_a = ag_at(at_a)
  ag_dadx = ag_at(at_Dx_at(at_ag(ag_a)))
  call check1d(ag_dadx(:,0), values(:,1), 'test of at_Boundaries_NN(0)')
  call check1d(ag_dadx(:,im), values(:,2), 'test of at_Boundaries_NN(im)')
  call MessageNotify('M','test at_BoundariesGrid_NN', &
       'succeeded!')

!----- at_BoundariesTau_DD ------
  call at_BoundariesTau_DD(at_a,values)
  ag_a = ag_at(at_a)
  call check1d(ag_a(:,0), values(:,1), 'test of at_BoundariesTau_DD(0)')
  call check1d(ag_a(:,im), values(:,2), 'test of at_BoundariesTau_DD(im)')
  call MessageNotify('M','test at_BoundariesTau_DD', &
       'succeeded!')
  
!----- at_BoundariesTau_DN ------
  call at_BoundariesTau_DN(at_a,values)
  ag_a = ag_at(at_a)
  ag_dadx = ag_at(at_Dx_at(at_ag(ag_a)))
  call check1d(ag_a(:,0), values(:,1), 'test of at_BoundariesTau_DN(0)')
  call check1d(ag_dadx(:,im), values(:,2), 'test of at_BoundariesTau_DN(im)')
  call MessageNotify('M','test at_BoundariesTau_DN', &
       'succeeded!')

!----- at_BoundariesTau_ND ------
  call at_BoundariesTau_ND(at_a,values)
  ag_a = ag_at(at_a)
  ag_dadx = ag_at(at_Dx_at(at_ag(ag_a)))
  call check1d(ag_dadx(:,0), values(:,1), 'test of at_BoundariesTau_ND(0)')
  call check1d(ag_a(:,im), values(:,2), 'test of at_BoundariesTau_ND(im)')
  call MessageNotify('M','test at_BoundariesTau_ND', &
       'succeeded!')

!----- at_BoundariesTau_NN ------
  call at_BoundariesTau_NN(at_a,values)
  ag_a = ag_at(at_a)
  ag_dadx = ag_at(at_Dx_at(at_ag(ag_a)))
  call check1d(ag_dadx(:,0), values(:,1), 'test of at_BoundariesTau_NN(0)')
  call check1d(ag_dadx(:,im), values(:,2), 'test of at_BoundariesTau_NN(im)')
  call MessageNotify('M','test at_BoundariesTau_NN', &
       'succeeded!')

contains
  subroutine check1d(var, true, funcname)
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

end program at_test_boundaries2d
