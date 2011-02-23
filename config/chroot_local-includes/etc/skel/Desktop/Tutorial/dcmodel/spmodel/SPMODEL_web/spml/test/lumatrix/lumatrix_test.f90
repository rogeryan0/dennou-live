!----------------------------------------------------------------------
! Copyright (c) 2009 SPMODEL Development Group. All rights reserved.!
!----------------------------------------------------------------------
!
!表題  lumatrix テストプログラム
!
!履歴  2009/08/06  竹広真一
!      2009/10/24  佐々木洋平 幾つかの数値代入を倍精度に変更
!                  dc_test を使用するように修正
!
program lumatrix_test

  use lumatrix
  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual

  integer,parameter  :: idim=2, jdim=2, ndim=2

  real(8) :: alu3(jdim,ndim,ndim)
  real(8) :: b3(idim,jdim,ndim)
  real(8) :: b3sol(idim,jdim,ndim)
  integer :: kp2(jdim,ndim)
  real(8) :: b2(jdim,ndim)
  real(8) :: b2sol(jdim,ndim)

  real(8) :: alu2(ndim,ndim)
  integer :: kp1(ndim)
  real(8) :: bb2(idim,ndim)
  real(8) :: bb2sol(idim,ndim)
  real(8) :: bb1(ndim)
  real(8) :: bb1sol(ndim)

  ! 判定誤差設定
  integer, parameter :: check_digits = 15
  integer, parameter :: ignore = -16

  call MessageNotify('M','lumatrix_test','lumatrix test')
  
  alu3(1,1,:) = (/1.0d0,1.0d0/)
  alu3(1,2,:) = (/1.0d0,0.0d0/)
  alu3(2,1,:) = (/0.0d0,1.0d0/)
  alu3(2,2,:) = (/1.0d0,-1.0d0/)

  b3(1,1,:)     = (/2.0d0,0.0d0/)
  b3(2,1,:)     = (/4.0d0,0.0d0/)
  b3(1,2,:)     = (/2.0d0,0.0d0/)
  b3(2,2,:)     = (/4.0d0,0.0d0/)

  b3sol(1,1,:)     = (/0.0d0,2.0d0/)
  b3sol(2,1,:)     = (/0.0d0,4.0d0/)
  b3sol(1,2,:)     = (/2.0d0,2.0d0/)
  b3sol(2,2,:)     = (/4.0d0,4.0d0/)

  b2(1,:)     = (/2.0d0,0.0d0/)
  b2(2,:)     = (/2.0d0,0.0d0/)

  b2sol(1,:)     = (/0.0d0,2.0d0/)
  b2sol(2,:)     = (/2.0d0,2.0d0/)

  call ludecomp(alu3,kp2)

  b3=lusolve(alu3,kp2,b3)
  call AssertEqual(&
    message = 'Test 323',                                         &
    answer  = b3sol,                                              &
    check   = b3,                                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  b2=lusolve(alu3,kp2,b2)
  call AssertEqual(&
    message = 'Test 322',                                         &
    answer  = b2sol,                                              &
    check   = b2,                                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  alu2(1,:) = (/1.0d0,1.0d0/)
  alu2(2,:) = (/1.0d0,0.0d0/)

  bb2(1,:)     = (/2.0d0,0.0d0/)
  bb2(2,:)     = (/4.0d0,0.0d0/)

  bb2sol(1,:)  = (/0.0d0,2.0d0/)
  bb2sol(2,:)  = (/0.0d0,4.0d0/)

  bb1(:)     = (/2.0d0,0.0d0/)
  bb1sol(:)  = (/0.0d0,2.0d0/)

  call ludecomp(alu2,kp1)

  bb2=lusolve(alu2,kp1,bb2)

  call AssertEqual(&
    message = 'Test 212',                                         &
    answer  = bb2sol,                                             &
    check   = bb2,                                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  
  bb1=lusolve(alu2,kp1,bb1)

  call AssertEqual(&
    message = 'Test 211',                                         &
    answer  = bb1sol,                                             &
    check   = bb1,                                                &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','lumatrix_test','lumatrix test succeeded')

end program lumatrix_test

