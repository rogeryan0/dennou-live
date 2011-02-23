!----------------------------------------------------------------------
! Copyright (c) 2002-2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_module テストプログラム
!
!履歴  2002/08/20  竹広真一
!      2007/10/24  竹広真一  エラーメッセージ追加
!      2010/01/01  佐々木洋平 dc_test を使うように修正
!
program at_intavr

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use at_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32            ! 格子点の設定
  integer, parameter :: km=32,lm=2        ! 切断波数の設定

 !---- 変数 ----
  real(8)            :: g_Data(0:im)     ! 格子データ
  real(8)            :: ag_Data(lm,0:im)    ! 格子データ

 !---- 座標変数など ----
  real(8), parameter :: xmin = -1.0D0, xmax=1.0D0

  real(8), parameter :: pi=3.1415926535897932385D0
  ! 判定誤差設定
  integer, parameter :: check_digits = 9
  integer, parameter :: ignore = -10

  integer :: i

 !---------------- 座標値の設定 ---------------------

  call MessageNotify('M','at_test_intavr', &
    & 'Test of integral/average functions')

  call MessageNotify('M', 'test function', &
    & 'f = 1-sin(2*pi*X)')

  do i=1,2
    call at_Initial(im,km/i,xmin,xmax) 

    !------------------- 初期値設定 ----------------------
    g_Data = 1.0d0 - sin(2.0d0*pi*g_X)

    call check0d(Int_g(g_Data), 2.0d0, 'test of Int_g')
    call check0d(Avr_g(g_Data), 1.0d0, 'test of Avr_g')

    call MessageNotify('M','Test of Int_g, Avr_g', &
      & 'suceeded!')

    ag_Data(1,:) = cos(pi*g_X)**2
    ag_Data(2,:) = sin(pi*g_X)**2
    
    call check1d(a_Int_ag(ag_Data), (/1.0d0,1.0d0/),    'test of a_Int_ag')
    call check1d(a_Avr_ag(ag_Data), (/5.0d-1,5.0d-1/),  'test of a_Avr_ag')
    
    call MessageNotify('M','Test of a_Int_ag, a_Avr_ag', &
      & 'suceeded!')

  end do
  
  call MessageNotify('M','at_test_intavr', &
    & 'Test of integral/average functions succeeded!')
  
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

end program at_intavr
