!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  eq_module テストプログラム (ヤコビアン計算)
!
!履歴  2008/04/13  竹広真一
!
program eq_test_jacobian

  use dc_message, only : MessageNotify
  use eq_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=16            ! 格子点の設定(Phi,Rad)
  integer, parameter :: km=10, lm=21            ! 切断波数の設定(Phi,Rad)

 !---- 変数 ----
  real(8)            :: rp_A(jm,0:im-1)        ! 格子データ
  real(8)            :: rp_B(jm,0:im-1)        ! 格子データ
  real(8)            :: rp_Jacob(jm,0:im-1)    ! 格子データ

  integer            :: k=3,l=2

 !---- 座標変数など ----
  real(8), parameter :: ra=2.0

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-8            ! 判定誤差

  call MessageNotify('M','eq_test_jacobian', &
       'eq_module jacobian function tests')

 !---------------- 座標値の設定 ---------------------
  call eq_Initial(im,jm,km,lm,ra)                ! スペクトル初期化

 !------------------- 初期値設定 ----------------------
  write(6,*) '*** Test of eq_module : Jacobian function check.'
  write(6,*) '  The result will be printed '
  write(6,*) '  only when the error is larger than ', eps
  write(6,*)

!!$  write(6,*) '  Input wavenumbers of the grid data, k and l :'
!!$  read(5,*) k,l
  write(6,*) '  k,l = ', k,l

  rp_A = sin(k*rp_Phi)*rp_Rad**k
  rp_B = sin(k*rp_Phi)*rp_Rad**k
  rp_Jacob = 0.0

  call check2d(rp_eq(eq_Jacobian_eq_eq(eq_rp(rp_A),eq_rp(rp_B)))-rp_Jacob, &
       eps, 'Jacob(sin(k*Phi)*rp_Rad**k,sin(k*Phi)*rp_Rad**k)')

  rp_A = sin(k*rp_Phi)*rp_Rad**k
  rp_B = cos(k*rp_Phi)*rp_Rad**k
  rp_Jacob = -k**2 * rp_Rad**(2*k-2)

  call check2d(rp_eq(eq_Jacobian_eq_eq(eq_rp(rp_A),eq_rp(rp_B)))-rp_Jacob, &
       eps, 'Jacob(sin(k*Phi)*rp_Rad**k,cos(k*rp_Phi)*Rad**k')

  rp_A = cos(k*rp_Phi)*rp_Rad**(k+2)
  rp_B = rp_Rad**2
  rp_Jacob =  2 * k * sin(k*rp_Phi)*rp_Rad**(k+2)

  call check2d(rp_eq(eq_Jacobian_eq_eq(eq_rp(rp_A),eq_rp(rp_B)))-rp_Jacob, &
       eps, 'Jacob(cos(k*Phi)*Rad**(k+2),Rad**2))')

  rp_A = rp_Rad**4
  rp_B = sin(l*rp_Phi)*rp_Rad**l
  rp_Jacob = 4 * l * rp_Rad**(l+2) * cos(l*rp_Phi)

  call check2d(rp_eq(eq_Jacobian_eq_eq(eq_rp(rp_A),eq_rp(rp_B)))-rp_Jacob, &
       eps, 'Jacob(Rad**4,sin(l*Phi)*Rad**l)')

  call MessageNotify('M','eq_test_jacobian', &
       'eq_module jacobian function tests succeeded!')

 stop
contains

  subroutine check2d(var,eps,funcname) ! 絶対値が eps 以上の var の要素を出力
    real(8) :: var(:,:)                ! 判定する配列
    real(8) :: eps                     ! 誤差
    character(len=*), optional :: funcname
    integer i, j

    if ( present(funcname) )then
       write(6,*) '  Checking ', funcname, '...'
    endif

    do i=1,size(var,1)
       do j=1,size(var,2)
          if (abs(var(i,j)) .gt. eps ) then
             write(6,*) &
                  '    Value larger than EPS : i= ', i, '  j= ', j, var(i,j)
             call MessageNotify('E','eq_test_jacobian', &
                  'jacobian error too large')
          endif
       enddo
    enddo
  end subroutine check2d

end program eq_test_jacobian
