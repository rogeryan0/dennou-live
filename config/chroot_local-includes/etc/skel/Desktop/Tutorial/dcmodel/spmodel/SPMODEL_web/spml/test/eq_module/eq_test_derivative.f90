!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  eq_module テストプログラム (微分計算)
!
!履歴  2008/04/12  竹広真一
!
program eq_test_derivative

  use dc_message, only : MessageNotify
  use eq_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=16              ! 格子点の設定(Phi,Rad)
  integer, parameter :: km=10, lm=21              ! 切断波数の設定(Phi,Rad)

 !---- 変数 ----
  real(8)            :: rp_Data(jm,0:im-1)        ! 格子データ
  real(8)            :: rp_Deriv(jm,0:im-1)       ! 格子データ

  integer            :: k=2, l=4

 !---- 座標変数など ----
  real(8), parameter :: ra=3.0

  real(8), parameter :: eps = 1.0d-7            ! 判定誤差

  call MessageNotify('M','eq_test_derivative', &
       'eq_module derivative function tests')

 !---------------- 座標値の設定 ---------------------
  call eq_Initial(im,jm,km,lm,ra)                ! スペクトル初期化

 !------------------- 初期値設定 ----------------------
  write(6,*)
  write(6,*) '  The result will be printed '
  write(6,*) '  only when the error is larger than ', eps
  write(6,*)

!!$  write(6,*) '  Input wavenumbers of the grid data, k and l :'
!!$  read(5,*) k,l
  write(6,*) '  k,l = ', k,l

  rp_Data = sin(k*rp_Phi) * rp_Rad**l

  rp_Deriv = k*cos(k*rp_Phi) * rp_Rad**l
  call check2d(rp_eq(eq_DPhi_eq(eq_rp(rp_Data)))-rp_Deriv, &
       eps, 'DPhi(sin(k*Phi)*Rad**l')

  rp_Deriv = sin(k*rp_Phi) * l * rp_Rad**l
  call check2d(rp_eq(eq_RadDRad_eq(eq_rp(rp_Data)))-rp_Deriv, &
       eps, 'RadDRad(sin(k*Phi)*Rad**l')

  rp_Deriv = (l**2 - k**2) * sin(k*rp_Phi) * rp_Rad**(l-2)
  call check2d(rp_er(er_Lapla_eq(eq_rp(rp_Data)))-rp_Deriv, &
       eps, 'Lapla(sin(k*Phi)*Rad**l')

  rp_Data = cos(k*rp_Phi) * rp_Rad**l

  rp_Deriv = -k*sin(k*rp_Phi) * rp_Rad**l
  call check2d(rp_eq(eq_DPhi_eq(eq_rp(rp_Data)))-rp_Deriv, &
       eps, 'DPhi(cos(k*Phi)*Rad**l')

  rp_Deriv = l * cos(k*rp_Phi) * rp_Rad**l
  call check2d(rp_eq(eq_RadDRad_eq(eq_rp(rp_Data)))-rp_Deriv, &
       eps, 'RadDRad(cos(k*Phi)*Rad**l')

  rp_Deriv = (l**2-k**2) * cos(k*rp_Phi) * rp_Rad**(l-2)
  call check2d(rp_er(er_Lapla_eq(eq_rp(rp_Data)))-rp_Deriv, &
       eps, 'Lapla(cos(k*Phi)*Rad**l')

  call MessageNotify('M','eq_test_derivative', &
       'eq_module derivative function tests succeeded!')

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
             call MessageNotify('E','ee_test_derivative', &
                  'derivative error too large')
          endif
       enddo
    enddo
  end subroutine check2d

end program eq_test_derivative
