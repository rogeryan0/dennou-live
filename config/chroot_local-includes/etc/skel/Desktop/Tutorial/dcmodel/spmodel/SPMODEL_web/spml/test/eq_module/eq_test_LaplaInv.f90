!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_module テストプログラム
!      Laplacian 逆解き問題
!
!履歴  2008/04/12  竹広真一
!      
!
program eq_test_LaplaInv

  use dc_message, only : MessageNotify
  use eq_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=16, jm=16              ! 格子点の設定(X,Y)
  integer, parameter :: km=5, lm=21             ! 切断波数の設定(X,Y)

 !---- 座標変数など ----
  real(8), parameter :: ra=1.5

  real(8), parameter :: eps = 1.0d-10         ! 判定誤差

  real(8)            :: rp_A(jm,0:im-1)       ! 格子データ
  real(8)            :: rp_A_sol(jm,0:im-1)   ! 格子データ
  real(8)            :: eq_A(-km:km,0:lm)     ! スペクトルデータ

  real(8), dimension(-km:km) :: value         ! 境界条件

  integer,parameter  :: k=2, l=k+2

  call MessageNotify('M','eq_test_LaplaInv', &
       'eq_module subroutine for Inverse Laplacian tests')

 !---------------- 座標値の設定 ---------------------
  call eq_Initial(im,jm,km,lm,ra)                ! スペクトル初期化

 !----- eq_LaplaInv_eq ------
  rp_A = sin(k*rp_Phi) * rp_Rad**l 
  rp_A_sol = sin(k*rp_Phi) * rp_Rad**(l+2)/((l+2)**2 - k**2)

  value = e_p(rp_A_sol(jm,:))

  eq_A = eq_rp(rp_A)
  rp_A = rp_eq(eq_LaplaInv_eq(eq_A,value))

  call check2d(rp_A-rp_A_sol, eps, 'LaplaInv(sin(k*Phi)*Rad**(l+2)')

  call MessageNotify('M','Test of eq_LaplaInv','Test of eq_LaplaInv suceeded!')

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
             call MessageNotify('E','ee_test_LaplaInv', &
                  'Inverse Laplacian error too large')
          endif
       enddo
    enddo
  end subroutine check2d

end program eq_test_LaplaInv
