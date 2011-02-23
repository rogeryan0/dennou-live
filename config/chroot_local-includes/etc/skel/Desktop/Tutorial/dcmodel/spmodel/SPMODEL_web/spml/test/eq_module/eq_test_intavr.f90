!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  eq_module テストプログラム (積分・平均計算)
!
!履歴  2008/04/12  竹広真一
!
program eq_test_intavr

  use dc_message, only : MessageNotify
  use eq_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=16            ! 格子点の設定(Phi,Rad)
  integer, parameter :: km=10, lm=31            ! 切断波数の設定(Phi,Rad)

 !---- 変数 ----
  real(8)            :: rp_Data(jm,0:im-1)    ! 格子データ
  real(8)            :: p_Data(0:im-1)        ! 格子データ
  real(8)            :: r_Data(jm)            ! 格子データ

 !---- 座標変数など ----
  real(8), parameter :: ra=2.0

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-5            ! 判定誤差

  call MessageNotify('M','eq_test_intvar', &
                     'eq_module integrate/averaging function tests')

 !---------------- 座標値の設定 ---------------------
  call eq_Initial(im,jm,km,lm,ra)                ! スペクトル初期化

 !------------------- 初期値設定 ----------------------
  rp_Data = 1
  write(6,*)
  write(6,*) 'f = 1.0'

    call check0d(IntRadPhi_rp(rp_Data) - pi*ra**2, eps, 'IntRadPhi_rp') 
    call check0d(AvrRadPhi_rp(rp_Data) - 1.0D0, eps, 'AvrRadPhi_rp')

    call check1d(r_IntPhi_rp(rp_Data) - 2*pi, eps, 'r_IntPhi_rp') 
    call check1d(r_AvrPhi_rp(rp_Data) - 1.0D0, eps, 'r_AvrPhi_rp')

    call check1d(p_IntRad_rp(rp_Data) - ra**2/2, eps, 'p_IntRad_rp')
    call check1d(p_AvrRad_rp(rp_Data) - 1.0, eps, 'p_AvrRad_rp')

  rp_Data = sin(rp_Phi) * rp_Rad**3
  write(6,*) 'f = sin(Phi)*Rad**3'

    call check0d(IntRadPhi_rp(rp_Data)-0.0, eps, 'IntRadPhi_rp') 
    call check0d(AvrRadPhi_rp(rp_Data)-0.0, eps, 'AvrRadPhi_rp')

    call check1d(r_IntPhi_rp(rp_Data) - 0.0, eps, 'r_IntPhi_rp') 
    call check1d(r_AvrPhi_rp(rp_Data) - 0.0, eps, 'r_AvrPhi_rp') 

    call check1d(p_IntRad_rp(rp_Data) - 1/5.0d0 *ra**5 * sin(p_Phi), eps, 'p_IntRad_rp')
    call check1d(p_AvrRad_rp(rp_Data) - 2/5.0d0 *ra**3 * sin(p_Phi), eps, 'p_AvrRad_rp')

  rp_Data = (1 - sin(rp_Phi))
  write(6,*)
  write(6,*) 'f = (1-sin(Phi)'

    call check0d(IntRadPhi_rp(rp_Data) - pi*ra**2, eps, 'IntRadPhi_rp') 
    call check0d(AvrRadPhi_rp(rp_Data) - 1.0D0, eps, 'AvrRadPhi_rp')

    call check1d(r_IntPhi_rp(rp_Data) - 2*pi, eps, 'r_IntPhi_rp') 
    call check1d(r_AvrPhi_rp(rp_Data) - 1.0D0, eps, 'r_AvrPhi_rp')

    call check1d(p_IntRad_rp(rp_Data) - (1 - sin(p_Phi))*ra**2/2, eps, 'p_IntRad_rp')
    call check1d(p_AvrRad_rp(rp_Data) - (1 - sin(p_Phi)), eps, 'p_AvrRad_rp')

  p_Data = 1 - sin(2*p_Phi)
  write(6,*)
  write(6,*) 'f = 1-sin(2*pi*Phi)'

    call check0d(IntPhi_p(p_Data) - 2.0*pi, eps, 'IntPhi_p')
    call check0d(AvrPhi_p(p_Data) - 1.0d0, eps, 'AvrPhi_p')

  r_Data = r_Rad**2
  write(6,*)
  write(6,*) 'f = Rad**2'

    call check0d(IntRad_r(r_Data) - 1.0D0/4.0D0*ra**4, eps, 'IntRad_r')
    call check0d(AvrRad_r(r_Data) - 1.0D0/2.0D0*ra**2, eps, 'AvrRad_r')

  call MessageNotify('M','eq_test_intvar', &
                     'eq_module integrate/averaging function tests succeeded!')
 stop
contains

  subroutine check1d(var,eps,funcname) ! 絶対値が eps 以上の var の要素を出力
    real(8) :: var(:)                  ! 判定する配列
    real(8) :: eps                     ! 誤差
    character(len=*), optional :: funcname
    integer i

    if ( present(funcname) )then
       write(6,*) '  Checking ', funcname, '...'
    endif

    do i=1,size(var)
       if (abs(var(i)) .gt. eps ) then
          write(6,*) '    Value larger than EPS : i= ', i, var(i)
          call MessageNotify('E','eq_test_intavr', &
               'Integrate/Avrage error too large.')
       endif
    enddo
  end subroutine check1d

  subroutine check0d(var,eps,funcname)   ! var の絶対値が eps 以上だと出力
    real(8) :: var                       ! 判定する配列
    real(8) :: eps                       ! 誤差
    real(8) :: vartmp(1)                 ! 
    character(len=*), optional :: funcname

    vartmp(1) = var
    if ( present(funcname) ) then
       call check1d(vartmp,eps,funcname)
    else
       call check1d(vartmp,eps)
    endif
  end subroutine check0d

end program eq_test_intavr

