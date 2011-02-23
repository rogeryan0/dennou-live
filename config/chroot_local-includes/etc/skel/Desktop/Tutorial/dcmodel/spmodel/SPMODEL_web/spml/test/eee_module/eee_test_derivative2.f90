!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  eee_module テストプログラム (微分計算)
!
!履歴  2008/05/03  竹広真一
!      2008/05/10  竹広真一  解像度複数設定チェック
!
program eee_test_derivative

  use dc_message, only : MessageNotify
  use eee_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=64, km=16          ! 格子点の設定(X,Y,Z)
  integer, parameter :: lm=10, mm=21, nm=5          ! 切断波数の設定(X,Y,Z)

  integer, parameter :: im2=64, jm2=64, km2=64       ! 格子点の設定(X,Y,Z)
  integer, parameter :: lm2=21, mm2=21, nm2=21       ! 切断波数の設定(X,Y,Z)

 !---- 変数 ----
  real(8)            :: zyx_Data(0:km-1,0:jm-1,0:im-1)    ! 格子データ
  real(8)            :: zyx_Deriv(0:km-1,0:jm-1,0:im-1)   ! 格子データ

  real(8)            :: zyx_Data2(0:km2-1,0:jm2-1,0:im2-1)    ! 格子データ
  real(8)            :: zyx_Deriv2(0:km2-1,0:jm2-1,0:im2-1)   ! 格子データ

  integer            :: l=2, m=3, n=5
  integer            :: id1, id2

 !---- 座標変数など ----
  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! 判定誤差

  call MessageNotify('M','eee_test_derivative', &
       'eee_module derivative function tests')

 !---------------- 座標値の設定 ---------------------
  call eee_initial(im,jm,km,lm,mm,nm,id1)
  call eee_initial(im2,jm2,km2,lm2,mm2,nm2,id2)

 !------------------- 初期値設定 ----------------------
  write(6,*) '*** Test of eee_module : derivative function check.'
  write(6,*) '  The result will be printed '
  write(6,*) '  only when the error is larger than ', eps
  write(6,*)

!!$  write(6,*) '  Input wavenumbers of the grid data, l,m and n :'
!!$  read(5,*) l,m,n
  write(6,*) '  l,m,n = ', l,m,n

  ! id1
  write(*,*) 'for id1'
  call eee_ChangeResolution(id1)

  zyx_Data = sin(l*zyx_X) * sin(m*zyx_Y) * sin(n*zyx_Z)
  zyx_Deriv = l*cos(l*zyx_X) * sin(m*zyx_Y)* sin(n*zyx_Z)
  call check3d(zyx_eee(eee_Dx_eee(eee_zyx(zyx_Data)))-zyx_Deriv, &
       eps, 'Dx(sin(l*X)*sin(m*Y)*sin(n*Z))')
  zyx_Deriv = m*sin(l*zyx_X) * cos(m*zyx_Y) * sin(n*zyx_Z)
  call check3d(zyx_eee(eee_Dy_eee(eee_zyx(zyx_Data)))-zyx_Deriv, &
       eps, 'Dy(sin(l*X)*sin(m*Y)*sin(n*Z))')
  zyx_Deriv = n*sin(l*zyx_X) * sin(m*zyx_Y) * cos(n*zyx_Z)
  call check3d(zyx_eee(eee_Dz_eee(eee_zyx(zyx_Data)))-zyx_Deriv, &
       eps, 'Dz(sin(l*X)*sin(m*Y)*sin(n*Z))')

  zyx_Deriv = -(l**2 + m**2 + n**2) &
                 * sin(l*zyx_X) * sin(m*zyx_Y) * sin(n*zyx_Z)
  call check3d(zyx_eee(eee_Lapla_eee(eee_zyx(zyx_Data)))-zyx_Deriv, &
       eps, 'Lapla(sin(l*X)*sin(m*Y)*sin(n*Z))')
  zyx_Deriv = -1.0/(l**2 + m**2+ n**2) &
                 * sin(l*zyx_X) * sin(m*zyx_Y) * sin(n*zyx_Z)
  call check3d(zyx_eee(eee_LaplaInv_eee(eee_zyx(zyx_Data)))-zyx_Deriv, &
       eps, 'LaplaInv(sin(l*X)*sin(m*Y)*sin(n*Z))')

  zyx_Data = cos(l*zyx_X) * cos(m*zyx_Y) * cos(n*zyx_Z)
  zyx_Deriv = -l*sin(l*zyx_X) * cos(m*zyx_Y)* cos(n*zyx_Z)
  call check3d(zyx_eee(eee_Dx_eee(eee_zyx(zyx_Data)))-zyx_Deriv, &
       eps, 'Dx(cos(l*X)*cos(m*Y)*cos(n*Z))')
  zyx_Deriv = -m*cos(l*zyx_X) * sin(m*zyx_Y) * cos(n*zyx_Z)
  call check3d(zyx_eee(eee_Dy_eee(eee_zyx(zyx_Data)))-zyx_Deriv, &
       eps, 'Dy(cos(l*X)*cos(m*Y)*cos(n*Z))')
  zyx_Deriv = -n*cos(l*zyx_X) * cos(m*zyx_Y) * sin(n*zyx_Z)
  call check3d(zyx_eee(eee_Dz_eee(eee_zyx(zyx_Data)))-zyx_Deriv, &
       eps, 'Dz(cos(l*X)*cos(m*Y)*cos(n*Z))')

  zyx_Deriv = -(l**2 + m**2 + n**2) &
                 * cos(l*zyx_X) * cos(m*zyx_Y) * cos(n*zyx_Z)
  call check3d(zyx_eee(eee_Lapla_eee(eee_zyx(zyx_Data)))-zyx_Deriv, &
       eps, 'Lapla(cos(l*X)*cos(m*Y)*cos(n*Z))')
  zyx_Deriv = -1.0/(l**2 + m**2+ n**2) &
                 * cos(l*zyx_X) * cos(m*zyx_Y) * cos(n*zyx_Z)
  call check3d(zyx_eee(eee_LaplaInv_eee(eee_zyx(zyx_Data)))-zyx_Deriv, &
       eps, 'LaplaInv(cos(l*X)*cos(m*Y)*cos(n*Z))')

  ! id2
  write(*,*) 'for id2'
  call eee_ChangeResolution(id2)

  zyx_Data2 = sin(l*zyx_X) * sin(m*zyx_Y) * sin(n*zyx_Z)
  zyx_Deriv2 = l*cos(l*zyx_X) * sin(m*zyx_Y)* sin(n*zyx_Z)
  call check3d(zyx_eee(eee_Dx_eee(eee_zyx(zyx_Data2)))-zyx_Deriv2, &
       eps, 'Dx(sin(l*X)*sin(m*Y)*sin(n*Z))')
  zyx_Deriv2 = m*sin(l*zyx_X) * cos(m*zyx_Y) * sin(n*zyx_Z)
  call check3d(zyx_eee(eee_Dy_eee(eee_zyx(zyx_Data2)))-zyx_Deriv2, &
       eps, 'Dy(sin(l*X)*sin(m*Y)*sin(n*Z))')
  zyx_Deriv2 = n*sin(l*zyx_X) * sin(m*zyx_Y) * cos(n*zyx_Z)
  call check3d(zyx_eee(eee_Dz_eee(eee_zyx(zyx_Data2)))-zyx_Deriv2, &
       eps, 'Dz(sin(l*X)*sin(m*Y)*sin(n*Z))')

  zyx_Deriv2 = -(l**2 + m**2 + n**2) &
                 * sin(l*zyx_X) * sin(m*zyx_Y) * sin(n*zyx_Z)
  call check3d(zyx_eee(eee_Lapla_eee(eee_zyx(zyx_Data2)))-zyx_Deriv2, &
       eps, 'Lapla(sin(l*X)*sin(m*Y)*sin(n*Z))')
  zyx_Deriv2 = -1.0/(l**2 + m**2+ n**2) &
                 * sin(l*zyx_X) * sin(m*zyx_Y) * sin(n*zyx_Z)
  call check3d(zyx_eee(eee_LaplaInv_eee(eee_zyx(zyx_Data2)))-zyx_Deriv2, &
       eps, 'LaplaInv(sin(l*X)*sin(m*Y)*sin(n*Z))')

  zyx_Data2 = cos(l*zyx_X) * cos(m*zyx_Y) * cos(n*zyx_Z)
  zyx_Deriv2 = -l*sin(l*zyx_X) * cos(m*zyx_Y)* cos(n*zyx_Z)
  call check3d(zyx_eee(eee_Dx_eee(eee_zyx(zyx_Data2)))-zyx_Deriv2, &
       eps, 'Dx(cos(l*X)*cos(m*Y)*cos(n*Z))')
  zyx_Deriv2 = -m*cos(l*zyx_X) * sin(m*zyx_Y) * cos(n*zyx_Z)
  call check3d(zyx_eee(eee_Dy_eee(eee_zyx(zyx_Data2)))-zyx_Deriv2, &
       eps, 'Dy(cos(l*X)*cos(m*Y)*cos(n*Z))')
  zyx_Deriv2 = -n*cos(l*zyx_X) * cos(m*zyx_Y) * sin(n*zyx_Z)
  call check3d(zyx_eee(eee_Dz_eee(eee_zyx(zyx_Data2)))-zyx_Deriv2, &
       eps, 'Dz(cos(l*X)*cos(m*Y)*cos(n*Z))')

  zyx_Deriv2 = -(l**2 + m**2 + n**2) &
                 * cos(l*zyx_X) * cos(m*zyx_Y) * cos(n*zyx_Z)
  call check3d(zyx_eee(eee_Lapla_eee(eee_zyx(zyx_Data2)))-zyx_Deriv2, &
       eps, 'Lapla(cos(l*X)*cos(m*Y)*cos(n*Z))')
  zyx_Deriv2 = -1.0/(l**2 + m**2+ n**2) &
                 * cos(l*zyx_X) * cos(m*zyx_Y) * cos(n*zyx_Z)
  call check3d(zyx_eee(eee_LaplaInv_eee(eee_zyx(zyx_Data2)))-zyx_Deriv2, &
       eps, 'LaplaInv(cos(l*X)*cos(m*Y)*cos(n*Z))')

  call MessageNotify('M','eee_test_derivative', &
       'eee_module derivative function tests succeeded!')

 stop
contains

  subroutine check3d(var,eps,funcname) ! 絶対値が eps 以上の var の要素を出力
    real(8) :: var(:,:,:)              ! 判定する配列
    real(8) :: eps                     ! 誤差
    character(len=*), optional :: funcname
    integer i, j, k

    if ( present(funcname) )then
       write(6,*) '  Checking ', funcname, '...'
    endif

    do k=1,size(var,3)
       do j=1,size(var,2)
          do i=1,size(var,1)
             if (abs(var(i,j,k)) .gt. eps ) then
                write(6,*) &
                  '    Value larger than EPS : i= ', i, '  j= ', j, '  k= ', k, &
                  var(i,j,k)
                call MessageNotify('E','eee_test_transform', &
                  'transform error too large')
             endif
          enddo
       enddo
    enddo
  end subroutine check3d

end program eee_test_derivative
