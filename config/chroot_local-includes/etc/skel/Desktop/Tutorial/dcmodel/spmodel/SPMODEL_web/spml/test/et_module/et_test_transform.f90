!----------------------------------------------------------------------
!     Copyright (c) 2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  et_module テストプログラム (正逆変換)
!
!履歴  2007/11/14 竹広真一
!      
program et_test_transform

  use dc_message, only : MessageNotify
  use et_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=32            ! 格子点の設定(X,Y)
  integer, parameter :: km=10, lm=10              ! 切断波数の設定(X,Y)

 !---- 変数 ----
  real(8)            :: yx_Data(0:jm,0:im-1)    ! 格子データ
  real(8)            :: et_Data(-km:km,0:lm)    ! スペクトルデータ
  real(8)            :: yx_xi(0:jm,0:im-1)      ! チェビシェフ座標

  integer            :: k=2

 !---- 座標変数など ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin =  0.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! 判定誤差

  call MessageNotify('M','et_test_transform', &
       'et_module transform function tests')

 !---------------- 座標値の設定 ---------------------
  call et_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! スペクトル初期化

  yx_xi = (yx_Y - (ymin+ymax)/2 )*2/(ymax-ymin)

 !------------------- 初期値設定 ----------------------
  write(6,*) '*** Test of et_module : grid -> spectral -> grid transformation check.'
  write(6,*) '  The result will be printed '
  write(6,*) '  only when the error is larger than ', eps
  write(6,*)

!!$  write(6,*) '  Input wavenumbers of the grid data, k and l :'
!!$  read(5,*) k,l

  yx_Data = sin(k*pi*yx_X) * yx_xi
  et_Data = 0.0 ; et_Data(-k,1) = -0.5
  call check2d(et_yx(yx_Data)-et_Data, eps, &
       'Transform sin(k*pi*X)*T_1(Y)')
  call check2d(yx_et(et_yx(yx_Data))-yx_Data, eps, &
       'Inverse transform sin(k*pi*X)*T_1(Y)')

  yx_Data = cos(k*pi*yx_X) * (2*yx_xi**2 -1)
  et_Data = 0.0 ; et_Data(k,2) = 0.5
  call check2d(et_yx(yx_Data)-et_Data, eps, &
       'Transform cos(k*pi*X)*T_2(Y)')
  call check2d(yx_et(et_yx(yx_Data))-yx_Data, eps,&
       'Inverse transoform cos(k*pi*X)*T_2(Y)')

  yx_Data = sin(k*pi*yx_X) 
  et_Data = 0.0 ; et_Data(-k,0) = -1.0
  call check2d(et_yx(yx_Data)-et_Data, eps, &
       'Transform sin(k*pi*X)*T_0(Y)')
  call check2d(yx_et(et_yx(yx_Data))-yx_Data, eps, &
       'Inverse transform sin(k*pi*X)*T_0(Y)')

  yx_Data = cos(k*pi*yx_X) * (4*yx_xi**3 - 3*yx_xi)
  et_Data = 0.0 ; et_Data(k,3) = 0.5
  call check2d(et_yx(yx_Data)-et_Data, eps, &
       'Transform cos(k*pi*X)*T_3(Y)')
  call check2d(yx_et(et_yx(yx_Data))-yx_Data, eps,&
       'Inverse transform cos(k*pi*X)*T_3(Y)')

  call MessageNotify('M','et_test_transform', &
       'et_module transform function tests succeeded!')

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
             call MessageNotify('E','et_test_transform', &
                  'transform error too large')
          endif
       enddo
    enddo
  end subroutine check2d

end program et_test_transform

