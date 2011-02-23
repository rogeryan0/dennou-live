!----------------------------------------------------------------------
!     Copyright (c) 2005 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  ee_module テストプログラム (正逆変換)
!
!履歴  2005/07/19  竹広真一
!
program ee_test_openmp

  use ee_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=512, jm=512             ! 格子点の設定(X,Y)
  integer, parameter :: km=170, lm=170             ! 切断波数の設定(X,Y)

 !---- 変数 ----
  real(8)            :: yx_Data0(0:jm-1,0:im-1)    ! 格子データ
  real(8)            :: yx_Data1(0:jm-1,0:im-1)    ! 格子データ

  integer            :: n=0

 !---- 座標変数など ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0

  integer, parameter :: k=3,l=2
  integer, parameter :: nmax=100

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! 判定誤差

 !---------------- 座標値の設定 ---------------------
  call ee_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! スペクトル初期化

 !------------------- 初期値設定 ----------------------
  write(6,*) '*** Test of ee_module : grid -> spectral -> grid transformation check.'
  write(6,*) '  The result will be printed '
  write(6,*) '  only when the error is larger than ', eps


  yx_Data0 = sin(k*pi*yx_X) * sin(l*pi*yx_Y)

  yx_Data1 = yx_Data0
  do n=1,nmax
     yx_Data1=yx_ee(ee_yx(yx_Data1))
  enddo

  call check2d(yx_Data1-yx_Data0, eps, 'sin(k*pi*X)*sin(l*pi*Y)')

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
          endif
       enddo
    enddo
  end subroutine check2d

end program ee_test_openmp

