!------------------------------------------------------------------------
! Copyright (c) 2002-2010 SPMODEL Development Group. All rights reserved.
!------------------------------------------------------------------------
!
!表題  at_module テストプログラム
!      1 次元境界値問題
!
!履歴  2002/01/24  竹広真一
!      2002/02/06  竹広真一 新関数名に対応
!      2002/04/10  竹広真一 新関数名に対応
!      2010/01/01  佐々木洋平 dc_test を使うように修正
!
program at_matrix

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use at_module
  use lumatrix
  use dcl

  implicit none
  integer, parameter :: im=8, km=5, nm=4
  real(8), parameter :: xmin=-1.0D0, xmax=1.0D0

  real(8), dimension(0:km,0:km) :: tt_data

  real(8), dimension(0:km,0:im) :: tg_data

  real(8), dimension(0:km,0:km) :: alu
  real(8), dimension(0:km) :: t_b, t_f, t_fsol
  integer, dimension(0:km) :: kp
  real(8), parameter       :: pi=3.1415926535897932385D0

  real(8), dimension(0:km) :: factor

  integer :: k, i, m

  call MessageNotify('M','at_matrix', &
       & 'test of at_matrix.')

  call at_initial(im,km,xmin,xmax)

! ---- 2 階微分方程式 d^2f/dx^2 = b(x), f=0 at x=xmin,xmax
!
!  方程式に対応する連立方程式をチェビシェフ空間で作成する.
!      alu * t_f = t_b
!  alu は d^2f/dx^2 のチェビシェフ空間における演算子

  tt_data = 0.0D0
  do k=0,km
     tt_data(k,k)=1.0D0                         ! 各成分が一つづつ存在する
  enddo

  alu = transpose( at_dx_at(at_dx_at(tt_data)) )! 係数行列
                                                ! 各成分に対する微分を計算
  t_b = t_g(cos(pi*g_x/2.0d0))                  ! 右辺

  tg_data = ag_at(tt_data)
  alu(km-1,:) = tg_data(:,0)                    ! 境界条件(x=xmax)
  alu(km,:)   = tg_data(:,im)                   ! 境界条件(x=xmin)

  t_b(km-1:km)  = 0.0d0                         ! 境界値

  t_fsol = t_g(-cos(pi*g_x/2.0d0)/pi**2*4.0d0)  ! 正解

  call ludecomp(alu,kp)                         ! LU 分解
  t_f = lusolve(alu,kp,t_b)                     ! 解を求める

  do k=0,km
     write(6,*) t_f(k), t_fsol(k)
  enddo

  call gropn(1)
  call grfrm
  call usgrph(im+1,real(g_x), real(g_t(t_f)))
  call uulin(im+1,real(g_x), real(g_t(t_fsol)))
  call grcls

! ---- 2 階微分方程式 d^2f/dx^2 = b(x), df/dx=0 at x=xmin,xmax
!
!  両端 Neumann の場合には波数 0 成分が定まらない. 
!  別途条件を与えるべし. 
!  以下は領域平均値を与える場合

  ! 全領域積分用の チェビシェフ係数に対する factor
  ! \int_{-1}^{1}T_n(x)dx =  -2/(n**2-1), n=even
  factor=0
  do k=0,km,2
     factor(k)=-2.0D0/(k**2-1)
  enddo
  factor(0)=factor(0)/2                    ! 0 次の和には重み 1/2

  tt_data = 0.0D0
  do k=0,km
     tt_data(k,k)=1.0                           ! 各成分が一つづつ存在する
  enddo

  alu = transpose( at_dx_at(at_dx_at(tt_data)) )! 係数行列
                                                ! 各成分に対する微分を計算

  t_b = t_g(sin(pi*g_x/2))                      ! 右辺

  tg_data = ag_at(at_dx_at(tt_data))
  alu(km-1,:) = tg_data(:,0)                    ! 境界条件(x=xmax)
  alu(km,:)   = tg_data(:,im)                   ! 境界条件(x=xmin)
  t_b(km-1:km)  = 0                             ! 境界値

  alu(0,:) = factor                             ! 波数 0 成分を定める式
  t_b(0) = 0                                    ! 平均値を 0 とする

  t_fsol = t_g(-sin(pi*g_x/2)/pi**2*4)          ! 正解

  call ludecomp(alu,kp)                         ! LU 分解
  t_f = lusolve(alu,kp,t_b)                     ! 解を求める

  do k=0,km
     write(6,*) t_f(k), t_fsol(k)
  enddo

  call gropn(1)
  call grfrm
  call usgrph(im+1,real(g_x), real(g_t(t_f)))
  call uulin(im+1,real(g_x), real(g_t(t_fsol)))
  call grcls

! ---- 1 階微分方程式 x df/dx = b(x), f=0 at x=(xmin+xmax)/2
!
!  係数が x に依存する場合 : 実空間への変換により行列を作成
!  
  tt_data = 0.0D0
  do k=0,km
     tt_data(k,k)=1.0                            ! 各成分が一つづつ存在する
  enddo

  alu = transpose( at_ag(spread(g_x,1,km+1)*ag_at(at_dx_at(tt_data))) )   ! 係数行列

  t_b = t_g(g_x**2)                             ! 右辺

  tg_data = ag_at(tt_data)
  alu(km,:)   = tg_data(:,im/2)                 ! 境界条件(x=(xmin+xmax)/2)
  t_b(km)     = 0                               ! 境界値

  t_fsol = t_g(g_x**2/2)                        ! 正解

  call ludecomp(alu,kp)                         ! LU 分解
  t_f = lusolve(alu,kp,t_b)                     ! 解を求める

  do k=0,km
     write(6,*) t_f(k), t_fsol(k)
  enddo

  call gropn(1)
  call grfrm
  call usgrph(im+1,real(g_x), real(g_t(t_f)))
  call uulin(im+1,real(g_x), real(g_t(t_fsol)))
  call grcls

end program at_matrix

