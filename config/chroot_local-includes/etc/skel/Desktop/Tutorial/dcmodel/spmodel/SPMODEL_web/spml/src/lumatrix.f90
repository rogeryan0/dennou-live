!--
!----------------------------------------------------------------------
!     Copyright (c) 2002-2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!表題  lumatrix : 行列の LU 分解による線形連立方程式の解
!
!      spml/lumatrix モジュールは, LU 分解法により連立 1 次方程式を解くための
!      Fortran90 関数を提供する. 
!
!      他のスペクトル計算用モジュールの中で登場する境界値問題を解くために
!      用いられている. 
!
!      ベクトル計算機を意識して, 同じ次数の複数個の連立1 次方程式 
!
!          A[ij]^(n) X [j]^(n) = B[i]^(n) 
!
!      の解を同時に複数個の右辺ベクトル B[i]^(n)b に対して求めることが
!      できるようになっている.
!
!
!履歴  2002/01/20  竹広真一
!      2002/06/10  竹広真一  ベクトル長問題回避のため lusol2 を使用
!      2005/01/10  竹広真一  msgdmp -> MessageNotify に変更
!      2006/03/04  竹広真一  コメントを RDoc 用に修正
!      2009/01/29  佐々木洋平  コメントを RDoc 用に修正
!      2009/08/06  竹広真一    ludecomp21 用ルーチン OMP 版変更
!
! * ドキュメントは後半部の module 宣言部に記載
!++
subroutine ludecomp21(alu,kp)
  !
  ! ALU(NDIM,NDIM), KP(NDIM)
  ! NDIM x NDIM の行列を LU 分解.
  ! ＬＵ行列は 入力行列に上書きされる.
  !
  use dc_message
  
  real(8), intent(inout) :: alu(:,:)                  ! 入力／ＬＵ行列
  integer, intent(out)   :: kp(size(alu,1))           ! ピボット
  
  if ( size(alu,1) > size(alu,2) ) then
    call MessageNotify('E','ludecomp',&
      'The third dimension is less than the second')
  elseif( size(alu,1) < size(alu,2) ) then
    call MessageNotify('W','ludecomp',&
      'The third dimension is grater than the second')
  endif
  
  !" 行列のＬＵ分解（部分ピボット選択）
  call LUMAK1( alu, kp, size(alu,1) )
  
end subroutine ludecomp21

subroutine ludecomp32(alu,kp)
  !
  ! ALU(JDIM,NDIM,NDIM), KP(JDIM,NDIM)
  ! NDIM x NDIM の行列 JDIM 個を一度に LU 分解.
  ! ＬＵ行列は 入力行列に上書きされる.
  !
  use dc_message
  
  real(8), intent(inout) :: alu(:,:,:)                  ! 入力／ＬＵ行列
  integer, intent(out)   :: kp(size(alu,1),size(alu,2)) ! ピボット
  
  if ( size(alu,2) > size(alu,3) ) then
    call MessageNotify('E','ludecomp',&
      'The third dimension is less than the second')
  elseif( size(alu,2) < size(alu,3) ) then
    call MessageNotify('W','ludecomp',&
      'The third dimension is grater than the second')
  endif
  
  !" 行列のＬＵ分解（部分ピボット選択）
  call LUMAKE( alu, kp, size(alu,1), size(alu,2) )
  
end subroutine ludecomp32

function lusolve211(alu,kp,b)
  !
  ! ALU(NDIM,NDIM), KP(NDIM), B(NDIM)
  ! NDIM x NDIM 型行列の連立方程式
  ! A X = B を 1 個の B に対して計算する. 
  !
  use dc_message
  
  real(8), intent(in)  :: alu(:,:)              ! 入力／ＬＵ行列
  integer, intent(in)  :: kp(:)                 ! ピボット
  real(8), intent(in)  :: b(:)                  ! 右辺ベクトル
  
  real(8) :: lusolve211(size(b))                   ! 解
  
  lusolve211 = b
  call LUSOL2( lusolve211, alu , kp, &             !" ＬＵ分解による解の計算
    1, size(b) )
  
end function lusolve211

function lusolve212(alu,kp,b)
  !
  ! ALU(NDIM,NDIM), KP(NDIM), B(JDIM,NDIM)
  ! NDIM x NDIM 型行列の連立方程式
  ! A X = B を JDIM 個の B に対して計算する. 
  !
  use dc_message
  
  real(8), intent(in)  :: alu(:,:)              ! 入力／ＬＵ行列
  integer, intent(in)  :: kp(:)                 ! ピボット
  real(8), intent(in)  :: b(:,:)                ! 右辺ベクトル
  
  real(8) :: lusolve212(size(b,1),size(b,2))       ! 解
  
  lusolve212 = b
  call LUSOL2( lusolve212, alu , kp, &             !" ＬＵ分解による解の計算
    size(b,1), size(b,2) )
  
end function lusolve212

function lusolve322(alu,kp,b)
  !
  ! ALU(JDIM,NDIM,NDIM), KP(JDIM,NDIM), B(JDIM,NDIM)
  ! NDIM x NDIM 型行列を JDIM 個並べた連立方程式
  ! A X = B をひとつの B の並びに対して計算する. 
  !
  use dc_message
  
  real(8), intent(in)  :: alu(:,:,:)                   ! 入力／ＬＵ行列
  integer, intent(in)  :: kp(:,:)                      ! ピボット
  real(8), intent(in)  :: b(:,:)                       ! 右辺ベクトル
  
  real(8) :: lusolve322(size(b,1),size(b,2))             ! 解
  
  lusolve322 = b
  call LUSOLV( lusolve322, alu , kp, &           !" ＬＵ分解による解の計算
    1, size(b,1), size(b,2) )
  
end function lusolve322

function lusolve323(alu,kp,b)
  !
  ! ALU(JDIM,NDIM,NDIM), KP(JDIM,NDIM), B(IDIM,JDIM,NDIM)
  ! NDIM x NDIM 型行列を JDIM 個並べた連立方程式
  ! A X = B を IDIM 個の B に対して計算する. 
  !
  use dc_message
  
  real(8), intent(in)  :: alu(:,:,:)                   ! 入力／ＬＵ行列
  integer, intent(in)  :: kp(:,:)                      ! ピボット
  real(8), intent(in)  :: b(:,:,:)                     ! 右辺ベクトル
  
  real(8) :: lusolve323(size(b,1),size(b,2),size(b,3)) ! 解
  
  lusolve323 = b
  call LUSOLV( lusolve323, alu , kp, &           !" ＬＵ分解による解の計算
    size(b,1), size(b,2), size(b,3) )
  
end function lusolve323

module lumatrix
  !
  != lumatrix
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: lumatrix.f90,v 1.9 2009-08-06 12:41:58 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/lumatrix モジュールは, LU 分解法により連立 1 次方程式を解くための
  ! Fortran90 関数を提供する. 
  !
  ! 他のスペクトル計算用モジュールの中で登場する境界値問題を解くために
  ! 用いられている. 
  !
  ! ベクトル計算機を意識して, 同じ次数の複数個の連立1 次方程式 
  !
  !     A[ij]^(n) X [j]^(n) = B[i]^(n) 
  !
  ! の解を同時に複数個の右辺ベクトル B[i]^(n)b に対して求めることが
  ! できるようになっている.
  !
  !== 変数・手続き群の要約
  !
  ! LUDecomp    :: 行列の LU 分解を行う
  ! LUSolve     :: 連立 1 次方程式の解を求める
  !
  private 
  public LUDecomp, LUSolve

  interface LUDecomp
     !
     !=== 与えられた行列の LU 分解を行い, ピボットを格納する.
     !
     ! * LU 分解された結果は Alu に上書きされる. 
     !   そのピボット情報は kp に格納される.
     !
     ! * LUSolve を用いる前にこのサブルーチンを呼んで Alu と kp を
     !   計算しておく.
     !
     ! * 入力する行列とピボット用配列の次元によって内部でサブルーチンを
     !   使い分けている. ユーザーインターフェースは共通であるので
     !   下部ルーチンである ludecomp21, ludecomp32 を呼ぶ必要はない.
     !
     !=== 引数と結果の型
     !
     ! * Alu が 2 次元配列(与える係数行列が 1 つ)の場合
     !
     !     ! ALU(NDIM,NDIM), KP(NDIM)
     !     ! NDIM x NDIM の行列を LU 分解.
     !     ! ＬＵ行列は 入力行列に上書きされる.
     !
     !     real(8), intent(inout) :: alu(:,: )         ! 入力／ＬＵ行列
     !     integer, intent(out)   :: kp(size(alu,1))   ! ピボット
     !
     !
     ! * Alu が 3 次元配列(与える係数行列が複数)の場合
     !
     !     ! ALU(JDIM,NDIM,NDIM), KP(JDIM,NDIM)
     !     ! NDIM x NDIM の行列 JDIM 個を一度に LU 分解.
     !     ! ＬＵ行列は 入力行列に上書きされる.
     !
     !     real(8), intent(inout) :: alu(:,:,:)      ! 入力／ＬＵ行
     !     integer, intent(out)   :: kp(size(alu,1),size(alu,2)) ! ピボット
     !
     !
     subroutine ludecomp21(alu,kp)
       !
       ! ALU(NDIM,NDIM), KP(NDIM)
       ! NDIM x NDIM の行列を LU 分解.
       ! ＬＵ行列は 入力行列に上書きされる.
       !
       real(8), intent(inout) :: alu(:,: )                 ! 入力／ＬＵ行列
       integer, intent(out)   :: kp(size(alu,1))           ! ピボット
     end subroutine ludecomp21

     subroutine ludecomp32(alu,kp)
       !
       ! ALU(JDIM,NDIM,NDIM), KP(JDIM,NDIM)
       ! NDIM x NDIM の行列 JDIM 個を一度に LU 分解.
       ! ＬＵ行列は 入力行列に上書きされる.
       !
       real(8), intent(inout) :: alu(:,:,:)                  ! 入力／ＬＵ行列
       integer, intent(out)   :: kp(size(alu,1),size(alu,2)) ! ピボット
     end subroutine ludecomp32
  end interface

  interface LUSolve
     !
     ! 連立 1 次方程式の解を求める
     !
     !  * LUSolve を用いる前に LUDecompを呼んで Alu を LU 分解し, 
     !    ピボット情報 kp を計算しておかねばならない.
     !
     !  * 入力する行列とピボット用配列の次元によって内部でサブルーチンを
     !    使い分けている. ユーザーインターフェースは共通であるので
     !    下部ルーチンである lusolve??? を呼ぶ必要はない.
     !
     ! 引数と結果の型
     !
     !  *  Alu が 2 次元配列(与える係数行列が 1 つ), 
     !     b が 1 次元配列(与える右辺ベクトルが 1 つ)の場合
     !
     !     ! ALU(NDIM,NDIM), KP(NDIM), B(NDIM)
     !     ! NDIM x NDIM 型行列の連立方程式
     !     ! A X = B を 1 個の B に対して計算する.
     !
     !     real(8), intent(in)  :: alu(:,:)              ! 入力／ＬＵ行列
     !     integer, intent(in)  :: kp(:)                 ! ピボット
     !     real(8), intent(in)  :: b(:)                  ! 右辺ベクトル
     !
     !     real(8) :: lusolve(size(b))                   ! 解
     !
     !  * Alu が 2 次元配列(与える係数行列が 1 つ), 
     !    b が 2 次元配列(与える右辺ベクトルが複数)の場合
     !
     !     ! ALU(NDIM,NDIM), KP(NDIM), B(JDIM,NDIM)
     !     ! NDIM x NDIM 型行列の連立方程式
     !     ! A X = B を JDIM 個の B に対して計算する.
     !
     !     real(8), intent(in)  :: alu(:,:)              ! 入力／ＬＵ行列
     !     integer, intent(in)  :: kp(:)                 ! ピボット
     !     real(8), intent(in)  :: b(:,:)                ! 右辺ベクトル
     !
     !     real(8) :: lusolve(size(b,1),size(b,2))       ! 解
     !
     !
     !  * Alu が 3 次元配列(与える係数行列が複数), 
     !    b が 2 次元配列(与える右辺ベクトルが 1 つ)の場合
     !
     !     ! ALU(JDIM,NDIM,NDIM), KP(JDIM,NDIM), B(JDIM,NDIM)
     !     ! NDIM x NDIM 型行列を JDIM 個並べた連立方程式
     !     ! A X = B をひとつの B の並びに対して計算する.
     !
     !     real(8), intent(in)  :: alu(:,:,:)            ! 入力／ＬＵ行列
     !     integer, intent(in)  :: kp(:,:)               ! ピボット
     !     real(8), intent(in)  :: b(:,:)                ! 右辺ベクトル
     !
     !     real(8) :: lusolve(size(b,1),size(b,2))             ! 解
     !
     !
     !  * Alu が 3 次元配列(与える係数行列が複数), 
     !    b が 3 次元配列(与える右辺ベクトルが複数)の場合
     !
     !     ! ALU(JDIM,NDIM,NDIM), KP(JDIM,NDIM), B(IDIM,JDIM,NDIM)
     !     ! NDIM x NDIM 型行列を JDIM 個並べた連立方程式
     !     ! A X = B を IDIM 個の B に対して計算する.
     !
     !     real(8), intent(in)  :: alu(:,:,:)                ! 入力／ＬＵ行列
     !     integer, intent(in)  :: kp(:,:)                   ! ピボット
     !     real(8), intent(in)  :: b(:,:,:)                  ! 右辺ベクトル
     !
     !     real(8) :: lusolve(size(b,1),size(b,2),size(b,3)) ! 解
     !
     !
     function lusolve211(alu,kp,b)
       !
       ! ALU(NDIM,NDIM), KP(NDIM), B(NDIM)
       ! NDIM x NDIM 型行列の連立方程式
       ! A X = B を IDIM 個の B に対して計算する. 
       ! 解は右辺の入力ベクトルに上書きされる
       !
       real(8), intent(in)  :: alu(:,:)              ! 入力／ＬＵ行列
       integer, intent(in)  :: kp(:)                 ! ピボット
       real(8), intent(in)  :: b(:)                  ! 右辺ベクトル
       real(8) :: lusolve211(size(b))                 ! 解
     end function lusolve211

     function lusolve212(alu,kp,b)
       !
       ! ALU(NDIM,NDIM), KP(NDIM), B(JDIM,NDIM)
       ! NDIM x NDIM 型行列の連立方程式
       ! A X = B を IDIM 個の B に対して計算する. 
       !
       real(8), intent(in)  :: alu(:,:)              ! 入力／ＬＵ行列
       integer, intent(in)  :: kp(:)                 ! ピボット
       real(8), intent(in)  :: b(:,:)                ! 右辺ベクトル

       real(8) :: lusolve212(size(b,1),size(b,2))       ! 解

     end function lusolve212

     function lusolve322(alu,kp,b)
       !
       ! ALU(JDIM,NDIM,NDIM), KP(JDIM,NDIM), B(JDIM,NDIM)
       ! NDIM x NDIM 型行列を JDIM 個並べた連立方程式
       ! A X = B を B に対して計算する. 
       !
       real(8), intent(in)  :: alu(:,:,:)                   ! 入力／ＬＵ行列
       integer, intent(in)  :: kp(:,:)                      ! ピボット
       real(8), intent(in)  :: b(:,:)                       ! 右辺ベクトル

       real(8) :: lusolve322(size(b,1),size(b,2))             ! 解
     end function lusolve322

     function lusolve323(alu,kp,b)
       !
       ! ALU(JDIM,NDIM,NDIM), KP(JDIM,NDIM), B(IDIM,JDIM,NDIM)
       ! NDIM x NDIM 型行列を JDIM 個並べた連立方程式
       ! A X = B を IDIM 個の B に対して計算する. 
       !
       real(8), intent(in)  :: alu(:,:,:)                   ! 入力／ＬＵ行列
       integer, intent(in)  :: kp(:,:)                      ! ピボット
       real(8), intent(in)  :: b(:,:,:)                     ! 右辺ベクトル

       real(8) :: lusolve323(size(b,1),size(b,2),size(b,3)) ! 解

     end function lusolve323
  end interface

end module lumatrix


