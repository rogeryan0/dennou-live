!--
!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  ガンマ関数, 階乗
!
!履歴  2009/02/06   竹広真一 大浦氏謹製関数 dlgamma 版
!                   http://www.kurims.kyoto-u.ac.jp/~ooura/gamerf.tar.gz
!
!++
!
!= gamma
!
! Authors:: Shin-ichi Takehiro, Youhei SASAKI
! Version:: $Id: gamma.f90,v 1.5 2009-02-27 05:51:08 uwabami Exp $
!
!== 概要
!
! このファイルは gamma 関数に関する計算用の関数を提供する
!
!== 関数
!
! factrl  :: 与えられた整数に対する階乗(ガンマ関数)の値を返す
! gammaln :: 与えられた正の実数に対する階乗の対数(ガンマ関数の対数)の値を返す
!
function gammaln(xx)
  !
  ! Returns the vaule ln[Γ(x)] for x >= 0
  !
  real(8) ::  gammaln, xx
  real(8) ::  dlgamma
  external dlgamma

  gammaln = dlgamma(xx)

  return
end function gammaln

function factrl(n)
  !
  ! Returns the value n! as a floating point number
  !
  integer, intent(IN) :: n
  real(8)             :: factrl
  real(8)             :: gammaln
  external gammaln

  factrl=exp(gammaln(n+1.0d0))

  return

end function factrl
