!--
!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  ����޴ؿ�, ����
!
!����  2009/02/06   �ݹ����� �籺������ؿ� dlgamma ��
!                   http://www.kurims.kyoto-u.ac.jp/~ooura/gamerf.tar.gz
!
!++
!
!= gamma
!
! Authors:: Shin-ichi Takehiro, Youhei SASAKI
! Version:: $Id: gamma.f90,v 1.5 2009-02-27 05:51:08 uwabami Exp $
!
!== ����
!
! ���Υե������ gamma �ؿ��˴ؤ���׻��Ѥδؿ����󶡤���
!
!== �ؿ�
!
! factrl  :: Ϳ����줿�������Ф��볬��(����޴ؿ�)���ͤ��֤�
! gammaln :: Ϳ����줿���μ¿����Ф��볬����п�(����޴ؿ����п�)���ͤ��֤�
!
function gammaln(xx)
  !
  ! Returns the vaule ln[��(x)] for x >= 0
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
