!----------------------------------------------------------------------
!     Copyright (c) 2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  at_af_galerkin_MM_test1d
!
!      �����ӥ����աݥ��顼����ˡ
!      �ǥ��ꥯ�졦�Υ��ޥ󺮹維������ѥ⥸�塼��ƥ��ȥץ����(2����)
!          cfdx1_xmax f'(:,i=0)  + cfdx0_xmax f(:,i=0)  = 0,
!          cfdx1_xmin f'(:,i=im) + cfdx0_xmin f(:,i=im) = 0,
!
!      �� : cfdx1_xmax=1, cfdx0_xmax =alpha, cfdx1_xmin=1, cfdx0_xmin =-alpha
!
!           f(x) = cos(lambda*t)
!              t=(xmax+xmin)/2 + (xmax-xmin)/2 * t
!              lambda tan(lambda) = alpha*(xmax-xmin)/2
!
!����  2006/01/04  �ݹ�����  ��������
!      2006/01/23  �ݹ�����  �⥸�塼��̾�ѹ���ȼ������
!      2010/01/01  ��������ʿ dc_test ��Ȥ��褦�˽���
!
program at_af_galerkin_MM_test2d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use at_af_galerkin_MM, only: at_af_galerkin_MM_Initial, &
                               af_at, at_af, af_ag, ag_af
  use at_module
  use dc_message

  implicit none

  integer, parameter :: jm=10                  ! 1 �����ܳʻ�����
  integer, parameter :: im=64                  ! �ʻ�����              
  integer, parameter :: km=64                  ! �����ӥ����������ȿ�  
  real(8), parameter :: xmin=0.0D0, xmax=1.0D0 ! �׻��ΰ�

  real(8), parameter :: alpha=2.0D0            ! ������ﷸ��

  real(8) :: ag_data(jm,0:im)
  real(8) :: ag_data_orig(jm,0:im)

  real(8) :: lambda(jm)                          ! ���ϲ򷸿�
  integer :: j
  ! Ƚ�������
  integer, parameter :: check_digits = 5
  integer, parameter :: ignore = - 6

  lambda = InvXtanX(alpha*(xmax-xmin)/2.0D0,jm)

  call MessageNotify('M','at_af_galerkin_MM_test2d', &
    & '2D Mixed B.C.')

  call at_Initial(im,km,xmin,xmax)
  !--- �ǥ��ꥯ�졦�Υ��ޥ󺮹維����� ---
  call at_af_galerkin_MM_Initial(im,km,        &
       cfdx0_xmax=alpha, cfdx1_xmax=1.0D0, &
       cfdx0_xmin=-alpha,  cfdx1_xmin=1.0D0    )

  do j=1,jm
     ag_data(j,:) = cos(lambda(j)*(2.0D0/(xmax-xmin)*(g_X-(xmax+xmin)/2.0D0)))
  enddo
  ag_data_orig = ag_data
  ag_data = ag_af(af_ag(ag_data))

  call check2d(ag_data, ag_data_orig, &
    & 'Grid -> Galerkin -> Grid ')

!  �������
!  ag_data = ag_af(af_Dx_af(af_Dx_af(af_data)))

!  �褤�� : ��ʬ�ͤϥ����ӥ����շ������ݻ�����٤�
  ag_data = ag_af(af_at(at_Dx_at(at_Dx_at(at_af(af_ag(ag_data))))))

  do j=1,jm
    ag_data_orig(j,:) = -(lambda(j)/(xmax-xmin)*2)**2 * ag_data_orig(j,:) 
  end do

  call check2d(ag_data, ag_data_orig, &
    & 'Grid -> Galerkin ->(Dx)^2 ->  Grid ')

  call MessageNotify('M','at_af_galerkin_MM_test2d', &
    & '2D Mixed B.C. succeeded')


contains
  subroutine check2d(var, true, funcname)
    real(8) :: var(:,:)
    real(8) :: true(:,:)
    character(len=*), optional :: funcname

    call AssertEqual(&
      & message = funcname,   &
      & answer  = true,       &
      & check   = var,        &
      significant_digits = check_digits, ignore_digits = ignore   &
      )
  end subroutine check2d

  !
  ! x*tan(x)=val �β�����
  !
  function InvXtanX(val,n)
    real(8), intent(IN) :: val                ! x*tan(x)=val > 0
    integer, intent(IN) :: n                  ! �����θĿ�
    real(8)             :: InvXtanX(n)
    real(8), parameter  :: eps = 1.0D-14     ! �������

    real(8) :: pi = 3.1415926535897932385D0
    integer :: i
    real(8) :: xs, xl, xm
    real(8) :: ValS, ValL, ValM

    do i=1,n
       xs=(i-1)*pi
       xl=pi/2.0D0  + (i-1.0d0)*pi - eps

       ValS = xs*tan(xs)-val ; ValL = xl*tan(xl)-val
       if ( ValS * ValL .GT. 0.0D0 ) &
            call MessageNotify('E','InvXtanX',&
            'Initial values of ValS and ValL are the same sign.')
1000   xm = (xs + xl)/2.0d0
       ValM = xm*tan(xm) - val

       if ( ValS * ValM .GT. 0.0D0 ) then
          xs = xm ; ValS=xs*tan(xs)-val
       else
          xl = xm ; ValL=xl*tan(xl)-val
       endif

       if ( abs(xl-xs) .lt. eps ) then
          InvXtanX(i) = xm
          goto 99
       endif

       goto 1000

99  end do
  end function InvXtanX

end program at_af_galerkin_MM_test2d
