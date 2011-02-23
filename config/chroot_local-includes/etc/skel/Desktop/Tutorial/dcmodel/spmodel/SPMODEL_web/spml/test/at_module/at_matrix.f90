!------------------------------------------------------------------------
! Copyright (c) 2002-2010 SPMODEL Development Group. All rights reserved.
!------------------------------------------------------------------------
!
!ɽ��  at_module �ƥ��ȥץ����
!      1 ��������������
!
!����  2002/01/24  �ݹ�����
!      2002/02/06  �ݹ����� ���ؿ�̾���б�
!      2002/04/10  �ݹ����� ���ؿ�̾���б�
!      2010/01/01  ��������ʿ dc_test ��Ȥ��褦�˽���
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

! ---- 2 ����ʬ������ d^2f/dx^2 = b(x), f=0 at x=xmin,xmax
!
!  ���������б�����ϢΩ������������ӥ����ն��֤Ǻ�������.
!      alu * t_f = t_b
!  alu �� d^2f/dx^2 �Υ����ӥ����ն��֤ˤ�����黻��

  tt_data = 0.0D0
  do k=0,km
     tt_data(k,k)=1.0D0                         ! ����ʬ����ĤŤ�¸�ߤ���
  enddo

  alu = transpose( at_dx_at(at_dx_at(tt_data)) )! ��������
                                                ! ����ʬ���Ф�����ʬ��׻�
  t_b = t_g(cos(pi*g_x/2.0d0))                  ! ����

  tg_data = ag_at(tt_data)
  alu(km-1,:) = tg_data(:,0)                    ! �������(x=xmax)
  alu(km,:)   = tg_data(:,im)                   ! �������(x=xmin)

  t_b(km-1:km)  = 0.0d0                         ! ������

  t_fsol = t_g(-cos(pi*g_x/2.0d0)/pi**2*4.0d0)  ! ����

  call ludecomp(alu,kp)                         ! LU ʬ��
  t_f = lusolve(alu,kp,t_b)                     ! ������

  do k=0,km
     write(6,*) t_f(k), t_fsol(k)
  enddo

  call gropn(1)
  call grfrm
  call usgrph(im+1,real(g_x), real(g_t(t_f)))
  call uulin(im+1,real(g_x), real(g_t(t_fsol)))
  call grcls

! ---- 2 ����ʬ������ d^2f/dx^2 = b(x), df/dx=0 at x=xmin,xmax
!
!  ξü Neumann �ξ��ˤ��ȿ� 0 ��ʬ����ޤ�ʤ�. 
!  ���Ӿ���Ϳ����٤�. 
!  �ʲ����ΰ�ʿ���ͤ�Ϳ������

  ! ���ΰ���ʬ�Ѥ� �����ӥ����շ������Ф��� factor
  ! \int_{-1}^{1}T_n(x)dx =  -2/(n**2-1), n=even
  factor=0
  do k=0,km,2
     factor(k)=-2.0D0/(k**2-1)
  enddo
  factor(0)=factor(0)/2                    ! 0 �����¤ˤϽŤ� 1/2

  tt_data = 0.0D0
  do k=0,km
     tt_data(k,k)=1.0                           ! ����ʬ����ĤŤ�¸�ߤ���
  enddo

  alu = transpose( at_dx_at(at_dx_at(tt_data)) )! ��������
                                                ! ����ʬ���Ф�����ʬ��׻�

  t_b = t_g(sin(pi*g_x/2))                      ! ����

  tg_data = ag_at(at_dx_at(tt_data))
  alu(km-1,:) = tg_data(:,0)                    ! �������(x=xmax)
  alu(km,:)   = tg_data(:,im)                   ! �������(x=xmin)
  t_b(km-1:km)  = 0                             ! ������

  alu(0,:) = factor                             ! �ȿ� 0 ��ʬ�����뼰
  t_b(0) = 0                                    ! ʿ���ͤ� 0 �Ȥ���

  t_fsol = t_g(-sin(pi*g_x/2)/pi**2*4)          ! ����

  call ludecomp(alu,kp)                         ! LU ʬ��
  t_f = lusolve(alu,kp,t_b)                     ! ������

  do k=0,km
     write(6,*) t_f(k), t_fsol(k)
  enddo

  call gropn(1)
  call grfrm
  call usgrph(im+1,real(g_x), real(g_t(t_f)))
  call uulin(im+1,real(g_x), real(g_t(t_fsol)))
  call grcls

! ---- 1 ����ʬ������ x df/dx = b(x), f=0 at x=(xmin+xmax)/2
!
!  ������ x �˰�¸������ : �¶��֤ؤ��Ѵ��ˤ���������
!  
  tt_data = 0.0D0
  do k=0,km
     tt_data(k,k)=1.0                            ! ����ʬ����ĤŤ�¸�ߤ���
  enddo

  alu = transpose( at_ag(spread(g_x,1,km+1)*ag_at(at_dx_at(tt_data))) )   ! ��������

  t_b = t_g(g_x**2)                             ! ����

  tg_data = ag_at(tt_data)
  alu(km,:)   = tg_data(:,im/2)                 ! �������(x=(xmin+xmax)/2)
  t_b(km)     = 0                               ! ������

  t_fsol = t_g(g_x**2/2)                        ! ����

  call ludecomp(alu,kp)                         ! LU ʬ��
  t_f = lusolve(alu,kp,t_b)                     ! ������

  do k=0,km
     write(6,*) t_f(k), t_fsol(k)
  enddo

  call gropn(1)
  call grfrm
  call usgrph(im+1,real(g_x), real(g_t(t_f)))
  call uulin(im+1,real(g_x), real(g_t(t_fsol)))
  call grcls

end program at_matrix

