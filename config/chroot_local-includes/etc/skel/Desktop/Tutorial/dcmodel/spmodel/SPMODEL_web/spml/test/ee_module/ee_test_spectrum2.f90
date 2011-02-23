!----------------------------------------------------------------------
!     Copyright (c) 2007--2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  ee_module �ƥ��ȥץ���� (�ѥ���ڥ��ȥ�׻�)
!
!����  2007/11/09  �ݹ����� 
!      2008/05/10  �ݹ�����  ������ʣ����������å�
!      
!
program ee_test_spectrum

  use dc_message, only : MessageNotify
  use ee_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=32            ! �ʻ���������(X,Y)
  integer, parameter :: km=10, lm=10              ! �����ȿ�������(X,Y)

  integer, parameter :: im2=64, jm2=64            ! �ʻ���������(X,Y)
  integer, parameter :: km2=21, lm2=21            ! �����ȿ�������(X,Y)

 !---- �ѿ� ----
  real(8)            :: yx_Data(0:jm-1,0:im-1)    ! �ʻҥǡ���
  real(8)            :: ee_Data(-lm:lm,-km:km)    ! ���ڥ��ȥ�ǡ���

  real(8)            :: yx_Data2(0:jm2-1,0:im2-1)      ! �ʻҥǡ���
  real(8)            :: ee_Data2(-lm2:lm2,-km2:km2)    ! ���ڥ��ȥ�ǡ���

  integer            :: k=2, l=3

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! Ƚ���

  integer            :: id1, id2

  call MessageNotify('M','ee_test_spectrum', &
       'ee_module spectrum function tests')

 !---------------- ��ɸ�ͤ����� ---------------------
  call ee_initial(im,jm,km,lm,xmin,xmax,ymin,ymax,id1)        ! ���ڥ��ȥ�����
  call ee_initial(im2,jm2,km2,lm2,xmin,xmax,ymin,ymax,id2)    ! ���ڥ��ȥ�����

 !------------------- ��������� ----------------------
  write(6,*) '*** Test of ee_module : derivative function check.'
  write(6,*) '  The result will be printed '
  write(6,*) '  only when the error is larger than ', eps
  write(6,*)

!!$  write(6,*) '  Input wavenumbers of the grid data, k and l :'
!!$  read(5,*) k,l
  write(6,*) '  k,l = ', k,l

  ! id1
  write(*,*) 'for id1'
  call ee_ChangeResolutionDomain(id1)

  yx_Data = sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  ee_Data = 0
  ee_Data(l,k) = ((k*pi)**2 + (l*pi)**2)*0.25**2/2
  ee_Data(-l,k) = ((k*pi)**2 + (l*pi)**2)*0.25**2/2
  ee_Data(l,-k) = ((k*pi)**2 + (l*pi)**2)*0.25**2/2
  ee_Data(-l,-k) = ((k*pi)**2 + (l*pi)**2)*0.25**2/2
  call check2d(ee_EnergyFromStreamfunc_ee(ee_yx(yx_Data))-ee_Data, eps, &
       'Energy spectrum sin(k*pi*X)*sin(l*pi*Y)')
  call check0d(sum(ee_EnergyFromStreamfunc_ee(ee_yx(yx_Data))) &
                 *(xmax-xmin)*(ymax-ymin) - ((k*pi)**2 + (l*pi)**2)/2, &
                 eps, 'total energy sin(k*pi*X)*sin(l*pi*Y)')

  ee_Data = 0
  ee_Data(l,k) = ((k*pi)**2 + (l*pi)**2)**2*0.25**2/2
  ee_Data(-l,k) = ((k*pi)**2 + (l*pi)**2)**2*0.25**2/2
  ee_Data(l,-k) = ((k*pi)**2 + (l*pi)**2)**2*0.25**2/2
  ee_Data(-l,-k) = ((k*pi)**2 + (l*pi)**2)**2*0.25**2/2
  call check2d(ee_EnstrophyFromStreamfunc_ee(ee_yx(yx_Data))-ee_Data, eps, &
       'Enstrophy spectrum sin(k*pi*X)*sin(l*pi*Y)')
  call check0d(sum(ee_EnstrophyFromStreamfunc_ee(ee_yx(yx_Data))) &
                 *(xmax-xmin)*(ymax-ymin) - ((k*pi)**2 + (l*pi)**2)**2/2, &
                 eps, 'total enstrophy sin(k*pi*X)*sin(l*pi*Y)')

  ! id2
  write(*,*) 'for id2'
  call ee_ChangeResolutionDomain(id2)

  yx_Data2 = sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  ee_Data2 = 0
  ee_Data2(l,k) = ((k*pi)**2 + (l*pi)**2)*0.25**2/2
  ee_Data2(-l,k) = ((k*pi)**2 + (l*pi)**2)*0.25**2/2
  ee_Data2(l,-k) = ((k*pi)**2 + (l*pi)**2)*0.25**2/2
  ee_Data2(-l,-k) = ((k*pi)**2 + (l*pi)**2)*0.25**2/2
  call check2d(ee_EnergyFromStreamfunc_ee(ee_yx(yx_Data2))-ee_Data2, eps, &
       'Energy spectrum sin(k*pi*X)*sin(l*pi*Y)')
  call check0d(sum(ee_EnergyFromStreamfunc_ee(ee_yx(yx_Data2))) &
                 *(xmax-xmin)*(ymax-ymin) - ((k*pi)**2 + (l*pi)**2)/2, &
                 eps, 'total energy sin(k*pi*X)*sin(l*pi*Y)')

  ee_Data2 = 0
  ee_Data2(l,k) = ((k*pi)**2 + (l*pi)**2)**2*0.25**2/2
  ee_Data2(-l,k) = ((k*pi)**2 + (l*pi)**2)**2*0.25**2/2
  ee_Data2(l,-k) = ((k*pi)**2 + (l*pi)**2)**2*0.25**2/2
  ee_Data2(-l,-k) = ((k*pi)**2 + (l*pi)**2)**2*0.25**2/2
  call check2d(ee_EnstrophyFromStreamfunc_ee(ee_yx(yx_Data2))-ee_Data2, eps, &
       'Enstrophy spectrum sin(k*pi*X)*sin(l*pi*Y)')
  call check0d(sum(ee_EnstrophyFromStreamfunc_ee(ee_yx(yx_Data2))) &
                 *(xmax-xmin)*(ymax-ymin) - ((k*pi)**2 + (l*pi)**2)**2/2, &
                 eps, 'total enstrophy sin(k*pi*X)*sin(l*pi*Y)')

  call MessageNotify('M','ee_test_spectrum', &
       'ee_module spectrum function tests succeeded!')

 stop
contains

  subroutine check2d(var,eps,funcname) ! �����ͤ� eps �ʾ�� var �����Ǥ����
    real(8) :: var(:,:)                ! Ƚ�ꤹ������
    real(8) :: eps                     ! ��
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
             call MessageNotify('E','ee_test_derivative', &
                  'derivative error too large')
          endif
       enddo
    enddo
  end subroutine check2d

  subroutine check0d(var,eps,funcname)   ! var �������ͤ� eps �ʾ���Ƚ���
    real(8) :: var                       ! Ƚ�ꤹ������
    real(8) :: eps                       ! ��
    real(8) :: vartmp(1,1)               ! 
    character(len=*), optional :: funcname

    vartmp(1,1) = var
    if ( present(funcname) ) then
       call check2d(vartmp,eps,funcname)
    else
       call check2d(vartmp,eps)
    endif
  end subroutine check0d

end program ee_test_spectrum
