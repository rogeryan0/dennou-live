!----------------------------------------------------------------------
!     Copyright (c) 2005-2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  ee_module �ƥ��ȥץ���� (�����Ѵ�)
!
!����  2005/07/19  �ݹ�����
!      2007/11/09  �ݹ�����  ���顼��å������ɲ�
!      2008/05/10  ��߷���� => �ݹ�����  ������ʣ����������å�
!
!����  �ɤ��⥹�ڥ��ȥ���ʬ�����٤Ƶ����ˤʤäƤ���褦�˻פ���. 
!
program ee_test_transform2

  use dc_message, only : MessageNotify
  use ee_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=32            ! �ʻ���������(X,Y)
  integer, parameter :: km=10, lm=10              ! �����ȿ�������(X,Y)

  integer, parameter :: im2=64, jm2=64            ! �ʻ���������(X,Y)
  integer, parameter :: km2=21, lm2=21              ! �����ȿ�������(X,Y)

 !---- �ѿ� ----
  real(8)            :: yx_Data(0:jm-1,0:im-1)    ! �ʻҥǡ���
  real(8)            :: ee_Data(-lm:lm,-km:km)    ! ���ڥ��ȥ�ǡ���

  real(8)            :: yx_Data2(0:jm2-1,0:im2-1)    ! �ʻҥǡ���
  real(8)            :: ee_Data2(-lm2:lm2,-km2:km2)    ! ���ڥ��ȥ�ǡ���

  real(8)            :: ee_Data3(-lm:lm,-km:km)    ! ���ڥ��ȥ�ǡ���

  integer            :: k=2,l=5

  integer            :: id1, id2, id3

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! Ƚ���

  call MessageNotify('M','ee_test_transform', &
       'ee_module transform function tests')

 !---------------- ��ɸ�ͤ����� ---------------------
  call ee_initial(im,jm,km,lm,xmin,xmax,ymin,ymax,id1)    ! ���ڥ��ȥ�����
  call ee_initial(im2,jm2,km2,lm2,xmin,xmax,ymin,ymax,id2)    ! ���ڥ��ȥ�����
  call ee_initial(im2,jm2,km,lm,xmin,xmax,ymin,ymax,id3)    ! ���ڥ��ȥ�����
  call ee_initial(im2,jm2,km,lm,xmin,xmax,ymin,ymax)
  call ee_initial(im2,jm2,km,lm,xmin,xmax,ymin,ymax)
  call ee_initial(im2,jm2,km,lm,xmin,xmax,ymin,ymax)
  call ee_initial(im2,jm2,km,lm,xmin,xmax,ymin,ymax)
  call ee_initial(im2,jm2,km,lm,xmin,xmax,ymin,ymax)
  call ee_initial(im2,jm2,km,lm,xmin,xmax,ymin,ymax)
  call ee_initial(im2,jm2,km,lm,xmin,xmax,ymin,ymax)
  call ee_initial(im2,jm2,km,lm,xmin,xmax,ymin,ymax)

 !------------------- ��������� ----------------------
  write(6,*) '*** Test of ee_module : grid -> spectral -> grid transformation check.'
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
  ee_Data = 0.0 ; ee_Data(-l,k) = -0.25  ; ee_Data(l,k) = 0.25  
  call check2d(ee_yx(yx_Data)-ee_Data, eps, &
       'Transform sin(k*pi*X)*sin(l*pi*Y) with id1')
  call check2d(yx_ee(ee_yx(yx_Data))-yx_Data, eps, &
       'Inverse transform sin(k*pi*X)*sin(l*pi*Y) with id1')

  yx_Data = cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  ee_Data = 0.0 ; ee_Data(l,k) = -0.25  ; ee_Data(-l,k) = -0.25  
  call check2d(ee_yx(yx_Data)-ee_Data, eps, &
       'Transform cos(k*pi*X)*cos(l*pi*Y) with id1')
  call check2d(yx_ee(ee_yx(yx_Data))-yx_Data, eps,&
       'Inverse transoform cos(k*pi*X)*cos(l*pi*Y) with id1')

  yx_Data = sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  ee_Data = 0.0 ; ee_Data(-l,-k) = 0.25  ; ee_Data(l,-k) = 0.25  
  call check2d(ee_yx(yx_Data)-ee_Data, eps, &
       'Transform sin(k*pi*X)*cos(l*pi*Y) with id1')
  call check2d(yx_ee(ee_yx(yx_Data))-yx_Data, eps, &
       'Inverse transform sin(k*pi*X)*cos(l*pi*Y) with id1')

  yx_Data = cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  ee_Data = 0.0 ; ee_Data(-l,-k) = 0.25  ; ee_Data(l,-k) = -0.25  
  call check2d(ee_yx(yx_Data)-ee_Data, eps, &
       'Transform cos(k*pi*X)*sin(l*pi*Y) with id1')
  call check2d(yx_ee(ee_yx(yx_Data))-yx_Data, eps,&
       'Inverse transform cos(k*pi*X)*sin(l*pi*Y) with id1')

  ! id2
  write(*,*) 'for id2'
  call ee_ChangeResolutionDomain(id2)
  yx_Data2 = sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  ee_Data2 = 0.0 ; ee_Data2(-l,k) = -0.25  ; ee_Data2(l,k) = 0.25  
  call check2d(ee_yx(yx_Data2)-ee_Data2, eps, &
       'Transform sin(k*pi*X)*sin(l*pi*Y) with id2')
  call check2d(yx_ee(ee_yx(yx_Data2))-yx_Data2, eps, &
       'Inverse transform sin(k*pi*X)*sin(l*pi*Y) with id2')

  yx_Data2 = cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  ee_Data2 = 0.0 ; ee_Data2(l,k) = -0.25  ; ee_Data2(-l,k) = -0.25  
  call check2d(ee_yx(yx_Data2)-ee_Data2, eps, &
       'Transform cos(k*pi*X)*cos(l*pi*Y) with id2')
  call check2d(yx_ee(ee_yx(yx_Data2))-yx_Data2, eps,&
       'Inverse transoform cos(k*pi*X)*cos(l*pi*Y) with id2')

  yx_Data2 = sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  ee_Data2 = 0.0 ; ee_Data2(-l,-k) = 0.25  ; ee_Data2(l,-k) = 0.25  
  call check2d(ee_yx(yx_Data2)-ee_Data2, eps, &
       'Transform sin(k*pi*X)*cos(l*pi*Y) with id2')
  call check2d(yx_ee(ee_yx(yx_Data2))-yx_Data2, eps, &
       'Inverse transform sin(k*pi*X)*cos(l*pi*Y) with id2')

  yx_Data2 = cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  ee_Data2 = 0.0 ; ee_Data2(-l,-k) = 0.25  ; ee_Data2(l,-k) = -0.25  
  call check2d(ee_yx(yx_Data2)-ee_Data2, eps, &
       'Transform cos(k*pi*X)*sin(l*pi*Y) with id2')
  call check2d(yx_ee(ee_yx(yx_Data2))-yx_Data2, eps,&
       'Inverse transform cos(k*pi*X)*sin(l*pi*Y) with id2')

  ! id1 and id3
  write(*,*) 'for id1 and id3'
  call ee_ChangeResolutionDomain(id1)
  yx_Data = sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  ee_Data = ee_yx(yx_Data)
  call ee_ChangeResolutionDomain(id3)
  yx_Data2 = sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  ee_Data3 = ee_yx(yx_Data2)
  call check2d(ee_Data3-ee_Data, eps, &
       'Transform sin(k*pi*X)*sin(l*pi*Y) with id1 and id3')

  call ee_ChangeResolutionDomain(id1)
  yx_Data = cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  ee_Data = ee_yx(yx_Data)
  call ee_ChangeResolutionDomain(id3)
  yx_Data2 = cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  ee_Data3 = ee_yx(yx_Data2)
  call check2d(ee_Data3-ee_Data, eps, &
       'Transform cos(k*pi*X)*cos(l*pi*Y) with id1 and id3')

  call ee_ChangeResolutionDomain(id1)
  yx_Data = sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  ee_Data = ee_yx(yx_Data)
  call ee_ChangeResolutionDomain(id3)
  yx_Data2 = sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  ee_Data3 = ee_yx(yx_Data2)
  call check2d(ee_Data3-ee_Data, eps, &
       'Transform sin(k*pi*X)*cos(l*pi*Y) with id1 and id3')

  call ee_ChangeResolutionDomain(id1)
  yx_Data = cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  ee_Data = ee_yx(yx_Data)
  call ee_ChangeResolutionDomain(id3)
  yx_Data2 = cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  ee_Data3 = ee_yx(yx_Data2)
  call check2d(ee_Data3-ee_Data, eps, &
       'Transform cos(k*pi*X)*sin(l*pi*Y) with id1 and id3')


  call MessageNotify('M','ee_test_transform', &
       'ee_module transform function tests succeeded!')

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
             call MessageNotify('E','ee_test_transform2', &
                  'transform error too large')
          endif
       enddo
    enddo
  end subroutine check2d

end program ee_test_transform2
