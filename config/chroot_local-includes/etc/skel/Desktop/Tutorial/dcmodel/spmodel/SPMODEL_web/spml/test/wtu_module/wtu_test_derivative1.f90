!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wtu_module �ƥ��ȥץ����
!
!      wt_DivRad_wt �Υƥ��� (1/r^2 d/dr r^2)
!  
!����  2008/01/13  �ݹ�����  wt_test_derivative1.f90 ������
!      2008/07/05  ��������ʿ  �����������ѹ�
!
program wtu_test_derivative1

  use dc_message, only : MessageNotify
  use wtu_module
  implicit none

  integer,parameter  :: im=32, jm=16         ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: kmo=16, kmi=8        ! �ʻ���������(���ư��, ��ư��)
  integer,parameter  :: nm=10                ! �����ȿ�������(��ʿ)
  integer,parameter  :: lmo=16, lmi=8        ! �����ȿ�������(���ư��, ��ư��)
  real(8),parameter  :: ri=0.5, ro=1.5       ! �⳰Ⱦ��

  real(8), dimension(0:im-1,1:jm,0:kmo)     :: xyz_Data
  real(8), dimension(0:im-1,1:jm,0:kmo)     :: xyz_Data1
  real(8), dimension((nm+1)**2,0:lmo) :: wt_Data
  real(8), parameter :: eps=1.0D-10
  integer, parameter :: nmin=1, nmax=10

  integer :: n

  call MessageNotify('M','wtu_test_derivative1', &
       'wtu_module derivative function test #1')

  write( 6,* ) 'Test for wt_Divrat_wt'
!!$  write(6,*) 'exponent N, of Radial dependence for r^N?'
!!$
!!$  write(6,*) 'n?'
!!$  read(5,*) n

  call wtu_Initial(im,jm,kmi,kmo,nm,lmi,lmo,ri,ro)

  do n=nmin,nmax
     write(6,*) 'n=',n
     xyz_Data = xyz_Rad**n
     xyz_Data1 = (n+2)*xyz_Rad**(n-1)

     xyz_Data = xyz_wt(wt_DivRad_wt(wt_xyz(xyz_Data)))

     if ( maxval(abs(xyz_Data-xyz_Data1)) .gt. eps ) then
        write( 6,* ) 'Error is...', maxval(abs(xyz_Data-xyz_Data1))
        call MessageNotify('E','wt_test_derivative1', &
             'Derivative error too large.')
     end if

  enddo
  call MessageNotify('M','wt_test_derivative1', &
       'wtu_module derivative function test #1 succeeded!')

end program wtu_test_derivative1
