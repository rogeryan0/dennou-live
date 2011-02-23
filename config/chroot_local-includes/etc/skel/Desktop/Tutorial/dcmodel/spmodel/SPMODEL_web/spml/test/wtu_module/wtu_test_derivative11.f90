!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wtu_module �ƥ��ȥץ����
!
!      wr_RotRad_wu �Υƥ��� (1/r d/dr r)
!      wr_DivRad_wu �Υƥ��� (1/r^2 d/dr r^2)
!  
!����  2008/01/13  �ݹ�����  wu_test_derivative1.f90 ������
!      2008/07/05  ��������ʿ  �����������ѹ�
!
program wtu_test_derivative11

  use dc_message, only : MessageNotify
  use wtu_module
  implicit none

  integer,parameter  :: im=32, jm=16         ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: kmo=16, kmi=8        ! �ʻ���������(���ư��, ��ư��)
  integer,parameter  :: nm=10                ! �����ȿ�������(��ʿ)
  integer,parameter  :: lmo=16, lmi=8        ! �����ȿ�������(���ư��, ��ư��)
  real(8),parameter  :: ri=0.5, ro=1.5       ! �⳰Ⱦ��

  real(8), dimension(0:im-1,1:jm,0:kmi)     :: xyr_Data
  real(8), dimension(0:im-1,1:jm,0:kmi)     :: xyr_Data1
  real(8), dimension(0:im-1,1:jm,0:kmi)     :: xyr_xi
  real(8), parameter :: eps=1.0D-10
  integer, parameter :: nmin=3, nmax=10

  integer :: n

  call MessageNotify('M','wt_test_derivative1', &
       'wt_module derivative function test #1')

  call wtu_Initial(im,jm,kmi,kmo,nm,lmi,lmo,ri,ro)

  write( 6,* ) 'Test for wr_RotRad_wu (even mode)'
  do n=nmin+1,nmax,2
     write(6,*) 'n=',n
     xyr_Data = xyr_Rad**n
     xyr_Data1 = (n+1)*xyr_Rad**(n-1)

     xyr_Data = xyr_wr(wr_RotRad_wu(wu_xyr(xyr_Data)))

     if ( maxval(abs(xyr_Data-xyr_Data1)) .gt. eps ) then
        write( 6,* ) 'Error is...', maxval(abs(xyr_Data-xyr_Data1))
        call MessageNotify('E','wtu_test_derivative1', &
             'RotRad Derivative error too large.')
     end if

  enddo

  write( 6,* ) 'Test for wr_RotRad_wu (odd mode)'
  do n=nmin,nmax,2
     write(6,*) 'n=',n
     xyr_Data = xyr_Rad**n*cos(xyr_Lat)*sin(xyr_Lon)
     xyr_Data1 = (n+1)*xyr_Rad**(n-1)*cos(xyr_Lat)*sin(xyr_Lon)
     xyr_Data = xyr_wr(wr_RotRad_wu(wu_xyr(xyr_Data)))

     if ( maxval(abs(xyr_Data-xyr_Data1)) .gt. eps ) then
        write( 6,* ) 'Error is...', maxval(abs(xyr_Data-xyr_Data1))
        call MessageNotify('E','wtu_test_derivative1', &
             'RotRad Derivative error too large.')
     end if
  enddo

  write( 6,* ) 'Test for wr_DivRad_wu(odd mode)'
  do n=nmin,nmax,2
     write(6,*) 'n=',n
     xyr_Data = xyr_Rad**n * cos(xyr_Lat)*sin(xyr_Lon)
     xyr_Data1 = (n+2)*xyr_Rad**(n-1)*cos(xyr_Lat)*sin(xyr_Lon)

     xyr_Data = xyr_wr(wr_DivRad_wu(wu_xyr(xyr_Data)))

     if ( maxval(abs(xyr_Data-xyr_Data1)) .gt. eps ) then
        write( 6,* ) 'Error is...', maxval(abs(xyr_Data-xyr_Data1))
        call MessageNotify('E','wtu_test_derivative1', &
             'DivRad Derivative error too large.')
     end if

  enddo

  write( 6,* ) 'Test for wr_DivRad_wu(even mode)'
  do n=nmin+1,nmax,2
     write(6,*) 'n=',n
     xyr_Data = xyr_Rad**n
     xyr_Data1 = (n+2)*xyr_Rad**(n-1)

     xyr_Data = xyr_wr(wr_DivRad_wu(wu_xyr(xyr_Data)))

     if ( maxval(abs(xyr_Data-xyr_Data1)) .gt. eps ) then
        write( 6,* ) 'Error is...', maxval(abs(xyr_Data-xyr_Data1))
        call MessageNotify('E','wtu_test_derivative1', &
             'DivRad Derivative error too large.')
     end if

  enddo
  call MessageNotify('M','wtu_test_derivative1', &
       'wu_module derivative function test #1 succeeded!')

end program wtu_test_derivative11
