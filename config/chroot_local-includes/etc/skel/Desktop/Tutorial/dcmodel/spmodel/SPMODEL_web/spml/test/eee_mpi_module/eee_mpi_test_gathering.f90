!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  eee_mpi_module �ƥ��ȥץ���� (�����Ѵ�)
!
!����  2008/05/21  �ݹ�����
!
program eee_mpi_test_gatherv

  use dc_message, only : MessageNotify
  use eee_mpi_module
  implicit none
  include 'mpif.h'

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=32, km=32          ! �ʻ���������(X,Y,Z)
  integer, parameter :: lm=10, mm=10, nm=10          ! �����ȿ�������(X,Y,Z)

 !---- �ѿ� ----
  real(8), allocatable :: zxv_Data(:,:,:)            ! �ʻҥǡ���
  real(8), allocatable :: xyz_Data(:,:,:)            ! �ʻҥǡ���

  integer            :: l=5,m=3,n=2
  integer            :: np, ip, ierr
  integer            :: i, j, k

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! Ƚ���

  call MessageNotify('M','eee_mpi_test_transform', &
       'eee_module transform function tests')

 !---------------- MPI �������� ---------------------
  call MPI_INIT(IERR)
  call MPI_COMM_RANK(MPI_COMM_WORLD,IP,IERR)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

 !---------------- ��ɸ�ͤ����� ---------------------
  call eee_mpi_initial(im,jm,km,lm,mm,nm)

 !---------------- �ѿ��γ��դ� ---------------------
  allocate(zxv_Data(0:km-1,0:im-1,js(ip):je(ip)))
  allocate(xyz_Data(0:im-1,0:jm-1,0:km-1))

 !------------------- ��������� ----------------------
  write(6,*) '*** Test of eee_mpi_module : grid -> grid transformation check.'
  write(6,*) '  The result will be printed '
  write(6,*) '  only when the error is larger than ', eps
  write(6,*)

!!$  write(6,*) '  Input wavenumbers of the grid data, l,m and n :'
!!$  read(5,*) l,m,n
!!$  write(6,*) '  l,m,n = ', l,m,n

  zxv_Data = sin(l*zxv_X) * sin(m*zxv_Y) * sin(n*zxv_Z)

  do k=0,km-1
     do j=0,jm-1
        do i=0,im-1
           xyz_Data(i,j,k) = &
                sin(l*(2*pi/im*i))*sin(m*(2*pi/jm*j))*sin(n*(2*pi/km*k))
        enddo
     enddo
  enddo

  call check3d((xyz_zxv(zxv_Data))-xyz_Data, eps, &
       'Data gathering of sin(l*X)*sin(m*Y)**sin(n*Z)')


  call MPI_FINALIZE(IERR)

 stop
contains

  subroutine check3d(var,eps,funcname) ! �����ͤ� eps �ʾ�� var �����Ǥ����
    real(8) :: var(:,:,:)              ! Ƚ�ꤹ������
    real(8) :: eps                     ! ��
    character(len=*), optional :: funcname
    character(len=3) ::cip
    integer i, j, k

    if ( present(funcname) )then
       write(cip,'(I3)') IP
       write(6,*) '  Checking ', funcname, ' for IP='//trim(adjustl(cip))
    endif

    do k=1,size(var,3)
       do j=1,size(var,2)
          do i=1,size(var,1)
             if (abs(var(i,j,k)) .gt. eps ) then
                write(6,*) &
                  '    Value larger than EPS : i= ', i, '  j= ', j, '  k= ', k, &
                  var(i,j,k)
                call MessageNotify('E','eee_mpi_test_transform', &
                  'transform error too large')
             endif
          enddo
       enddo
    enddo
  end subroutine check3d

end program eee_mpi_test_gatherv

