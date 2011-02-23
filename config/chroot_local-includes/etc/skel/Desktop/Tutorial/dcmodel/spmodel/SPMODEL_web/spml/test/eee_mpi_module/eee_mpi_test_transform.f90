!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  eee_mpi_module �ƥ��ȥץ���� (�����Ѵ�)
!
!����  2008/05/21  �ݹ�����
!
program eee_mpi_test_transform

  use dc_message, only : MessageNotify
  use eee_mpi_module
  implicit none
  include 'mpif.h'

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=32, km=32          ! �ʻ���������(X,Y,Z)
  integer, parameter :: lm=10, mm=10, nm=10          ! �����ȿ�������(X,Y,Z)

 !---- �ѿ� ----
  real(8), allocatable :: zxv_Data(:,:,:)            ! �ʻҥǡ���
  real(8), allocatable :: eef_Data(:,:,:)            ! ���ڥ��ȥ�ǡ���

  integer            :: l=5,m=3,n=2
  integer            :: np, ip, ierr

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
  allocate(eef_Data(-nm:nm,-mm:mm,2*lc(ip)))

 !------------------- ��������� ----------------------
  write(6,*) '*** Test of eee_module : grid -> spectral -> grid transformation check.'
  write(6,*) '  The result will be printed '
  write(6,*) '  only when the error is larger than ', eps
  write(6,*)

!!$  write(6,*) '  Input wavenumbers of the grid data, l,m and n :'
!!$  read(5,*) l,m,n
!!$  write(6,*) '  l,m,n = ', l,m,n

  zxv_Data = sin(l*zxv_X) * sin(m*zxv_Y) * sin(n*zxv_Z)

  eef_Data = 0.0 
  if ( l /= 0 .and. m/=0 .and. n/=0 )then
     if ( ls(ip) <= abs(l) .AND. abs(l) <= le(ip) ) then
        eef_Data(-n,-m,lf_l(-l)) = 0.125 ; eef_Data(-n,m,lf_l(-l))=-0.125
        eef_Data(n,-m,lf_l(-l)) =-0.125  ; eef_Data(n,m,lf_l(-l)) = 0.125
     endif
  endif

  call check3d(eef_zxv(zxv_Data)-eef_Data, eps, &
       'Transform sin(l*X)*sin(m*Y)**sin(n*Z)')

  call check3d(zxv_eef(eef_zxv(zxv_Data))-zxv_Data, eps, &
       'Inverse transform sin(l*X)*sin(m*Y)**sin(n*Z)')

  zxv_Data = cos(l*zxv_X) * cos(m*zxv_Y) * cos(n*zxv_Z)
  eef_Data = 0.0 

  if ( ls(ip) <= abs(l) .AND. abs(l) <= le(ip) ) then
     if ( l /= 0 .and. m/=0 .and. n/=0 )then
        eef_Data(n,m,lf_l(l)) = 0.125  ; eef_Data(-n,m,lf_l(l)) = 0.125  
        eef_Data(n,-m,lf_l(l)) = 0.125  ; eef_Data(-n,-m,lf_l(l)) = 0.125 
     elseif( l==0 .and. m/=0 .and. n/=0 )then
        eef_Data(n,m,0) = -0.25 ;  eef_Data(-n,m,0) = -0.25
     elseif( l/=0 .and. m==0 .and. n/=0 )then
        eef_Data(n,0,lf_l(l)) = -0.25 ;  eef_Data(-n,0,lf_l(l)) = -0.25
     elseif( l/=0 .and. m/=0 .and. n==0 )then
        eef_Data(0,m,lf_l(l)) = -0.25 ;  eef_Data(0,-m,lf_l(l)) = -0.25
     elseif( l==0 .and. m==0 .and. n/=0 )then
        eef_Data(n,0,0) = 0.5
     elseif( l/=0 .and. m==0 .and. n==0 )then
        eef_Data(0,0,lf_l(l)) = -0.5
     elseif( l==0 .and. m/=0 .and. n==0 )then
        eef_Data(0,m,0) = -0.5
     endif
  end if

  call check3d(eef_zxv(zxv_Data)-eef_Data, eps, &
       'Transform cos(l*X)*cos(m*Y)*cos(n*Z)')

  call check3d(zxv_eef(eef_zxv(zxv_Data))-zxv_Data, eps,&
       'Inverse transoform cos(l*X)*cos(m*Y)*cos(n*Z)')

  zxv_Data = sin(l*zxv_X) * cos(m*zxv_Y) * cos(n*zxv_Z)
  eef_Data = 0.0

  if ( ls(ip) <= abs(l) .AND. abs(l) <= le(ip) ) then
     if ( l /= 0 .and. m/=0 .and. n/=0 )then
        eef_Data(-n,-m,lf_l(-l)) = -0.125  ; eef_Data(n,-m,lf_l(-l)) = -0.125  
        eef_Data(-n,m,lf_l(-l)) = -0.125  ; eef_Data(n,m,lf_l(-l)) = -0.125  
     elseif ( l /= 0 .and. m==0 .and. n/=0 )then
        eef_Data(-n,0,lf_l(-l)) = 0.25  ;  eef_Data(n,0,lf_l(-l)) = 0.25
     elseif ( l /= 0 .and. m/=0 .and. n==0 )then
        eef_Data(0,m,lf_l(-l)) = 0.25  ;  eef_Data(0,-m,lf_l(-l)) = 0.25
     elseif ( l /= 0 .and. m==0 .and. n==0 )then
        eef_Data(0,0,lf_l(-l)) = 0.5
     endif
  end if
  call check3d(eef_zxv(zxv_Data)-eef_Data, eps, &
       'Transform sin(l*X)*cos(m*Y)*cos(n*Z)')

  call check3d(zxv_eef(eef_zxv(zxv_Data))-zxv_Data, eps, &
      'Inverse transform sin(l*X)*cos(m*Y)*cos(n*Z)')

  zxv_Data = cos(l*zxv_X) * sin(m*zxv_Y) * sin(n*zxv_Z)
  eef_Data = 0.0 ;
  if ( ls(ip) <= abs(l) .AND. abs(l) <= le(ip) ) then
     if ( l /= 0 .and. m/=0 .and. n/=0 )then
        eef_Data(n,m,lf_l(l))  = -0.125  ; eef_Data(n,-m,lf_l(l))  = 0.125  
        eef_Data(-n,m,lf_l(l)) = 0.125  ; eef_Data(-n,-m,lf_l(l)) = -0.125  
     elseif ( l == 0 .and. m/=0 .and. n/=0 )then
        eef_Data(n,m,0) = 0.25  ;  eef_Data(-n,m,0) = -0.25
     endif
  end if

  call check3d(eef_zxv(zxv_Data)-eef_Data, eps, &
       'Transform cos(l*X)*sin(m*Y)*sin(n*Z)')
  call check3d(zxv_eef(eef_zxv(zxv_Data))-zxv_Data, eps,&
       'Inverse transform cos(l*X)*sin(m*Y)*sin(n*Z)')

  call MessageNotify('M','eee_mpi_test_transform', &
       'eee_mpi_module transform function tests succeeded!')

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

end program eee_mpi_test_transform

