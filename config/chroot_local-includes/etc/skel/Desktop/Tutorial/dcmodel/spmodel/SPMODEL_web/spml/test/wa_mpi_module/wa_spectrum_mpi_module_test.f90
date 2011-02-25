!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wa_spectrum_module �ƥ��ȥץ������ :: ���ڥ��ȥ�ؿ��Υƥ���
!
!����  2009/09/24  �ݹ����� wa_spectrum_module.f90 ����¤
!
program wa_spectrum_mpi_module_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wa_mpi_module
  implicit none
  include 'mpif.h'

  integer, parameter :: im=16, jm=8, nm=5, km=2

  real(8), dimension((nm+1)**2,km)   ::  wa_data
  real(8), dimension(0:im-1,1:jm,km) ::  xya_data

  real(8), dimension(0:nm,-nm:nm,km) ::  nma_data
  real(8), dimension(0:nm,km)        ::  na_data
  real(8), dimension(km)             ::  a_data

  ! Ƚ���������
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11

  integer :: n,m
  integer :: iproc, np, ierr

 !---------------- MPI �������� ---------------------
  call MPI_INIT(IERR)
  call MPI_COMM_RANK(MPI_COMM_WORLD,IPROC,IERR)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

  call wa_mpi_Initial( nm, im, jm, km )

  call MessageNotify('M','wa_spectrum_mpi_module_test',&
                         'wa_spectrum_mpi_module functions tests.') 

  ! 2Y_1^0+3Y_2^1
  xya_data(:,:,1) = 2*sqrt(3.0D0)*sin(xy_Lat) &
                + 3*sqrt(2*5.0D0/6)*3.0*sin(xy_Lat)*cos(xy_Lat) * cos(xy_Lon)! 
  ! 3*Y_1^0 + 2Y_1^1 + 2Y_2^0 +Y_2^-2
  xya_data(:,:,2) = 3*sqrt(3.0D0)*sin(xy_Lat) & 
                + 2* sqrt(2*3.0D0/2)*cos(xy_Lat)*cos(xy_Lon) &
                + 2*sqrt(5.0D0)*(3.0/2*sin(xy_Lat)**2-1/2.0) &
                - sqrt(2*5.0D0/24)*3.0*cos(xy_Lat)**2*sin(2*xy_Lon)

  wa_data = wa_xya(xya_data)

  !--------- Energy spectrum ---------
  nma_data=wa_spectrum_VMiss

  do n=0,nm
     do m=-n,n
        nma_data(n,m,:) = 0.0D0
     enddo
  enddo

  nma_data(1,0,1) = 4.0D0 ;   nma_data(2,1,1) = 27.0D0
  nma_data(1,0,2) = 9.0D0  ; nma_data(1,1,2)  = 4.0D0
  nma_data(2,0,2) = 12.0D0 ; nma_data(2,-2,2) = 3.0D0

  call AssertEqual(&
    message='nma_EnergyFromStreamfunc_wa',                        &
    answer = nma_EnergyFromStreamfunc_wa(wa_data),                &
    check =  nma_data,                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  na_data = 0.0D0
  na_data(:,1) = 0.0D0
  na_data(1,1) = 4.0D0 ;   na_data(2,1) = 27.0D0
  na_data(1,2) = 13.0D0 ;   na_data(2,2) = 15.0D0

  call AssertEqual(&
    message='na_EnergyFromStreamfunc_wa',                         &
    answer = na_EnergyFromStreamfunc_wa(wa_data),                 &
    check =  na_data,                                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  nma_Data = nma_EnergyFromStreamfunc_wa(wa_data)
  a_data = 0.0D0
  do n=0,nm
     do m=-n,n
        a_data = a_data + nma_Data(n,m,:)
     enddo
  enddo

  call AssertEqual(&
    message='total energy by nm-spectrum',                        &
    answer = a_AvrLonLat_xya(xya_wa(wa_xva( &
       0.5*(xva_GradLon_wa(wa_data)**2+xva_GradLat_wa(wa_data)**2) ))), &
    check = a_data,                                               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  a_data = a_AvrLonLat_xya(xya_wa(wa_xva( &
       0.5*(xva_GradLon_wa(wa_data)**2+xva_GradLat_wa(wa_data)**2) )))
  call AssertEqual(&
    message='total energy with n-spectrum',                       &
    answer = sum(na_EnergyFromStreamfunc_wa(wa_data),1),          &
    check =  a_data,                                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !--------- Enstrophy spectrum ---------
  nma_data=wa_spectrum_VMiss
  do n=0,nm
     do m=-n,n
        nma_data(n,m,:) = 0.0
     enddo
  enddo

  nma_data(1,0,1) = 8.0D0 ;   nma_data(2,1,1) = 162.0D0

  nma_data(1,0,2) = 18.0D0 ; nma_data(1,1,2)  = 8.0D0
  nma_data(2,0,2) = 72.0D0 ; nma_data(2,-2,2) = 18.0D0

  call AssertEqual(&
    message='nma_EnstrophyFromStreamfunc_wa',                     &
    answer = nma_EnstrophyFromStreamfunc_wa(wa_data),             &
    check =  nma_data,                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  na_data = 0.0
  na_data(1,1) = 8.0D0  ;   na_data(2,1) = 162.0D0
  na_data(1,2) = 26.0D0 ;   na_data(2,2) = 90.0D0

  call AssertEqual(&
    message='na_EnstrophyFromStreamfunc_wa',                      &
    answer = na_EnstrophyFromStreamfunc_wa(wa_data),              &
    check =  na_data,                                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  nma_Data = nma_EnstrophyFromStreamfunc_wa(wa_data)
  a_data = 0.0D0
  do n=0,nm
     do m=-n,n
        a_data = a_data + nma_Data(n,m,:)
     enddo
  enddo

  call AssertEqual(&
    message='total enstrophy by nm-spectrum',                     &
    answer = a_AvrLonLat_xya( 0.5*(xya_wa(wa_Lapla_wa(wa_data))**2)),&
    check = a_data,                                               &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  a_data = a_AvrLonLat_xya( 0.5*(xya_wa(wa_Lapla_wa(wa_data))**2))

  call AssertEqual(&
    message='total energy with n-spectrum',                       &
    answer = sum(na_EnstrophyFromStreamfunc_wa(wa_data),1),       &
    check =  a_data,                                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','wa_spectrum_mpi_module_test', &
                     'wa_spectrum_mpi_module tests succeeded!') 

 !------ MPI�ν�λ ------

  call MPI_FINALIZE(IERR)

end program wa_spectrum_mpi_module_test