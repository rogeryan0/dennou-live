!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wt_zonal_module_sjpack �ƥ��ȥץ���� :: �����Ѵ��ؿ��Υƥ���
!
!����  2009/09/25  �ݹ�����
!
program wt_zonal_module_sjpack_base_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wt_zonal_module_sjpack
  implicit none

  integer,parameter  :: im=1, jm=16, km=16  ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=16        ! �����ȿ�������(��ʿ, ư��)
  real(8),parameter  :: ri=0.5, ro=1.5      ! �⳰Ⱦ��

  real(8), dimension(nm+1,0:lm)            ::  wt_data
  real(8), dimension(0:im-1,1:jm,0:km)     ::  xyz_data
  real(8), dimension(0:im-1,1:jm,0:km)     ::  xyz_xi

  ! Ƚ�������
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11

  call MessageNotify('M','wt_zonal_module_sjpack_test', &
       'wt_zonal_module_sjpack basic transformation functions tests') 

  call wt_Initial(im,jm,km,nm,lm,ri,ro)

  xyz_xi = (xyz_Rad - (ro+ri)/2 )*2/(ro-ri)

  !---- Y_1^* �Υƥ��� ----
  xyz_data = sqrt(3.0D0)*sin(xyz_Lat)*xyz_xi            ! Y_1^0 T_1
  wt_data= 0.0D0 ; wt_data(l_nm(1,0),1)=1.0D0

  call AssertEqual(&
    message='wt_xyz(xyz_data) with Y_1^0 T_1',                    &
    answer = wt_data,                                             &
    check =  wt_xyz(xyz_data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='xyz_wt(wt_xyz(xyz_data)) with Y_1^0 T_1',            &
    answer = xyz_data,                                            &
    check =  xyz_wt(wt_xyz(xyz_data)),                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Y_2^* �Υƥ��� ----
  ! Y_2^0 T_3
  xyz_data = sqrt(5.0D0)*(3.0/2*sin(xyz_Lat)**2-1/2.0)*(4*xyz_xi**3-3*xyz_xi) 
  wt_data= 0.0D0 ; wt_data(l_nm(2,0),3)=1.0D0

  call AssertEqual(&
    message='wt_xyz(xyz_data) with Y_2^0 T_2',                    &
    answer = wt_data,                                             &
    check =  wt_xyz(xyz_data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='xyz_wt(wt_xyz(xyz_data)) with Y_2^0 T_3',            &
    answer = xyz_data,                                            &
    check =  xyz_wt(wt_xyz(xyz_data)),                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- ����Ū�ؿ��Υƥ��� ----
  xyz_data = (sin(xyz_Lat)-1)**2*(sin(xyz_Lat)-0.5)*(sin(xyz_Lat)+1) &
       *exp(xyz_Rad)

  call AssertEqual(&
    message='xyz_wt(wt_xyz(xyz_data)) with general function',     &
    answer = xyz_data,                                            &
    check =  xyz_wt(wt_xyz(xyz_data)),                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','wt_zonal_module_sjpack_test', &
       'wt_zonal_module_sjpack basic transformation functions tests succeeded!') 

end program wt_zonal_module_sjpack_base_test
