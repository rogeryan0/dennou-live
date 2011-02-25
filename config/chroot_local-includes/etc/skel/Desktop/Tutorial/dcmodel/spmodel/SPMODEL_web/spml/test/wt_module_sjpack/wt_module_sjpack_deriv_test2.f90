!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wt_module_sjpack �ƥ��ȥץ������
!
!      �ȥ�������ݥ�������ط���ʬ�ؿ��Υƥ���
!  
!����  2009/09/06  �ݹ�����  wt_test_defrivative[45].f90 �� SJPACK �Ѥ˲�¤
!
program wt_module_sjpack_deriv_test2

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wt_module_sjpack
  implicit none

  integer,parameter  :: im=32, jm=16, km=16  ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=16         ! �����ȿ�������(��ʿ, ư��)
  real(8),parameter  :: ri=0.5, ro=1.5       ! �⳰Ⱦ��

  ! Ƚ���������
  integer, parameter :: check_digits = 8
  integer, parameter :: ignore = -9

  real(8), dimension(0:im-1,jm,0:km)     :: xyz_DData
  real(8), dimension(0:im-1,jm,0:km)     :: xyz_Psi
  real(8), dimension((nm+1)**2,0:lm)     :: wt_Psi
  real(8), dimension((nm+1)**2,0:lm)     :: wt_Phi

  real(8), dimension(0:im-1,jm,0:km)     :: xyz_VLon
  real(8), dimension(0:im-1,jm,0:km)     :: xyz_VLat
  real(8), dimension(0:im-1,jm,0:km)     :: xyz_VRad
  real(8), dimension(0:im-1,jm,0:km)     :: xyz_RadRot
  real(8), dimension(0:im-1,jm,0:km)     :: xyz_RadRotRot

  integer, parameter :: n=2

  call MessageNotify('M','wt_module_sjpack_deriv_test2', &
       'wt_module_sjpack derivative function test #2')

  call wt_initial(im,jm,km,nm,lm,ri,ro)

  !========== wt_KxRGrad_wt, xyz_KGrad_wt, wt_QOperator_wt ==========

  ! ----------------- �� 1 --------------------
  xyz_Psi = xyz_rad**n * cos(xyz_lat)*sin(xyz_lon)   ! r**2 Y_1^-1

  xyz_DData = xyz_rad**n * cos(xyz_lat)*cos(xyz_lon)
  call AssertEqual(&
       message='wt_KxRGrad_wt with r^2 Y_1^-1',                      &
       answer = xyz_DData,                                           &
       check = xyz_wt(wt_KxRGrad_wt(wt_xyz(xyz_Psi))),               &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  ! k ���� r**n Y_1^1 = (n-1)*r**(n-1)* Y_2^1
  xyz_DData = (n-1)*xyz_rad**(n-1)* cos(xyz_lat)*sin(xyz_lat)*sin(xyz_lon) 
  call AssertEqual(&
       message='xyz_KGrad_wt with r^2 Y_1^-1',                       &
       answer = xyz_DData,                                           &
       check = xyz_KGrad_wt(wt_xyz(xyz_Psi)),                        &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  ! Q r**n Y_1^1 = -3*(n-1)*r**(n-1)* Y_2^1
  xyz_DData = - 3*(n-1)*xyz_rad**(n-1)* cos(xyz_lat)*sin(xyz_lat)*sin(xyz_lon) 
  call AssertEqual(&
       message='wt_QOperator_wt r^2 Y_1^-1',                         &
       answer = xyz_DData,                                           &
       check = xyz_wt(wt_QOperator_wt(wt_xyz(xyz_Psi))),             &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

! ----------------- �� 2 --------------------
  xyz_Psi = cos(xyz_lat)*sin(xyz_lat) * sin(xyz_lon) ! Y_2^1

  xyz_DData = cos(xyz_lat)*sin(xyz_lat) * cos(xyz_lon) ! Y_2^1
  call AssertEqual(&
       message='wt_KxRGrad_wt with Y_2^1',                           &
       answer = xyz_DData,                                           &
       check = xyz_wt(wt_KxRGrad_wt(wt_xyz(xyz_Psi))),               &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  ! k���� Y_2^1 = (- 4 Y_3^1 / 15 - Y_1^1 /5) 
  xyz_DData = cos(2*xyz_lat)*cos(xyz_lat)*sin(xyz_lon)/xyz_rad
  call AssertEqual(&
       message='xyz_KGrad_wt with Y_2^1',                            &
       answer = xyz_DData,                                           &
       check = xyz_KGrad_wt(wt_xyz(xyz_Psi)),                        &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  xyz_DData = (16*sin(xyz_lat)**2 - 5)*cos(xyz_lat)*sin(xyz_lon)/xyz_rad
  call AssertEqual(&
       message='wt_QOperator_wt Y_2^1',                              &
       answer = xyz_DData,                                           &
       check = xyz_wt(wt_QOperator_wt(wt_xyz(xyz_Psi))),             &
       significant_digits = check_digits, ignore_digits = ignore     &
       )


  !========== wt_RadRot_xyz_xyz, wt_RadRotRot_xyz_xyz_xyz ==========

  !---------- ñ��ή�� v_r=r ----------
  xyz_VLon = 0 ; xyz_VLat = 0 ; xyz_VRad = xyz_Rad
  xyz_RadRot = 0 ; xyz_RadRotRot = 0

  call AssertEqual(&
       message='wt_RadRot_xyz_xyz with v_r=r',                       &
       answer = xyz_RadRot,                                          &
       check = xyz_wt(wt_RadRot_xyz_xyz(xyz_VLon,xyz_VLat)),         &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='wt_RadRotRot_xyz_xyz with v_r=r',                    &
       answer = xyz_RadRotRot,                                       &
       check = xyz_wt(wt_RadRotRot_xyz_xyz_xyz(xyz_VLon,xyz_VLat,xyz_VRad)), &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  !---------- ���β�ž����������(����ή��) ----------
  xyz_VLon = 0
  xyz_VLat = xyz_Rad*sin(xyz_Lat)*cos(xyz_Lat)
  xyz_VRad = -xyz_Rad*cos(xyz_Lat)**2

  xyz_RadRot = 0 ; xyz_RadRotRot = 0

  call AssertEqual(&
       message='wt_RadRot_xyz_xyz with rigid rotation(E-W)',         &
       answer = xyz_RadRot,                                          &
       check = xyz_wt(wt_RadRot_xyz_xyz(xyz_VLon,xyz_VLat)),         &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='wt_RadRotRot_xyz_xyz with rigid rotation(E-W)',      &
       answer = xyz_RadRotRot,                                       &
       check = xyz_wt(wt_RadRotRot_xyz_xyz_xyz(xyz_VLon,xyz_VLat,xyz_VRad)), &
       significant_digits = check_digits, ignore_digits = ignore     &
       )


  !---------- ���β�ž����������(����ή��) ----------
  xyz_VLon = xyz_Rad*cos(xyz_Lat)*sin(xyz_Lon)*cos(xyz_Lon)
  xyz_VLat = -xyz_Rad*sin(xyz_Lat)*cos(xyz_Lat)*sin(xyz_Lon)**2
  xyz_VRad = -xyz_Rad*(sin(xyz_Lat)**2*sin(xyz_Lon)**2 + cos(xyz_Lon)**2)
  xyz_RadRot = 0 ; xyz_RadRotRot = 0

  call AssertEqual(&
      message='wt_RadRot_xyz_xyz with rigid rotation(N-S)',          &
       answer = xyz_RadRot,                                          &
       check = xyz_wt(wt_RadRot_xyz_xyz(xyz_VLon,xyz_VLat)),         &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='wt_RadRotRot_xyz_xyz with rigid rotation(N-S)',      &
       answer = xyz_RadRotRot,                                       &
       check = xyz_wt(wt_RadRotRot_xyz_xyz_xyz(xyz_VLon,xyz_VLat,xyz_VRad)), &
       significant_digits = check_digits, ignore_digits = ignore     &
       )


  !--------- ��ľ���٤�ȼ���٥��ȥ�� ----------
  xyz_Psi = xyz_Rad**2 * cos(xyz_Lat)*sin(xyz_Lon)   ! r**2 P_1^1
  !xyz_Psi = xyz_Rad**2 * cos(xyz_Lat)*sin(xyz_Lat)*sin(xyz_Lon)   ! r**2 P_2^1

  xyz_VLon =   xyz_gRadLat_wt(wt_xyz(xyz_Psi*xyz_Rad))
  xyz_VLat = - xyz_gRadLon_wt(wt_xyz(xyz_Psi*xyz_Rad))
  xyz_VRad = 0
  xyz_RadRot = 2 * xyz_Psi                           ! r�����ߢ���(��r) = L_2��
  !xyz_RadRot = 6 * xyz_Psi                           ! r�����ߢ���(��r) = L_2��
  xyz_RadRotRot = 0

  call AssertEqual(&
      message='wt_RadRot_xyz_xyz with r^2 Y_1^-1 toroidal field',    &
       answer = xyz_RadRot,                                          &
       check = xyz_wt(wt_RadRot_xyz_xyz(xyz_VLon,xyz_VLat)),         &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='wt_RadRotRot_xyz_xyz with r^2 Y_1^-1 toroidal field',&
       answer = xyz_RadRotRot,                                       &
       check = xyz_wt(wt_RadRotRot_xyz_xyz_xyz(xyz_VLon,xyz_VLat,xyz_VRad)), &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  !---------- ��ľ®�٤�ȼ���٥��ȥ�� -----------
  xyz_VRad = xyz_wt(wt_l2_wt(wt_xyz(xyz_Psi/xyz_Rad)))
  xyz_VLat = xyz_gRadLat_wt(wt_dRad_wt(wt_xyz(xyz_Psi*xyz_Rad)))
  xyz_VLon = xyz_gRadLon_wt(wt_dRad_wt(wt_xyz(xyz_Psi*xyz_Rad)))

  xyz_RadRot = 0
  xyz_RadRotRot = -xyz_wt(wt_l2_wt(wt_lapla_wt(wt_xyz(xyz_Psi))))
                 ! r�����ߢ��ߢ��ߢ���(��r) = -L_2��^2��

  call AssertEqual(&
      message='wt_RadRot_xyz_xyz with non-vortical field',           &
       answer = xyz_RadRot,                                          &
       check = xyz_wt(wt_RadRot_xyz_xyz(xyz_VLon,xyz_VLat)),         &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='wt_RadRotRot_xyz_xyz with r^2 Y_1^-1 toroidal field',&
       answer = xyz_RadRotRot,                                       &
       check = xyz_wt(wt_RadRotRot_xyz_xyz_xyz(xyz_VLon,xyz_VLat,xyz_VRad)), &
       significant_digits = check_digits, ignore_digits = ignore     &
       )


  !========== wt_Potential2Vector, wt_Potential2Rotation ==========

  !----------------- ���β�ž�� --------------------
  ! 
  wt_Psi = wt_xyz(xyz_Rad * sin(xyz_Lat))
  wt_Phi = 0.0D0

  call wt_Potential2Vector(&
       xyz_VLon,xyz_VLat,xyz_VRad, wt_Psi, wt_Phi )

  xyz_DData = xyz_Rad * cos(xyz_Lat)
  call AssertEqual(&
      message='wt_Potential2Vector (Lon) with rigid rotation field(E-W)', &
       answer = xyz_VLon,                                            &
       check = xyz_DData,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )
  xyz_DData = 0.0D0
  call AssertEqual(&
      message='wt_Potential2Vector (Lat) with rigid rotation field(E-W)', &
       answer = xyz_VLat,                                            &
       check = xyz_DData,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )
  xyz_DData = 0.0D0
  call AssertEqual(&
      message='wt_Potential2Vector (Rad) with rigid rotation field(E-W)', &
       answer = xyz_VRad,                                            &
       check = xyz_DData,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call wt_Potential2Rotation(&
       xyz_VLon,xyz_VLat,xyz_VRad, wt_Psi, wt_Phi )

  xyz_DData = 0.0D0
  call AssertEqual(&
      message='wt_Potential2Rotation (Lon) with rigid rotation field(E-W)', &
       answer = xyz_VLon,                                            &
       check = xyz_DData,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )
  xyz_DData = 2*cos(xyz_LAT)
  call AssertEqual(&
      message='wt_Potential2Rotation (Lat) with rigid rotation field(E-W)', &
       answer = xyz_VLat,                                            &
       check = xyz_DData,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )
  xyz_DData = 2*sin(xyz_LAT)
  call AssertEqual(&
      message='wt_Potential2Rotation (Rad) with rigid rotation field(E-W)', &
       answer = xyz_VRad,                                            &
       check = xyz_DData,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  !----------------- ���β�ž��(����ή) --------------------
  ! 
  wt_Psi = wt_xyz(xyz_Rad * cos(xyz_Lat) * sin(xyz_Lon))
  wt_Phi = 0.0D0

  call wt_Potential2Vector(&
       xyz_VLon,xyz_VLat,xyz_VRad, wt_Psi, wt_Phi )

  xyz_DData = -xyz_Rad*sin(xyz_Lat)*sin(xyz_Lon)
  call AssertEqual(&
      message='wt_Potential2Vector (Lon) with rigid rotation field(N-S)', &
       answer = xyz_VLon,                                            &
       check = xyz_DData,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )
  xyz_DData = -xyz_Rad*cos(xyz_Lon)
  call AssertEqual(&
      message='wt_Potential2Vector (Lat) with rigid rotation field(N-S)', &
       answer = xyz_VLat,                                            &
       check = xyz_DData,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )
  xyz_DData = 0.0D0
  call AssertEqual(&
      message='wt_Potential2Vector (Rad) with rigid rotation field(N-S)', &
       answer = xyz_VRad,                                            &
       check = xyz_DData,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call wt_Potential2Rotation(&
       xyz_VLon,xyz_VLat,xyz_VRad, wt_Psi, wt_Phi )

  xyz_DData = 2*cos(xyz_Lon)
  call AssertEqual(&
      message='wt_Potential2Rotation (Lon) with rigid rotation field(N-S)', &
       answer = xyz_VLon,                                            &
       check = xyz_DData,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )
  xyz_DData = -2*sin(xyz_Lon)*sin(xyz_Lat)
  call AssertEqual(&
      message='wt_Potential2Rotation (Lat) with rigid rotation field(N-S)', &
       answer = xyz_VLat,                                            &
       check = xyz_DData,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )
  xyz_DData = 2*sin(xyz_Lon)*cos(xyz_Lat)
  call AssertEqual(&
      message='wt_Potential2Rotation (Rad) with rigid rotation field(N-S)', &
       answer = xyz_VRad,                                            &
       check = xyz_DData,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  ! ----------------- ��̵���� --------------------
  wt_Psi = 0.0D0
  wt_Phi = wt_xyz(xyz_Rad * sin(xyz_Lat))

  call wt_Potential2Vector(&
       xyz_VLon,xyz_VLat,xyz_VRad, wt_Psi, wt_Phi )

  xyz_DData = 0
  call AssertEqual(&
       message='wt_Potential2Vector (Lon) with non-vortical field',  &
       answer = xyz_VLon,                                            &
       check = xyz_DData,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )
  xyz_DData = 2 * cos(xyz_Lat)
  call AssertEqual(&
       message='wt_Potential2Vector (Lat) with non-vortical field',  &
       answer = xyz_VLat,                                            &
       check = xyz_DData,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )
  xyz_DData = 2 * sin(xyz_Lat)
  call AssertEqual(&
      message='wt_Potential2Vector (Rad) with non-vortical field', &
       answer = xyz_VRad,                                            &
       check = xyz_DData,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call wt_Potential2Rotation(&
       xyz_VLon,xyz_VLat,xyz_VRad, wt_Psi, wt_Phi )

  xyz_DData = 0.0D0
  call AssertEqual(&
      message='wt_Potential2Rotation (Lon) with non-vortical field', &
       answer = xyz_VLon,                                            &
       check = xyz_DData,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )
  xyz_DData = 0.0D0
  call AssertEqual(&
      message='wt_Potential2Rotation (Lat) with non-vortical field', &
       answer = xyz_VLat,                                            &
       check = xyz_DData,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )
  xyz_DData = 0.0D0
  call AssertEqual(&
      message='wt_Potential2Rotation (Rad) with non-vortical field', &
       answer = xyz_VRad,                                            &
       check = xyz_DData,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

! ----------------- �� 4 --------------------
 ! �ݥ�������®�پ�

  wt_Psi = 0.0D0
  wt_Phi = wt_xyz(xyz_Rad**2 * cos(xyz_Lat)*sin(xyz_Lon))

  call wt_Potential2Vector(&
       xyz_VLon,xyz_VLat,xyz_VRad, wt_Psi, wt_Phi )

  xyz_DData = 3 * xyz_Rad * cos(xyz_Lon)
  call AssertEqual(&
      message='wt_Potential2Vector (Lon) with poloidal field',   &
       answer = xyz_VLon,                                            &
       check = xyz_DData,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )
  xyz_DData = - 3 * xyz_Rad * sin(xyz_Lat) * sin(xyz_Lon)
  call AssertEqual(&
      message='wt_Potential2Vector (Lat) with poloidal field', &
       answer = xyz_VLat,                                            &
       check = xyz_DData,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )
  xyz_DData = 2 * xyz_Rad * cos(xyz_Lat) * sin(xyz_Lon)
  call AssertEqual(&
      message='wt_Potential2Vector (Rad) with poloidal field', &
       answer = xyz_VRad,                                            &
       check = xyz_DData,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call wt_Potential2Rotation(&
       xyz_VLon,xyz_VLat,xyz_VRad, wt_Psi, wt_Phi )

  xyz_DData = 4*sin(xyz_Lat)*sin(xyz_Lon)
  call AssertEqual(&
      message='wt_Potential2Rotation (Lon) with poloidal field', &
       answer = xyz_VLon,                                            &
       check = xyz_DData,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )
  xyz_DData = 4*cos(xyz_Lon)
  call AssertEqual(&
      message='wt_Potential2Rotation (Lat) with poloidal field', &
       answer = xyz_VLat,                                            &
       check = xyz_DData,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )
  xyz_DData = 0.0D0
  call AssertEqual(&
      message='wt_Potential2Rotation (Rad) with poloidal field', &
       answer = xyz_VRad,                                            &
       check = xyz_DData,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call MessageNotify('M','wt_module_sjpack_deriv_test2', &
       'wt_module_sjpack derivative function test #2 succeeded!')

end program wt_module_sjpack_deriv_test2