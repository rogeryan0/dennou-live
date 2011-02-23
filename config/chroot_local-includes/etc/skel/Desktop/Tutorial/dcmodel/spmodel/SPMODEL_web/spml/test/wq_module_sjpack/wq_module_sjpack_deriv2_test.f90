!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  wq_module �ƥ��ȥץ����
!
!      wq_KxRGrad_wq, xyr_KGrad_wq, wq_QOperator_wq �Υƥ���
!      wr_RadRot_xyr_xyr, wr_RadRotRot_xyr_xyr_xyr �Υƥ���
!      wq_Potential2Vector, wq_Potential2Rotation �Υƥ���
!
!����  2009/12/07  �ݹ�����  wq_test_derivative[4-7].f90 �� SJPACK �Ѥ˲�¤
!
program wq_module_sjpack_deriv2_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wq_module_sjpack
  implicit none

  integer,parameter  :: im=32, jm=16, km=8   ! �ʻ���������(����, ����, ư��)
  integer,parameter  :: nm=10, lm=15         ! �����ȿ�������(��ʿ, ư��)
  real(8),parameter  :: ra=1.5               ! ��Ⱦ��

  real(8), dimension(0:im-1,1:jm,km)     :: xyr_Psi
  real(8), dimension(0:im-1,1:jm,km)     :: xyr_KxRGrad
  real(8), dimension(0:im-1,1:jm,km)     :: xyr_KGrad
  real(8), dimension(0:im-1,1:jm,km)     :: xyr_QOperator

  real(8), dimension(0:im-1,1:jm,km)     :: xyr_VLon
  real(8), dimension(0:im-1,1:jm,km)     :: xyr_VLat
  real(8), dimension(0:im-1,1:jm,km)     :: xyr_VRad
  real(8), dimension(0:im-1,1:jm,km)     :: xyr_V0Lon  ! ®��(����, ����)
  real(8), dimension(0:im-1,1:jm,km)     :: xyr_V0Lat  ! ®��(����, ����)
  real(8), dimension(0:im-1,1:jm,km)     :: xyr_V0rad  ! ®��(����,ư��)
  real(8), dimension(0:im-1,1:jm,km)     :: xyr_RadRot
  real(8), dimension(0:im-1,1:jm,km)     :: xyr_RadRotRot
  real(8), dimension(0:im-1,1:jm,km)     :: xyr_Torvel
  real(8), dimension(0:im-1,1:jm,km)     :: xyr_Polvel

  real(8), dimension((nm+1)*(nm+1),0:lm) :: wq_Torvel  ! �ȥ�����ݥƥ󥷥��
  real(8), dimension((nm+1)*(nm+1),0:lm) :: wq_Polvel  ! �ݥ�����ݥƥ󥷥��

  integer, parameter :: n=3

  ! Ƚ�������
  integer, parameter :: check_digits = 8
  integer, parameter :: ignore = -9

  call MessageNotify('M','wq_module_sjpack_deriv2_test', &
       'wq_module_sjpack derivative function test #1')

  call wq_Initial(im,jm,km,nm,lm,ra)

  !========== wq_KxRGrad_wq, xyr_KGrad_wq, wq_QOperator_wq =========
  call MessageNotify('M','wq_module_sjpack_deriv2_test', &
       'Test for wq_KxRGrad_wq, xyr_KGrad_wq, wq_QOperator_wq')

  xyr_Psi = xyr_rad**n * cos(xyr_lat)*sin(xyr_lon)       ! r**n Y_1^1

  xyr_KxRGrad = xyr_Rad**n * cos(xyr_lat)*cos(xyr_lon)   ! r**n Y_1^-1

  ! k ���� r**n Y_1^1 = (n-1)*r**(n-1)* Y_2^1
  xyr_KGrad = (n-1)*xyr_rad**(n-1)* cos(xyr_lat)*sin(xyr_lat)*sin(xyr_lon) 

  ! Q r**n Y_1^1 = -3*(n-1)*r**(n-1)* Y_2^1
  xyr_QOperator = - 3*(n-1)*xyr_rad**(n-1)* &
                   cos(xyr_lat)*sin(xyr_lat)*sin(xyr_lon) 

  call AssertEqual(&
       message='Checking k x r grad with r**n Y_1^1',                &
       answer = xyr_KxRgrad,                                         &
       check = xyr_wq(wq_KxRGrad_wq(wq_xyr(xyr_Psi))),               &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='Checking k grad with r**n Y_1^1',                    &
       answer = xyr_KGrad,                                           &
       check = xyr_KGrad_wq(wq_xyr(xyr_Psi)),                        &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='Checking Q operator with r**n Y_1^1',                &
       answer = xyr_QOperator,                                       &
       check = xyr_wq(wq_QOperator_wq(wq_xyr(xyr_Psi))),             &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  xyr_Psi = 3*cos(xyr_lat)*sin(xyr_lat) * sin(xyr_lon) * xyr_Rad**2 ! Y_2^1

  xyr_KxRGrad = 3*cos(xyr_lat)*sin(xyr_lat) * cos(xyr_lon) * xyr_Rad**2 ! Y_2^-1

  ! k����r^2 Y_2^1 = 3r Y_1^1 
  xyr_KGrad = 3*xyr_Rad*cos(2*xyr_Lat)*cos(xyr_Lat)*sin(xyr_Lon) &
             +3*xyr_Rad*sin(2*xyr_Lat)*sin(xyr_Lat)*sin(xyr_Lon)

  xyr_QOperator = -9*cos(xyr_lat)*sin(xyr_lon)*xyr_rad

  call AssertEqual(&
       message='Checking k x r grad with r**2 Y_2^1',                &
       answer = xyr_KxRgrad,                                         &
       check = xyr_wq(wq_KxRGrad_wq(wq_xyr(xyr_Psi))),               &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='Checking k grad with r**2 Y_2^1',                    &
       answer = xyr_KGrad,                                           &
       check = xyr_KGrad_wq(wq_xyr(xyr_Psi)),                        &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='Checking Q operator with r**2 Y_2^1',                &
       answer = xyr_QOperator,                                       &
       check = xyr_wq(wq_QOperator_wq(wq_xyr(xyr_Psi))),             &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  !=========== wr_RadRot_xyr_xyr, wr_RadRotRot_xyr_xyr_xyr ============
  call MessageNotify('M','wq_module_sjpack_deriv2_test', &
       'Test for wr_RadRot_xyr_xyr, wr_RadRotRot_xyr_xyr_xyr')

  xyr_VLon = 0.0D0 ; xyr_VLat = 0.0D0 ; xyr_VRad = xyr_Rad
  xyr_RadRot = 0.0D0 ; xyr_RadRotRot = 0.0D0

  call AssertEqual(&
       message='Checking r Rot v with simple example (v_r=r)',       &
       answer = xyr_RadRot,                                          &
       check = xyr_wr(wr_RadRot_xyr_xyr(xyr_VLon,xyr_VLat)),         &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='Checking r Rot Rot v with simple example (v_r=r)',   &
       answer = xyr_RadRotRot,                                       &
       check = xyr_wr(wr_RadRotRot_xyr_xyr_xyr(xyr_VLon,xyr_VLat,xyr_VRad)),&
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  ! ���β�ž����������(����ή��)
  xyr_VLon = 0.0D0
  xyr_VLat = xyr_Rad*sin(xyr_Lat)*cos(xyr_Lat)
  xyr_VRad = -xyr_Rad*cos(xyr_Lat)**2
  xyr_RadRot = 0.0D0 ; xyr_RadRotRot = 0.0D0

  call AssertEqual(&
       message='Checking r Rot v with rigid rotation(E-W flow)',     &
       answer = xyr_RadRot,                                          &
       check = xyr_wr(wr_RadRot_xyr_xyr(xyr_VLon,xyr_VLat)),         &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='Checking r Rot Rot v with rigid rotation(E-W flow)', &
       answer = xyr_RadRotRot,                                       &
       check = xyr_wr(wr_RadRotRot_xyr_xyr_xyr(xyr_VLon,xyr_VLat,xyr_VRad)),&
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  ! ���β�ž����������(����ή��)
  xyr_VLon = xyr_Rad*cos(xyr_Lat)*sin(xyr_Lon)*cos(xyr_Lon)
  xyr_VLat = -xyr_Rad*sin(xyr_Lat)*cos(xyr_Lat)*sin(xyr_Lon)**2
  xyr_VRad = -xyr_Rad*(sin(xyr_Lat)**2*sin(xyr_Lon)**2 + cos(xyr_Lon)**2)
  xyr_RadRot = 0.0D0 ; xyr_RadRotRot = 0.0D0

  call AssertEqual(&
       message='Checking r Rot v with rigid rotation(N-S flow)',     &
       answer = xyr_RadRot,                                          &
       check = xyr_wr(wr_RadRot_xyr_xyr(xyr_VLon,xyr_VLat)),         &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='Checking r Rot Rot v with rigid rotation(N-S flow)', &
       answer = xyr_RadRotRot,                                       &
       check = xyr_wr(wr_RadRotRot_xyr_xyr_xyr(xyr_VLon,xyr_VLat,xyr_VRad)),&
       significant_digits = check_digits, ignore_digits = ignore     &
       )

! ��ľ���٤�ȼ���٥��ȥ��
  xyr_Polvel = xyr_Rad**2 * cos(xyr_Lat)*sin(xyr_Lon)   ! r**2 P_1^1
  !xyr_Psi = xyr_Rad**3 * cos(xyr_Lat)*sin(xyr_Lat)*sin(xyr_Lon)   ! r**3 P_2^1

  xyr_VLon =   xyr_GradLat_wq(wq_xyr(xyr_Polvel*xyr_Rad))
  xyr_VLat = - xyr_GradLon_wq(wq_xyr(xyr_Polvel*xyr_Rad))
  xyr_VRad = 0.0D0
  xyr_RadRot = 2 * xyr_Polvel                       ! r�����ߢ���(��r) = L_2��
  !xyr_RadRot = 6 * xyr_Polvel                      ! r�����ߢ���(��r) = L_2��
  xyr_RadRotRot = 0.0D0

  call AssertEqual(&
       message='Checking r Rot v with vortical field',               &
       answer = xyr_RadRot,                                          &
       check = xyr_wr(wr_RadRot_xyr_xyr(xyr_VLon,xyr_VLat)),         &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='Checking r Rot Rot v with vortical field',           &
       answer = xyr_RadRotRot,                                       &
       check = xyr_wr(wr_RadRotRot_xyr_xyr_xyr(xyr_VLon,xyr_VLat,xyr_VRad)),&
       significant_digits = check_digits, ignore_digits = ignore     &
       )

! ��ľ®�٤�ȼ���٥��ȥ��
  xyr_Torvel = 0.0D0
  xyr_Polvel = xyr_Rad**3 * cos(xyr_Lat)*sin(xyr_Lon)   ! r**2 P_1^1
  call wq_Potential2Vector(&
         xyr_VLon,xyr_VLat,xyr_VRad, wq_xyr(xyr_Torvel),wq_xyr(xyr_Polvel) )

  xyr_RadRot = 0.0D0
  xyr_RadRotRot = -xyr_wq(wq_L2_wq(wq_Lapla_wq(wq_xyr(xyr_Polvel))))
                 ! r�����ߢ��ߢ��ߢ���(��r) = -L_2��^2��

  call AssertEqual(&
       message='Checking r Rot v with non-vortical field',           &
       answer = xyr_RadRot,                                          &
       check = xyr_wr(wr_RadRot_xyr_xyr(xyr_VLon,xyr_VLat)),         &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='Checking r Rot Rot v with non-vortical field',       &
       answer = xyr_RadRotRot,                                       &
       check = xyr_wr(wr_RadRotRot_xyr_xyr_xyr(xyr_VLon,xyr_VLat,xyr_VRad)),&
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  !==================== wq_Potential2Vector ====================
  call MessageNotify('M','wq_module_sjpack_deriv2_test', &
       'Test for wq_Potential2Vector')

  ! ----------------- �� 1 --------------------
  ! ���β�ž��
  wq_Torvel = wq_xyr(xyr_Rad * sin(xyr_Lat))
  wq_Polvel = 0.0D0

  xyr_V0lon = xyr_Rad * cos(xyr_Lat)
  xyr_V0lat = 0.0D0
  xyr_V0Rad = 0.0D0

  call wq_Potential2Vector(&
       xyr_VLon,xyr_VLat,xyr_VRad, wq_Torvel, wq_Polvel )

  call AssertEqual(&
       message='Checking Potential2Vector(VLat) with rigid rotation(EW)',&
       answer = xyr_VLon,                                            &
       check = xyr_V0Lon,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='Checking Potential2Vector(VLat) with rigid rotation(EW)',&
       answer = xyr_VLat,                                            &
       check = xyr_V0Lat,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='Checking Potential2Vector(VRad) with rigid rotation(EW)',&
       answer = xyr_VRad,                                            &
       check = xyr_V0Rad,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  ! ----------------- �� 2 --------------------
  ! ���β�ž��(����ή)
  wq_Torvel = wq_xyr(xyr_Rad * cos(xyr_Lat) * sin(xyr_Lon))
  wq_Polvel = 0.0D0

  xyr_V0Lon = -xyr_Rad*sin(xyr_Lat)*sin(xyr_Lon)
  xyr_V0Lat = -xyr_Rad*cos(xyr_Lon)
  xyr_V0Rad = 0.0D0

  call wq_Potential2Vector(&
       xyr_VLon,xyr_VLat,xyr_VRad, wq_Torvel, wq_Polvel )

  call AssertEqual(&
       message='Checking Potential2Vector(VLat) with rigid rotation(NS)',&
       answer = xyr_VLon,                                            &
       check = xyr_V0Lon,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='Checking Potential2Vector(VLat) with rigid rotation(NS)',&
       answer = xyr_VLat,                                            &
       check = xyr_V0Lat,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='Checking Potential2Vector(VRad) with rigid rotation(NS)',&
       answer = xyr_VRad,                                            &
       check = xyr_V0Rad,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  ! ----------------- �� 3 --------------------
  ! ��̵����

  wq_Torvel = 0.0D0
  wq_Polvel = wq_xyr(xyr_Rad**3 * sin(xyr_Lat))

  xyr_V0lon = 0.0D0
  xyr_V0lat = 4 * xyr_Rad**2 * cos(xyr_Lat)
  xyr_V0rad = 2 * xyr_Rad**2 * sin(xyr_Lat)

  call wq_Potential2Vector(&
       xyr_VLon,xyr_VLat,xyr_VRad, wq_Torvel, wq_Polvel )

  call AssertEqual(&
       message='Checking Potential2Vector(VLat) with no rotation',   &
       answer = xyr_VLon,                                            &
       check = xyr_V0Lon,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='Checking Potential2Vector(VLat) with no rotation',   &
       answer = xyr_VLat,                                            &
       check = xyr_V0Lat,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='Checking Potential2Vector(VRad) with no rotation',   &
       answer = xyr_VRad,                                            &
       check = xyr_V0Rad,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  ! ----------------- �� 4 --------------------
  ! �ݥ�����®�پ�

  wq_Torvel = 0.0D0
  wq_Polvel = wq_xyr(xyr_Rad**5 * cos(xyr_Lat)*sin(xyr_Lon))

  xyr_V0lon = 6 * xyr_Rad**4 * cos(xyr_Lon)
  xyr_V0lat = - 6 * xyr_Rad**4 * sin(xyr_Lat) * sin(xyr_Lon)
  xyr_V0rad = 2 * xyr_Rad**4 * cos(xyr_Lat) * sin(xyr_Lon)

  call wq_Potential2Vector(&
       xyr_VLon,xyr_VLat,xyr_VRad, wq_Torvel, wq_Polvel )

  call AssertEqual(&
       message='Checking Potential2Vector(VLat) with poloidal field',&
       answer = xyr_VLon,                                            &
       check = xyr_V0Lon,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='Checking Potential2Vector(VLat) with poloidal field',&
       answer = xyr_VLat,                                            &
       check = xyr_V0Lat,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='Checking Potential2Vector(VRad) with poloidal field',&
       answer = xyr_VRad,                                            &
       check = xyr_V0Rad,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  !==================== wq_Potential2Rotation ====================
  call MessageNotify('M','wq_module_sjpack_deriv2_test', &
       'Test for wq_Potential2Rotation')

  ! ----------------- �� 1 --------------------
  ! ���β�ž��
  wq_Torvel = wq_xyr(xyr_Rad * sin(xyr_Lat))
  wq_Polvel = 0.0D0

  ! xyr_Vlon = xyr_Rad * cos(xyr_Lat)'
  ! xyr_Vlat = 0.0'
  ! xyr_VRad = 0.0'

  xyr_V0lon = 0.0D0
  xyr_V0lat = 2*cos(xyr_LAT)
  xyr_V0rad = 2*sin(xyr_LAT)

  call wq_Potential2Rotation(&
         xyr_VLon,xyr_VLat,xyr_VRad, wq_Torvel, wq_Polvel )

  call AssertEqual(&
       message='Checking Potential2Rotation(VLat) with rigid rotation(EW)',&
       answer = xyr_VLon,                                            &
       check = xyr_V0Lon,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='Checking Potential2Rotation(VLat) with rigid rotation(EW)',&
       answer = xyr_VLat,                                            &
       check = xyr_V0Lat,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='Checking Potential2Rotation(VRad) with rigid rotation(EW)',&
       answer = xyr_VRad,                                            &
       check = xyr_V0Rad,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

! ----------------- �� 2 --------------------
  ! ���β�ž��(����ή)
  wq_Torvel = wq_xyr(xyr_Rad * cos(xyr_Lat) * sin(xyr_Lon))
  wq_Polvel = 0.0D0

  !  xyr_VLon = -xyr_Rad*sin(xyr_Lat)*sin(xyr_Lon)
  !  xyr_VLat = -xyr_Rad*cos(xyr_Lon)
  !  xyr_VRad = 0.0

  xyr_V0Lon = 2*cos(xyr_Lon)
  xyr_V0Lat = -2*sin(xyr_Lon)*sin(xyr_Lat)
  xyr_V0Rad = 2*sin(xyr_Lon)*cos(xyr_Lat)

  call wq_Potential2Rotation(&
       xyr_VLon,xyr_VLat,xyr_VRad, wq_Torvel, wq_Polvel )

  call AssertEqual(&
       message='Checking Potential2Rotation(VLat) with rigid rotation(NS)',&
       answer = xyr_VLon,                                            &
       check = xyr_V0Lon,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='Checking Potential2Rotation(VLat) with rigid rotation(NS)',&
       answer = xyr_VLat,                                            &
       check = xyr_V0Lat,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='Checking Potential2Rotation(VRad) with rigid rotation(NS)',&
       answer = xyr_VRad,                                            &
       check = xyr_V0Rad,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  ! ----------------- �� 3 --------------------
  ! ��̵����

  wq_Torvel = 0.0D0
  wq_Polvel = wq_xyr(xyr_Rad * sin(xyr_Lat))

  ! xyr_Vlon = 0
  ! xyr_Vlat = 2 * cos(xyr_Lat)
  ! xyr_Vrad = 2 * sin(xyr_Lat)

  xyr_V0lon = 0.0D0
  xyr_V0lat = 0.0D0
  xyr_V0rad = 0.0D0

  call wq_Potential2Rotation(&
       xyr_VLon,xyr_VLat,xyr_VRad, wq_Torvel, wq_Polvel )

  call AssertEqual(&
       message='Checking Potential2Rotation(VLat) with no rotation',   &
       answer = xyr_VLon,                                            &
       check = xyr_V0Lon,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='Checking Potential2Rotation(VLat) with no rotation',   &
       answer = xyr_VLat,                                            &
       check = xyr_V0Lat,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='Checking Potential2Rotation(VRad) with no rotation',   &
       answer = xyr_VRad,                                            &
       check = xyr_V0Rad,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  ! ----------------- �� 4 --------------------
  ! �ݥ�����®�پ�

  wq_Torvel = 0.0D0
  wq_Polvel = wq_xyr(xyr_Rad **3 * cos(xyr_Lat)*sin(xyr_Lon))

  ! xyr_Vlon = 4 * xyr_Rad**2 * cos(xyr_Lon)
  ! xyr_Vlat = -4 * xyr_Rad**2 * sin(xyr_Lat) * sin(xyr_Lon)
  ! xyr_Vrad = 2 * xyr_Rad**2  * cos(xyr_Lat) * sin(xyr_Lon)

  xyr_V0lon = 10*xyr_Rad*sin(xyr_Lat)*sin(xyr_Lon)
  xyr_V0lat = 10*xyr_Rad*cos(xyr_Lon)
  xyr_V0rad = 0.0D0

  call wq_Potential2Rotation(&
       xyr_VLon,xyr_VLat,xyr_VRad, wq_Torvel, wq_Polvel )

  call AssertEqual(&
       message='Checking Potential2Rotation(VLat) with poloidal field',&
       answer = xyr_VLon,                                            &
       check = xyr_V0Lon,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='Checking Potential2Rotation(VLat) with poloidal field',&
       answer = xyr_VLat,                                            &
       check = xyr_V0Lat,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )

  call AssertEqual(&
       message='Checking Potential2Rotation(VRad) with poloidal field',&
       answer = xyr_VRad,                                            &
       check = xyr_V0Rad,                                            &
       significant_digits = check_digits, ignore_digits = ignore     &
       )


  call MessageNotify('M','wq_module_sjpack_deriv2_test', &
       'wq_module_sjpack derivative function test #2 succeeded!')

end program wq_module_sjpack_deriv2_test
