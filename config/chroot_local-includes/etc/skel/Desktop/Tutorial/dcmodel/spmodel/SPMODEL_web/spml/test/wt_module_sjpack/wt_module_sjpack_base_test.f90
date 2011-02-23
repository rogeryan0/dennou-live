!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_module テストプログラム :: 基本変換関数のテスト
!
!履歴  2009/09/06  竹広真一   wt_test_base.f90 より改造
!
program wt_module_sjpack_base_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use wt_module_sjpack
  implicit none

  integer,parameter  :: im=32, jm=16, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ri=0.5, ro=1.5      ! 内外半径

  real(8), dimension((nm+1)**2,0:lm)       ::  wt_data
  real(8), dimension(0:im-1,1:jm,0:km)     ::  xyz_data
  real(8), dimension(0:im-1,1:jm,0:km)     ::  xyz_xi

  ! 判定誤差設定
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11

  real(8) :: pi

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','wt_module_sjpack_base_test', &
                         'wt_module_sjpack basic transformation functions tests') 

  call wt_Initial(im,jm,km,nm,lm,ri,ro)

  xyz_xi = (xyz_Rad - (ro+ri)/2 )*2/(ro-ri)

  !---- Y_1^* のテスト ----
  xyz_data = sqrt(3.0D0)*sin(xyz_Lat)*xyz_xi            ! Y_1^0 T_1
  wt_data= 0.0D0 ; wt_data(l_nm(1,0),1)=1.0D0

  call AssertEqual(&
    message='wt_xyz with Y_1^0 T_1',                              &
    answer = wt_data,                                             &
    check = wt_xyz(xyz_data),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='xyz_wt with Y_1^0 T_1',                              &
    answer = xyz_data,                                            &
    check = xyz_wt(wt_xyz(xyz_data)),                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xyz_data = sqrt(3.0D0/2)*cos(xyz_Lat)*cos(xyz_Lon)     ! Y_1^1 T_0
  wt_data= 0.0D0 ;  wt_data(l_nm(1,1),0)=1.0D0/2.0D0*2

  call AssertEqual(&
    message='wt_xyz with Y_1^1 T_0',                              &
    answer = wt_data,                                             &
    check = wt_xyz(xyz_data),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='xyz_wt with Y_1^1 T_0',                              &
    answer = xyz_data,                                            &
    check = xyz_wt(wt_xyz(xyz_data)),                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  xyz_data = -sqrt(3.0D0/2)*cos(xyz_Lat)*sin(xyz_Lon)*(2*xyz_xi**2-1) !Y_1^{-1}T_2
  wt_data= 0.0D0 ;  wt_data(l_nm(1,-1),2)=1.0D0/2.0D0

  call AssertEqual(&
    message='wt_xyz with Y_1^-1 T_2',                             &
    answer = wt_data,                                             &
    check = wt_xyz(xyz_data),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='xyz_wt with Y_1^-1 T_2',                             &
    answer = xyz_data,                                            &
    check = xyz_wt(wt_xyz(xyz_data)),                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- Y_2^* のテスト ----
  ! Y_2^0 T_3
  xyz_data = sqrt(5.0D0)*(3.0/2*sin(xyz_Lat)**2-1/2.0)*(4*xyz_xi**3-3*xyz_xi) 
  wt_data= 0.0D0 ; wt_data(l_nm(2,0),3)=1.0D0

  call AssertEqual(&
    message='wt_xyz with Y_2^0 T_3',                              &
    answer = wt_data,                                             &
    check = wt_xyz(xyz_data),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='xyz_wt with Y_2^0 T_3',                              &
    answer = xyz_data,                                            &
    check = xyz_wt(wt_xyz(xyz_data)),                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !Y_2^1 T_4
  xyz_data = sqrt(5.0D0/6)*3.0*sin(xyz_Lat)*cos(xyz_Lat)*cos(xyz_Lon) &
            *(8*xyz_xi**4 - 8*xyz_xi**2 + 1 )
  wt_data= 0.0D0 ; wt_data(l_nm(2,1),4)=1.0D0/2.0D0

  call AssertEqual(&
    message='wt_xyz with Y_2^1 T_4',                              &
    answer = wt_data,                                             &
    check = wt_xyz(xyz_data),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='xyz_wt with Y_2^1 T_4',                              &
    answer = xyz_data,                                            &
    check = xyz_wt(wt_xyz(xyz_data)),                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  ! Y_2^-2
  xyz_data = -sqrt(5.0D0/24)*3.0*cos(xyz_Lat)**2*sin(2*xyz_Lon) &
            *(16*xyz_xi**5-20*xyz_xi**3+5*xyz_xi)
  wt_data= 0.0D0 ; wt_data(l_nm(2,-2),5)=1.0D0/2.0D0

  call AssertEqual(&
    message='wt_xyz with Y_2^-2 T_5',                             &
    answer = wt_data,                                             &
    check = wt_xyz(xyz_data),                                     &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='xyz_wt with Y_2^-2 T_5',                             &
    answer = xyz_data,                                            &
    check = xyz_wt(wt_xyz(xyz_data)),                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- 一般的関数のテスト ----
  xyz_data = cos(2*xyz_Lon-pi/3) &
       *(sin(xyz_Lat)-1)**2*(sin(xyz_Lat)-0.5)*(sin(xyz_Lat)+1) &
       *exp(xyz_Rad)

  call AssertEqual(&
    message='xyz_wt and wt_xyz with a general function',          &
    answer = xyz_data,                                            &
    check = xyz_wt(wt_xyz(xyz_data)),                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','wt_module_sjpack_base_test_base', &
                         'wt_module_sjpack basic functions tests succeeded!') 

end program wt_module_sjpack_base_test
