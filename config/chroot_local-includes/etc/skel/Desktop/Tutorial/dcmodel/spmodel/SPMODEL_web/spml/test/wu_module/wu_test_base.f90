!----------------------------------------------------------------------
!     Copyright (c) 2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wu_module テストプログラム :: 基本変換関数のテスト
!
!履歴  2008/01/01  竹広真一
!      2008/06/28  佐々木洋平  配列の宣言を変更
!
program wu_test_base

  use dc_message, only : MessageNotify
  use wu_module
  implicit none

  integer,parameter  :: im=16, jm=8, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=5, lm=16         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ra=2.0               ! 球半径

  real(8), dimension((nm+1)**2,0:lm) ::  wu_data
  real(8), dimension(0:im-1,1:jm,0:km)     ::  xyr_data
  real(8), dimension(0:im-1,1:jm,0:km)     ::  xyr_xi
  real(8), parameter                 ::  eps = 1.0D-13
  real(8) :: pi

  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','wu_test_base', &
                         'wu_module basic transformation functions tests') 

  call wu_Initial(im,jm,km,nm,lm,ra)

  xyr_xi = 2*(xyr_Rad/ra)**2 - 1.0D0

  !---- Y_1^* のテスト ----
  xyr_data = sqrt(3.0D0)*sin(xyr_Lat)*xyr_xi*xyr_Rad        ! Y_1^0 T_1
  wu_data= 0.0D0 ; wu_data(l_nm(1,0),1)=1.0D0

  if ( maxval(abs(wu_xyr(xyr_data)-wu_data)) > eps ) then
     call MessageNotify('E','Test of Y_1^0 T_1', &
          'Spectral transform error too large') 
  endif
  if ( maxval(abs(xyr_wu(wu_xyr(xyr_data))-xyr_data)) > eps ) then
     call MessageNotify('E','Test of Y_1^0 T_1',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of Y_1^0 T_1', &
                     'transformation tests succeeded!') 

  xyr_data = sqrt(3.0D0/2)*cos(xyr_Lat)*cos(xyr_Lon)*xyr_Rad   ! Y_1^1 T_0
  wu_data= 0.0D0 ;  wu_data(l_nm(1,1),0)=1.0D0/sqrt(2.0D0)

  if ( maxval(abs(wu_xyr(xyr_data)-wu_data)) > eps ) then
     call MessageNotify('E','Test of Y_1^1 T_0', &
          'Spectral transform error too large') 
  endif
  if ( maxval(abs(xyr_wu(wu_xyr(xyr_data))-xyr_data)) > eps ) then
     call MessageNotify('E','Test of Y_1^1 T_0',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of Y_1^1 T_0', &
                     'transformation tests succeeded!') 


  !Y_1^{-1}T_2
  xyr_data = -sqrt(3.0D0/2)*cos(xyr_Lat)*sin(xyr_Lon) &
               *(2*xyr_xi**2-1)*xyr_Rad
  wu_data= 0.0D0 ;  wu_data(l_nm(1,-1),2)=1.0D0/sqrt(2.0D0)
  if ( maxval(abs(wu_xyr(xyr_data)-wu_data)) > eps ) then
     call MessageNotify('E','Test of Y_1^-1 T_2', &
          'Spectral transform error too large') 
  endif
  if ( maxval(abs(xyr_wu(wu_xyr(xyr_data))-xyr_data)) > eps ) then
     call MessageNotify('E','Test of Y_1^-1 T_2',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of Y_1^-1 T_2', &
                     'transformation tests succeeded!') 

  !---- Y_2^* のテスト ----
  ! Y_2^0 T_3
!!$  xyr_data = sqrt(5.0D0)*(3.0/2*sin(xyr_Lat)**2-1/2.0) &
!!$               *(4*xyr_xi**3-3*xyr_xi) * xyr_Rad**2
  xyr_data = sqrt(5.0D0)*(3.0/2*sin(xyr_Lat)**2-1/2.0) &
               *(4*xyr_xi**3-3*xyr_xi) 
  wu_data= 0.0D0 ; wu_data(l_nm(2,0),3)=1.0D0
  if ( maxval(abs(wu_xyr(xyr_data)-wu_data)) > eps ) then
     call MessageNotify('E','Test of Y_2^0 T_3', &
          'Spectral transform error too large') 
  endif
  if ( maxval(abs(xyr_wu(wu_xyr(xyr_data))-xyr_data)) > eps ) then
     call MessageNotify('E','Test of Y_2^0 T_3',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of Y_2^0 T_3', &
                     'transformation tests succeeded!') 

  !Y_2^1 T_4
!!$  xyr_data = sqrt(5.0D0/6)*3.0*sin(xyr_Lat)*cos(xyr_Lat)*cos(xyr_Lon) &
!!$            *(8*xyr_xi**4 - 8*xyr_xi**2 + 1 )* xyr_Rad**2
  xyr_data = sqrt(5.0D0/6)*3.0*sin(xyr_Lat)*cos(xyr_Lat)*cos(xyr_Lon) &
            *(8*xyr_xi**4 - 8*xyr_xi**2 + 1 )
  wu_data= 0.0D0 ; wu_data(l_nm(2,1),4)=1.0D0/sqrt(2.0D0)
  if ( maxval(abs(wu_xyr(xyr_data)-wu_data)) > eps ) then
     call MessageNotify('E','Test of Y_2^1 T_4', &
          'Spectral transform error too large') 
  endif
  if ( maxval(abs(xyr_wu(wu_xyr(xyr_data))-xyr_data)) > eps ) then
     call MessageNotify('E','Test of Y_2^1 T_4',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of Y_2^1 T_4', &
                     'transformation tests succeeded!') 

  ! Y_2^-2
!!$  xyr_data = -sqrt(5.0D0/24)*3.0*cos(xyr_Lat)**2*sin(2*xyr_Lon) &
!!$            *(16*xyr_xi**5-20*xyr_xi**3+5*xyr_xi)* xyr_Rad**2
  xyr_data = -sqrt(5.0D0/24)*3.0*cos(xyr_Lat)**2*sin(2*xyr_Lon) &
            *(16*xyr_xi**5-20*xyr_xi**3+5*xyr_xi)
  wu_data= 0.0D0 ; wu_data(l_nm(2,-2),5)=1.0D0/sqrt(2.0D0)
  if ( maxval(abs(wu_xyr(xyr_data)-wu_data)) > eps ) then
     call MessageNotify('E','Test of Y_2^-2 T_5', &
          'Spectral transform error too large') 
  endif
  if ( maxval(abs(xyr_wu(wu_xyr(xyr_data))-xyr_data)) > eps ) then
     call MessageNotify('E','Test of Y_2^-2 T_5',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of Y_2^-2 T_5', &
                     'transformation tests succeeded!') 

  !---- 一般的関数のテスト ----
  xyr_data = cos(2*xyr_Lon-pi/3) &
       *(sin(xyr_Lat)-1)**2*(sin(xyr_Lat)-0.5)*(sin(xyr_Lat)+1) &
       *exp(xyr_Rad)
  if ( maxval(abs(xyr_wu(wu_xyr(xyr_data))-xyr_data)) > eps ) then
     write(6,*) maxval(abs(xyr_wu(wu_xyr(xyr_data))-xyr_data))
     call MessageNotify('E','Test of general function',&
          'Spectral and inverse transorms error too large') 
  endif
  call MessageNotify('M','Test of genral function', &
       'Test of general function succeeded!') 

  call MessageNotify('M','wu_test_base', &
                         'wu_base_module functions tests succeeded!') 

end program wu_test_base
