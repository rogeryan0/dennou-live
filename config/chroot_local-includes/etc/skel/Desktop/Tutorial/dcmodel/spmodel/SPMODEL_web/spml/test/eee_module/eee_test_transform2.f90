!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  eee_module テストプログラム (正逆変換)
!
!履歴  2008/05/02  竹広真一
!      2008/05/10  竹広真一  解像度複数設定チェック
!
!備考  少なくとも一つ波数 が 0 のスペクトル成分のいくつかが逆負号になる. 
!
program eee_test_transform

  use dc_message, only : MessageNotify
  use eee_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=32, jm=32, km=32          ! 格子点の設定(X,Y,Z)
  integer, parameter :: lm=10, mm=10, nm=10          ! 切断波数の設定(X,Y,Z)

  integer, parameter :: im2=64, jm2=64, km2=64       ! 格子点の設定(X,Y,Z)
  integer, parameter :: lm2=21, mm2=21, nm2=21       ! 切断波数の設定(X,Y,Z)

 !---- 変数 ----
  real(8)            :: zyx_Data(0:km-1,0:jm-1,0:im-1)    ! 格子データ
  real(8)            :: eee_Data(-nm:nm,-mm:mm,-lm:lm)    ! スペクトルデータ

  real(8)            :: zyx_Data2(0:km2-1,0:jm2-1,0:im2-1)    ! 格子データ
  real(8)            :: eee_Data2(-nm2:nm2,-mm2:mm2,-lm2:lm2) ! スペクトルデータ
  real(8)            :: eee_Data3(-nm:nm,-mm:mm,-lm:lm)    ! スペクトルデータ

  integer            :: l=5,m=3,n=2
  integer            :: id1, id2, id3

 !---- 座標変数など ----
  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! 判定誤差

  call MessageNotify('M','eee_test_transform', &
       'eee_module transform function tests')

 !---------------- 座標値の設定 ---------------------
  call eee_initial(im,jm,km,lm,mm,nm,id1)
  call eee_initial(im2,jm2,km2,lm2,mm2,nm2,id2)
  call eee_initial(im2,jm2,km2,lm,mm,nm,id3)
  call eee_initial(im2,jm2,km2,lm2,mm2,nm2)
  call eee_initial(im2,jm2,km2,lm2,mm2,nm2)
  call eee_initial(im2,jm2,km2,lm2,mm2,nm2)
  call eee_initial(im2,jm2,km2,lm2,mm2,nm2)
  call eee_initial(im2,jm2,km2,lm2,mm2,nm2)
  call eee_initial(im2,jm2,km2,lm2,mm2,nm2)
  call eee_initial(im2,jm2,km2,lm2,mm2,nm2)
  call eee_initial(im2,jm2,km2,lm2,mm2,nm2)

 !------------------- 初期値設定 ----------------------
  write(6,*) '*** Test of eee_module : grid -> spectral -> grid transformation check.'
  write(6,*) '  The result will be printed '
  write(6,*) '  only when the error is larger than ', eps
  write(6,*)

!!$  write(6,*) '  Input wavenumbers of the grid data, l,m and n :'
!!$  read(5,*) l,m,n
!!$  write(6,*) '  l,m,n = ', l,m,n

  ! id1
  write(*,*) 'for id1'
  call eee_ChangeResolution(id1)

  zyx_Data = sin(l*zyx_X) * sin(m*zyx_Y) * sin(n*zyx_Z)

  eee_Data = 0.0 
  if ( l /= 0 .and. m/=0 .and. n/=0 )then
     eee_Data(-n,-m,-l) = 0.125 ; eee_Data(-n,m,-l)=-0.125
     eee_Data(n,-m,-l) =-0.125  ; eee_Data(n,m,-l) = 0.125
  endif

  call check3d(eee_zyx(zyx_Data)-eee_Data, eps, &
       'Transform sin(l*X)*sin(m*Y)**sin(n*Z)')
  call check3d(zyx_eee(eee_zyx(zyx_Data))-zyx_Data, eps, &
       'Inverse transform sin(l*X)*sin(m*Y)**sin(n*Z)')

  zyx_Data = cos(l*zyx_X) * cos(m*zyx_Y) * cos(n*zyx_Z)
  eee_Data = 0.0 

  if ( l /= 0 .and. m/=0 .and. n/=0 )then
     eee_Data(n,m,l) = 0.125  ; eee_Data(-n,m,l) = 0.125  
     eee_Data(n,-m,l) = 0.125  ; eee_Data(-n,-m,l) = 0.125 
  elseif( l==0 .and. m/=0 .and. n/=0 )then
     eee_Data(n,m,0) = -0.25 ;  eee_Data(-n,m,0) = -0.25
  elseif( l/=0 .and. m==0 .and. n/=0 )then
     eee_Data(n,0,l) = -0.25 ;  eee_Data(-n,0,l) = -0.25
  elseif( l/=0 .and. m/=0 .and. n==0 )then
     eee_Data(0,m,l) = -0.25 ;  eee_Data(0,-m,l) = -0.25
  elseif( l==0 .and. m==0 .and. n/=0 )then
     eee_Data(n,0,0) = 0.5
  elseif( l/=0 .and. m==0 .and. n==0 )then
     eee_Data(0,0,l) = -0.5
  elseif( l==0 .and. m/=0 .and. n==0 )then
     eee_Data(0,m,0) = -0.5
  endif
     
  call check3d(eee_zyx(zyx_Data)-eee_Data, eps, &
       'Transform cos(l*X)*cos(m*Y)*cos(n*Z)')
  call check3d(zyx_eee(eee_zyx(zyx_Data))-zyx_Data, eps,&
       'Inverse transoform cos(l*X)*cos(m*Y)*cos(n*Z)')

  zyx_Data = sin(l*zyx_X) * cos(m*zyx_Y) * cos(n*zyx_Z)
  eee_Data = 0.0
  if ( l /= 0 .and. m/=0 .and. n/=0 )then
     eee_Data(-n,-m,-l) = -0.125  ; eee_Data(n,-m,-l) = -0.125  
     eee_Data(-n,m,-l) = -0.125  ; eee_Data(n,m,-l) = -0.125  
  elseif ( l /= 0 .and. m==0 .and. n/=0 )then
     eee_Data(-n,0,-l) = 0.25  ;  eee_Data(n,0,-l) = 0.25
  elseif ( l /= 0 .and. m/=0 .and. n==0 )then
     eee_Data(0,m,-l) = 0.25  ;  eee_Data(0,-m,-l) = 0.25
  elseif ( l /= 0 .and. m==0 .and. n==0 )then
     eee_Data(0,0,-l) = 0.5
  endif
  call check3d(eee_zyx(zyx_Data)-eee_Data, eps, &
       'Transform sin(l*X)*cos(m*Y)*cos(n*Z)')
  call check3d(zyx_eee(eee_zyx(zyx_Data))-zyx_Data, eps, &
       'Inverse transform sin(l*X)*cos(m*Y)*cos(n*Z)')

  zyx_Data = cos(l*zyx_X) * sin(m*zyx_Y) * sin(n*zyx_Z)
  eee_Data = 0.0 ;
  if ( l /= 0 .and. m/=0 .and. n/=0 )then
     eee_Data(n,m,l)  = -0.125  ; eee_Data(n,-m,l)  = 0.125  
     eee_Data(-n,m,l) = 0.125  ; eee_Data(-n,-m,l) = -0.125  
  elseif ( l == 0 .and. m/=0 .and. n/=0 )then
     eee_Data(n,m,0) = 0.25  ;  eee_Data(-n,m,0) = -0.25
  endif

  call check3d(eee_zyx(zyx_Data)-eee_Data, eps, &
       'Transform cos(l*X)*sin(m*Y)*sin(n*Z)')
  call check3d(zyx_eee(eee_zyx(zyx_Data))-zyx_Data, eps,&
       'Inverse transform cos(l*X)*sin(m*Y)*sin(n*Z)')

  ! id2
  write(*,*) 'for id2'
  call eee_ChangeResolution(id2)

  zyx_Data2 = sin(l*zyx_X) * sin(m*zyx_Y) * sin(n*zyx_Z)

  eee_Data2 = 0.0 
  if ( l /= 0 .and. m/=0 .and. n/=0 )then
     eee_Data2(-n,-m,-l) = 0.125 ; eee_Data2(-n,m,-l)=-0.125
     eee_Data2(n,-m,-l) =-0.125  ; eee_Data2(n,m,-l) = 0.125
  endif

  call check3d(eee_zyx(zyx_Data2)-eee_Data2, eps, &
       'Transform sin(l*X)*sin(m*Y)**sin(n*Z)')
  call check3d(zyx_eee(eee_zyx(zyx_Data2))-zyx_Data2, eps, &
       'Inverse transform sin(l*X)*sin(m*Y)**sin(n*Z)')

  zyx_Data2 = cos(l*zyx_X) * cos(m*zyx_Y) * cos(n*zyx_Z)
  eee_Data2 = 0.0 

  if ( l /= 0 .and. m/=0 .and. n/=0 )then
     eee_Data2(n,m,l) = 0.125  ; eee_Data2(-n,m,l) = 0.125  
     eee_Data2(n,-m,l) = 0.125  ; eee_Data2(-n,-m,l) = 0.125 
  elseif( l==0 .and. m/=0 .and. n/=0 )then
     eee_Data2(n,m,0) = -0.25 ;  eee_Data2(-n,m,0) = -0.25
  elseif( l/=0 .and. m==0 .and. n/=0 )then
     eee_Data2(n,0,l) = -0.25 ;  eee_Data2(-n,0,l) = -0.25
  elseif( l/=0 .and. m/=0 .and. n==0 )then
     eee_Data2(0,m,l) = -0.25 ;  eee_Data2(0,-m,l) = -0.25
  elseif( l==0 .and. m==0 .and. n/=0 )then
     eee_Data2(n,0,0) = 0.5
  elseif( l/=0 .and. m==0 .and. n==0 )then
     eee_Data2(0,0,l) = -0.5
  elseif( l==0 .and. m/=0 .and. n==0 )then
     eee_Data2(0,m,0) = -0.5
  endif
     
  call check3d(eee_zyx(zyx_Data2)-eee_Data2, eps, &
       'Transform cos(l*X)*cos(m*Y)*cos(n*Z)')
  call check3d(zyx_eee(eee_zyx(zyx_Data2))-zyx_Data2, eps,&
       'Inverse transoform cos(l*X)*cos(m*Y)*cos(n*Z)')

  zyx_Data2 = sin(l*zyx_X) * cos(m*zyx_Y) * cos(n*zyx_Z)
  eee_Data2 = 0.0
  if ( l /= 0 .and. m/=0 .and. n/=0 )then
     eee_Data2(-n,-m,-l) = -0.125  ; eee_Data2(n,-m,-l) = -0.125  
     eee_Data2(-n,m,-l) = -0.125  ; eee_Data2(n,m,-l) = -0.125  
  elseif ( l /= 0 .and. m==0 .and. n/=0 )then
     eee_Data2(-n,0,-l) = 0.25  ;  eee_Data2(n,0,-l) = 0.25
  elseif ( l /= 0 .and. m/=0 .and. n==0 )then
     eee_Data2(0,m,-l) = 0.25  ;  eee_Data2(0,-m,-l) = 0.25
  elseif ( l /= 0 .and. m==0 .and. n==0 )then
     eee_Data2(0,0,-l) = 0.5
  endif
  call check3d(eee_zyx(zyx_Data2)-eee_Data2, eps, &
       'Transform sin(l*X)*cos(m*Y)*cos(n*Z)')
  call check3d(zyx_eee(eee_zyx(zyx_Data2))-zyx_Data2, eps, &
       'Inverse transform sin(l*X)*cos(m*Y)*cos(n*Z)')

  zyx_Data2 = cos(l*zyx_X) * sin(m*zyx_Y) * sin(n*zyx_Z)
  eee_Data2 = 0.0 ;
  if ( l /= 0 .and. m/=0 .and. n/=0 )then
     eee_Data2(n,m,l)  = -0.125  ; eee_Data2(n,-m,l)  = 0.125  
     eee_Data2(-n,m,l) = 0.125  ; eee_Data2(-n,-m,l) = -0.125  
  elseif ( l == 0 .and. m/=0 .and. n/=0 )then
     eee_Data2(n,m,0) = 0.25  ;  eee_Data2(-n,m,0) = -0.25
  endif

  call check3d(eee_zyx(zyx_Data2)-eee_Data2, eps, &
       'Transform cos(l*X)*sin(m*Y)*sin(n*Z)')
  call check3d(zyx_eee(eee_zyx(zyx_Data2))-zyx_Data2, eps,&
       'Inverse transform cos(l*X)*sin(m*Y)*sin(n*Z)')

  ! id1 and id3
  write(*,*) 'for id1 and id3'

  call eee_ChangeResolution(id1)
  zyx_Data = sin(l*zyx_X) * sin(m*zyx_Y) * sin(n*zyx_Z)
  eee_Data = eee_zyx(zyx_Data)
  call eee_ChangeResolution(id3)
  zyx_Data2 = sin(l*zyx_X) * sin(m*zyx_Y) * sin(n*zyx_Z)
  eee_Data3 = eee_zyx(zyx_Data2)
  call check3d(eee_Data3-eee_Data, eps, &
       'Transform sin(l*X)*sin(m*Y)**sin(n*Z)')

  call eee_ChangeResolution(id1)
  zyx_Data = cos(l*zyx_X) * cos(m*zyx_Y) * cos(n*zyx_Z)
  eee_Data = eee_zyx(zyx_Data)
  call eee_ChangeResolution(id3)
  zyx_Data2 = cos(l*zyx_X) * cos(m*zyx_Y) * cos(n*zyx_Z)
  eee_Data3 = eee_zyx(zyx_Data2)
  call check3d(eee_Data3-eee_Data, eps, &
       'Transform cos(l*X)*cos(m*Y)*cos(n*Z)')

  call eee_ChangeResolution(id1)
  zyx_Data = sin(l*zyx_X) * cos(m*zyx_Y) * cos(n*zyx_Z)
  eee_Data = eee_zyx(zyx_Data)
  call eee_ChangeResolution(id3)
  zyx_Data2 = sin(l*zyx_X) * cos(m*zyx_Y) * cos(n*zyx_Z)
  eee_Data3 = eee_zyx(zyx_Data2)
  call check3d(eee_Data3-eee_Data, eps, &
       'Transform sin(l*X)*cos(m*Y)*cos(n*Z)')

  call eee_ChangeResolution(id1)
  zyx_Data = cos(l*zyx_X) * sin(m*zyx_Y) * sin(n*zyx_Z)
  eee_Data = eee_zyx(zyx_Data)
  call eee_ChangeResolution(id3)
  zyx_Data2 = cos(l*zyx_X) * sin(m*zyx_Y) * sin(n*zyx_Z)
  eee_Data3 = eee_zyx(zyx_Data2)
  call check3d(eee_Data3-eee_Data, eps, &
       'Transform cos(l*X)*sin(m*Y)*sin(n*Z)')

  call MessageNotify('M','eee_test_transform', &
       'eee_module transform function tests succeeded!')

 stop
contains

  subroutine check3d(var,eps,funcname) ! 絶対値が eps 以上の var の要素を出力
    real(8) :: var(:,:,:)              ! 判定する配列
    real(8) :: eps                     ! 誤差
    character(len=*), optional :: funcname
    integer i, j, k

    if ( present(funcname) )then
       write(6,*) '  Checking ', funcname, '...'
    endif

    do k=1,size(var,3)
       do j=1,size(var,2)
          do i=1,size(var,1)
             if (abs(var(i,j,k)) .gt. eps ) then
                write(6,*) &
                  '    Value larger than EPS : i= ', i, '  j= ', j, '  k= ', k, &
                  var(i,j,k)
                call MessageNotify('E','eee_test_transform', &
                  'transform error too large')
             endif
          enddo
       enddo
    enddo
  end subroutine check3d

end program eee_test_transform

