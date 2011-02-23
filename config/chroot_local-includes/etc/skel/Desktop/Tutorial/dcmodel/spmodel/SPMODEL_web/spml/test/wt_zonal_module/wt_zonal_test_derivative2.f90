!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  wt_zonal_module テストプログラム
!
!      関数のテスト
!      xyz_GradLon_wt, xyz_GradLat_wt, wt_Div_xyz_xyz_xyz
!  
!履歴  2008/12/29  竹広真一
!
program wt_zonal_test_derivative2

  use dc_message, only : MessageNotify
  use wt_zonal_module
  implicit none

  integer,parameter  :: im=1, jm=16, km=16   ! 格子点の設定(経度, 緯度, 動径)
  integer,parameter  :: nm=10, lm=16         ! 切断波数の設定(水平, 動径)
  real(8),parameter  :: ri=0.5, ro=1.5      ! 内外半径

  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_VLon
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_VLat
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_VRad
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_GradLon
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_GradLat
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_Div
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_Data
  real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_Psi

  real(8), parameter :: eps = 1D-10
  integer, parameter :: n=2

  integer :: i,j,k

  call MessageNotify('M','wt_zonal_test_derivative2', &
       'wt_zonal_module derivative function test #2')

  call wt_Initial(im,jm,km,nm,lm,ri,ro)

  write( 6,* ) 'Test for xyz_GradLon_wt, xyz_GradLat_wt, wt_Div_xyz_xyz_xyz.'
  write( 6,* ) 'Output is displayed if computational error is larger than',eps

! ----------------- 例 1 --------------------
  xyz_VRad = 0.0D0
  xyz_Psi = xyz_Rad**n * sin(xyz_Lat)   ! r**2 P_1^0

  xyz_GradLon =  0.0D0
  xyz_GradLat =  xyz_Rad**(n-1)*cos(xyz_Lat)

  xyz_Div = - 2.0D0* xyz_Psi/xyz_Rad**2.0D0

  write(6,*)
  write(6,*)'P11 field'
  call checkresult

! ----------------- 例 2 --------------------
  xyz_VRad = 0.0D0
  xyz_Psi = xyz_Rad**n * (3.0D0/2.0D0*sin(xyz_Lat)**2 - 1.0D0/2.0D0)  ! P_2^0

  xyz_GradLon =  0.0D0
  xyz_GradLat =  xyz_Rad**(n-1)*3*sin(xyz_Lat)*cos(xyz_Lat)

  xyz_Div = - 6* xyz_Psi/xyz_Rad**2

  write(6,*)
  write(6,*)'P21 field'
  call checkresult

  call MessageNotify('M','wt_zonal_test_derivative2', &
       'wt_zonal_module derivative function test #2 succeeded!')

  stop
contains

  subroutine checkresult

    xyz_VLon =  xyz_GradLon_wt(wt_xyz(xyz_Psi))
    write(6,*)'Checking GradLon (1/r cos(Lat) d/dLon)'
    do k=0,km
       do j=1,jm
          do i=0,im-1
            if (( abs(xyz_VLon(i,j,k)-xyz_GradLon(i,j,k)) > eps )) then 
              write(6,*) i,j,k, xyz_VLon(i,j,k), xyz_GradLon(i,j,k)
              call MessageNotify('E','wt_test_derivative2', &
                'Derivative error too large.')
            endif
          end do
        end do
      end do

    xyz_VLat =  xyz_GradLat_wt(wt_xyz(xyz_Psi))
    write(6,*)'Checking GradLat (1/r dLat)'
    do k=0,km
       do j=1,jm
          do i=0,im-1
             if ( abs(xyz_VLat(i,j,k)-xyz_GradLat(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_VLat(i,j,k), xyz_GradLat(i,j,k)
                call MessageNotify('E','wt_test_derivative2', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

    xyz_Data = xyz_wt(wt_Div_xyz_xyz_xyz(xyz_VLon,xyz_VLat,xyz_VRad))

    write(6,*)'Checking Divergence'
    do k=0,km
       do j=1,jm
          do i=0,im-1
             if ( abs(xyz_Data(i,j,k)-xyz_Div(i,j,k)) > eps ) then
                write(6,*) i,j,k, xyz_Data(i,j,k), xyz_Div(i,j,k)
                call MessageNotify('E','wt_test_derivative2', &
                     'Derivative error too large.')
             endif
          end do
       end do
    end do

  end subroutine checkresult

end program wt_zonal_test_derivative2

