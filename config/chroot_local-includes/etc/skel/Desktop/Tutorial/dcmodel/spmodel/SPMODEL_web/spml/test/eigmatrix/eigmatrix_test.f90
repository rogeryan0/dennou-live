!----------------------------------------------------------------------
!     Copyright (c) 2005 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  eigmatrix テストプログラム
!
!履歴  2005/01/26  竹広真一
!      2007/11/02  竹広真一  メッセージ, 正誤判定追加
!      2008/12/05 佐々木洋平. 文字列 sort の長さを 2 に
!
!備考  lapack, blas ライブラリが必要. Debian/GNU Linux + Fujitsu frt ならば
!      lapack, lapack-deb パッケージをインストールして, 
!         -llapack -lblas -L/usr/lib/gcc-lib/i386-linux/2.95.4 -lg2c
!      といったオプションをつけるべし. 
!
!
program eigmatrix_test

  use dc_message, only : MessageNotify
  use eigmatrix
  implicit none

  real(8), dimension(:,:), allocatable  :: amatrix
  real(8), dimension(:),   allocatable  :: eigval_r, eigval_i
  real(8), dimension(:,:), allocatable  :: eigvec_r, eigvec_i

  real(8), dimension(:),   allocatable          :: eigval_r_sol, eigval_i_sol
  real(8), dimension(:,:), allocatable          :: eigvec_r_sol, eigvec_i_sol
  complex(kind(0d0)), dimension(:),allocatable  :: cwork
  real(8),dimension(:), allocatable             :: rwork
  integer                     :: info
  integer                     :: i, j

  real(8),parameter  :: eps=1.0D-10
  real(8)            :: error

  call MessageNotify('M','eigmatrix_test','Test of eigmatrix') 

!--------------- 3x3 行列 ------------------
!  VALUE = 1, 2, 3
!  VECTOR = (-15,12,4), (-16,13,4), (-4,3,1)

  allocate(amatrix(3,3))
  allocate(eigval_r(3), eigval_i(3))
  allocate(eigvec_r(3,3), eigvec_i(3,3))

  allocate(eigval_r_sol(3), eigval_i_sol(3))
  allocate(eigvec_r_sol(3,3), eigvec_i_sol(3,3))

  !---- 行列 ----
  amatrix(:,1) = (/ 33, -24,  -8 /)
  amatrix(:,2) = (/ 16, -10,  -4 /)
  amatrix(:,3) = (/ 72, -57, -17 /)

  !---- 正解 ----
  eigval_r_sol = (/3.0D0,2.0D0,1.0D0/)
  eigval_i_sol = (/0.0D0,0.0D0,0.0D0/)

  eigvec_r_sol(:,1) = (/-4.0D0,3.0D0,1.0D0/)
  eigvec_r_sol(:,2) = (/-16.0D0,13.0D0,4.0D0/)
  eigvec_r_sol(:,3) = (/-15.0D0,12.0D0,4.0D0/)

  eigvec_i_sol = 0.0D0
  
  !---- 固有値計算 ----
  call eigen(amatrix,eigval_r,eigval_i,eigvec_r,eigvec_i,info,&
             sort=' R',reverse=.true.)

  !---- 出力 ----
  write(6,*)
  do i=1,3
     write(6,*) 'EIGENVALUE  : ', eigval_r(i),eigval_i(i)
     write(6,*) 'EIGENVECTOR : ', (eigvec_r(j,i),eigvec_i(j,i),j=1,3)
  enddo
  write(6,*)

  !---- 判定 ----
  error = maxval(abs(eigval_r_sol - eigval_r)+abs(eigval_i_sol-eigval_i))
  if ( error > eps ) then
     call MessageNotify('E','Test of eigmatrix', &
                            '3x3 arrray eigenvalue error too large') 
  endif

  do i=1,3
     eigvec_r(:,i) = eigvec_r(:,i)/eigvec_r(1,i)
     eigvec_r_sol(:,i) = eigvec_r_sol(:,i)/eigvec_r_sol(1,i)
  enddo
  error = maxval(abs(eigvec_r_sol - eigvec_r)+abs(eigvec_i_sol-eigvec_i))
  if ( error > eps ) then
     call MessageNotify('E','Test of eigmatrix', &
                            '3x3 arrray eigenvector error too large') 
  endif

  call MessageNotify('M','Test of eigmatrix','Test of 3x3 array succeeded!') 

  deallocate(amatrix)
  deallocate(eigval_r, eigval_i)
  deallocate(eigvec_r, eigvec_i)
  deallocate(eigval_r_sol, eigval_i_sol)
  deallocate(eigvec_r_sol, eigvec_i_sol)

!--------------- 4x4 行列 ------------------
!  VALUE = 12, 1+5I, 1-5i, 2
!  VECTOR = (1,-1,1,1), (1,i,i,-1), (1,-i,-i,-1), (1,1,-1,1)

  allocate(amatrix(4,4))
  allocate(eigval_r(4), eigval_i(4))
  allocate(eigvec_r(4,4), eigvec_i(4,4))
  allocate(eigval_r_sol(4), eigval_i_sol(4))
  allocate(eigvec_r_sol(4,4), eigvec_i_sol(4,4))
  allocate(cwork(4),rwork(4))

  !---- 行列 ----
  amatrix(:,1) = (/  4,  0,  5,  3 /)
  amatrix(:,2) = (/ -5,  4, -3,  0 /)
  amatrix(:,3) = (/  0, -3,  4,  5 /)
  amatrix(:,4) = (/  3, -5,  0,  4 /)

  !---- 正解 ----
  eigval_r_sol = (/ 1.0D0,1.0D0,2.0D0,12.0D0/)
  eigval_i_sol = (/-5.0D0,5.0D0,0.0D0, 0.0D0/)

  eigvec_r_sol(:,1) = (/1.0D0, 0.0D0, 0.0D0,-1.0D0/)
  eigvec_i_sol(:,1) = (/0.0D0,-1.0D0,-1.0D0, 0.0D0/)

  eigvec_r_sol(:,2) = (/1.0D0, 0.0D0, 0.0D0,-1.0D0/)
  eigvec_i_sol(:,2) = (/0.0D0, 1.0D0, 1.0D0, 0.0D0/)

  eigvec_r_sol(:,3) = (/1.0D0, 1.0D0,-1.0D0, 1.0D0/)
  eigvec_i_sol(:,3) = (/0.0D0, 0.0D0, 0.0D0, 0.0D0/)

  eigvec_r_sol(:,4) = (/1.0D0,-1.0D0, 1.0D0, 1.0D0/)
  eigvec_i_sol(:,4) = (/0.0D0, 0.0D0, 0.0D0, 0.0D0/)

  !---- 固有値計算 ----
  call eigen(amatrix,eigval_r,eigval_i,eigvec_r,eigvec_i,info,&
            sort='RA')

  do i=4,1,-1
     cwork = (eigvec_r(i,:)+(0,1)*eigvec_i(i,:))&
          /(eigvec_r(1,:)+(0,1)*eigvec_i(1,:))
     eigvec_r(i,:) = real(cwork)
     eigvec_i(i,:) = imag(cwork)
  enddo

  !---- 出力 ----
  write(6,*)
  do i=1,4
     write(6,*) 'EIGENVALUE  : ', eigval_r(i),eigval_i(i)
     write(6,*) 'EIGENVECTOR : '
     do j=1,4
        write(6,*) eigvec_r(j,i),eigvec_i(j,i)
     enddo
  enddo

  !---- 判定 ----
  if (eigval_i(1) > 0.0 ) then
     error = eigval_r_sol(1) 
     eigval_r_sol(1) = eigval_r_sol(2)
     eigval_r_sol(2) = error

     error = eigval_i_sol(1) 
     eigval_i_sol(1) = eigval_i_sol(2)
     eigval_i_sol(2) = error

     do i=1,4
        rwork = eigvec_r_sol(:,1) 
        eigvec_r_sol(:,1) = eigvec_r_sol(:,2)
        eigvec_r_sol(:,2) = rwork
        rwork = eigvec_i_sol(:,1) 
        eigvec_i_sol(:,1) = eigvec_i_sol(:,2)
        eigvec_i_sol(:,2) = rwork
     end do
  end if
  write(6,*) eigvec_r_sol(:,1:2)
  write(6,*) eigvec_i_sol(:,1:2)

  error = maxval(abs(eigval_r_sol - eigval_r)+abs(eigval_i_sol-eigval_i))
  if ( error > eps ) then
     call MessageNotify('E','Test of eigmatrix', &
                            '4x4 arrray eigenvalue error too large') 
  endif

  error = maxval(abs(eigvec_r_sol - eigvec_r)+abs(eigvec_i_sol-eigvec_i))
  if ( error > eps ) then
     call MessageNotify('E','Test of eigmatrix', &
                            '4x4 arrray eigenvector error too large') 
  endif

  call MessageNotify('M','Test of eigmatrix','Test of 4x4 array succeeded!') 

end program eigmatrix_test

