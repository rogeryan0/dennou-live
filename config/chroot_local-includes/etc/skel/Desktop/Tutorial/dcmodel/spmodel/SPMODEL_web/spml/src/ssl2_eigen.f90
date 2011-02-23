!--
!----------------------------------------------------------------------
!     Copyright (c) 2005 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  固有値問題サブルーチン (SSL2)
!
!履歴  2005/01/25  竹広真一
!      2006/03/08  竹広真一 コメントを RDoc 用に変更
!      2009/01/29  佐々木洋平 コメントを RDoc 用に変更
!
!++
module ssl2_eigen
  !
  != ssl2_eigen
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: ssl2_eigen.f90,v 1.4 2009-02-28 21:33:47 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  ! 
  !== 概要
  !
  ! spml/ssl2_eigen は eigmatrix モジュールの公開サブルーチン eigen
  ! 用の固有値計算用共通インターフェースを与える.
  !
  !   * 行列 AMAT の i 番目固有値を eigen_r(i), eigen_i(i) に格納
  !   * 対応する固有ベクトルを eigvec_r(:,i), eigvec_i(:,i) に格納
  !   * 格納する固有値の数は引数 eigen_r の大きさで決まる
  !   * 固有値の順番は sort と order で定められる. 
  !   * sort によって順番を定めるために用いる量を指定する. 
  !     実部(R), 実部の絶対値(RA), 虚部(I), 虚部の絶対値(IA)
  !   * reverse によって小さい順(.false.), 大きい順(.true.)を指定できる.
  !   * デフォルトは sort='R', reverse=.false.
  !
  ! 内部では DEIG1/SSL2 ルーチンによる実行列の固有値/固有ベクトル計算を
  ! 行っている. が, ユーザーは用いているライブラリとサブルーチンを意識
  ! することなく使うことができる. 
  use dc_message, only : MessageNotify

  implicit none
  private
  public deig1_ssl2

contains
  subroutine deig1_ssl2(amat,eigen_r,eigen_i,eigvec_r,eigvec_i,&
                           info,sort,reverse )
    !
    ! このサブルーチンは固有値計算用共通インターフェースを与える
    ! eigmatrix モジュールの公開サブルーチン eigen として用いられる. 
    !
    !   * 行列 AMAT の i 番目固有値を eigen_r(i), eigen_i(i) に格納
    !   * 対応する固有ベクトルを eigvec_r(:,i), eigvec_i(:,i) に格納
    !   * 格納する固有値の数は引数 eigen_r の大きさで決まる
    !
    !   * 固有値の順番は sort と order で定められる. 
    !   * sort によって順番を定めるために用いる量を指定する. 
    !     実部(R), 実部の絶対値(RA), 虚部(I), 虚部の絶対値(IA)
    !   * reverse によって小さい順(.false.), 大きい順(.true.)を指定できる.
    !   * デフォルトは sort='R', reverse=.false.
    !
    ! 内部では DEIG1/SSL2 ルーチンによる実行列の固有値/固有ベクトル計算を
    ! 行っている. が, ユーザーは用いているライブラリとサブルーチンを意識
    ! することなく使うことができる. 
    !
    interface 
       function indexx(arrin)
         implicit none
         real(8), dimension(:), intent(in)  :: arrin
         integer, dimension(size(arrin))    :: indexx
       end function indexx
    end interface

   !------------ 引数 ------------
    real(8), dimension(:,:)                   :: amat      ! 入力正方行列
    real(8), intent(out), dimension(:)        :: eigen_r   ! 固有値実数部
    real(8), intent(out), dimension(:)        :: eigen_i   ! 固有値虚数部
    real(8), intent(out), &
      dimension(size(amat,1),size(eigen_r))   :: eigvec_r  ! 固有ベクトル実部
    real(8), intent(out), &
      dimension(size(amat,1),size(eigen_i))   :: eigvec_i  ! 固有ベクトル虚部
    integer, intent(out)                      :: info      ! ステータス
    character(len=2), intent(in), optional    :: sort      ! 並び変えの量
    logical, intent(in), optional             :: reverse   ! 並び変えスイッチ

   !------------ 作業変数 ------------
    real(8), dimension(size(amat,1))              :: wr    ! 固有値実数部
    real(8), dimension(size(amat,1))              :: wi    ! 固有値虚数部
    real(8), dimension(size(amat,1),size(amat,1)) :: vec   ! 固有ベクトル
    real(8), dimension(size(amat,1))              :: work  ! 作業変数
    integer, dimension(size(amat,1))              :: index ! 並び変え用
    integer, parameter :: mode=0! DEIG1へ渡すスイッチ

    integer :: nm, i, j

    !------- 形状チェック ------
    if (size(amat,1) /= size(amat,2))then
       call MessageNotify('E','DEIG1_SSL2','Input matrix not square')
    else
       nm = size(amat,1)
    endif

    !------- DEIG1/SSL2 による計算 ------
    call deig1(amat,nm,nm,mode,wr,wi,vec,work,info)

    !------- サブルーチンエラー処理 -------
    if ( info /= 0 ) then
       call MessageNotify('W','DEIG1_SSL2',&
            'Error in calculating eigenvalues/vectors...',i=(/info/) )
       return
    endif

    !------- 固有ベクトル入れ換え -------
    if ( present(sort) ) then
       if ( sort == 'RA' ) then          ! 固有値実部の絶対値
          index=indexx(abs(wr))
       elseif ( trim(sort) == 'I' ) then ! 固有値虚部
          index=indexx(wi)
       elseif ( sort == 'IA' ) then      ! 固有値虚部の絶対値
          index=indexx(abs(wi))
       else
          index=indexx(wr)               ! defaultは固有値実部
       endif
    else
       index=indexx(wr)                  ! defaultは固有値実部
    endif

    if ( present(reverse) )then
       if ( reverse ) then               ! 大きい順
          index=index(size(index):1:-1)
       endif
    endif

    do i=1,size(eigen_r)
       j = index(i)
       eigen_r(i) = wr(j)
       eigen_i(i) = wi(j)

       if ( wi(j) == 0 ) then
          eigvec_r(:,i) = vec(:,j)
          eigvec_i(:,i) = 0.0
       else if ( wi(j) == -wi(j+1) ) then
          eigvec_r(:,i) = vec(:,j)
          eigvec_i(:,i) = vec(:,j+1)
       else
          eigvec_r(:,i) = vec(:,j-1)
          eigvec_i(:,i) = -vec(:,j)
       endif
    enddo

  end subroutine deig1_ssl2

end module ssl2_eigen

