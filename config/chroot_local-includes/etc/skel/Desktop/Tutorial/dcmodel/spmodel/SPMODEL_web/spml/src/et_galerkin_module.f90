!--
!----------------------------------------------------------------------
! Copyright(c) 2006-2010 SPMDODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!表題  et_galerkin_module
!
!      2 次元水路領域
!      Fourier 展開 + チェビシェフ−ガラーキン法用モジュール
!
!履歴  2006/01/24  竹広真一  新規作成
!      2006/02/28  竹広真一  コメントを RDoc 用に変更
!      2009/06/03  竹広真一  微分関数追加修正
!      2009/06/05  竹広真一  et_ef, et_eh 係数名変更, コメント増強
!      2009/07/05  竹広真一  et_LaplaInv_et 追加
!      2009/07/31  竹広真一  境界条件計算用配列を threadprivate 指定(OpenMP)
!      2010/03/10  佐々木洋平  threadprivate 削除(コンパイラ依存)
!
!++
module et_galerkin_module
  !
  != et_galerkin_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: et_galerkin_module.f90,v 1.8 2010-03-02 10:09:03 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/et_galerkin_module モジュールは 2 次元チャネル領域での流体運動
  ! をスペクトル法により数値計算するための Fortran90 関数を提供する.
  ! 周期的な境界条件を扱うための X 方向へのフーリエ変換, 
  ! 境界壁を扱うための Y 方向のチェビシェフ変換(チェビシェフーガラーキン法)
  ! のスペクトル計算のためのさまざまな関数を提供する
  !
  ! 内部で et_module, および境界条件を解くためにチェビシェフーガラーキン法の
  ! モジュール群(at_ab_galerkin_ND, at_ad_galerkin_DD, at_af_galerkin_MM,
  ! at_ah_galerkin_MMex, at_ap_galerkin_DN, at_aq_galerkin_RRFF,
  ! at_av_galerkin_NN) を用いている.
  !
  !== 関数・変数の名前と型について
  !
  !=== 命名法
  !
  ! * 関数名の先頭 (et_, yx_, x_, y_...) は, 返す値の形を示している.
  !
  !   et_ :: 2次元スペクトルデータ
  !   yx_ :: 2 次元格子点データ
  !   x_  :: X 方向 1 次元格子点データ
  !   y_  :: Y 方向 1 次元格子点データ
  !
  !   ed_ :: 2次元ガラーキンデータ(y=ymin,ymax で値が 0)
  !   ev_ :: 2次元ガラーキンデータ(y=ymin,ymax で 1 階微分値が 0)
  !   eb_ :: 2次元ガラーキンデータ(y=ymin で値が 0, ymax で 1 階微分値が 0)
  !   ep_ :: 2次元ガラーキンデータ(y=ymin で1 階微分値が 0, ymax で 値が 0)
  !   ef_ :: 2次元ガラーキンデータ(y=ymin,ymax で混合境界条件が 0)
  !   eh_ :: 2次元ガラーキンデータ(y=ymin,ymax で水平波数毎の混合境界条件が 0)
  !   eq_ :: 2次元流線関数ガラーキンデータ(y=ymin,ymax で自由すべり/粘着条件)
  !
  ! * 関数名の間の文字列(Dx, Dy, Lapla, LaplaInv, Jacobian)は, 
  !   その関数の作用を表している.
  !
  ! * 関数名の最後 (_et_et, _et, _yx, _x, _y...) は, 入力変数のスペクトルデータ
  !   格子点データおよびガラーキンであることを示している.
  !
  !   _et :: 2次元スペクトルデータ
  !   _yx :: 2 次元格子点データ
  !   _x  :: X 方向 1 次元格子点データ
  !   _y  :: Y 方向 1 次元格子点データ
  !   _ed :: 2次元ガラーキンデータ(y=ymin,ymax で値が 0)
  !   _ev :: 2次元ガラーキンデータ(y=ymin,ymax で 1 階微分値が 0)
  !   _eb :: 2次元ガラーキンデータ(y=ymin で値が 0, ymax で 1 階微分値が 0)
  !   _ep :: 2次元ガラーキンデータ(y=ymin で1 階微分値が 0, ymax で 値が 0)
  !   _ef :: 2次元ガラーキンデータ(y=ymin,ymax で混合境界条件が 0)
  !   _eh :: 2次元ガラーキンデータ(y=ymin,ymax で水平波数毎の混合境界条件が 0)
  !   _eq :: 2次元流線関数ガラーキンデータ(y=ymin,ymax で自由すべり/粘着条件)
  !
  !=== 各データの種類の説明
  !
  ! * yx : 2 次元格子点データ.
  !   * 変数の種類と次元は real(8), dimension(0:jm,0:im-1). 
  !   * im, jm はそれぞれ X, Y 座標の格子点数であり, サブルーチン
  !     et_galerkin_Initial にてあらかじめ設定しておく.
  !   * 第 1 次元が Y 座標の格子点位置番号, 第 2 次元が X 座標の
  !     格子点位置番号である (X, Y の順ではない)ことに注意.
  !
  ! * et : 2 次元スペクトルデータ.
  !   * 変数の種類と次元は real(8), dimension(-km:km,0:lm). 
  !   * km, lm はそれぞれ X, Y 方向の最大波数であり, サブルーチン
  !     et_galerkin_Initial にてあらかじめ設定しておく. 
  !   * スペクトルデータの格納のされ方については...
  !
  ! * ed, ev, eb, ef, eh : 2 次元ガラーキンデータ. 
  !   * 変数の種類と次元は real(8), dimension(-km:km,2:lm). 
  !   * km, lm はそれぞれ X, Y 方向の最大波数であり, サブルーチン
  !     et_galerkin_Initial にてあらかじめ設定しておく. 
  !
  ! * eq : 2 次元ガラーキンデータ. 
  !   * 変数の種類と次元は real(8), dimension(-km:km,4:lm). 
  !   * km, lm はそれぞれ X, Y 方向の最大波数であり, サブルーチン
  !     et_galerkin_Initial にてあらかじめ設定しておく. 
  !
  ! * x, y : X, Y 方向 1 次元格子点データ.
  !   * 変数の種類と次元はそれぞれ real(8), dimension(0:im-1)
  !     および real(8), dimension(0:jm).
  !
  ! * e, t : 1 次元スペクトルデータ.
  !   * 変数の種類と次元は real(8), dimension(-km:km) 
  !     および real(8), dimension(-lm:lm).
  !
  ! * ax, ay : 1 次元格子点データの並んだ 2 次元配列.
  !   * 変数の種類と次元は real(8), dimension(:,0:im-1) 
  !     および real(8), dimension(:,0:jm).
  !
  ! * ae, at : 1 次元スペクトルデータの並んだ 2 次元配列.
  !   * 変数の種類と次元は real(8), dimension(:,-km:km) 
  !     および real(8), dimension(:,0:lm).
  !
  ! * et_ で始まる関数が返す値はスペクトルデータに同じ.
  !
  ! * ed_, ev_, eb_, ep_, ef_, eh_, eq_ で始まる関数が返す値は
  !   ガラーキンデータに同じ.
  !
  ! * yx_ で始まる関数が返す値は 2 次元格子点データに同じ.
  !
  ! * x_, y_ で始まる関数が返す値は 1 次元格子点データに同じ.
  !
  ! * スペクトル/ガラーキンデータに対する微分等の作用とは, 
  !   対応する格子点データに微分などを作用させたデータを
  !   スペクトル/ガラーキン変換したものことである.
  !
  !== 変数・手続き群の要約
  !
  !==== 初期化 
  !
  ! et_galerlin_Initial :: スペクトル/ガラーキン変換の格子点数, 波数, 
  !                        領域の大きさ, 使用するガラーキン変数の設定
  ! 
  !==== 座標変数
  !
  ! x_X, y_Y     ::  格子点座標(X,Y座標)を格納した 1 次元配列
  ! x_X_Weight, y_Y_Weight ::  重み座標を格納した 1 次元配列
  ! yx_X, yx_Y   :: 格子点データの XY 座標(X,Y)(格子点データ型 2 次元配列)
  !
  !==== 基本変換
  !
  ! yx_et :: スペクトルデータから格子データへの変換
  ! et_yx :: 格子データからスペクトルデータへの変換
  ! ax_ae, x_e :: X 方向のスペクトルデータから格子データへの変換
  ! ay_at, y_t :: Y 方向のスペクトルデータから格子データへの変換
  ! ae_ax, e_x :: X 方向の格子点データからスペクトルデータへの変換
  ! at_ay, t_y :: Y 方向の格子点データからスペクトルデータへの変換
  !
  ! ed_yx      :: ガラーキンデータ->格子点変換 
  ! yx_ed      :: 格子点->ガラーキンデータ変換 
  ! ed_et, d_t :: チェビシェフ->ガラーキン変換
  ! et_ed, t_d :: ガラーキン->チェビシェフ変換
  ! ed_ey, d_y :: 格子点->ガラーキンデータ変換
  ! ey_ed, y_d :: ガラーキン->格子点データ変換
  !
  ! ev_yx      :: ガラーキンデータ->格子点変換 
  ! yx_ev      :: 格子点->ガラーキンデータ変換 
  ! ev_et, v_t :: チェビシェフ->ガラーキン変換
  ! et_ev, t_v :: ガラーキン->チェビシェフ変換
  ! ev_ey, v_y :: 格子点->ガラーキンデータ変換
  ! ey_ev, y_v :: ガラーキン->格子点データ変換
  !
  ! eb_yx      :: ガラーキンデータ->格子点変換 
  ! yx_eb      :: 格子点->ガラーキンデータ変換 
  ! eb_et, b_t :: チェビシェフ->ガラーキン変換
  ! et_eb, t_b :: ガラーキン->チェビシェフ変換
  ! eb_ey, b_y :: 格子点->ガラーキンデータ変換
  ! ey_eb, y_b :: ガラーキン->格子点データ変換
  !
  ! ep_yx      :: ガラーキンデータ->格子点変換 
  ! yx_ep      :: 格子点->ガラーキンデータ変換 
  ! ep_et, p_t :: チェビシェフ->ガラーキン変換
  ! et_ep, t_p :: ガラーキン->チェビシェフ変換
  ! ep_ey, p_y :: 格子点->ガラーキンデータ変換
  ! ey_ep, y_p :: ガラーキン->格子点データ変換
  !
  ! ef_yx      :: ガラーキンデータ->格子点変換 
  ! yx_ef      :: 格子点->ガラーキンデータ変換 
  ! ef_et, f_t :: チェビシェフ->ガラーキン変換
  ! et_ef, t_f :: ガラーキン->チェビシェフ変換
  ! ef_ey, f_y :: 格子点->ガラーキンデータ変換
  ! ey_ef, y_f :: ガラーキン->格子点データ変換
  !
  ! eh_yx      :: ガラーキンデータ->格子点変換 
  ! yx_eh      :: 格子点->ガラーキンデータ変換 
  ! eh_et      :: チェビシェフ->ガラーキン変換
  ! et_eh      :: ガラーキン->チェビシェフ変換
  ! eh_ey      :: 格子点->ガラーキンデータ変換
  ! ey_eh      :: ガラーキン->格子点データ変換
  !
  ! eq_yx      :: ガラーキンデータ->格子点変換 
  ! yx_eq      :: 格子点->ガラーキンデータ変換 
  ! eq_et, q_t :: チェビシェフ->ガラーキン変換
  ! et_eq, t_q :: ガラーキン->チェビシェフ変換
  ! eq_ey, q_y :: 格子点->ガラーキンデータ変換
  ! ey_eq, y_q :: ガラーキン->格子点データ変換
  !
  !==== 微分
  !
  ! et_Lapla_et     :: スペクトルデータにラプラシアンを作用させる
  ! et_LaplaInv_et  :: スペクトルデータに逆ラプラシアンを作用させる
  ! et_Dx_et, ae_Dx_ae, e_Dx_e :: スペクトルデータに X 微分を作用させる
  ! et_Dy_et, at_Dy_at, t_Dy_t :: スペクトルデータに Y 微分を作用させる
  ! et_Jacobian_et_et :: 2 つのスペクトルデータからヤコビアンを計算する
  !
  ! ed_Dx_ed         :: X 微分
  ! ed_Dy_ed, d_Dy_d :: Y 微分
  ! ev_Dx_ev         :: X 微分
  ! ev_Dy_ev, v_Dy_v :: Y 微分
  ! eb_Dx_eb         :: X 微分
  ! eb_Dy_eb, b_Dy_b :: Y 微分
  ! ep_Dx_ep         :: X 微分
  ! ep_Dy_ep, p_Dy_p :: Y 微分
  ! ef_Dx_ef         :: X 微分
  ! ef_Dy_ef, f_Dy_f :: Y 微分
  ! eh_Dx_eh         :: X 微分
  ! eh_Dy_eh         :: Y 微分
  ! eq_Dx_eq         :: X 微分
  ! eq_Dy_eq, q_Dy_q :: Y 微分
  !
  !==== 境界値問題
  !
  ! eq_Vor2Strm_eq :: 渦度から流線を計算する
  !
  !==== 積分・平均
  !
  ! IntYX_yx, AvrYX_yx   :: 2 次元格子点データの全領域積分および平均
  ! y_IntX_yx, y_AvrX_yx :: 2 次元格子点データの X 方向積分および平均
  ! IntX_x, AvrX_x       :: 1 次元(X)格子点データの X 方向積分および平均
  ! x_IntY_yx, x_AvrY_yx :: 2 次元格子点データの Y 方向積分および平均
  ! IntY_y, AvrY_y       :: 1 次元(Y)格子点データの Y 方向積分および平均
  !
  !== 注意
  !
  ! Y 微分計算を連続して行う場合にはガラーキンデータ用関数を用いるのでなく
  ! チェビシェフスペクトル用関数を用いること
  !
  !   × : ed_Dy_ed(ed_Dy_ed(ed)
  !   ○ : ed_et(et_Dy_et(et_Dy_et(et_ed(ed))))
  !
  use dc_message
  use lumatrix
  use et_module
  use at_ad_galerkin_DD, only: at_ad_galerkin_DD_Initial,      &
                               ed_et => ad_at, et_ed => at_ad, &
                               ed_ey => ad_ag, ey_ed => ag_ad, &
                               ed_Dy_ed => ad_Dx_ad,           &
                               d_t, t_d,                       &
                               d_y => d_g, y_d => g_d,         &
                               d_Dy_d => d_Dx_d
  use at_av_galerkin_NN, only: at_av_galerkin_NN_Initial,      &
                               ev_et => av_at, et_ev => at_av, &
                               ev_ey => av_ag, ey_ev => ag_av, &
                               ev_Dy_ev => av_Dx_av,           &
                               v_t, t_v,                       &
                               v_y => v_g, y_v => g_v,         &
                               v_Dy_v => v_Dx_v
  use at_ab_galerkin_ND, only: at_ab_galerkin_ND_Initial,      &
                               eb_et => ab_at, et_eb => at_ab, &
                               eb_ey => ab_ag, ey_eb => ag_ab, &
                               eb_Dy_eb => ab_Dx_ab,           &
                               b_t, t_b,                       &
                               b_y => b_g, y_b => g_b,         &
                               b_Dy_b => b_Dx_b
  use at_ap_galerkin_DN, only: at_ap_galerkin_DN_Initial,      &
                               ep_et => ap_at, et_ep => at_ap, &
                               ep_ey => ap_ag, ey_ep => ag_ap, &
                               ep_Dy_ep => ap_Dx_ap,           &
                               p_t, t_p,                       &
                               p_y => p_g, y_p => g_p,         &
                               p_Dy_p => p_Dx_p
  use at_af_galerkin_MM, only: at_af_galerkin_MM_Initial,      &
                               ef_et => af_at, et_ef => at_af, &
                               ef_ey => af_ag, ey_ef => ag_af, &
                               ef_Dy_ef => af_Dx_af,           &
                               f_t, t_f,                       &
                               f_y => f_g, y_f => g_f,         &
                               f_Dy_f => f_Dx_f
  use at_ah_galerkin_MMex, only: at_ah_galerkin_MMex_Initial,    &
                                 eh_et => ah_at, et_eh => at_ah, &
                                 eh_ey => ah_ag, ey_eh => ag_ah, &
                                 eh_Dy_eh => ah_Dx_ah
  use at_aq_galerkin_RRFF, only: at_aq_galerkin_RRFF_Initial,    &
                                 eq_et => aq_at, et_eq => at_aq, &
                                 eq_ey => aq_ag, ey_eq => ag_aq, &
                                 eq_Dy_eq => aq_Dx_aq,           &
                                 q_t, t_q,                       &
                                 q_y => q_g, y_q => g_q,         &
                                 q_Dy_q => q_Dx_q,               &
                                 TQ, QT, alpha, beta

  implicit none
  private

  public et_galerkin_Initial                              ! 初期化
  public eq_Vor2Strm_et                                   ! 渦度->流線

  !-- et_module 
  public x_X, y_Y, x_X_Weight, y_Y_Weight, yx_X, yx_Y     ! 座標変数
  public yx_et, et_yx                                     ! 基本変換
  public e_x, x_e, ae_ax, ax_ae                           ! 基本変換
  public t_y, y_t, at_ay, ay_at                           ! 基本変換

  public et_Dx_et, e_Dx_e, ae_Dx_ae                       ! 微分
  public et_Dy_et, t_Dy_t, at_Dy_at                       ! 微分
  public et_Lapla_et                                      ! 微分
  public et_LaplaInv_et                                   ! 微分

  public et_Jacobian_et_et                                ! 非線形計算

  public IntYX_yx, y_IntX_yx, x_IntY_yx, IntX_x, IntY_y   ! 積分
  public AvrYX_yx, y_AvrX_yx, x_AvrY_yx, AvrX_x, AvrY_y   ! 平均

  !-- at_ad_galerkin_DD
  public :: ed_yx, yx_ed               ! 格子点<->ガラーキンデータ変換
  public :: ed_et, d_t                 ! チェビシェフ->ガラーキン変換
  public :: ed_ey, d_y                 ! 格子点->ガラーキンデータ変換
  public :: et_ed, t_d                 ! ガラーキン->チェビシェフ変換
  public :: ey_ed, y_d                 ! ガラーキン->格子点データ変換
  public :: ed_Dx_ed                   ! X 微分
  public :: ed_Dy_ed, d_Dy_d           ! Y 微分

  !-- at_av_galerkin_NN
  public :: ev_yx, yx_ev               ! 格子点<->ガラーキンデータ変換
  public :: ev_et, v_t                 ! チェビシェフ->ガラーキン変換
  public :: ev_ey, v_y                 ! 格子点->ガラーキンデータ変換
  public :: et_ev, t_v                 ! ガラーキン->チェビシェフ変換
  public :: ey_ev, y_v                 ! ガラーキン->格子点データ変換
  public :: ev_Dx_ev                   ! X 微分
  public :: ev_Dy_ev, v_Dy_v           ! Y 微分

  !-- at_ab_galerkin_ND
  public :: eb_yx, yx_eb               ! 格子点<->ガラーキンデータ変換
  public :: eb_et, b_t                 ! チェビシェフ->ガラーキン変換
  public :: eb_ey, b_y                 ! 格子点->ガラーキンデータ変換
  public :: et_eb, t_b                 ! ガラーキン->チェビシェフ変換
  public :: ey_eb, y_b                 ! ガラーキン->格子点データ変換
  public :: eb_Dx_eb                   ! X 微分
  public :: eb_Dy_eb, b_Dy_b           ! Y 微分

  !-- at_ap_galerkin_DN
  public :: ep_yx, yx_ep               ! 格子点<->ガラーキンデータ変換
  public :: ep_et, p_t                 ! チェビシェフ->ガラーキン変換
  public :: ep_ey, p_y                 ! 格子点->ガラーキンデータ変換
  public :: et_ep, t_p                 ! ガラーキン->チェビシェフ変換
  public :: ey_ep, y_p                 ! ガラーキン->格子点データ変換
  public :: ep_Dx_ep                   ! X 微分
  public :: ep_Dy_ep, p_Dy_p           ! Y 微分

  !-- at_af_galerkin_MM
  public :: ef_yx, yx_ef               ! 格子点<->ガラーキンデータ変換
  public :: ef_et, f_t                 ! チェビシェフ->ガラーキン変換
  public :: ef_ey, f_y                 ! 格子点->ガラーキンデータ変換
  public :: et_ef, t_f                 ! ガラーキン->チェビシェフ変換
  public :: ey_ef, y_f                 ! ガラーキン->格子点データ変換
  public :: ef_Dx_ef                   ! X 微分
  public :: ef_Dy_ef, f_Dy_f           ! Y 微分

  !-- at_ah_galerkin_MMex
  public :: eh_yx, yx_eh               ! 格子点<->ガラーキンデータ変換
  public :: eh_et                      ! チェビシェフ->ガラーキン変換
  public :: eh_ey                      ! 格子点->ガラーキンデータ変換
  public :: et_eh                      ! ガラーキン->チェビシェフ変換
  public :: ey_eh                      ! ガラーキン->格子点データ変換
  public :: eh_Dx_eh                   ! X 微分
  public :: eh_Dy_eh                   ! Y 微分

  !-- at_aq_galerkin_RRFF
  public :: eq_yx, yx_eq               ! 格子点<->ガラーキンデータ変換
  public :: eq_et, q_t                 ! チェビシェフ->ガラーキン変換
  public :: eq_ey, q_y                 ! 格子点->ガラーキンデータ変換
  public :: et_eq, t_q                 ! ガラーキン->チェビシェフ変換
  public :: ey_eq, y_q                 ! ガラーキン->格子点データ変換
  public :: eq_Dx_eq                   ! X 微分
  public :: eq_Dy_eq, q_Dy_q           ! Y 微分


  !-- 内部変数--
  integer :: im=128, jm=64               ! 格子点の設定(X,Y)
  integer :: km=42,  lm=42               ! 切断波数の設定(X,Y)

  real(8) :: xl=2.0                      ! 領域の大きさ
  real(8), parameter :: pi=3.1415926535897932385D0

  logical :: Set_DD    =.false.        ! モジュール読み込みスイッチ
  logical :: Set_NN    =.false.        ! モジュール読み込みスイッチ
  logical :: Set_DN    =.false.        ! モジュール読み込みスイッチ
  logical :: Set_ND    =.false.        ! モジュール読み込みスイッチ
  logical :: Set_MM    =.false.        ! モジュール読み込みスイッチ
  logical :: Set_MMex  =.false.        ! モジュール読み込みスイッチ
  logical :: Set_RRFF  =.false.        ! モジュール読み込みスイッチ

  save im, jm, km, lm, xl, &
       Set_DD, Set_NN, Set_DN, Set_ND, Set_MM, Set_MMex, Set_RRFF

  contains
  !--------------- 初期化 -----------------
    subroutine et_galerkin_Initial(i,j,k,l,xmin,xmax,ymin,ymax,&
         DD,NN,DN,ND,velBC,&
         MM_cfdy0_ymin, MM_cfdy1_ymin, MM_cfdy0_ymax, MM_cfdy1_ymax,&
         MMex_cfdy0_ymin, MMex_cfdy1_ymin, MMex_cfdy0_ymax, MMex_cfdy1_ymax)
      !
      !  2 次元水路領域
      !  Fourier 展開 + チェビシェフ−ガラーキン法用モジュール
      !
      !  初期化サブルーチン
      !
      integer,intent(in) :: i, j           ! 格子点の設定(X,Y)
      integer,intent(in) :: k, l           ! 切断波数の設定(X,Y)

      real(8),intent(in) :: xmin, xmax     ! X 座標範囲
      real(8),intent(in) :: ymin, ymax     ! Y 座標範囲

      logical,intent(in),optional :: DD  ! モジュール読み込みスイッチ
      logical,intent(in),optional :: NN  ! モジュール読み込みスイッチ
      logical,intent(in),optional :: DN  ! モジュール読み込みスイッチ
      logical,intent(in),optional :: ND  ! モジュール読み込みスイッチ

      ! at_af_galerkin_MM_module 境界条件係数
      real(8),intent(in),optional :: MM_cfdy0_ymin ! (0階微分@x=ymin)
      real(8),intent(In),optional :: MM_cfdy1_ymin ! (1階微分@x=ymin)
      real(8),intent(in),optional :: MM_cfdy0_ymax ! (0階微分@x=ymax)
      real(8),intent(in),optional :: MM_cfdy1_ymax ! (1階微分@x=ymax)

      ! at_ah_galerkin_MMex_module 境界条件係数
      real(8),intent(in),optional :: MMex_cfdy0_ymin(-k:k) ! (0階微分@x=ymin)
      real(8),intent(in),optional :: MMex_cfdy1_ymin(-k:k) ! (1階微分@x=ymin)
      real(8),intent(in),optional :: MMex_cfdy0_ymax(-k:k) ! (0階微分@x=ymax)
      real(8),intent(in),optional :: MMex_cfdy1_ymax(-k:k) ! (1階微分@x=ymax)

      ! at_aq_galerkin_RRFF_module 速度場境界条件
      character(LEN=2),intent(in),optional :: velBC  ! 境界条件(RR/SS/RS/SR)

      !--------------- 引数処理 -----------------
      im=i ; jm = j ; km=k ; lm=l ; xl = xmax-xmin

      if ( present(DD) ) Set_DD = DD
      if ( present(NN) ) Set_NN = NN
      if ( present(DN) ) Set_DN = DN
      if ( present(ND) ) Set_ND = ND

      if ( present(MM_cfdy0_ymin) .AND.present(MM_cfdy1_ymin) .AND.&
           present(MM_cfdy0_ymax) .AND.present(MM_cfdy1_ymax) ) &
           Set_MM=.true.

      if ( present(MMex_cfdy0_ymin) .AND.present(MMex_cfdy1_ymin) .AND.&
           present(MMex_cfdy0_ymax) .AND.present(MMex_cfdy1_ymax) ) &
           Set_MMex=.true.

      if ( present(velBC) ) Set_RRFF = .true.

      !--------------- モジュール初期化 -----------------
      call et_Initial(im,jm,km,lm,xmin,xmax,ymin,ymax)
      if ( Set_DD ) call at_ad_galerkin_DD_Initial(jm,lm)
      if ( Set_NN ) call at_av_galerkin_NN_Initial(jm,lm)
      if ( Set_DN ) call at_ap_galerkin_DN_Initial(jm,lm)
      if ( Set_ND ) call at_ab_galerkin_ND_Initial(jm,lm)
      if ( Set_MM ) call at_af_galerkin_MM_Initial      &
                               (jm,lm, MM_cfdy0_ymax, MM_cfdy1_ymax,&
                                       MM_cfdy0_ymin, MM_cfdy1_ymin   )
      if ( Set_MMex ) call at_ah_galerkin_MMex_Initial      &
                               (jm,lm,2*km+1, &
                                MMex_cfdy0_ymax, MMex_cfdy1_ymax,&
                                MMex_cfdy0_ymin, MMex_cfdy1_ymin   )

      if ( Set_RRFF ) call at_aq_galerkin_RRFF_Initial(jm,lm,velBC)

    end subroutine et_galerkin_initial


  !--------------- 基本変換 -----------------

    function yx_ed(ed)
      !
      ! ガラーキンデータから格子データへ変換する.
      !
      real(8), dimension(0:jm,0:im-1)              :: yx_ed
      !(out) 格子点データ

      real(8), dimension(-km:km,2:lm), intent(in)  :: ed
      !(in) スペクトルデータ

      yx_ed = yx_et(et_ed(ed))

    end function yx_ed

    function ed_yx(yx)
      !
      ! 格子データからガラーキンデータへ変換する.
      !
      real(8), dimension(-km:km,2:lm)              :: ed_yx
      !(out) スペクトルデータ

      real(8), dimension(0:jm,0:im-1), intent(in)  :: yx
      !(in) 格子点データ

      ed_yx = ed_et(et_yx(yx))

    end function ed_yx

    function yx_ev(ev)
      !
      ! ガラーキンデータから格子データへ変換する.
      !
      real(8), dimension(0:jm,0:im-1)              :: yx_ev
      !(out) 格子点データ

      real(8), dimension(-km:km,2:lm), intent(in)  :: ev
      !(in) スペクトルデータ

      yx_ev = yx_et(et_ev(ev))

    end function yx_ev

    function ev_yx(yx)
      !
      ! 格子データからガラーキンデータへ変換する.
      !
      real(8), dimension(-km:km,2:lm)              :: ev_yx
      !(out) スペクトルデータ

      real(8), dimension(0:jm,0:im-1), intent(in)  :: yx
      !(in) 格子点データ

      ev_yx = ev_et(et_yx(yx))

    end function ev_yx

    function yx_eb(eb)
      !
      ! ガラーキンデータから格子データへ変換する.
      !
      real(8), dimension(0:jm,0:im-1)              :: yx_eb
      !(out) 格子点データ

      real(8), dimension(-km:km,2:lm), intent(in)  :: eb
      !(in) スペクトルデータ

      yx_eb = yx_et(et_eb(eb))

    end function yx_eb

    function eb_yx(yx)
      !
      ! 格子データからガラーキンデータへ変換する.
      !
      real(8), dimension(-km:km,2:lm)              :: eb_yx
      !(out) スペクトルデータ

      real(8), dimension(0:jm,0:im-1), intent(in)  :: yx
      !(in) 格子点データ

      eb_yx = eb_et(et_yx(yx))

    end function eb_yx

    function yx_ep(ep)
      !
      ! ガラーキンデータから格子データへ変換する.
      !
      real(8), dimension(0:jm,0:im-1)              :: yx_ep
      !(out) 格子点データ

      real(8), dimension(-km:km,2:lm), intent(in)  :: ep
      !(in) スペクトルデータ

      yx_ep = yx_et(et_ep(ep))

    end function yx_ep

    function ep_yx(yx)
      !
      ! 格子データからガラーキンデータへ変換する.
      !
      real(8), dimension(-km:km,2:lm)              :: ep_yx
      !(out) スペクトルデータ

      real(8), dimension(0:jm,0:im-1), intent(in)  :: yx
      !(in) 格子点データ

      ep_yx = ep_et(et_yx(yx))

    end function ep_yx

    function yx_ef(ef)
      !
      ! ガラーキンデータから格子データへ変換する.
      !
      real(8), dimension(0:jm,0:im-1)              :: yx_ef
      !(out) 格子点データ

      real(8), dimension(-km:km,2:lm), intent(in)  :: ef
      !(in) スペクトルデータ

      yx_ef = yx_et(et_ef(ef))

    end function yx_ef

    function ef_yx(yx)
      !
      ! 格子データからガラーキンデータへ変換する.
      !
      real(8), dimension(-km:km,2:lm)              :: ef_yx
      !(out) スペクトルデータ

      real(8), dimension(0:jm,0:im-1), intent(in)  :: yx
      !(in) 格子点データ

      ef_yx = ef_et(et_yx(yx))

    end function ef_yx

    function yx_eh(eh)
      !
      ! ガラーキンデータから格子データへ変換する.
      !
      real(8), dimension(0:jm,0:im-1)              :: yx_eh
      !(out) 格子点データ

      real(8), dimension(-km:km,2:lm), intent(in)  :: eh
      !(in) スペクトルデータ

      yx_eh = yx_et(et_eh(eh))

    end function yx_eh

    function eh_yx(yx)
      !
      ! 格子データからガラーキンデータへ変換する.
      !
      real(8), dimension(-km:km,2:lm)              :: eh_yx
      !(out) スペクトルデータ

      real(8), dimension(0:jm,0:im-1), intent(in)  :: yx
      !(in) 格子点データ

      eh_yx = eh_et(et_yx(yx))

    end function eh_yx

    function yx_eq(eq)
      !
      ! ガラーキンデータから格子データへ変換する.
      !
      real(8), dimension(0:jm,0:im-1)              :: yx_eq
      !(out) 格子点データ

      real(8), dimension(-km:km,4:lm), intent(in)  :: eq
      !(in) スペクトルデータ

      yx_eq = yx_et(et_eq(eq))

    end function yx_eq

    function eq_yx(yx)
      !
      ! 格子データからガラーキンデータへ変換する.
      !
      real(8), dimension(-km:km,4:lm)              :: eq_yx
      !(out) スペクトルデータ

      real(8), dimension(0:jm,0:im-1), intent(in)  :: yx
      !(in) 格子点データ

      eq_yx = eq_et(et_yx(yx))

    end function eq_yx

  !--------------- 微分計算 -----------------

    function ed_Dx_ed(ed)
      !
      ! 入力スペクトルデータに X 微分(∂x)を作用する.
      !
      ! スペクトルデータの X 微分とは, 対応する格子点データに X 微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに X 方向波数 k をかけて
      ! sin(kx) <-> cos(kx) 成分に入れ換える計算を行っている.
      !
      real(8), dimension(-km:km,2:lm)                :: ed_Dx_ed
      real(8), dimension(-km:km,2:lm), intent(in)    :: ed
      integer k

      do k=-km,km
         ed_Dx_ed(k,:)  =  (-2*pi*k/xl)*ed(-k,:)
      enddo
    end function ed_Dx_ed

    function ev_Dx_ev(ev)
      !
      ! 入力スペクトルデータに X 微分(∂x)を作用する.
      !
      ! スペクトルデータの X 微分とは, 対応する格子点データに X 微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに X 方向波数 k をかけて
      ! sin(kx) <-> cos(kx) 成分に入れ換える計算を行っている.
      !
      real(8), dimension(-km:km,2:lm)                :: ev_Dx_ev
      real(8), dimension(-km:km,2:lm), intent(in)    :: ev
      integer k

      do k=-km,km
         ev_Dx_ev(k,:)  =  (-2*pi*k/xl)*ev(-k,:)
      enddo
    end function ev_Dx_ev

    function eb_Dx_eb(eb)
      !
      ! 入力スペクトルデータに X 微分(∂x)を作用する.
      !
      ! スペクトルデータの X 微分とは, 対応する格子点データに X 微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに X 方向波数 k をかけて
      ! sin(kx) <-> cos(kx) 成分に入れ換える計算を行っている.
      !
      real(8), dimension(-km:km,2:lm)                :: eb_Dx_eb
      real(8), dimension(-km:km,2:lm), intent(in)    :: eb
      integer k

      do k=-km,km
         eb_Dx_eb(k,:)  =  (-2*pi*k/xl)*eb(-k,:)
      enddo
    end function eb_Dx_eb

    function ep_Dx_ep(ep)
      !
      ! 入力スペクトルデータに X 微分(∂x)を作用する.
      !
      ! スペクトルデータの X 微分とは, 対応する格子点データに X 微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに X 方向波数 k をかけて
      ! sin(kx) <-> cos(kx) 成分に入れ換える計算を行っている.
      !
      real(8), dimension(-km:km,2:lm)                :: ep_Dx_ep
      real(8), dimension(-km:km,2:lm), intent(in)    :: ep
      integer k

      do k=-km,km
         ep_Dx_ep(k,:)  =  (-2*pi*k/xl)*ep(-k,:)
      enddo
    end function ep_Dx_ep

    function ef_Dx_ef(ef)
      !
      ! 入力スペクトルデータに X 微分(∂x)を作用する.
      !
      ! スペクトルデータの X 微分とは, 対応する格子点データに X 微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに X 方向波数 k をかけて
      ! sin(kx) <-> cos(kx) 成分に入れ換える計算を行っている.
      !
      real(8), dimension(-km:km,2:lm)                :: ef_Dx_ef
      real(8), dimension(-km:km,2:lm), intent(in)    :: ef
      integer k

      do k=-km,km
         ef_Dx_ef(k,:)  =  (-2*pi*k/xl)*ef(-k,:)
      enddo
    end function ef_Dx_ef

    function eh_Dx_eh(eh)
      !
      ! 入力スペクトルデータに X 微分(∂x)を作用する.
      !
      ! スペクトルデータの X 微分とは, 対応する格子点データに X 微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに X 方向波数 k をかけて
      ! sin(kx) <-> cos(kx) 成分に入れ換える計算を行っている.
      !
      real(8), dimension(-km:km,2:lm)                :: eh_Dx_eh
      real(8), dimension(-km:km,2:lm), intent(in)    :: eh
      integer k

      do k=-km,km
         eh_Dx_eh(k,:)  =  (-2*pi*k/xl)*eh(-k,:)
      enddo
    end function eh_Dx_eh

    function eq_Dx_eq(eq)
      !
      ! 入力スペクトルデータに X 微分(∂x)を作用する.
      !
      ! スペクトルデータの X 微分とは, 対応する格子点データに X 微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに X 方向波数 k をかけて
      ! sin(kx) <-> cos(kx) 成分に入れ換える計算を行っている.
      !
      real(8), dimension(-km:km,4:lm)                :: eq_Dx_eq
      real(8), dimension(-km:km,4:lm), intent(in)    :: eq
      integer k

      do k=-km,km
         eq_Dx_eq(k,:)  =  (-2*pi*k/xl)*eq(-k,:)
      enddo
    end function eq_Dx_eq

   !------------------- 渦度から流線を求める ----------------------
    function eq_Vor2Strm_et(et_Zeta)
      !
      !  2 次元水路領域
      !  Fourier 展開 + チェビシェフ−ガラーキン法用モジュール
      !
      !  渦度から流線を計算する.
      !
      real(8), intent(IN) :: et_Zeta(-km:km,0:lm)        !(in)  渦度 ζ=▽^2ψ
      real(8)             :: eq_Vor2Strm_et(-km:km,4:lm) !(out) 流線 ψ

      real(8), allocatable :: LaplaMT(:,:,:)
      real(8), allocatable :: LaplaInvMT(:,:,:)
      integer, allocatable :: kpvot(:,:)

      integer, parameter :: ls=4

      real(8) :: eq_work(-km:km,ls:lm)
      real(8) :: et_work(-km:km,0:lm)

      integer :: k, m, n, l, p
      logical :: first = .true.
      save LaplaInvMT, kpvot, first

      if ( .not. Set_RRFF ) &
           call MessageNotify('E','eq_Vor2Strm_et',&
                    'at_aq_galerkin_RRFF_module not initialized.')

      if ( first ) then
         first = .false.
         allocate(LaplaMT(-km:km,0:lm,0:lm))
         allocate(LaplaInvMT(-km:km,ls:lm,ls:lm),kpvot(-km:km,ls:lm))

         LaplaMT=0.0D0
         do l=0,lm
            et_work = 0.0D0 ; et_work(:,l) = 1.0D0
            et_work = et_Lapla_et(et_work)
            LaplaMT(:,:,l) = et_work
         enddo

         LaplaInvMT=0.0
         do n=ls,lm
            do m=ls,lm
               do l=0,lm
                  do p=0,lm
                     LaplaInvMT(:,n,m)=LaplaInvMT(:,n,m) &
                          +TQ(l,n)*LaplaMT(:,l,p)*alpha(p)*TQ(p,m)
                  enddo
               enddo
            enddo
         enddo

         call LUDecomp(LaplaInvMT,kpvot)
         deallocate(LaplaMT)
      endif

      eq_work=0.0
      do m=ls,lm
         do k=0,lm
            eq_work(:,m)=eq_work(:,m) &
                 + alpha(k) * beta(k) * et_Zeta(:,k)* TQ(k,m) 
         enddo
      enddo

      eq_Vor2Strm_et = LUSolve(LaplaInvMT,kpvot,eq_work)

    end function eq_Vor2Strm_et

end module et_galerkin_module
