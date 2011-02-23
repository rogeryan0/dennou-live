**********************************************************************
* PACKAGE UMTLU   !" ＬＵ分解による連立１次方程式の解
*
*"  [HIS] 90/08/31(numaguti)
*"        99/11/10(takepiro) LUSOLM 追加
*       2002/01/20(takepiro) ispack-f90 用に抜きだし
*       2002/06/10(takepiro) ベクトル長問題対応 lusol2 を用意.
*       2009/08/06(takepiro) OMP 版とそろえて LUMAK1 を作成
*
**********************************************************************
      SUBROUTINE LUMAKE    !" 行列のＬＵ分解（部分ピボット選択）
     M         ( ALU   ,
     O           KP    ,
     D           JDIM  , NDIM   )
*
*"    NDIM x NDIM の行列 JDIM 個を一度に計算
*"    ＬＵ行列は 入力行列に上書きされる.
*
      IMPLICIT NONE
*
*   [PARAM] 
      INTEGER    JDIM
      INTEGER    NDIM
*
*   [MODIFY] 
      DOUBLE PRECISION       ALU ( JDIM, NDIM, NDIM )     !" 入力／ＬＵ行列
*
*   [OUTPUT] 
      INTEGER    KP  ( JDIM, NDIM )           !" ピボット
*
*   [INTERNAL WORK] 
      INTEGER    J, K, M, N
      DOUBLE PRECISION       PIVOT, TEMP
*
*
      DO 3000 K = 1, NDIM-1
         DO 3000  J = 1, JDIM
*
*"         < 1. 部分ピボット選択 >
*
*
            PIVOT      = ALU ( J,K,K )
            KP ( J,K ) = K
            DO 1100  M = K+1, NDIM
               IF (  ABS( ALU ( J,M,K ) ) .GT. ABS( PIVOT )  ) THEN
                  PIVOT      = ALU ( J,M,K )
                  KP ( J,K ) = M
               ENDIF
 1100       CONTINUE
*
            IF ( KP ( J,K ) .NE. K ) THEN
               DO 1200 N = 1, NDIM
                  TEMP                = ALU ( J,K,N )
                  ALU ( J,K,N )       = ALU ( J,KP(J,K),N )
                  ALU ( J,KP(J,K),N ) = TEMP
 1200          CONTINUE
            ENDIF
*
*"         < 2. ＬＵ分解 >
*
            DO 2100  N = K+1, NDIM
*"                                                   U[kj], j>k+1
               ALU ( J,K,N ) = ALU( J,K,N ) / PIVOT
*
               DO 2100 M = K+1, NDIM
*"                                                   L[i,k+1], i>=k+1
                  ALU ( J,M,N ) = ALU( J,M,N )
     &                          - ALU( J,M,K ) * ALU( J,K,N )
*
 2100          CONTINUE
 2200       CONTINUE
*
 3000 CONTINUE
*
      RETURN
      END
***********************************************************************
      SUBROUTINE LUMAK1    !" 行列のＬＵ分解（部分ピボット選択）
     M         ( ALU   ,
     O           KP    ,
     D           NDIM   )
*
*"    NDIM x NDIM の行列 1 個を計算
*"    ＬＵ行列は 入力行列に上書きされる.
*
      IMPLICIT NONE
*
*   [PARAM] 
      INTEGER    NDIM
*
*   [MODIFY] 
      DOUBLE PRECISION       ALU ( NDIM, NDIM )     !" 入力／ＬＵ行列
*
*   [OUTPUT] 
      INTEGER    KP  ( NDIM )           !" ピボット
*
      CALL LUMAKE
     M         ( ALU   ,
     O           KP    ,
     D           1     , NDIM   )
*
      RETURN
      END
***********************************************************************
      SUBROUTINE LUSOLV      !" ＬＵ分解による解の計算
     M         ( XV    ,
     I           ALU   , KP    ,
     D           IDIM  , JDIM  , NDIM   )
*
*"   NDIM x NDIM 型行列を JDIM 個並べた連立方程式
*"    A X = B を IDIM 個の B に対して計算する. 
*"   解は右辺の入力ベクトルに上書きされる
*
      IMPLICIT NONE
*
*   [PARAM] 
      INTEGER    IDIM
      INTEGER    JDIM
      INTEGER    NDIM
*
*   [MODIFY] 
      DOUBLE PRECISION       XV  ( IDIM, JDIM, NDIM )     !" 右辺ベクトル／解
*
*   [INPUT] 
      DOUBLE PRECISION       ALU ( JDIM, NDIM, NDIM )     !" ＬＵ行列
      INTEGER    KP  ( JDIM, NDIM )           !" ピボット
*
*   [INTERNAL WORK] 
      INTEGER    I, J, K, N, NN
      DOUBLE PRECISION       TEMP
*
*
*"         < 1. ピボット選択による並び換え >
*
      DO 1100 K = 1, NDIM-1
         DO 1100 I = 1, IDIM
*
            DO 1110 J = 1, JDIM
               IF ( KP ( J,K ) .NE. K ) THEN
                  TEMP               = XV ( I,J,K )
                  XV ( I,J,K )       = XV ( I,J,KP(J,K) )
                  XV ( I,J,KP(J,K) ) = TEMP
               ENDIF
 1110      CONTINUE
*
 1100 CONTINUE
*
*"         < 2. 前進代入 >
*
      DO 2100 N = 1, NDIM
*"                                               Y[i]
         DO 2110 I = 1, IDIM
            DO 2110 J = 1, JDIM
               XV ( I,J,N ) = XV ( I,J,N ) / ALU ( J,N,N )
 2110    CONTINUE
*
         DO 2130 NN = N+1, NDIM
            DO 2120 I = 1, IDIM
               DO 2120 J = 1, JDIM
                  XV ( I,J,NN ) = XV ( I,J,NN )
     &                          - XV ( I,J,N ) * ALU ( J,NN,N )
 2120       CONTINUE
 2130    CONTINUE
*
 2100 CONTINUE
*
*"         < 3. 後退代入 >
*
      DO 3100 K = NDIM-1, 1, -1
         DO 3100 N = K+1, NDIM
*"                                               X[k]
            DO 3110 I = 1, IDIM
               DO 3110 J = 1, JDIM
                  XV ( I,J,K ) = XV ( I,J,K )
     &                         - XV ( I,J,N ) * ALU ( J,K,N )
 3110       CONTINUE
*
 3100 CONTINUE
*
      RETURN
      END
**********************************************************************
      SUBROUTINE LUSOL2      !" ＬＵ分解による解の計算
     M         ( XV    ,
     I           ALU   , KP    ,
     D           IDIM  , NDIM   )
*
*"   NDIM x NDIM 型行列の連立方程式 A X = B を IDIM 個の B に対して計算する. 
*"   解は右辺の入力ベクトルに上書きされる.
*"   LUSOLV の JDIM=1 の場合に相当. LUSOLV は JDIM=1 の場合ベクトル長が
*"   短くなるのでこのサブルーチンが用意されている. 
*
      IMPLICIT NONE
*
*   [PARAM] 
      INTEGER    IDIM
      INTEGER    NDIM
*
*   [MODIFY] 
      DOUBLE PRECISION       XV  ( IDIM, NDIM )     !" 右辺ベクトル／解
*
*   [INPUT] 
      DOUBLE PRECISION       ALU ( NDIM, NDIM )     !" ＬＵ行列
      INTEGER    KP  ( NDIM )                       !" ピボット
*
*   [INTERNAL WORK] 
      INTEGER    I, K, N, NN
      DOUBLE PRECISION       TEMP
*
*
*"         < 1. ピボット選択による並び換え >
*
      DO 1100 K = 1, NDIM-1
         DO 1100 I = 1, IDIM
               IF ( KP ( K ) .NE. K ) THEN
                  TEMP           = XV ( I,K )
                  XV ( I,K )     = XV ( I,KP(K) )
                  XV ( I,KP(K) ) = TEMP
               ENDIF
 1110      CONTINUE
*
 1100 CONTINUE
*
*"         < 2. 前進代入 >
*
      DO 2100 N = 1, NDIM
*"                                               Y[i]
         DO 2110 I = 1, IDIM
               XV ( I,N ) = XV ( I,N ) / ALU ( N,N )
 2110    CONTINUE
*
         DO 2130 NN = N+1, NDIM
            DO 2120 I = 1, IDIM
                  XV ( I,NN ) = XV ( I,NN )
     &                          - XV ( I,N ) * ALU ( NN,N )
 2120       CONTINUE
 2130    CONTINUE
*
 2100 CONTINUE
*
*"         < 3. 後退代入 >
*
      DO 3100 K = NDIM-1, 1, -1
         DO 3100 N = K+1, NDIM
*"                                               X[k]
            DO 3110 I = 1, IDIM
                  XV ( I,K ) = XV ( I,K )
     &                         - XV ( I,N ) * ALU ( K,N )
 3110       CONTINUE
*
 3100 CONTINUE
*
      RETURN
      END
**********************************************************************
      SUBROUTINE LUSOLM      !" ＬＵ分解による解の計算
     M         ( XV    , 
     I           ALU   , KP    , JMTX , 
     D           IDIM  , JDIM  , NDIM   )
*
*"   JDIM 個のNDIM x NDIM 型行列に対して
*"   A X = B を IDIM 個の B に対して計算する. 
*"   各 B に対して用いる行列を JMTX で指定する. 
*"   解は右辺の入力ベクトルに上書きされる
*
      IMPLICIT NONE
*
*   [PARAM] 
      INTEGER    IDIM      !" ベクトルの並び数
      INTEGER    JDIM      !" 行列の並び数
      INTEGER    NDIM      !" 各行列の次元
*
*   [MODIFY] 
      DOUBLE PRECISION       XV  ( IDIM, NDIM )       !" 右辺ベクトル／解
*                                         !" (NDIM 次元, IDIM 個)
*   [INPUT] 
      DOUBLE PRECISION       ALU ( JDIM, NDIM, NDIM ) !" ＬＵ行列
      INTEGER    KP  ( JDIM, NDIM )       !" ピボット
      INTEGER    JMTX( IDIM )             !" 各ベクトルにして作用させる行列
*
*   [INTERNAL WORK] 
      INTEGER    I, K, N, NN
      DOUBLE PRECISION       TEMP
*
*
*"         < 1. ピボット選択による並び換え >
*
      DO 1100 K = 1, NDIM-1
         DO 1100 I = 1, IDIM
*
            IF ( KP ( JMTX(I),K ) .NE. K ) THEN
               TEMP             = XV ( I,K )
               XV ( I,K )       = XV ( I,KP(JMTX(I),K) )
               XV ( I,KP(JMTX(I),K) ) = TEMP
            ENDIF
 1110      CONTINUE
*
 1100 CONTINUE
*
*"         < 2. 前進代入 >
*
      DO 2100 N = 1, NDIM
*"                                               Y[i]
         DO 2110 I = 1, IDIM
            XV ( I,N ) = XV ( I,N ) / ALU ( JMTX(I),N,N )
 2110    CONTINUE
*
         DO 2130 NN = N+1, NDIM
            DO 2120 I = 1, IDIM
               XV ( I,NN ) = XV ( I,NN )
     &                       - XV ( I,N ) * ALU ( JMTX(I),NN,N )
 2120       CONTINUE
 2130    CONTINUE
*
 2100 CONTINUE
*
*"         < 3. 後退代入 >
*
      DO 3100 K = NDIM-1, 1, -1
         DO 3100 N = K+1, NDIM
*"                                               X[k]
            DO 3110 I = 1, IDIM
               XV ( I,K ) = XV ( I,K )
     &                      - XV ( I,N ) * ALU ( JMTX(I),K,N )
 3110       CONTINUE
*
 3100 CONTINUE
*
      RETURN
      END
