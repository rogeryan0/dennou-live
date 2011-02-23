**********************************************************************
* PACKAGE UMTLU   !" �̣�ʬ��ˤ��ϢΩ�����������β�
*
*"  [HIS] 90/08/31(numaguti)
*"        99/11/10(takepiro) LUSOLM �ɲ�
*       2002/01/20(takepiro) ispack-f90 �Ѥ�ȴ������
*       2002/06/10(takepiro) �٥��ȥ�Ĺ�����б� lusol2 ���Ѱ�.
*       2009/08/06(takepiro) OMP �ǤȤ����� LUMAK1 �����
*
**********************************************************************
      SUBROUTINE LUMAKE    !" ����Σ̣�ʬ�����ʬ�ԥܥå������
     M         ( ALU   ,
     O           KP    ,
     D           JDIM  , NDIM   )
*
*"    NDIM x NDIM �ι��� JDIM �Ĥ���٤˷׻�
*"    �̣չ���� ���Ϲ���˾�񤭤����.
*
      IMPLICIT NONE
*
*   [PARAM] 
      INTEGER    JDIM
      INTEGER    NDIM
*
*   [MODIFY] 
      DOUBLE PRECISION       ALU ( JDIM, NDIM, NDIM )     !" ���ϡ��̣չ���
*
*   [OUTPUT] 
      INTEGER    KP  ( JDIM, NDIM )           !" �ԥܥå�
*
*   [INTERNAL WORK] 
      INTEGER    J, K, M, N
      DOUBLE PRECISION       PIVOT, TEMP
*
*
      DO 3000 K = 1, NDIM-1
         DO 3000  J = 1, JDIM
*
*"         < 1. ��ʬ�ԥܥå����� >
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
*"         < 2. �̣�ʬ�� >
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
      SUBROUTINE LUMAK1    !" ����Σ̣�ʬ�����ʬ�ԥܥå������
     M         ( ALU   ,
     O           KP    ,
     D           NDIM   )
*
*"    NDIM x NDIM �ι��� 1 �Ĥ�׻�
*"    �̣չ���� ���Ϲ���˾�񤭤����.
*
      IMPLICIT NONE
*
*   [PARAM] 
      INTEGER    NDIM
*
*   [MODIFY] 
      DOUBLE PRECISION       ALU ( NDIM, NDIM )     !" ���ϡ��̣չ���
*
*   [OUTPUT] 
      INTEGER    KP  ( NDIM )           !" �ԥܥå�
*
      CALL LUMAKE
     M         ( ALU   ,
     O           KP    ,
     D           1     , NDIM   )
*
      RETURN
      END
***********************************************************************
      SUBROUTINE LUSOLV      !" �̣�ʬ��ˤ���η׻�
     M         ( XV    ,
     I           ALU   , KP    ,
     D           IDIM  , JDIM  , NDIM   )
*
*"   NDIM x NDIM ������� JDIM ���¤٤�ϢΩ������
*"    A X = B �� IDIM �Ĥ� B ���Ф��Ʒ׻�����. 
*"   ��ϱ��դ����ϥ٥��ȥ�˾�񤭤����
*
      IMPLICIT NONE
*
*   [PARAM] 
      INTEGER    IDIM
      INTEGER    JDIM
      INTEGER    NDIM
*
*   [MODIFY] 
      DOUBLE PRECISION       XV  ( IDIM, JDIM, NDIM )     !" ���ե٥��ȥ롿��
*
*   [INPUT] 
      DOUBLE PRECISION       ALU ( JDIM, NDIM, NDIM )     !" �̣չ���
      INTEGER    KP  ( JDIM, NDIM )           !" �ԥܥå�
*
*   [INTERNAL WORK] 
      INTEGER    I, J, K, N, NN
      DOUBLE PRECISION       TEMP
*
*
*"         < 1. �ԥܥå�����ˤ���¤Ӵ��� >
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
*"         < 2. �������� >
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
*"         < 3. �������� >
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
      SUBROUTINE LUSOL2      !" �̣�ʬ��ˤ���η׻�
     M         ( XV    ,
     I           ALU   , KP    ,
     D           IDIM  , NDIM   )
*
*"   NDIM x NDIM �������ϢΩ������ A X = B �� IDIM �Ĥ� B ���Ф��Ʒ׻�����. 
*"   ��ϱ��դ����ϥ٥��ȥ�˾�񤭤����.
*"   LUSOLV �� JDIM=1 �ξ�������. LUSOLV �� JDIM=1 �ξ��٥��ȥ�Ĺ��
*"   û���ʤ�ΤǤ��Υ��֥롼�����Ѱդ���Ƥ���. 
*
      IMPLICIT NONE
*
*   [PARAM] 
      INTEGER    IDIM
      INTEGER    NDIM
*
*   [MODIFY] 
      DOUBLE PRECISION       XV  ( IDIM, NDIM )     !" ���ե٥��ȥ롿��
*
*   [INPUT] 
      DOUBLE PRECISION       ALU ( NDIM, NDIM )     !" �̣չ���
      INTEGER    KP  ( NDIM )                       !" �ԥܥå�
*
*   [INTERNAL WORK] 
      INTEGER    I, K, N, NN
      DOUBLE PRECISION       TEMP
*
*
*"         < 1. �ԥܥå�����ˤ���¤Ӵ��� >
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
*"         < 2. �������� >
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
*"         < 3. �������� >
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
      SUBROUTINE LUSOLM      !" �̣�ʬ��ˤ���η׻�
     M         ( XV    , 
     I           ALU   , KP    , JMTX , 
     D           IDIM  , JDIM  , NDIM   )
*
*"   JDIM �Ĥ�NDIM x NDIM ��������Ф���
*"   A X = B �� IDIM �Ĥ� B ���Ф��Ʒ׻�����. 
*"   �� B ���Ф����Ѥ������� JMTX �ǻ��ꤹ��. 
*"   ��ϱ��դ����ϥ٥��ȥ�˾�񤭤����
*
      IMPLICIT NONE
*
*   [PARAM] 
      INTEGER    IDIM      !" �٥��ȥ���¤ӿ�
      INTEGER    JDIM      !" ������¤ӿ�
      INTEGER    NDIM      !" �ƹ���μ���
*
*   [MODIFY] 
      DOUBLE PRECISION       XV  ( IDIM, NDIM )       !" ���ե٥��ȥ롿��
*                                         !" (NDIM ����, IDIM ��)
*   [INPUT] 
      DOUBLE PRECISION       ALU ( JDIM, NDIM, NDIM ) !" �̣չ���
      INTEGER    KP  ( JDIM, NDIM )       !" �ԥܥå�
      INTEGER    JMTX( IDIM )             !" �ƥ٥��ȥ�ˤ��ƺ��Ѥ��������
*
*   [INTERNAL WORK] 
      INTEGER    I, K, N, NN
      DOUBLE PRECISION       TEMP
*
*
*"         < 1. �ԥܥå�����ˤ���¤Ӵ��� >
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
*"         < 2. �������� >
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
*"         < 3. �������� >
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
