      SUBROUTINE HTOI(I,J,K)
      INCLUDE 'syspar.d'
C
C  PURPOSE:   PACK I AND J INTO K
C
C  OFFSET I BY MULTIPLYING BY ZHTOI
C
      K = J + (ZHTOI * I)
      RETURN
      END
