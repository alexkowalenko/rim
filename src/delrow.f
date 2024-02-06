      SUBROUTINE DELROW(*)

         USE Globals, only : DFLAG, DMFLAG, DBNAME
         Use Lexer, only: KXNAME, TOKTYP, ITEMS, EQKEYW, LXSREC
         USE Message, only : WARN
         USE Text, only : BLANK

         INCLUDE 'syspar.inc'
C
C     DELETE ROWS FROM A RELATION
C
         INCLUDE 'ascpar.inc'
         INCLUDE 'rimcom.inc'
         INCLUDE 'tuplea.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'attble.inc'
         INCLUDE 'buffer.inc'
         LOGICAL NE
         LOGICAL EQ
         LOGICAL SELWHR
         INCLUDE 'dclar1.inc'
         INCLUDE 'rmatts.inc'
         INCLUDE 'whcom.inc'
C
C
C     CHECK FOR A DATABASE
C
         IF (.NOT.DFLAG) THEN
            CALL WARN(2)
            GOTO 999
         ENDIF
C
C     MAKE SURE THE DATABASE MAY BE MODIFIED
C
         IF(.NOT.DMFLAG) THEN
            CALL WARN(RMSTAT,DBNAME)
            GO TO 999
         ENDIF
C
C     CHECK THE COMMAND SYNTAX
C
         IF(.NOT.EQKEYW(3,'FROM') .OR.
     X      .NOT.EQKEYW(5,'WHERE') .OR.
     X      .NOT.TOKTYP(4,KXNAME)) THEN
            CALL WARN(4,BLANK,BLANK)
            GOTO 999
         ENDIF
C
C  FIND THE RELATION NAME IN THE RELATION TABLE.
C
         CALL LXSREC(4,RNAME,ZC)
         I = LOCREL(RNAME)
         IF(I.NE.0) THEN
            CALL WARN(1,RNAME,BLANK)
            GOTO 999
         ENDIF
C
C     CHECK FOR MODIFY PERMISSION
C
         L = LOCPRM(RNAME,2)
         IF(L.NE.0) GO TO 999
C
C
C  EVALUATE THE WHERE CLAUSE.
C
         NBOO = 0
         LIMTU = ALL9S
         IF (.NOT.SELWHR(5,ITEMS-4)) GOTO 999
         IF(RMSTAT.NE.0) GO TO 999
C
C  CALL DELETE TO FINISH PROCESSING THE COMMAND.
C
         CALL BLKDEF(7,MAXCOL,1)
         KQ1 = BLKLOC(7)
         CALL DELETE(BUFFER(KQ1))
         CALL BLKCLR(7)
C
  999    RETURN 1
      END
