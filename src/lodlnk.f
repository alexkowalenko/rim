      SUBROUTINE LODLNK

         Use Lexer, only: KXNAME, TOKTYP, ITEMS, EQKEYW
         USE Message, only : WARN
         USE Utils, only : ZMOVE

         INCLUDE 'syspar.inc'
C
C     LOADS LINK DEFINITIONS.
C
         INCLUDE 'ascpar.inc'
         INCLUDE 'rmatts.inc'
         INCLUDE 'buffer.inc'
         INCLUDE 'files.inc'
         INCLUDE 'tuplel.inc'
C
         INCLUDE 'dclar1.inc'
         INTEGER LKNAM(Z)
C
C     READ LINK DEFINITIONS
C
  100    CONTINUE
         CALL LODREC
         IF(ITEMS.EQ.1) GO TO 999
C
C     ASSUME LINK DEFINITION - CHECK SYNTAX
C
         IF (ITEMS.NE.9            .OR.
     1       .NOT.EQKEYW(2,'FROM') .OR.
     1       .NOT.EQKEYW(4,'IN')   .OR.
     1       .NOT.EQKEYW(6,'TO')   .OR.
     1       .NOT.EQKEYW(8,'IN')) THEN
            CALL WARN(4)
            GOTO 800
         ENDIF
C
C     CHECK FOR VALID LINK NAME.
C
  200    IF(.NOT.TOKTYP(1,KXNAME)) THEN
            CALL MSG('E','THE LINK NAME IS NOT VALID.',' ')
            GO TO 800
         ENDIF
C
         CALL LXSREC(1,LKNAM,ZC)
         I = LOCLNK(LKNAM)
         IF(I.EQ.0) THEN
            CALL MSG('E','THAT LINK HAS ALREADY BEEN DEFINED.',' ')
            GO TO 800
         ENDIF
C
C     CHECK RELATION AND ATTRIBUTE NAMES
C
         CALL LXSREC(3,A1NAME,ZC)
         CALL LXSREC(5,R1NAME,ZC)
         IF (LOCATT(A1NAME,R1NAME).NE.0) THEN
            CALL WARN(3,A1NAME,R1NAME)
            GOTO 800
         ENDIF
         CALL ATTGET(I1)
         FTYP = ATTYPE
         FLEN = ATTLEN

         CALL LXSREC(7,A2NAME,ZC)
         CALL LXSREC(9,R2NAME,ZC)
         IF (LOCATT(A2NAME,R2NAME).NE.0) THEN
            CALL WARN(3,A2NAME,R2NAME)
            GOTO 800
         ENDIF
         CALL ATTGET(I2)
  111    IF (ATTYPE.NE.FTYP .OR. ATTLEN.NE.FLEN) THEN
            CALL MSG('E',
     1        'COLUMNS DO NOT HAVE THE SAME TYPE AND LENGTH.',' ')
            GOTO 800
         ENDIF

         CALL ZMOVE(LNAME,LKNAM)
         CALL LNKADD
         GOTO 100
C
C
  800    CALL MSG(' ','STILL DEFINING LINKS',' ')
         GOTO 100
C
C  END RELATION PROCESSING.
C
  999    CONTINUE
         RETURN
      END
