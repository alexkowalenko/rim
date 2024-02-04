      SUBROUTINE RNAMEA(*)

         USE Globals, only : DFLAG, USERID, USERID
         Use Lexer, only: KXNAME, TOKTYP, ASCREC, IDP, ITEMS, EQKEYW
         USE Message, only: WARN
         USE Text, only : BLANK
         USE Utils, only : ZMOVE

         INCLUDE 'syspar.inc'
C
C     RENAME AN ATTRIBUTE
C
C     : RENAME <COLUMN> NAME TO NEW_NAME <IN TABLE>
C
         INCLUDE 'ascpar.inc'
         INCLUDE 'files.inc'
         INCLUDE 'tuplea.inc'
         INCLUDE 'tuplel.inc'
         INCLUDE 'dclar1.inc'
         INCLUDE 'rimcom.inc'
         LOGICAL NE,EQ
C
C
C     CHECK FOR A DATABASE
C
         IF (.NOT.DFLAG) THEN
            CALL WARN(2)
            GOTO 999
         ENDIF
C
C     CHECK SYNTAX
C
         STOK = 2
         IF (EQKEYW(2,'ATTRIBUTE') .OR. EQKEYW(2,'COLUMN')) STOK = 3
         IF(.NOT.EQKEYW(STOK+1,'TO')) GO TO 900
         IF((ITEMS.GT.2+STOK).AND.(.NOT.EQKEYW(3+STOK,'IN'))) GO TO 900
         IF((ITEMS.NE.2+STOK).AND.(ITEMS.NE.4+STOK)) GO TO 900
         IF( .NOT.TOKTYP(STOK,KXNAME) ) THEN
            CALL WARN(7,ASCREC(IDP(STOK)))
            GOTO 999
         ENDIF
         IF( .NOT.TOKTYP(2+STOK,KXNAME) ) THEN
            CALL WARN(7,ASCREC(IDP(2+STOK)))
            GOTO 999
         ENDIF
         CALL LXSREC(STOK,ANAME1,ZC)
         CALL LXSREC(2+STOK,ANAME2,ZC)
C
C     LOOK FOR RELATION
C
         CALL ZMOVE(RNAME1,BLANK)
         IFLAG = 0
         IF (EQKEYW(3+STOK,'IN')) THEN
            IFLAG = 1
            CALL LXSREC(4+STOK,RNAME1,ZC)
C  CHECK THAT RELATION EXISTS
            I = LOCREL(RNAME1)
            IF(I.NE.0) THEN
               CALL WARN(1,RNAME1,BLANK)
               GO TO 999
            ENDIF
         ENDIF
C
C     SEE IF ANAME1 EXISTS
C
         I = LOCATT(ANAME1,RNAME1)
         IF(I.NE.0) GO TO 910
C
C     SEE IF ANAME2 ALREADY EXISTS
C
         I = LOCATT(ANAME2,BLANK )
         IF(I.NE.0) GO TO 200
C
C     EXISTS - CHECK TYPE AND LENGTH
C
         CALL ATTGET(STATUS)
         ILEN = ATTLEN
         ITYPE = ATTYPE
         I = LOCATT(ANAME1,RNAME1)
         CALL ATTGET(STATUS)
         IF(ILEN.NE.ATTLEN) GO TO 920
         IF(ITYPE.NE.ATTYPE) GO TO 920
C
C     NOW CHECK THAT OLD AND NEW DON'T COHABITATE IN SAME RELATION
C
         NUM = 0
  120    NUM = NUM + 1
         I = LOCATT(ANAME1,RNAME1)
         DO 130 II=1,NUM
            CALL ATTGET(STATUS)
            IF(STATUS.NE.0) GO TO 200
  130    CONTINUE
         I = LOCATT(ANAME2,RELNAM)
         IF(I.NE.0) GO TO 120
         CALL MSG('E','COLUMN ''','+')
         CALL AMSG(ANAME2,-ZC,'+')
         CALL MSG(' ',''' ALREADY EXISTS IN TABLE ''','+')
         CALL AMSG(RELNAM,-ZC,' ')
         CALL MSG(' ','''',' ')
         GO TO 999
C
C     RENAME ATTRIBUTE
C
  200    I = LOCATT(ANAME1,RNAME1)
  210    CALL ATTGET(STATUS)
         IF(STATUS.NE.0) GO TO 300
C
C     CHECK FOR PERMISSION
C
         I = LOCREL(RELNAM)
         I = LOCPRM(RELNAM,2)
         IF(I.NE.0) THEN
            IF(IFLAG.NE.0) GO TO 930
            GO TO 210
         ENDIF
         CALL ZMOVE(ATTNAM,ANAME2)
         CALL ATTPUT(STATUS)
         IF(IFLAG.EQ.0) GO TO 210
C
C     ALSO RENAME IN THE LINKS TABLES
C
  300    IF (LOCLNK(BLANK).NE.0) GOTO 800
  400    CALL LNKGET(STATUS)
         IF (STATUS.NE.0) GOTO 800
         IF (EQ(A1NAME,ANAME1) .AND.
     1       (EQ(R1NAME,RNAME1).OR.IFLAG.EQ.0))
     1             CALL ZMOVE(A1NAME,ANAME2)
         IF (EQ(A2NAME,ANAME1) .AND.
     1       (EQ(R2NAME,RNAME1).OR.IFLAG.EQ.0))
     1             CALL ZMOVE(A2NAME,ANAME2)
         CALL LNKPUT(STATUS)
         GOTO 400
C
  800    CALL MSG(' ','COLUMN ''','+')
         CALL AMSG(ANAME1,-ZC,'+')
         CALL MSG(' ',''' RENAMED TO ''','+')
         CALL AMSG(ANAME2,-ZC,'+')
         CALL MSG(' ',''',',' ')
         GOTO 999
C
C     BAD SYNTAX
C
  900    CALL WARN(4)
         GO TO 999
C
C     ANAME1 NOT THERE
C
  910    CALL WARN(3,ANAME1,RNAME1)
         GO TO 999
C
C     TYPE/LENGTH DIFFERS
C
  920    CALL MSG('E','COLUMN ''','+')
         CALL AMSG(ANAME2,-ZC,'+')
         CALL MSG(' ',''' HAS DIFFERENT TYPE OR LENGTH THAN ''','+')
         CALL AMSG(ANAME1,ZC,'+')
         CALL MSG(' ','''.',' ')
         GO TO 999
C
  930    CALL WARN(8)
         GO TO 999
C
C     ALL DONE
C
  999    CONTINUE
         RETURN 1
      END
