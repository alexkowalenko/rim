      SUBROUTINE PARVAL(ID,MAT,ATYPE,NWORDS,ROW,NCOLT,IERR)

         USE, intrinsic :: iso_fortran_env

         USE Extern, only: IMSG
         USE Formater, only : TYPER, LXDATE
         USE Lexer, only: KXTEXT, KXINT, KXREAL, TOKTYP, IDP, IDL, KWS
         USE Lexer, only: ITEMS, IDI, IDR, LFIND, LXLENW, LXSREC
         USE Maths, only : DTOR
         USE Message, only : WARN
         USE Rim, only: RMTYPT
         USE RM_Text, only : BLANK

         INCLUDE 'syspar.inc'
C
C     THIS ROUTINE PARSES A VALUE SPECIFICATION AND STORES THE
C     VALUE IN MAT.
C
C     RM_Parameters.......
C     ID.......INPUT - STARTING LXLREC ITEM NUMBER
C              OUTPUT- 1+ITEM NUMBER OF LAST ITEM IN VALUE
C     MAT......OUTPUT- ARRAY OF VALUES
C     ATYPE....INPUT - RVEC,IMAT,DOUB STUFF
C     NWORDS...INPUT - NWORDS PART OF ATTLEN
C              OUTPUT- ACTUAL NWORDS
C     ROW......INPUT - OTHER PART OF ATTLEN
C              OUTPUT- ACTUAL VALUE
C     IERR.....OUTPUT- ERROR FLAG
C                      0 MEANS OK
C                      1 IF TYPE MISMATCH
C                      2 IF COUNT MISMATCH
C                      3 IF PAREN MISMATCH
C              INPUT- -1 PUT NULL INTO MAT
C
         INCLUDE 'rmatts.inc'
         INCLUDE 'tuplea.inc'

         REAL(real64) :: RR
         INTEGER IR(2)
         EQUIVALENCE (IR(1),RR)

         DIMENSION MAT(Z)
C
         NULLVV = ZIMISS
         IF(IERR.EQ.-1) GO TO 600
         IF(ID.GT.ITEMS) GO TO 8400
         IF(NCOLT.GT.MAXCOL) GO TO 8300
         IERR = 0
         CALL TYPER(ATYPE,VECMAT,JTYPE)
         TYPE = JTYPE
         IF(TYPE.EQ.KZDOUB) TYPE = KZREAL
         NULLVV = IDI(ID)
         IF(IDI(ID).EQ.ZIMISS) GO TO 600
         IF(IDI(ID).EQ.ZINAPP) GO TO 600
         NWORD = NWORDS
         IF(JTYPE.EQ.KZDOUB) NWORD = NWORDS/2
         IF(TYPE.EQ.KZDATE .OR. TYPE.EQ.KZTIME) THEN
C
C        DATE/TIME ITEM
C
            IF (.NOT.LXDATE(ID,MAT(1),ATTFOR,TYPE)) GOTO 8100
            ID = ID + 1
            RETURN
         ENDIF
         IF(TYPE.NE.KZTEXT) GO TO 100
C
C     RM_Text STUFF
C
         IF(.NOT.TOKTYP(ID,KXTEXT)) GO TO 8000
         NW = LXLENW(ID)
         IF(NWORD.EQ.0) GO TO 50
C
C     FIXED RM_Text
C
         IF(IDL(ID).GT.ROW) THEN
            CALL MSG('W','TEXT FOR COLUMN ''' ,'+')
            CALL AMSG(ATTNAM,-ZC,'+')
            CALL MSG(' ',''' WILL BE TRUNCATED.',' ')
         ENDIF
         NW = NWORD
         GO TO 80
C
C     VARIABLE RM_Text
C
   50    IF((NCOLT+NW).GT.ZMASC) GO TO 8300
         NWORD = NW
         ROW = IDL(ID)
   80    CONTINUE
         DO 90 I=1,NW
            MAT(I) = BLANK(1)
   90    CONTINUE
         NWX = ROW
         IF(IDL(ID).LT.NWX) NWX = IDL(ID)
         CALL LXSREC(ID,MAT,NWX)
         ID = ID + 1
         NWORDS = NWORD
         RETURN
C
C
C
  100    CONTINUE
         NUMI = ITEMS - ID + 1
         IF(NWORD.GT.NUMI) GO TO 8100
C
C     NON-RM_Text STUFF - CONVERT TYPE CODE
C
         DECPOI = MOD(ATTFOR,10000) / 100
         IF (TYPE.EQ.KZINT) THEN
            XTYPE = KXINT
            IF (DECPOI.NE.0) XTYPE = KXREAL
         ELSE
            XTYPE = KXREAL
         ENDIF
         IDPT = IDP(ID)
  101    IF(KWS(ID).NE.'(') GO TO 500
C
C     WE HAVE PARENS
C
         IF(VECMAT.EQ.KZMAT) GO TO 300
C
C     VECTOR
C
         IF(NWORD.EQ.0) GO TO 200
C
C     FIXED LENGTH VECTOR
C
         IF(KWS(ID).NE.'(') GO TO 8100
         DO 150 I=1,NWORD
            IF(.NOT.TOKTYP(ID+I,XTYPE)) GOTO 8000
  150    CONTINUE
         IS = ID + 1
         NW = NWORD
         ID = ID + NWORD + 2
         GO TO 1000
C
C     VARIABLE
C
  200    L = LFIND(ID,ITEMS-ID+1,')')
  201    IF(L.EQ.0) GO TO 8200
         NW = L - ID - 1
         IF((NCOLT+NW).GT.MAXCOL) GO TO 8300
         NWORD = NW
         ROW = 1
         DO 250 I=1,NWORD
            IF(.NOT.TOKTYP(ID+I,XTYPE)) GOTO 8000
  250    CONTINUE
         IS = ID + 1
         ID = L +  1
         GO TO 1000
C
  300    IF(NWORD.EQ.0) GO TO 400
C
C     FIXED MATRIX
C
         ISKIP = ROW + 2
         NCOLS = NWORD/ROW
         IP = ID + 1
         DO 320 I=1,NCOLS
            IF(KWS(IP).NE.'(') GO TO 8250
            DO 310 J=1,ROW
               IF(.NOT.TOKTYP(IP+J,XTYPE)) GOTO 8000
  310       CONTINUE
            IF(KWS(IP+ROW+1).NE.')') GO TO 8200
            IP = IP + ISKIP
  320    CONTINUE
         IF(KWS(IP-1).NE.')') GO TO 8200
         IS = ID + 2
         NW = ISKIP*NCOLS
         ID = IS + NW
         GO TO 1000
  400    CONTINUE
C
C     VARIABLE MATRIX - SET NWORD AND ROW THEN USE FIXED CODE
C
         L = LFIND(ID,ITEMS-ID+1,')')
         IF(L.EQ.0) GO TO 8200
         IROW = L - ID - 2
         IF(IROW.LE.0) GO TO 8100
         IF(ROW.EQ.0) ROW = IROW
         IF(IROW.NE.ROW) GO TO 8100
         ISKIP = ROW + 2
         IS = ID + 1
         NCOLS = 0
         DO 420 I=IS,ITEMS,ISKIP
            IF(KWS(I).EQ.')') GO TO 450
            NCOLS = NCOLS + 1
  420    CONTINUE
         GO TO 8200
  450    CONTINUE
         NWX = ROW*NCOLS
         IF(JTYPE.EQ.KZDOUB) NWX = 2*NWX
         IF((NCOLT+NWX).GT.MAXCOL) GO TO 8300
         NWORD = ROW*NCOLS
         GO TO 300
C
C     NO PARENS
C
  500    IF(NWORD.EQ.0) GO TO 8250
         DO 550 I=1,NWORD
            IF(.NOT.TOKTYP(ID+I-1,XTYPE)) GOTO 8000
  550    CONTINUE
         IS = ID
         NW = NWORD
         ID = ID + NWORD
         GO TO 1000
C
C     NULL VALUES
C
  600    IERR = 0
         ID = ID + 1
         IF(NWORDS .EQ.0) GO TO 650
C
C     FIXED NULL
C
         NW = NWORDS
         DO 620 I=1,NW
            MAT(I) = 0
  620    CONTINUE
         MAT(1) = NULLVV
         GO TO 9999
C
C VARIABLE NULL
C
  650    IF((NCOLT+1).GT.MAXCOL) GO TO 8300
         MAT(1) = NULLVV
         NWORDS = 1
         ROW = 1
         IF (ATYPE.EQ.KZTEXT) ROW = ZCW
         IF(JTYPE.NE.KZDOUB) GO TO 9999
         IF((NCOLT+2).GT.MAXCOL) GO TO 8300
         NWORDS = 2
         MAT(2) = 0
         GO TO 9999
C
C     DUMP STUFF INTO MAT
C
 1000    NW = NW + IS - 1
         MATIN = 1
C
         DO 1050 I=IS,NW
            IF (KWS(I).EQ.'(' .OR. KWS(I).EQ.')') GOTO 1050
            IF (TYPE.EQ.KZINT) THEN
               MAT(MATIN) = IDI(I)
               IF (DECPOI.NE.0) MAT(MATIN) = IDR(I)*(10**DECPOI)+.2
               MATIN = MATIN + 1
            ELSE IF (JTYPE.EQ.KZREAL) THEN
               RR = IDR(I)
               MAT(MATIN) = DTOR(IR)
               MATIN = MATIN + 1
            ELSE
               RR = IDR(I)
               MAT(MATIN) = IR(1)
               MAT(MATIN+1) = IR(2)
               MATIN = MATIN + 2
            ENDIF
 1050    CONTINUE
         GO TO 9990
C
 8000    CALL MSG('E','THE DATA FOR COLUMN ','+')
         CALL AMSG(ATTNAM,-ZC,'+')
         CALL MSG(' ',' MUST BE ' // RMTYPT(TYPE) // '.',' ')
         ID = ID + 1
         IERR = 1
         GO TO 9999
C
 8100    CALL MSG('E','THE DATA FOR COLUMN ''','+')
         CALL AMSG(ATTNAM,-ZC,'+')
         CALL MSG(' ',''' HAS AN INCORRECT LENGTH.',' ')
         ID = ID + 1
         IERR = 2
         GO TO 9999
C
 8200    CALL MSG('E','A '')'' WAS EXPECTED AT ITEM ','+')
         CALL IMSG(ID,4,' ')
         IERR = 3
         GO TO 9999
C
 8250    CALL MSG('E','A ''('' WAS EXPECTED AT ITEM ','+')
         CALL IMSG(ID,4,' ')
         IERR = 3
         GO TO 9999
C
 8300    CALL WARN(15)
         IERR = 2
         GO TO 9999
C
 8400    CALL MSG('E','THERE IS NO VALUE FOR COLUMN ''','+')
         CALL AMSG(ATTNAM,ZC,'+')
         CALL MSG(' ','''',' ')
         IERR = 2
         GO TO 9999
C
C     RESET NWORDS
C
 9990    NWORDS = NWORD
         IF(JTYPE.EQ.KZDOUB) NWORDS = 2*NWORD
 9999    RETURN
      END
