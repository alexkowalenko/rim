      SUBROUTINE UNION(RNAME,RNAME3,MATN3,NCOL3,NATT3,PTABLE,
     X  KEYCOL,KEYTYP,UPASS)

         USE Message, only: WARN
         USE Utils, only : ITOH

         INCLUDE 'syspar.inc'
C
C  THIS ROUTINE PERFORMS THE ACTUAL UNION BETWEEN
C  RELATION 1 AND 2 FORMING 3
C
C  PARAMETERS:
C         RNAME---NAME OF SEARCHED RELATION
C         RNAME3--NAME OF RELATION 3
C         MATN3---DATA TUPLE FOR RELATION 3
C         NCOL3---NUMBER OF FIXED LENGTH COLUMNS IN MATN3
C         NATT3---NUMBER OF ATTRIBUTES IN MATN3
C         PTABLE--POINTER TABLE FOR THIS UNION
C         KEYCOL--COLUMN OF MATN2 USED FOR SUPPLYING KEY VALUES
C         KEYTYP--ATTRIBUTE TYPE OF MATN1 USED FOR KEY VALUES
C         UPASS---1 = 1ST PASS (SOURCE RELATION = RNAME2)
C                              (RNAME = RNAME1) (INCLUDE MATCHES)
C                 1 = 2ND PASS (SOURCE RELATION = RNAME1)
C                              (RNAME = RNAME2) (EXCLUDE MATCHES)


         INCLUDE 'rmatts.inc'
         INCLUDE 'files.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'rimptr.inc'
         INCLUDE 'rimcom.inc'
         INCLUDE 'buffer.inc'
         INCLUDE 'whcom.inc'
         INCLUDE 'dclar1.inc'
         DIMENSION MATN3(1)
         INCLUDE 'ptbl.inc'
         INTEGER PTABLE(PTBLL,1)
         INTEGER ATTLEN
         INTEGER ENDCOL
         LOGICAL NOMAT
C
C  INITIALIZE THE MATRIX POINTERS.
C
         IERR = 0
         IDCUR = NID
C
C  GET RNAME3 POINTERS
C
         I = LOCREL(RNAME3)
         CALL RELGET(ISTAT)
         IDST = RSTART
         IDNEW = REND
         NTUP3 = NTUPLE
C
C  GET THE PARAMETERS FOR THE SCANED MATRIX.
C
         I = LOCREL(RNAME)
         IDM1 = NID
         NSP = 0
         IF(KSTRT.NE.0) NSP = 2
C
C     SET FLAG FOR LOADING NULL FILLED TUPLES
C
         NOMAT=.FALSE.
C
C  SEQUENCE THROUGH MATN2.
C
  100    IF(NOMAT) GOTO 410
         NOMAT=.TRUE.

  105    IF(IDCUR.EQ.0) GO TO 1000
         CALL ITOH(N1,N2,IDCUR)
         IF(N2.EQ.0) GO TO 1000
         CALL GETDAT(1,IDCUR,MATN1,NCOL1)
         IF(IDCUR.LT.0) GO TO 1000
C
C  MOVE THE COMPARISON VALUE INTO THE WHCOM ARRAYS.
C
         CALL ITOH(NCHAR,NWORDS,KATTL(1))
         IP = MATN1 + KEYCOL - 1
         IF(NWORDS.EQ.0) THEN
C       USE VARIABLE LENGTH POINTERS
            IP1 = BUFFER(IP)
            IP = MATN1 + IP1 + 1
         ENDIF
         WHRVAL(1) = BUFFER(IP)
         NID = IDM1
         NS = NSP

  200    CALL RMLOOK(MATN2,2,1,NCOL1)
         IF(RMSTAT.NE.0) GO TO 100
C
C  CHECK TO SEE IF THE ATTRIBUTES MATCH.
C
         K = 1
  300    CALL PTRS(IPT1,IPT2,K,NATT3,PTABLE,LEN,ITYPE)
         IF (UPASS.EQ.1) THEN
            I1 = MATN1 + IPT2 - 1
            I2 = MATN2 + IPT1 - 1
         ELSE
            I1 = MATN1 + IPT1 - 1
            I2 = MATN2 + IPT2 - 1
         ENDIF
C
C  IF K IS 0 WE HAVE LOOKED AT ALL THE COMMON ATTRIBUTES.
C
         IF(K.EQ.0) GO TO 400
         IF(LEN.EQ.0) GO TO 320
         DO 310 I=1,LEN
            IF(BUFFER(I1).NE.BUFFER(I2)) GO TO 200
            I1 = I1 + 1
            I2 = I2 + 1
  310    CONTINUE
C
C  A MATCH. LOOK AT MORE ATTRIBUTES.
C
         GO TO 300
C
C  VARIABLE LENGTH ATTRIBUTE PROCESSING.
C
  320    IPT1 = BUFFER(I1)
         IPT2 = BUFFER(I2)
         I1 = MATN1 + IPT1 - 1
         I2 = MATN2 + IPT2 - 1
         IF(BUFFER(I1).NE.BUFFER(I2)) GO TO 200
         LEN = BUFFER(I1)
         I1 = I1 + 2
         I2 = I2 + 2
         DO 340 I=1,LEN
            IF(BUFFER(I1).NE.BUFFER(I2)) GO TO 200
            I1 = I1 + 1
            I2 = I2 + 1
  340    CONTINUE
         GO TO 300
C
C  OKAY -- NOW LOAD THE DATA.
C
  400    NOMAT=.FALSE.
  410    IF (UPASS.EQ.2 .AND. .NOT.NOMAT) GOTO 100
         ENDCOL = NCOL3
         DO 900 KLM=1,NATT3
            IF (UPASS.EQ.1) THEN
               KOL1 = PTABLE(PTBL3,KLM)
               KOL2 = PTABLE(PTBL2,KLM)
            ELSE
               KOL1 = PTABLE(PTBL2,KLM)
               KOL2 = PTABLE(PTBL3,KLM)
            ENDIF
            KOL3 = PTABLE(PTBL4,KLM)
            ATTLEN = PTABLE(PTBL5,KLM)
            CALL ITOH(NCHAR,NWORDS,ATTLEN)
            IF(NWORDS.EQ.0) GO TO 700

C     FIXED LENGTH PROCESSING

            DO 600 I=1,NWORDS
               IF (KOL1.NE.0) THEN
C        LOAD THE ATTRIBUTE FROM MATN1.
                  I1 = MATN1 + KOL1 - 1
                  MATN3(KOL3) = BUFFER(I1)
                  KOL1 = KOL1 + 1
               ELSE IF (NOMAT .OR. KOL2.EQ.0) THEN
C        NO MATCH FROM MATN1 NULL FILL
                  MATN3(KOL3)=NULL
               ELSE
C        LOAD THE ATTRIBUTE FROM MATN2.
                  I2 = MATN2 + KOL2 - 1
                  MATN3(KOL3) = BUFFER(I2)
                  KOL2 = KOL2 + 1
               ENDIF
C
  590          KOL3=KOL3+1
  600       CONTINUE
            GO TO 900

C     VAR LENGTH PROCESSING


  700       ENDCOL = ENDCOL + 1
            MATN3(KOL3) = ENDCOL
            IF (KOL1.NE.0) THEN
C        USE POINTERS FROM MATN1.
               I1 = MATN1 + KOL1 - 1
               KOL1 = BUFFER(I1)
               I2 = MATN1 + KOL1 - 1
               NWORDS = BUFFER(I2)
               NWORD2 = BUFFER(I2+1)
            ELSE IF (NOMAT .OR. KOL2.EQ.0) THEN
C        NO MATCH, NULL FILL
               NWORDS=1
               NWORD2 = 1
            ELSE
C        USE POINTERS FROM MATN2.
               I2 = MATN2 + KOL2 - 1
               KOL2 = BUFFER(I2)
               I2 = MATN2 + KOL2 - 1
               NWORDS = BUFFER(I2)
               NWORD2 = BUFFER(I2+1)
            ENDIF
C
C  LOAD UP THE VALUES.
C
            IF((ENDCOL+NWORDS+2).GT.MAXCOL) GO TO 950
            MATN3(ENDCOL) = NWORDS
            ENDCOL = ENDCOL + 1
            I2 = I2 + 1
            MATN3(ENDCOL) = NWORD2
            DO 800 I=1,NWORDS
               ENDCOL = ENDCOL + 1
               I2 = I2 + 1
               IF(NOMAT.AND.KOL1.EQ.0) THEN
                  MATN3(ENDCOL) = NULL
               ELSE
                  MATN3(ENDCOL) = BUFFER(I2)
               ENDIF
  800       CONTINUE
  900    CONTINUE
         CALL ADDDAT(3,IDNEW,MATN3,ENDCOL)
         IF(IDST.EQ.0) IDST = IDNEW
         NTUP3 = NTUP3 + 1
C
C  LOOK FOR MORE IN MATN1.
C
         IF(NOMAT) GO TO 105
         GO TO 200
C
C  TUPLE LENGTH EXCEEDS MAXCOL
C
  950    CONTINUE
         IERR = 1
         CALL WARN(15)
C
C  ALL DONE.
C
 1000    I = LOCREL(RNAME3)
         CALL RELGET(ISTAT)
         RSTART = IDST
         REND = IDNEW
         NTUPLE = NTUP3
         CALL RELPUT
         IF(UPASS.EQ.2 .AND. IERR.EQ.0) THEN
            CALL MSG(' ','UNION COMPLETED,','+')
            CALL IMSG(NTUP3,6,'+')
            CALL MSG(' ',' ROWS GENERATED.',' ')
         ENDIF
C
C  RETURN
C
         RETURN
      END
