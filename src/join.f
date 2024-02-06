      SUBROUTINE JOIN(RNAME1,RNAME3,MATN3,NCOL3,NATT3,PTABLE,
     XKEYCOL,KEYTYP)

         USE Message, only : WARN
         USE Utils, only : HTOI, ITOH

         INCLUDE 'syspar.inc'
C
C  THIS ROUTINE PERFORMS THE ACTUAL JOIN BETWEEN
C  RELATION 1 AND 2 FORMING 3
C
C  PARAMETERS:
C         NAME1---NAME OF THE FIRST RELATION
C         MATN3---DATA TUPLE FOR RELATION 3
C         NCOL3---NUMBER OF FIXED LENGTH COLUMNS IN MATN3
C         NATT3---NUMBER OF ATTRIBUTES IN MATN3
C         PTABLE--POINTER TABLE FOR THIS INTERSECT
C         KEYCOL--COLUMN OF MATN2 USED FOR SUPPLYING KEY VALUES
C         KEYTYP--ATTRIBUTE TYPE OF MATN1 USED FOR KEY VALUES
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
C
C  INITIALIZE THE MATRIX POINTERS.
C
         IERR = 0
         IDST = 0
         IDNEW = 0
         IDCUR = NID
C
C  GET THE PARAMETERS FOR THE FIRST MATRIX.
C
         I = LOCREL(RNAME1)
         IDM1 = NID
         NSP = 0
         IF(KSTRT.NE.0) NSP = 2
         NTUP3 = 0
         ICROW = 0
C
C  SEQUENCE THROUGH MATN2.
C
  100    IF(IDCUR.EQ.0) GO TO 1000
         CALL ITOH(N1,N2,IDCUR)
         IF(N2.EQ.0) GO TO 1000
         CALL GETDAT(1,IDCUR,MATN2,NCOL2)
         IF(IDCUR.LT.0) GO TO 1000
         ICROW = ICROW + 1
C
C  MOVE THE COMPARISON VALUE INTO THE WHCOM ARRAYS.
C
         CALL ITOH(NCHAR,NWORDS,KATTL(1))
         IP = MATN2 + KEYCOL - 1
         IF(NWORDS.EQ.0) THEN
C       USE VARIABLE LENGTH POINTERS
            IP2 = BUFFER(IP)
            IP = MATN2 + IP2 - 1
            NWORDS = BUFFER(IP)
            IF(NWORDS.GT.ZMWHVL) THEN
               CALL MSG('W','ROW','+')
               CALL IMSG(ICROW,6,'+')
               CALL MSG(' ',' IGNORED BECAUSE COLUMN TOO LONG.',' ')
               GO TO 100
            ENDIF
            IP = IP + 2
            NCHAR = BUFFER(IP-1)
         ENDIF
         CALL HTOI(NCHAR,NWORDS,WHRLEN(1))
         CALL BLKMOV(WHRVAL(1),BUFFER(IP),NWORDS)
         NID = IDM1
         NS = NSP

  200    CALL RMLOOK(MATN1,2,1,NCOL1)
         IF(RMSTAT.NE.0) GO TO 100
C
C  OKAY -- NOW LOAD THE DATA.
C
  400    ENDCOL = NCOL3
         DO 900 KLM=1,NATT3
            KOL1 = PTABLE(PTBL2,KLM)
            KOL2 = PTABLE(PTBL3,KLM)
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
                  KOL3 = KOL3 + 1
                  KOL1 = KOL1 + 1
               ELSE
C        LOAD THE ATTRIBUTE FROM MATN2.
                  I2 = MATN2 + KOL2 - 1
                  MATN3(KOL3) = BUFFER(I2)
                  KOL3 = KOL3 + 1
                  KOL2 = KOL2 + 1
               ENDIF
  600       CONTINUE
            GO TO 900

C     VARIABLE LENGTH PROCESSING

  700       ENDCOL = ENDCOL + 1
            MATN3(KOL3) = ENDCOL
            IF (KOL1.NE.0) THEN
C        USE POINTERS FROM MATN1.
               I1 = MATN1 + KOL1 - 1
               KOL1 = BUFFER(I1)
               I2 = MATN1 + KOL1 - 1
               NWORDS = BUFFER(I2)
            ELSE
C        USE POINTERS FROM MATN2.
               I2 = MATN2 + KOL2 - 1
               KOL2 = BUFFER(I2)
               I2 = MATN2 + KOL2 - 1
               NWORDS = BUFFER(I2)
            ENDIF
C
C  LOAD UP THE VALUES.
C
            IF((ENDCOL+NWORDS+2).GT.MAXCOL) GO TO 950
            MATN3(ENDCOL) = NWORDS
            NWORDS = NWORDS + 1
            DO 800 I=1,NWORDS
               ENDCOL = ENDCOL + 1
               I2 = I2 + 1
               MATN3(ENDCOL) = BUFFER(I2)
  800       CONTINUE
  900    CONTINUE

C     ADD THIS TUPLE TO THE DATABASE

         CALL ADDDAT(3,IDNEW,MATN3,ENDCOL)
         IF(IDST.EQ.0) IDST = IDNEW
         NTUP3 = NTUP3 + 1
C
C  LOOK FOR MORE IN MATN1.
C
         GO TO 200
C
C  TUPLE LENGTH EXCEEDS MAXCOL
C
  950    IERR = 1
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
         IF(IERR.EQ.0) THEN
            CALL MSG(' ','JOIN COMPLETED.',' ')
            CALL IMSG(NTUP3,6,'+')
            CALL MSG(' ',' ROWS WERE GENERATED.',' ')
         ENDIF
C
C  RETURN
C
         RETURN
      END
