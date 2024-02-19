      SUBROUTINE SUBTRC(RNAME2,RNAME3,MATN3,NCOL3,NATT3,PTABLE,
     XKEYCOL,KEYTYP)

         USE RM_Globals, only : RMSTAT
         USE RM_BufferData, only: BUFFER
         USE RM_Buffer, only: ADDDAT, GETDAT
         USE Extern, only: IMSG, MSG
         USE Utils, only : ITOH

         INCLUDE 'syspar.inc'
C
C  THIS ROUTINE PERFORMS THE ACTUAL SUBTRACT
C       ( REL2 - REL2 = REL3 )
C
C  RM_Parameters:
C         RNAME2---NAME OF THE SUBTRAHEND RELATION
C         RNAME3---NAME OF THE RESULTANT RELATION
C                  NOTE.. NID POINTS TO RELATION 1
C         MATN3---DATA TUPLE FOR RELATION 3
C         NCOL3---NUMBER OF FIXED LENGTH COLUMNS IN MATN3
C         NATT3---NUMBER OF ATTRIBUTES IN MATN3
C         PTABLE--POINTER TABLE FOR THIS SUBTRACT
C         KEYCOL--COLUMN OF MATN2 USED FOR SUPPLYING KEY VALUES
C         KEYTYP--ATTRIBUTE TYPE OF MATN1 USED FOR KEY VALUES
         INCLUDE 'rmatts.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'rimptr.inc'
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
         IDST = 0
         IDNEW = 0
         IDCUR = NID
C
C  GET THE RM_Parameters FOR THE FIRST MATRIX.
C
         I = LOCREL(RNAME2)
         IDM2 = NID
         NSP = 0
         IF(KSTRT.NE.0) NSP = 2
         NTUP3 = 0
C
C  SEQUENCE THROUGH MATN1.
C
  100    CONTINUE
         IF(IDCUR.EQ.0) GO TO 1000
         CALL ITOH(N1,N2,IDCUR)
         IF(N2.EQ.0) GO TO 1000
         CALL GETDAT(1,IDCUR,MATN1,NCOL1)
         IF(IDCUR.LT.0) GO TO 1000
C
C  MOVE THE COMPARISON VALUE INTO THE WHCOM ARRAYS.
C
         CALL ITOH(NCHAR,NWORDS,KATTL(1))
         IP = MATN1 + KEYCOL - 1
         IF(NWORDS.NE.0) GO TO 110
C
C  SPECIAL GYRATIONS FOR VARIABLE LENGTH STUFF.
C
         IP1 = BUFFER(IP)
         IP = MATN1 + IP1 + 1
  110    CONTINUE
         WHRVAL(1) = BUFFER(IP)
         NID = IDM2
         NS = NSP
  200    CONTINUE
         CALL RMLOOK(MATN2,2,1,NCOL2)
         IF(RMSTAT.NE.0) GO TO 400
C
C  CHECK TO SEE IF THE ATTRIBUTES MATCH.
C
         K = 1
  300    CONTINUE
         CALL PTRS(IPT1,IPT2,K,NATT3,PTABLE,LEN,ITYPE)
C
C  IF K IS 0 WE HAVE LOOKED AT ALL THE COMMON ATTRIBUTES.
C
         IF(K.EQ.0) GO TO 100
         I1 = MATN1 + IPT1 - 1
         I2 = MATN2 + IPT2 - 1
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
  320    CONTINUE
         IPT1 = BUFFER(I1)
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
  400    CONTINUE
         ENDCOL = NCOL3
         DO 900 KLM=1,NATT3
            KOL1 = PTABLE(PTBL2,KLM)
            KOL3 = PTABLE(PTBL4,KLM)
            ATTLEN = PTABLE(PTBL5,KLM)
            CALL ITOH(NCHAR,NWORDS,ATTLEN)
            IF(NWORDS.EQ.0) GO TO 700
            DO 600 I=1,NWORDS
C
C  LOAD THE ATTRIBUTE FROM MATN2.
C
               I1 = MATN1 + KOL1 - 1
               MATN3(KOL3) = BUFFER(I1)
               KOL3 = KOL3 + 1
               KOL1 = KOL1 + 1
  600       CONTINUE
            GO TO 900
  700       CONTINUE
            ENDCOL = ENDCOL + 1
            MATN3(KOL3) = ENDCOL
            I1 = MATN1 + KOL1 - 1
            KOL1 = BUFFER(I1)
            I1 = MATN1 + KOL1 - 1
            NWORDS = BUFFER(I1)
            MATN3(ENDCOL) = NWORDS
            NWORDS = NWORDS + 1
            DO 800 I=1,NWORDS
               ENDCOL = ENDCOL + 1
               I1 = I1 + 1
               MATN3(ENDCOL) = BUFFER(I1)
  800       CONTINUE
  900    CONTINUE
         CALL ADDDAT(3,IDNEW,MATN3,ENDCOL)
         IF(IDST.EQ.0) IDST = IDNEW
         NTUP3 = NTUP3 + 1
C
C  LOOK FOR MORE IN MATN1.
C
         GO TO 100
C
C  ALL DONE.
C
 1000    CONTINUE
         I = LOCREL(RNAME3)
         CALL RELGET(ISTAT)
         RSTART = IDST
         REND = IDNEW
         NTUPLE = NTUP3
         CALL RELPUT
         NUM = NTUP3
         CALL MSG(' ','SUBTRACT COMPLETED,','+')
         CALL IMSG(NUM,6,'+')
         CALL MSG(' ',' ROWS GENERATED.',' ')
C
C  RETURN
C
         RETURN
      END
