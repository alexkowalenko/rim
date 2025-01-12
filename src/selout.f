      SUBROUTINE SELOUT(MAT,IATT,ADONE)

         USE RM_Globals, only : KMSSVL, KMSSVT, KNAPVL, KNAPVT
         USE DateTime, only : ASCDAT
         USE Formater, only : TYPER
         USE RM_Text, only : ABLANK, ITOA, ASCAN, STRMOV
         USE RM_Text, only : FILCH, ASSTAR
         USE Utils, only : ITOH

         INCLUDE 'syspar.inc'
C
C     PRINT ATTRIBUTE TO LINE
C
C     MAT.......DATA FOR THIS ATTRIBUTE
C     IATT......ATTRIBUTE NUMBER IN SELCOM
C     ADONE.....SET TO .TRUE. IF NO PARAGRAPHING LEFT
C
         INCLUDE 'rmatts.inc'
         INCLUDE 'selcom.inc'
C
         DIMENSION MAT(1)
         LOGICAL ADONE

         ADONE = .TRUE.
         IPOS = 1
         FMT = FORMT(IATT)
         IF (FMT.EQ.0) FMT = ITEMW(IATT)
         PGRAPH = FMT/10000
         CALL TYPER(ATYPE(IATT),SVM,TYP)


         IF (TYP.EQ.KZDATE .OR. TYP.EQ.KZTIME) PGRAPH = 0
         IF(CURPOS(IATT).NE.1 .AND. PGRAPH.EQ.0) GO TO 9999
         IF(CURPOS(IATT).GT.LEN(IATT)) GO TO 9999
         IF(TYP.EQ.KZDATE .OR. TYP.EQ.KZTIME) GO TO 90
         IF(TYP.NE.KZTEXT) GO TO 100
C
C     RM_Text
C
         IF(PGRAPH.EQ.0) THEN

C       NON-PARAGRAPHED RM_Text
            NC = NUMCOL(IATT)
            IF(NC.GT.LEN(IATT)) NC = LEN(IATT)
         ELSE

C       PARAGRAPHED RM_Text
            NC = NUMCOL(IATT)
            MAX = LEN(IATT) - CURPOS(IATT) + 1
            IF(NC.GT.MAX) NC = MAX
            IF(NC.LT.MAX) THEN

C         SEE IF WE NEED WORRY ABOUT BROKEN WORDS
               MC = 0
               IPOS = ASCAN(MAT(1),CURPOS(IATT)+1,-NC,ABLANK,.TRUE.)
               IF(IPOS.NE.0) MC = IPOS - CURPOS(IATT) + 1
               IF(MC.GT.4) NC = MC
               ADONE = .FALSE.
C
C         CHECK IF REMAINDER OF LINE IS BLANK
C
               N = LEN(IATT) - CURPOS(IATT) - NC
               IPOS = ASCAN(MAT(1),CURPOS(IATT)+NC,N,ABLANK,.FALSE.)
               IF(IPOS.EQ.0) ADONE = .TRUE.
            ENDIF
         ENDIF
C
         IF (MAT(1).EQ.ZIMISS) THEN
            CALL STRMOV(KMSSVT,1,
     +                  MIN(NUMCOL(IATT),KMSSVL),LINE,COL1(IATT))
         ELSE IF (MAT(1).EQ.ZINAPP) THEN
            CALL STRMOV(KNAPVT,1,
     +                   MIN(NUMCOL(IATT),KNAPVL),LINE,COL1(IATT))
         ELSE
            CALL STRMOV(MAT(1),CURPOS(IATT),NC,LINE,COL1(IATT))
         ENDIF
         CURPOS(IATT) = CURPOS(IATT) + NC
         IF(IPOS.EQ.0) CURPOS(IATT) = LEN(IATT) + 1
         GO TO 9999
C
C     DATE/TIME STUFF
C
   90    CALL ASCDAT(LINE,COL1(IATT),L,MAT(1),FMT,TYP)
         CURPOS(IATT) = 2
         GOTO 9999
C
C     NON-RM_Text, NON-DATE STUFF
C
  100    IF(SVM.EQ.KZMAT) GO TO 1000
         IF(SINGLE(IATT).NE.0) GO TO 3000
C
C     WE HAVE NON-MATRIX STUFF
C
         NUMTOP = (NUMCOL(IATT)+2)/(ITEMW(IATT)+2)
         IF((PGRAPH.NE.0).AND.(PGRAPH.LT.NUMTOP)) NUMTOP = PGRAPH
         IP = CURPOS(IATT)
         IF(TYP.EQ.KZDOUB) IP = 2*IP - 1
         IC = COL1(IATT)
         IF(.NOT.VAR(IATT)) GO TO 150
         IF(SLVMHD) THEN
C       PUT IN DIMENSION
            IF(CURPOS(IATT).EQ.1) CALL ITOA(LINE,IC,6,LEN(IATT),IERR)
            IC = IC + 10
         ENDIF

  150    NUMT = LEN(IATT) - CURPOS(IATT) + 1
         IF(NUMTOP.GT.NUMT) NUMTOP = NUMT
         DO 200 I=1,NUMTOP
            CALL SELPUT(MAT(IP),TYP,FMT,IC,LINE)
            IP = IP + 1
            IF(TYP.EQ.KZDOUB) IP = IP + 1
            IC = IC + 2 + ITEMW(IATT)
  200    CONTINUE

         CURPOS(IATT) = CURPOS(IATT) + NUMTOP
         IF(PGRAPH.EQ.0) GO TO 9999
         IF(CURPOS(IATT).LE.LEN(IATT)) ADONE = .FALSE.
         GO TO 9999
C
C     MATRICIES
C
 1000    IF(SINGLE(IATT).NE.0) GO TO 3500
         NUMTOP = (NUMCOL(IATT)+2)/(ITEMW(IATT)+2)
         IF((PGRAPH.NE.0).AND.(PGRAPH.LT.NUMTOP)) NUMTOP = PGRAPH
         IP = CURPOS(IATT)
         JC = (IP-1)/ROWD(IATT)
         JR = IP - JC*ROWD(IATT)
         JC = JC + 1
         IC = COL1(IATT)
         IF(.NOT.VAR(IATT)) GO TO 1150
         IF(NUMCOL(IATT).LT.20) GO TO 1150
C
C     PUT IN ROW AND COLUMN
C
         IF (SLVMHD) THEN
            IF(CURPOS(IATT).EQ.1) THEN
               CALL ITOA(LINE,IC,4,ROWD(IATT),IERR)
               CALL ITOA(LINE,IC+4,4,COLD(IATT),IERR)
            ENDIF
            IC = IC + 10
         ENDIF

 1150    CONTINUE
         NUMT = COLD(IATT)*(ROWD(IATT)-JR) + COLD(IATT) - JC + 1
         IF(NUMTOP.GT.NUMT) NUMTOP = NUMT
         DO 1200 I=1,NUMTOP
            IP = ROWD(IATT)*(JC-1) + JR
            IF(TYP.EQ.KZDMAT) IP = 2 * IP - 1
            CALL SELPUT(MAT(IP),TYP,FMT,IC,LINE)
            JC = JC + 1
            IF(JC.LE.COLD(IATT)) GO TO 1170
            JC = 1
            JR = JR + 1
            IF(PGRAPH.NE.0) GO TO 1220
 1170       CONTINUE
            IC = IC + 2 + ITEMW(IATT)
 1200    CONTINUE
 1220    CONTINUE
C     IF(.NOT.TRUNC(IATT)) GO TO 1240
C     IF(JC.EQ.1) GO TO 1240
C     JR = JR + 1
C     JC = 1
 1240    CONTINUE
         CURPOS(IATT) = ROWD(IATT)*(JC-1) + JR
         IF(PGRAPH.EQ.0) GO TO 9999
         IF(JR.LE.ROWD(IATT)) ADONE = .FALSE.
         IF(ADONE)CURPOS(IATT) = LEN(IATT) + 1
         GO TO 9999
C
C     SINGLE ITEM FROM A VECTOR
C
 3000    IP = SINGLE(IATT)
         CURPOS(IATT) = LEN(IATT) + 1
         IF(IP.GT.LEN(IATT)) GO TO 3800
         CALL SELPUT(MAT(IP),TYP,FMT,COL1(IATT),LINE)
         GO TO 9999
C
C     SINGLE ITEM FROM A MATRIX
C
 3500    CURPOS(IATT) = LEN(IATT) + 1
         CALL ITOH(JR,JC,SINGLE(IATT))
         IF(JR.GT.ROWD(IATT)) GO TO 3800
         IF(JC.GT.COLD(IATT)) GO TO 3800
         IP = ROWD(IATT)*(JC-1) + JR
         IF(TYP.EQ.KZDMAT) IP = 2 * IP - 1
         CALL SELPUT(MAT(IP),TYP,FMT,COL1(IATT),LINE)
         GO TO 9999
C
C     OUT OF RANGE
C
 3800    CALL FILCH(LINE,COL1(IATT),ITEMW(IATT),ASSTAR)
 9999    CONTINUE
         RETURN
      END
