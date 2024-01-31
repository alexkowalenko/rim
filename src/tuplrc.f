      SUBROUTINE TUPLRC(OPCODE,*)

         USE Globals, only : DFLAG, DMFLAG
         USE DateTime, only : RMDATE
         USE Message, only: WARN
         USE Text, only : BLANK
         USE Utils, only : ZMOVE

         INCLUDE 'syspar.inc'
C
C     PERFORM TUPLE RELATIONAL CALCULUS
C
C     OPCODE MAY BE: UNION, INTERSECT, OR SUBTRACT
C
C     FORMATS:
C
C     UNION     REL1 WITH REL2 FORMING REL3 USING ATTR1 ATTR2...ATTR-N
C     INTERSECT REL1 WITH REL2 FORMING REL3 USING ATTR1 ATTR2...ATTR-N
C     SUBTRACT  REL2 FROM REL1 FORMING REL3 USING ATTR1 ATTR2...ATTR-N
C
         CHARACTER*(*) OPCODE
         PARAMETER (OPIS=1, OPUN=2, OPSB=3)
C
C
         INCLUDE 'ascpar.inc'
         INCLUDE 'tokens.inc'
         INCLUDE 'rmatts.inc'
         INCLUDE 'rimptr.inc'
         INCLUDE 'rimcom.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'tuplea.inc'
         INCLUDE 'files.inc'
         INCLUDE 'buffer.inc'
         INCLUDE 'whcom.inc'
C
         INCLUDE 'ptbl.inc'
         INTEGER PTABLE
         LOGICAL EQ
         LOGICAL NE
         LOGICAL EQKEYW
         LOGICAL GETKCA, REQKCA
         INCLUDE 'dclar1.inc'
         INCLUDE 'dclar3.inc'
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
            CALL WARN(8)
            GO TO 999
         ENDIF
C
C     GET OPCODE TO INTEGER
C
         OP = 0
         IF (OPCODE.EQ.'INTERSECT') OP = OPIS
         IF (OPCODE.EQ.'UNION')     OP = OPUN
         IF (OPCODE.EQ.'SUBTRACT')  OP = OPSB
         IF (OP.EQ.0) GOTO 900

C
C  EDIT COMMAND SYNTAX
C
         CALL BLKCLN
         NS = 0
         IF(.NOT.EQKEYW(3,'WITH') .AND. .NOT.EQKEYW(3,'FROM')) GO TO 900
         IF(.NOT.EQKEYW(5,'FORMING')) GO TO 900
         IF(ITEMS.GT.6 .AND. .NOT.EQKEYW(7,'USING')) GO TO 900
C
C  KEYWORD SYNTAX OKAY
C
         IF (OP.NE.OPSB) THEN
            CALL LXSREC(2,RNAME1,ZC)
            CALL LXSREC(4,RNAME2,ZC)
         ELSE
            CALL LXSREC(4,RNAME1,ZC)
            CALL LXSREC(2,RNAME2,ZC)
         ENDIF
         I = LOCREL(RNAME1)
         IF(I.NE.0) THEN
C        MISSING FIRST RELATION.
            CALL WARN(1,RNAME1)
            GO TO 999
         ENDIF
C
C  SAVE DATA ABOUT RELATION 1
C
         I1 = LOCPRM(RNAME1,1)
         IF(I1.NE.0) THEN
            CALL WARN(9,RNAME1)
            GO TO 999
         ENDIF
         NCOL1 = NCOL
         NATT1 = NATT
         CALL ZMOVE(RPW1,RPW)
         CALL ZMOVE(MPW1,MPW)
         NTUP1 = NTUPLE
         I = LOCREL(RNAME2)
         IF(I.NE.0) THEN
C  MISSING SECOND RELATION.
            CALL WARN(1,RNAME2)
            GO TO 999
         ENDIF
C
C  SAVE DATA ABOUT RELATION 2
C
         I2 = LOCPRM(RNAME2,1)
         IF(I2.NE.0) THEN
            CALL WARN(9,RNAME2)
            GO TO 999
         ENDIF
         NCOL2 = NCOL
         NATT2 = NATT
         CALL ZMOVE(RPW2,RPW)
         CALL ZMOVE(MPW2,MPW)
         NTUP2 = NTUPLE
         NTUPSQ = (NTUP1+1)*(NTUP2+1)
C
C  CHECK FOR LEGAL RNAME3
C
         IF(.NOT.TOKTYP(6,KXNAME)) THEN
            CALL WARN(7,ASCREC(IDP(6)))
            GO TO 999
         ENDIF
C
C  CHECK FOR DUPLICATE RELATION 3
C
         CALL LXSREC(6,RNAME3,ZC)
         I = LOCREL(RNAME3)
         IF(I.EQ.0) THEN
            CALL WARN(5,RNAME3)
            GO TO 999
         ENDIF
C
C  CHECK USER READ SECURITY
C
         IF((I1.NE.0).OR.(I2.NE.0)) GO TO 999
C
C  RELATION NAMES OKAY -- CHECK THE ATTRIBUTES
C
C  SET UP PTABLE IN BUFFER 7
C
         NATTT = NATT1 + NATT2
         IF (OP.EQ.OPSB) NATTT = NATT1
         CALL BLKDEF(7,PTBLL,NATTT)
         PTABLE = BLKLOC(7)
         NATT3 = 0
         IF(ITEMS.EQ.6) GO TO 500
C
C  OPERATE ON SOME OF THE ATTRIBUTES
C
         IF(ITEMS-7.GT.NATTT) THEN
            CALL MSG('E','TOO MANY COLUMNS SPECIFIED.',' ')
            GO TO 999
         ENDIF
         IJ = 1
         DO 400 I=8,ITEMS
C
C  RETRIEVE ATTRIBUTE LENGTH FOR OLD ATTRIBUTE
C
C
C     GET THE ATTRIBUTE DATA
C
            CALL LXSREC(I,ANAME,ZC)
            ICHK1 = LOCATT(ANAME,RNAME1)
            IF(ICHK1.EQ.0) GO TO 300
            ICHK2 = LOCATT(ANAME,RNAME2)
            IF(OP.NE.OPSB. AND. ICHK2.EQ.0) GO TO 300
C
C  ATTRIBUTE WAS NOT FOUND
C
            CALL WARN(3,ANAME,BLANK)
            GO TO 999
C
C  ATTRIBUTE IS OKAY -- SET UP PTABLE
C
  300       CALL ATTGET(ISTAT)
            NATT3 = NATT3 + 1
            CALL ZMOVE(BUFFER(PTABLE),ANAME)
            BUFFER(PTABLE+PTBL4-1) = IJ
            NWORDS = ATTWDS
            BUFFER(PTABLE+PTBL5-1) = ATTLEN
            IF(NWORDS.EQ.0) NWORDS = 1
            IJ = IJ + NWORDS
            BUFFER(PTABLE+PTBL6-1) = ATTYPE
            BUFFER(PTABLE+PTBL7-1) = ATTFOR
            IF(ICHK1.EQ.0) THEN
               BUFFER(PTABLE+PTBL2-1) = ATTCOL
               ICHK2 = LOCATT(ANAME,RNAME2)
            ELSE
               BUFFER(PTABLE+PTBL2-1) = 0
            ENDIF
            IF(ICHK2.EQ.0) THEN
               CALL ATTGET(ISTAT)
               BUFFER(PTABLE+PTBL3-1) = ATTCOL
            ELSE
               BUFFER(PTABLE+PTBL3-1) = 0
            ENDIF
            PTABLE = PTABLE + PTBLL
C
  400    CONTINUE
         ICT = IJ - 1
         GO TO 555
C
C  OPERATION IS ON ALL ATTRIBUTES
C
  500    CONTINUE
         ICT = 1
C
C  STORE DATA FROM RELATION 1 IN PTABLE
C
         I = LOCATT(BLANK,RNAME1)
         DO 515 I=1,NATT1
            CALL ATTGET(ISTAT)
            IF(ISTAT.NE.0) GO TO 515
            NATT3 = NATT3 + 1
            CALL ZMOVE(BUFFER(PTABLE),ATTNAM)
            BUFFER(PTABLE+PTBL2-1) = ATTCOL
            BUFFER(PTABLE+PTBL4-1) = ICT
            NWORDS = ATTWDS
            BUFFER(PTABLE+PTBL5-1) = ATTLEN
            IF(NWORDS.EQ.0) NWORDS = 1
            ICT = ICT + NWORDS
            BUFFER(PTABLE+PTBL6-1) = ATTYPE
            BUFFER(PTABLE+PTBL7-1) = ATTFOR
            PTABLE = PTABLE + PTBLL
  515    CONTINUE
C
C  STORE DATA FROM RELATION 2 IN PTABLE
C
         KATT3 = NATT3
         I = LOCATT(BLANK,RNAME2)
         DO 550 I=1,NATT2
            CALL ATTGET(ISTAT)
            IF(ISTAT.NE.0) GO TO 550
C
C  FIRST CHECK TO SEE IF ATTRIBUTE IS ALREADY IN PTABLE.
C
            KQ1 = BLKLOC(7) - PTBLL
            DO 520 J=1,KATT3
               KQ1 = KQ1 + PTBLL
               IF(BUFFER(KQ1+PTBL3-1).NE.0) GO TO 520
               IF(EQ(BUFFER(KQ1),ATTNAM)) GO TO 530
  520       CONTINUE
C
C  NOT THERE -- PUT IT IN.
C
            IF (OP.EQ.OPSB) GOTO 550
            NATT3 = NATT3 + 1
            CALL ZMOVE(BUFFER(PTABLE),ATTNAM)
            BUFFER(PTABLE+PTBL3-1) = ATTCOL
            BUFFER(PTABLE+PTBL4-1) = ICT
            NWORDS = ATTWDS
            BUFFER(PTABLE+PTBL5-1) = ATTLEN
            IF(NWORDS.EQ.0) NWORDS = 1
            ICT = ICT + NWORDS
            BUFFER(PTABLE+PTBL6-1) = ATTYPE
            BUFFER(PTABLE+PTBL7-1) = ATTFOR
            PTABLE = PTABLE + PTBLL
            GO TO 550
C
C  ALREADY THERE -- CHANGE THE 2ND POINTER
C
  530       CONTINUE
            BUFFER(KQ1+PTBL3-1) = ATTCOL
  550    CONTINUE
         ICT = ICT - 1
C
C  DONE LOADING PTABLE
C
C  SEE IF THERE ARE ANY COMMON ATTRIBUTES.
C
  555    PTABLE = BLKLOC(7)
         DO 570 I = 1,NATT3
            IF((BUFFER(PTABLE+2).NE.0)
     +         .AND.(BUFFER(PTABLE+3).NE.0)) GO TO 600
            PTABLE = PTABLE + PTBLL
  570    CONTINUE
C
C  NO COMMON ATTRIBUTES
C
         CALL MSG('E','TABLES HAVE NO COMMON ATTRIBUTES.',' ')
         GO TO 999
C
C  PTABLE IS CONSTRUCTED
C
C  NOW CREATE ATTRIBUTE AND RELATION TABLES AND THE RELATION
C
  600    CONTINUE
         IF(ICT.GT.MAXCOL) GO TO 910
C
C  SET UP THE WHERE CLAUSE FOR THE OPERATION
C  THIS IS A DUMMY WHERE CLAUSE USED ONLY BY THE KEY PROCESSING
C  PTABLE IS NOW POINTING AT THE FIRST COMMON ATTRIBUTE.
C
         KEYCOL = BUFFER(PTABLE+PTBL3-1)
         KEYTYP = BUFFER(PTABLE+PTBL6-1)
         NBOO = -1
         KATTL(1) = BUFFER(PTABLE+PTBL5-1)
         KATTY(1) = KEYTYP
         IF(KEYTYP.EQ.KZIVEC) KATTY(1) = KZINT
         IF(KEYTYP.EQ.KZRVEC) KATTY(1) = KZREAL
         IF(KEYTYP.EQ.KZDVEC) KATTY(1) = KZDOUB
         IF(KEYTYP.EQ.KZIMAT) KATTY(1) = KZINT
         IF(KEYTYP.EQ.KZRMAT) KATTY(1) = KZREAL
         IF(KEYTYP.EQ.KZDMAT) KATTY(1) = KZDOUB
         KOMPOS(1) = 1
         KSTRT = 0
         MAXTU = ALL9S
         LIMTU = ALL9S
C
C  SET UP RELATION TABLE.
C
         CALL ZMOVE(NAME,RNAME3)
         RDATE = RMDATE()
         NCOL = ICT
         NCOL3 = ICT
         NATT = NATT3
         NTUPLE = 0
         RSTART = 0
         REND = 0
         CALL ZMOVE(RPW,RPW1)
         CALL ZMOVE(MPW,MPW1)
         IF (OP.NE.OPSB) THEN
            IF(EQ(RPW,NONE)) CALL ZMOVE(RPW,RPW2)
            IF(EQ(MPW,NONE)) CALL ZMOVE(MPW,MPW2)
         ENDIF
         CALL RELADD
C
         CALL ATTNEW(NAME,NATT)
         PTABLE = BLKLOC(7)
         DO 700 K=1,NATT3
            CALL ZMOVE(ATTNAM,BUFFER(PTABLE))
            CALL ZMOVE(RELNAM,NAME)
            ATTCOL = BUFFER(PTABLE+PTBL4-1)
            ATTLEN = BUFFER(PTABLE+PTBL5-1)
            ATTYPE = BUFFER(PTABLE+PTBL6-1)
            ATTFOR = BUFFER(PTABLE+PTBL7-1)
            ATTKEY = 0
            CALL ATTADD
            PTABLE = PTABLE + PTBLL
  700    CONTINUE
C
C     INTERSECT USES COMMON ATTRIBUTE IN RELATION 1
C     SUBTRACT USES COMMON ATTRIBUTE IN RELATION 2
C     UNION USES BOTH
C
         IF (OP.EQ.OPSB) GOTO 800
C
C     LOOK FOR A KEYED COMMON ATTRIBUTE IN RELATION 1
C
         REQKCA = NTUPSQ .GT. 2500
         IF (GETKCA(PTABLE,NATT3,RNAME1,REQKCA)) THEN
            KSTRT = ATTKEY
            NS = 2
            KATTL(1) = BUFFER(PTABLE+PTBL5-1)
            KATTY(1) = BUFFER(PTABLE+PTBL6-1)
            KEYCOL = BUFFER(PTABLE+PTBL3-1)
         ENDIF
C
C     CALL PROPER OPERATOR ROUTINE
C
         CALL BLKDEF(8,MAXCOL,1)
         KQ3 = BLKLOC(8)
         PTABLE = BLKLOC(7)
         I = LOCREL(RNAME2)
         IF (OP.EQ.OPIS) THEN
            CALL ISECT(RNAME1,RNAME3,BUFFER(KQ3),
     X           NCOL3,NATT3,BUFFER(PTABLE),KEYCOL,KEYTYP)
            GOTO 999
         ELSE IF (OP.EQ.OPUN) THEN
            CALL UNION(RNAME1,RNAME3,BUFFER(KQ3),
     X           NCOL3,NATT3,BUFFER(PTABLE),KEYCOL,KEYTYP,1)
         ENDIF
C
C     GET A KEYED COMMON ATTRIBUTE IN RELATION 2
C
  800    CALL BLKCLR(8)
         KSTRT = 0
         NS = 0
         KATTL(1) = 0
         KATTY(1) = 0
         KEYCOL = 0
C
         REQKCA = NTUPSQ .GT. 2500
         IF (GETKCA(PTABLE,NATT3,RNAME2,REQKCA)) THEN
            KSTRT = ATTKEY
            NS = 2
            KATTL(1) = BUFFER(PTABLE+PTBL5-1)
            KATTY(1) = BUFFER(PTABLE+PTBL6-1)
            KEYCOL = BUFFER(PTABLE+PTBL2-1)
         ENDIF
C
C  CALL UNION2 TO CONSTRUCT THE REST OF MATN3
C
         CALL BLKDEF(8,MAXCOL,1)
         KQ3 = BLKLOC(8)
         PTABLE = BLKLOC(7)
         I = LOCREL(RNAME1)
         IF (OP.EQ.OPUN) THEN
            CALL UNION(RNAME2,RNAME3,BUFFER(KQ3),
     X           NCOL3,NATT3,BUFFER(PTABLE),KEYCOL,KEYTYP,2)
         ELSE IF (OP.EQ.OPSB) THEN
            CALL SUBTRC(RNAME2,RNAME3,BUFFER(KQ3),
     X           NCOL3,NATT3,BUFFER(PTABLE),KEYCOL,KEYTYP)
         ENDIF
         GO TO 999
C
C  SYNTAX ERROR
C
  900    CALL WARN(4)
C
C
C  TUPLE LENGTH EXCEEDS MAXCOL
C
  910    CALL WARN(15)
C
C     DONE
C
  999    CALL BLKCLR(7)
         CALL BLKCLR(8)
         RETURN 1
      END
