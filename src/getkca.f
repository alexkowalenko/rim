      LOGICAL FUNCTION GETKCA(PTABLE,NATT,RNAME,REQKCA)

         USE RM_Globals, only : RMSTAT
         USE Extern, only: IMSG
         USE Lexer, only : KXKEYW, KXNAME, IDT, ASCREC, IDP, IDL, KWS
         USE Lexer, only : ITEMS
         USE RIM, only: BUILD
         USE Utils, only : ZMOVE

         INCLUDE 'syspar.inc'
C
C     GET A KEYED COMMON ATTRIBUTE FROM A SET OF ATTRIBUTES
C     DEFINED IN BLOCK 10
C
C     IF REQKCA IS TRUE AND NO KEYED ATTRIBUTE EXISTS
C     THEN ONE WILL BE BUILT.
C
C       PTABLE -- POINTER TO KCA IN BLOCK 10
C       NATT ---- NUMBER OF ATTRIBUTES IN BLOCK 10
C       RNAME --- RELATION TO SEARCH
C       REQKCA -- FLAG TO FORCE KEY BUILDING
C
C       GETKCA RETURNS TRUE IF A KEYED COMMON ATTRIBUTE WAS FOUND
C
         LOGICAL REQKCA
C
         INCLUDE 'tuplea.inc'
         INCLUDE 'buffer.inc'
         INCLUDE 'dclar1.inc'
         INCLUDE 'ptbl.inc'
C
C     TOKEN DATA TO BUILD KEYS
C
         INTEGER BKIDT(6), BKIDP(6), BKIDL(6)
         DATA BKIDT /KXKEYW,KXKEYW,KXKEYW,KXNAME,KXKEYW,KXNAME/
         DATA BKIDL /4,3,3,ZC,2,ZC/
         DATA BKIDP /1,2,3,4,0,0/
         BKIDP(5) = 4+Z
         BKIDP(6) = 5+Z
C
C     FIRST LOOP LOOKS FOR A KEYED COMMON ATTRIBUTE
C     SECOND LOOP BUILDS ONE
C
         DO 500 KL = 1, 2
            PT = BLKLOC(10) - PTBLL
C
            DO 200 K=1,NATT
               PT = PT + PTBLL
               IF(BUFFER(PT+PTBL2-1).EQ.0) GO TO 200
               IF(BUFFER(PT+PTBL3-1).EQ.0) GO TO 200
               J = LOCATT(BUFFER(PT),RNAME)
               IF(J.NE.0) GO TO 200
               CALL ATTGET(ISTAT)
               IF(ATTKEY.NE.0) GO TO 900
C
C     IF SECOND PASS, THEN BUILD KEY ON THIS ATTRIBUTE.
               IF (KL.EQ.2) THEN
C
C        FAKE A BUILD KEY COMMAND AND USE BUILD TO DO THE KEY
C
                  ITEMS = 6
                  DO 150 I = 1, 6
                     IDT(I) = BKIDT(I)
                     IDP(I) = BKIDP(I)
  150             IDL(I) = BKIDL(I)
                  KWS(1) = 'BUIL'
                  KWS(2) = 'KEY'
                  KWS(3) = 'FOR'
                  CALL ZMOVE(ATTNAM,ASCREC(IDP(4)))
                  KWS(4) = 'IN'
                  CALL ZMOVE(RNAME,ASCREC(IDP(6)))
C
C        SUBROUTINE BUILD CLEARS BLANK COMMON.
C        BLOCK 10 NEEDS TO BE SAVED.
C
C        STORE BLOCK 10 INFO AND ZERO OUT THE BLOCKS.
                  CALL BLKUP
                  IF (RMSTAT .GT. 0) THEN
                     CALL MSG('E','TUPRC BLOCK ERROR',' ')
                     GOTO 900
                  ENDIF
C
                  CALL BUILD
C
C        RE-ESTABLISH BLOCK 10.
C
                  CALL BLKDWN
C
                  RMSTAT = MAX(RMSTAT,0)
                  IF (RMSTAT.NE.0) THEN
                     CALL MSG('E','TOKRC KEY BUILD ERROR NUMBER ','+')
                     CALL IMSG(RMSTAT,4,' ')
                     GOTO 900
                  ENDIF
                  CALL MSG(' ','A KEY HAS BEEN BUILT FOR COLUMN ''','+')
                  CALL AMSG(ATTNAM,-ZC,'+')
                  CALL MSG(' ',''' IN THE ''','+')
                  CALL AMSG(RNAME,-ZC,'+')
                  CALL MSG(' ',''' TABLE.','+')
                  GOTO 900
               ENDIF
C
  200       CONTINUE
            IF (.NOT.REQKCA) GOTO 800
  500    CONTINUE
         CALL MSG('E','THERE ARE NO COMMON ATTRIBUTES.',' ')
C
C     KEY COMMON ATTRIBUTE NOT FOUND
C
  800    GETKCA = .FALSE.
         RETURN
C
C     KEY COMMON ATTRIBUTE FOUND
C
  900    PTABLE = PT
         GETKCA = .TRUE.
         RETURN
      END
