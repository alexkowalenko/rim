      SUBROUTINE CHKATT(TMPA,NUMELE,ERROR)

         USE DateTime, only : RMDATE
         USE Extern, only : AMSG, MSG
         USE Lexer, only : ITEMS, LXSREC
         USE Message, only : WARN
         USE RM_Text, only: BLANK, NONE
         USE Utils, only : ZMOVE, ITOH

         INCLUDE 'syspar.inc'
C
C     THIS ROUTINE EDITS THE ATTRIBUTE LIST ON THE RELATION CARD
C     AND CREATES A NEW RELATION.
C
C  RM_Parameters:
C         TMPA----SCRATCH ARRAY WITH NEW ATTRIBUTE NAMES
         INCLUDE 'tmpa.inc'
C
C         NUMELE--THE NUMBER OF ATTRIBUTES IN TMPA
C         ERROR---COUNT OF THE ERRORS ENCOUNTERED
C
         INTEGER TMPA(TMPAL,1)
C
         INCLUDE 'tuplea.inc'
         INCLUDE 'tupler.inc'
C
         LOGICAL EQ, NE
         INTEGER IFLAG
         INTEGER CSTART
         INCLUDE 'dclar1.inc'
C
         ERROR = 0
         NCOLS = 0
         IFLAG = 0
C
C  SEARCH THE LIST
C
         CALL ZMOVE(RNAME,BLANK)
         DO 600 I=3,ITEMS
            CALL LXSREC(I,ANAME,ZC)
C
C  LOOK FOR THIS ATTRIBUTE AMONG EXISTING ATTRIBUTES.
C
            IF(LOCATT(ANAME,RNAME).EQ.0) THEN
               CALL ATTGET(IDUMMY)
               NCHAR = ATTCHA
               NWORDS = ATTWDS
               GO TO 500
            ENDIF
C
C  LOOK FOR THIS ATTRIBUTE AMONG NEW ATTRIBUTES.
C
            IF(NUMELE.EQ.0) GO TO 300

            DO 200 J=1,NUMELE
               IF(NE(TMPA(TMPA1,J),ANAME)) GO TO 200
               CALL ITOH(NCHAR,NWORDS,TMPA(TMPA3,J))
               GOTO 500
  200       CONTINUE
C
C  CANNOT FIND THIS ATTRIBUTE.
C
  300       CALL MSG('E','THERE IS NO COLUMN NAMED ''','+')
            CALL AMSG(ANAME,ZC,'+')
            CALL MSG(' ','''.',' ')
            ERROR = ERROR + 1
            IFLAG = 1
            GO TO 600
C
C  THE NUMBER OF WORDS NEEDED DEPEND ON THE ATTRIBUTE TYPE.
C
  500       IF(NWORDS.EQ.0) NWORDS = 1
            NCOLS = NCOLS + NWORDS
  600    CONTINUE
         IF(IFLAG.EQ.1) GO TO 999
         IF(NCOLS.GT.MAXCOL) THEN
            CALL WARN(15)
            ERROR = ERROR + 1
            GO TO 999
         ENDIF
C
C  LOAD THIS RELATION USING TUPLER AND TUPLEA.
C
         CALL LXSREC(1,RNAME,ZC)
         NATT = ITEMS - 2
         CALL ATTNEW(RNAME,NATT)
C
C  SET UP THE NEW TUPLER.
C
         CALL ZMOVE(NAME,RNAME)
         RDATE = RMDATE()
         NCOL = NCOLS
         NTUPLE = 0
         RSTART = 0
         REND = 0
         CALL ZMOVE(RPW,NONE)
         CALL ZMOVE(MPW,NONE)
         CALL RELADD
C
C  NOW ADD TO THE ATTRIBUTE RELATION VIA TUPLEA.
C
         CSTART = 1
         DO 1600 I=3,ITEMS
            CALL LXSREC(I,ANAME,ZC)
C
C  LOOK FOR THIS ATTRIBUTE AMONG EXISTING ATTRIBUTES.
C
            CALL ZMOVE(RNAME,BLANK)
            IF(LOCATT(ANAME,RNAME).EQ.0) THEN
               CALL ATTGET(IDUMMY)
               CALL ZMOVE(RELNAM,NAME)
               ATTCOL = CSTART
               GO TO 1500
            ENDIF
C
C  LOOK FOR THIS ATTRIBUTE AMONG NEW ATTRIBUTES.
C
            IF(NUMELE.EQ.0) GO TO 1500
            DO 1200 J=1,NUMELE
               IF(EQ(TMPA(TMPA1,J),ANAME)) GO TO 1400
 1200       CONTINUE
 1400       CONTINUE
            CALL ZMOVE(ATTNAM,ANAME)
            CALL ZMOVE(RELNAM,NAME)
            ATTCOL = CSTART
            ATTLEN = TMPA(TMPA3,J)
            ATTYPE = TMPA(TMPA2,J)
            ATTKEY = TMPA(TMPA4,J)
            ATTFOR = TMPA(TMPA5,J)
 1500       CONTINUE
            CALL ITOH(NCHAR,NWORDS,ATTLEN)
            IF(NWORDS.EQ.0) NWORDS = 1
            CSTART = CSTART + NWORDS
            IF(ATTKEY.NE.0) CALL BTINIT(ATTKEY)
            CALL ATTADD
 1600    CONTINUE
C
C  DONE
C
  999    RETURN
      END
