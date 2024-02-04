      SUBROUTINE RIMCMD

         USE Globals, only: HXFLAG
         USE Lexer, only: KXKEYW, TOKTYP, KWS, ITEMS, EQKEYW
         USE System, only : SYSCOM

         INCLUDE 'syspar.inc'
C
C     RIM COMMAND DISPATCHER
C
C
         INCLUDE 'ascpar.inc'
         INCLUDE 'rmatts.inc'
         INCLUDE 'rimcom.inc'
         INCLUDE 'files.inc'
C
         LOGICAL SELREL
C

C     -----------------------------------------------------
C
C
  100    CONTINUE
C
         IF (HXFLAG.GT.0) CALL SETIN(ZTRMIN)
         HXFLAG = 0
C
         CALL LODREC
         IF (ITEMS.LT.1) GOTO 100
         IF (KWS(1).EQ.'*') GOTO 100
C
C     GET THE COMMAND
C
         IF (.NOT.TOKTYP(1,KXKEYW)) GOTO 800
C
C---- QUERY COMMANDS
C
         IF (EQKEYW(1,'LIST'))    CALL LSTREL(*100)
         IF (EQKEYW(1,'EXHIBIT')) CALL XHIBIT(*100)
C
         IF (EQKEYW(1,'SELECT'))  CALL SELECT(*100)
         IF (EQKEYW(1,'FROM'))    THEN
            IF(SELREL(1,2)) GOTO 100
            GOTO 100
         ENDIF
C
         IF (EQKEYW(1,'UNLOAD'))  CALL UNLOAD(*100)
C
         IF (EQKEYW(1,'REPORT'))  CALL REPORT(*100)
C
C---- MODIFICATION COMMANDS
C
         IF (EQKEYW(1,'BUILD'))   CALL BUILD(*100)
         IF (EQKEYW(1,'CHANGE'))  THEN
            IF (ITEMS.EQ.4 .AND. EQKEYW(2,'OWNER')) CALL CHGPSW(*100)
            IF (ITEMS.EQ.6 .AND. EQKEYW(2,'RPW'))   CALL CHGPSW(*100)
            IF (ITEMS.EQ.6 .AND. EQKEYW(2,'MPW'))   CALL CHGPSW(*100)
            CALL CHGDAT(*100)
         ENDIF
         IF (EQKEYW(1,'DELETE'))  THEN
            IF (EQKEYW(2,'ROWS'))      CALL DELROW(*100)
            IF (EQKEYW(2,'DUPLICATES'))CALL DELDUP(*100)
         ENDIF
         IF (EQKEYW(1,'REFORMAT'))     CALL RFORMT(*100)
         IF (EQKEYW(1,'RENAME')) THEN
            IF (EQKEYW(2,'RELATION'))  CALL RNAMER(*100)
            IF (EQKEYW(2,'TABLE'))     CALL RNAMER(*100)
            IF (EQKEYW(2,'LINK'))      CALL RNAMEL(*100)
            CALL RNAMEA(*100)
         ENDIF
         IF (EQKEYW(1,'REMOVE'))  THEN
            IF (EQKEYW(2,'KEY'))       CALL REMKEY(*100)
            IF (EQKEYW(2,'LINK'))      CALL REMLNK(*100)
            CALL REMREL(*100)
         ENDIF
         IF (EQKEYW(1,'LOAD'))    CALL DBLOAD(*100)
C
C---- DATABASE IDENTIFICATION
C
         IF (EQKEYW(1,'OPEN'))    CALL DBOPCL(*100,'OPEN')
         IF (EQKEYW(1,'CLOSE'))   CALL DBOPCL(*100,'CLOSE')
C
C---- SCHEMA MODIFICATION
C
         IF (EQKEYW(1,'DEFINE'))  CALL DBDEFN(*100)
C
C---- RELATION ALGEBRA
C
         IF (EQKEYW(1,'INTERSECT'))  CALL TUPLRC('INTERSECT',*100)
         IF (EQKEYW(1,'UNION'))      CALL TUPLRC('UNION',*100)
         IF (EQKEYW(1,'JOIN'))       CALL JOIREL(*100)
         IF (EQKEYW(1,'SUBTRACT'))   CALL TUPLRC('SUBTRACT',*100)
         IF (EQKEYW(1,'PROJECT'))    CALL PJECT(*100)
CCCCCCIF (EQKEYW(1,'DIVIDE'))     CALL TUPLRC(*100)
C
C---- COMMANDS THAT DO NOT AFFECT A DATABASE
C
         IF (EQKEYW(1,'MACRO'))    CALL MACDEF(*100)
         IF (EQKEYW(1,'HELP'))     CALL RMHELP(*100)
         IF (EQKEYW(1,'?'   ))     CALL RMHELP(*100)
         IF (EQKEYW(1,'SHOW'))     CALL RMSHOW(*100)
         IF (EQKEYW(1,'SET'))      CALL RMSET(*100)
         IF (EQKEYW(1,'USER'))     CALL RMSET(*100)
         IF (EQKEYW(1,'ECHO'))     CALL RMSET(*100)
         IF (EQKEYW(1,'NOECHO'))   CALL RMSET(*100)
         IF (EQKEYW(1,'INPUT'))    CALL RMSET(*100)
         IF (EQKEYW(1,'OUTPUT'))   CALL RMSET(*100)
         IF (EQKEYW(1,'SYSTEM'))   CALL RMZIP(*100)

         IF (EQKEYW(1,'NEWPAGE')) THEN
            FFFLAG = 1
            GOTO 100
         ENDIF

C---- EXIT
C
C---C IF (EQKEYW(1,'END'))     GOTO 900
         IF (EQKEYW(1,'EXIT'))    GOTO 900
         IF (EQKEYW(1,'QUIT'))    GOTO 900
C
C---- LODREC EOF-GENERATED END  (IGNORE IT)
C
         IF (KWS(1).EQ.'END' .AND. KWS(2).EQ.'*EOF*')    GOTO 100
C
C     UNRECOGNISED COMMAND - POSSIBLY SYSTEM DEPENDENT
C
         CALL SYSCOM()
C
  800    CALL MSG('EU', KWS(1),'+')
         CALL MSG(' L',' IS NOT A RIM COMMAND.',' ')
         GOTO 100
C
C     EXIT
C
  900    CALL RMCLOS
         RETURN
      END
