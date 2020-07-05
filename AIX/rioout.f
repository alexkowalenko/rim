      SUBROUTINE RIOOUT(FILE,RECORD,BUFFER,NWDS,IOS)
      INCLUDE 'syspar.d'
C
C     **UNIX SYSTEM DEPENDENT ROUTINE **
C
C  PURPOSE:  ROUTINE FOR RANDOM OUTPUT
C
C  PARAMETERS
C         FILE----UNIT FOR WRITING
C         RECORD--RECORD NUMBER
C         BUFFER--BUFFER TO WRITE FROM
C         NWDS----NUMBER OF WORDS PER BUFFER
C         IOS-----STATUS VARIABLE - 0 MEANS SUCCESS, ELSE TILT
C
      INCLUDE 'flags.d'
      INCLUDE 'rio.d'
      INTEGER BUFFER(1)
 
      IUN = FILE - ZNFIL1 + 1
      IF(RECORD.NE.0) THEN
        WRITE(FILE,REC=RECORD,IOSTAT=IOS) (BUFFER(I),I=1,NWDS)
      ELSE
        N = IRECPS(IUN)
        WRITE(FILE,REC=N,IOSTAT=IOS) (BUFFER(I),I=1,NWDS)
      ENDIF
      IRECPS(IUN) = IRECPS(IUN) + 1
      IF (TRACE.GE.3) THEN                                              
         CALL MSG('T','RIOOUT:','+')                                    
         CALL IMSG(FILE,5,'+')                                          
         CALL IMSG(IPOS,5,'+')                                          
         CALL IMSG(NWDS,5,'+')                                          
         CALL IMSG(IOS,4,'+')                                           
         CALL IMSG(RECORD,5,' ')                                        
      ENDIF                                                             
      RETURN
      END
