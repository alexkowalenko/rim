MODULE Message
   implicit none
   private

   public WARN

contains

   SUBROUTINE WARN(W,TEXT1,TEXT2)

      USE RM_Parameters, only : Z, ZC
      USE Extern, only: IMSG
      USE RM_Globals, only: HXFLAG, RMSTAT

      !
      !  PURPOSE:   GENERAL PURPOSE ERROR PRINT ROUTINE
      !
      !  PARAMETERS:
      ! INPUT:  W-------WARNING NUMBER
      !         TEXT1----OPTIONAL ASCII-TEXT
      !         TEXT2----OPTIONAL ASCII-TEXT
      !

      INTEGER, intent(in) :: W
      INTEGER, intent(in), optional :: TEXT1(*), TEXT2(*)

      INTEGER :: NUM

      !
      ! MESSAGES
      !
      !     1   RELATION NOT FOUND
      !     2   NO DATABASE OPEN
      !     3   ATTRIBUTE NOT FOUND
      !     4   SYNTAX ERROR
      !     5   RELATION ALREADY EXISTS
      !     6   TERMINAL INTERRUPT
      !     7   INVALID NAME
      !     8   NO AUTHORITY (GENERIC)
      !     9   NO PERMISSION ON RELATION
      !    10   NOT A RIM DATABASE
      !    11   DATABASE NAME DOESN'T AGREE WITH FILENAMES
      !    12   DATABASE NOT UPDATED PROPERLY
      !
      !    15   ROW TOO LONG
      !
      SELECT CASE(W)
       CASE (1)
         CALL MSG('E','TABLE ''','+')
         CALL AMSG(TEXT1,-ZC,'+')
         CALL MSG(' ',''' IS NOT IN THE DATABASE.',' ')
       CASE (2)
         CALL MSG('E','NO DATABASE IS OPEN.',' ')
       CASE (3)
         CALL MSG('E','TABLE ''','+')
         CALL AMSG(TEXT2,-ZC,'+')
         CALL MSG(' ',''' DOES NOT CONTAIN COLUMN ''','+')
         CALL AMSG(TEXT1,-ZC,'+')
         CALL MSG(' ','''.',' ')
       CASE (4)
         CALL MSG('E','CHECK THE COMMAND SYNTAX.',' ')
       CASE (5)
         CALL MSG('E','TABLE ''','+')
         CALL AMSG(TEXT1,-ZC,'+')
         CALL MSG(' ',''' ALREADY EXISTS IN THE DATABASE.',' ')
       CASE (6)
         IF (HXFLAG.EQ.1) &
            CALL MSG('W','PROCESSING STOPPED AT YOUR REQUEST.',' ')
         HXFLAG = 2
       CASE (7)
         CALL MSG('E',' ','+')
         CALL AMSG(TEXT1,-ZC,'+')
         CALL MSG('E',' IS NOT A VALID NAME',' ')
       CASE (8)
         CALL MSG('E','YOU DO NOT HAVE AUTHORITY FOR THIS ' // &
            'OPERATION.',' ')
       CASE (9)
         ! NO PERMISSION FOR RELATION
         CALL MSG('E','YOU ARE NOT PERMITTED TO THE ''','+')
         CALL AMSG(TEXT1,-ZC,'+')
         CALL MSG(' ',''' TABLE.',' ')
         !
         ! RMSTAT ERROR CODES
         !
       CASE (10)
         CALL MSG('E','THE FILES DO NOT CONTAIN A RIM DATABASE.',' ')
       CASE (11)
         CALL MSG('W','RIM ERROR: 11',' ')
       CASE (12)
         CALL MSG('W','FILES WERE NOT UPDATED PROPERLY.',' ')
         CALL MSG(' ','I RECOMMEND RELOADING THE DATABASE.',' ')
       CASE (15)
         CALL MSG('W','THE ROW IS TOO LONG.',' ')
       CASE DEFAULT
         CALL MSG('E','WARNING CODE ','+')
         CALL IMSG(NUM,5,'+')
         CALL MSG(' ',' - ','+')
         IF(present(TEXT1)) CALL AMSG(TEXT1,ZC,'+')
         IF(present(TEXT2)) CALL AMSG(TEXT2,ZC,' ')
      END SELECT

      IF (RMSTAT.EQ.0) RMSTAT = W
      RETURN
   END SUBROUTINE WARN


END MODULE Message

