!
!  *** / W H C O M / ***
!
!  WHERE CLAUSE COMMON BLOCK
!
      INTEGER, PARAMETER :: WHAND=1,WHOR=2,WHNOT=3
!
      COMMON /WHCOM/ NBOO,BOO(ZMWHR),KATTP(ZMWHR),KATTL(ZMWHR),KATTY(ZMWHR),KOMTYP(ZMWHR),KOMPOS(ZMWHR),KOMLEN(ZMWHR), & 
      KOMPOT(ZMWHR),KSTRT,MAXTU,LIMTU,WHRVAL(ZMWHVL),WHRLEN(ZMWHLL),KBOOP

      INTEGER :: NBOO,BOO,KATTP,KATTL,KATTY,KOMTYP,KOMPOS,KOMLEN,KOMPOT,KSTRT,MAXTU,LIMTU,WHRVAL,WHRLEN,KBOOP
!
      INTEGER, PARAMETER :: ZWHCOM=5+8*ZMWHR+ZMWHVL+ZMWHLL
      INTEGER :: WHCOM0(ZWHCOM)
      EQUIVALENCE (WHCOM0(1),NBOO)
!
!  VARIABLE DEFINITIONS:
!              NBOO----NUMBER OF SIMPLE BOOLEAN CONDITIONS
!              BOO-----MOD 10 = BOOLEAN OPERATOR TO NEXT VALUE         )
!                      DIV 10 = LOGICAL NESTING LEVEL (1 = TOP)
!              KATTP---ATTRIBUTE COLUMN NUMBER
!              KATTL---ARRAY OF ATTRIBUTE LENGTHS IN WORDS EXCEPT TEXT
!                      IS IN CHARACTERS (0 IF "TUPLE" WHERE CLAUSE)
!              KATTY---ARRAY OF ATTRIBUTE TYPES
!                      (0 IF "TUPLE" WHERE CLAUSE)
!              KOMTYP--ARRAY OF BOOLEAN COMPARISON INTEGER IDENTIFIERS
!              KOMPOS--ARRAY OF POSITION POINTERS IN WHRVAL
!                      FOR THE START OF THE VALUE LIST
!                        OR SECOND ATTRIBUTE COLUMN NUMBER
!              KOMLEN--ARRAY INDICATING THE NUMBER OF ITEMS IN THE
!                      VALUE LIST
!              KOMPOT--ARRAY OF POINTERS TO WHRLEN
!              KSTRT---RECORD NUMBER OF THE STARTING NODE IN THE
!                      B-TREE IF KEY PROCESSING IS USED
!              MAXTU---MAXIMUM TUPLE NUMBER REQUESTED OR 0
!              LIMTU---MAXIMUM NUMBER OF TUPLES TO ACTUALLY PROCESS
!              WHRVAL--ARRAY OF VALUES POINTED TO BY KOMPOS
!              WHRLEN--ARRAY OF "ATTLEN" STYLE INFORMATION
!              WHRTUP--RELATION TUPLE POINTERS (IN BUFFER)
!              KBOOP---KEY ATTRIBUTE POINTER
!              ZWHCOM--LENGTH OF WHCOM IN WORDS