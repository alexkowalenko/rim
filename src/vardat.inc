C
C  *** / V A R D A T / ***
C
C  VARIABLE ATTRIBUTE DATA FOR THE FORTRAN INTERFACE
C
      PARAMETER (ZPIVAT=10)
C
      COMMON /VARDAT/ NUMVAR,POSVAR(2,ZPIVAT)
C
C  VARIABLE DEFINITION:
C         NUMVAR--NUMBER OF VARIABLE ATTRIBUTES (CURRENT RELATION)
C         POSVAR--ROW 1 - TUPLE COLUMN POINTERS FOR THE ATTRIBUTES
C                 ROW 2 - ATTRIBUTE TYPES
C