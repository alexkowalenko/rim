PROGRAM Test1

   USE iso_fortran_env

   USE RM_Parameters

   PRINT *, compiler_version()
   PRINT *, compiler_options()

   PRINT *, "Input unit = ", input_unit
   PRINT *, "Output unit = ", output_unit
   PRINT *, "Error unit = ", error_unit

   PRINT "(A4,I2)", "Z =", Z
   PRINT "(A9,I3)", "ZCARDW =", ZCARDW
   PRINT "(A9,I3)", "ZPRINW =", ZPRINW

END PROGRAM Test1
