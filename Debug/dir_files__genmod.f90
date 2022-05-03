        !COMPILER-GENERATED INTERFACE MODULE: Tue May  3 09:17:51 2022
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DIR_FILES__genmod
          INTERFACE 
            SUBROUTINE DIR_FILES(FOLDER,EXTENSION,FILE_LIST)
              USE MOD_STRING_LIST
              CHARACTER(*), INTENT(IN) :: FOLDER
              CHARACTER(*), INTENT(IN) :: EXTENSION
              TYPE (T_STRING_LIST), INTENT(OUT) :: FILE_LIST
            END SUBROUTINE DIR_FILES
          END INTERFACE 
        END MODULE DIR_FILES__genmod
