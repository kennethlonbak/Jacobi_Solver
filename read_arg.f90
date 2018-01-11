module read_arg

    INTERFACE read_vari_arg
        MODULE PROCEDURE read_vari_arg_integer, read_vari_arg_double, read_vari_arg_char
    END INTERFACE read_vari_arg
    CONTAINS
    FUNCTION read_vari_arg_integer(var_name,default_val)
        INTEGER :: read_vari_arg_integer
        CHARACTER(LEN=*), INTENT(IN) :: var_name
        INTEGER, INTENT(IN) :: default_val
        INTEGER :: i, i_arg
        CHARACTER(LEN=200) :: arg
        read_vari_arg_integer = default_val
        DO i = 1,iargc()
            CALL GETARG(i,arg)
            i_arg = INDEX(arg,var_name)
            IF (i_arg > 0) THEN
                i_arg = INDEX(arg,"=")
                arg = arg(i_arg+1:len(arg))
                READ(arg,*) read_vari_arg_integer
                WRITE(*,*) "The variable ", var_name, " was set,", var_name,"=",read_vari_arg_integer
            end if
        end do
    END FUNCTION read_vari_arg_integer

    FUNCTION read_vari_arg_double(var_name,default_val)
        REAL(KIND(0D0)) :: read_vari_arg_double
        CHARACTER(LEN=*), INTENT(IN) :: var_name
        REAL(KIND(0D0)), INTENT(IN) :: default_val
        INTEGER :: i, i_arg
        CHARACTER(LEN=200) :: arg
        read_vari_arg_double = default_val
        DO i = 1,iargc()
            CALL GETARG(i,arg)
            i_arg = INDEX(arg,var_name)
            IF (i_arg > 0) THEN
                i_arg = INDEX(arg,"=")
                arg = arg(i_arg+1:len(arg))
                READ(arg,*) read_vari_arg_double
                WRITE(*,*) "The variable ", var_name, " was set,", var_name,"=",read_vari_arg_double
            end if
        end do
    END FUNCTION read_vari_arg_double

    FUNCTION read_vari_arg_char(var_name,default_val)
        CHARACTER(LEN=200) :: read_vari_arg_char
        CHARACTER(LEN=*), INTENT(IN) :: var_name
        CHARACTER(LEN=*), INTENT(IN) :: default_val
        INTEGER :: i, i_arg
        CHARACTER(LEN=200) :: arg
        read_vari_arg_char = default_val
        DO i = 1,iargc()
            CALL GETARG(i,arg)
            i_arg = INDEX(arg,var_name)
            IF (i_arg > 0) THEN
                i_arg = INDEX(arg,"=")
                arg = arg(i_arg+1:len(arg))
                read_vari_arg_char = arg
                WRITE(*,*) "The variable ", var_name, " was set,", var_name,"=",read_vari_arg_char
            end if
        end do
    END FUNCTION read_vari_arg_char


end module read_arg