MODULE declare_var
    INTEGER, PARAMETER :: MK = KIND(0d0)
    ! Variabels that can be changed by user
    INTEGER :: N, k_max, k, N_th
    REAL(MK) :: d_min, d
    CHARACTER(LEN=200) :: filename
    ! Variabels to be used by the solver
    REAL(MK), DIMENSION(:,:), ALLOCATABLE :: uk, ukp1, fdx2
    REAL(MK) :: dx, dx2, wall_time
    INTEGER :: solver_type ! Solver type: 1= Jacobi, 2=Gauss-Seidel
    INTEGER :: mod_state, problem
    LOGICAL :: show_state

    CONTAINS
    SUBROUTINE initilize
        USE read_arg
        CHARACTER(LEN=200) :: temp_name

        ! Set N, k_max, d_min (Reads from argument or sets default value)
        N = read_vari_arg("N",100)
        k_max = read_vari_arg("k_max",100)
        d_min = read_vari_arg("d_min",1d-2)

        ! Setting problem
        problem = read_vari_arg("problem",1)

        ! Setting Solver Type
        solver_type = read_vari_arg("solver_type",1)

        ! Setting Number of threats
        N_th = read_vari_arg("N_th",1)
        call omp_set_num_threads(N_th)

        ! Setting output filename
        filename = "A"
        IF (filename /= read_vari_arg_char("filesub",filename)) THEN
            temp_name = read_vari_arg_char("filesub",filename)
            IF (solver_type > 2) THEN
                WRITE(filename,"(A,A,A,I4.4,A,I2.2,A)") "DATA/",TRIM(temp_name),"_N",N,"_Nt", N_th,".dat"
            ELSE
                WRITE(filename,"(A,A,A,I4.4,A)") "DATA/",TRIM(temp_name),"_N",N,".dat"
            end if
        ELSE
            IF (solver_type > 2) THEN
                WRITE(filename,"(A,A,A,I4.4,A,I2.2,A)") "DATA/","DATA" ,"_N",N,"_Nt", N_th,".dat"
            ELSE
                WRITE(filename,"(A,I4.4,A)") "DATA/DATA_N",N,".dat"
            END IF
        end if
        filename = read_vari_arg("filename",filename)

        ! Show state
        show_state = read_vari_arg("show_state",.true.)

        ! Setting mod_state
        mod_state = read_vari_arg("mod_state",20)

        ! Grid spacing
        dx = 2d0/(N-1)
        dx2 = (dx)**2

        ! Allocate fields (The fiels are initilized when setting bounday conditions)
        ALLOCATE(uk(N,N))
        ALLOCATE(ukp1(N,N))
        ALLOCATE(fdx2(N,N))

    END SUBROUTINE initilize

    SUBROUTINE set_bond_harmonic
        uk = 0d0
        ukp1 = 0d0
    END SUBROUTINE set_bond_harmonic

    SUBROUTINE set_bond_problem
        uk = 20d0
        uk(1:N,1) = 0d0
        ukp1 = uk
    END SUBROUTINE set_bond_problem

    SUBROUTINE set_f_harmonic
        INTEGER :: i,j
        REAL(MK), PARAMETER :: pi = ACOS(-1d0)
        REAL(MK) :: x,y
        DO i = 1,N
            DO j = 1,N
                x = (i-1)*dx-1d0
                y = (j-1)*dx-1d0
                fdx2(i,j) = SIN(pi*x)*SIN(pi*y)*dx2*2d0*(pi**2)
            end do
        end do
    END SUBROUTINE set_f_harmonic

    SUBROUTINE set_f_problem
        INTEGER :: i,j
        REAL(MK) :: x,y
        ! 0 ≤ x ≤ 1/3, −2/3 ≤ y ≤ −1/3
        DO i = 1,N
            DO j = 1,N
                x = (i-1)*dx-1d0
                y = (j-1)*dx-1d0
                IF ((0d0.LE.x).and.(x.LE.1d0/3d0)) THEN
                    IF ((-2d0/3d0.LE.y).and.(y.LE.-1d0/3d0)) THEN
                        fdx2(i,j) = 200d0*dx2
                    end if
                end if
            end do
        end do
    END SUBROUTINE set_f_problem

    SUBROUTINE write_matrix(mat, filename)
        REAL(MK), DIMENSION(:,:), INTENT(IN) :: mat
        CHARACTER(LEN=*), INTENT(IN) :: filename
        INTEGER :: i,j

        OPEN(61, FILE=filename, action="write")

        ! Writing header
        WRITE(61,*) "N=", N
        WRITE(61,*) "k_max=", k_max
        WRITE(61,*) "k=", k
        WRITE(61,*) "d_min=", d_min
        WRITE(61,*) "d=", d
        WRITE(61,*) "Wall_time=", wall_time

        IF (k>k_max) THEN
            WRITE(61,*) "State= NOT converged"
        ELSE
            WRITE(61,*) "State= Converged"
        end if

        IF (solver_type == 2) THEN
            WRITE(61,*) "solver_type= Gauss-Seidel"
        ELSE IF (solver_type == 3) THEN
            WRITE(61,"(A,I2)") "solver_type= Jacobi Pal 1, N_threads: ", N_th
        ELSE
            WRITE(61,*) "solver_type= Jacobi"
        end if

        IF (problem == 2) THEN
            WRITE(61,*) "problem= Harmonic"
        ELSE
            WRITE(61,*) "problem= Problem"
        end if
        ! writing out matrix
        DO i = 1,N
            DO j = 1,N
                WRITE(61,"(E16.8E2,A)",advance="no") mat(i,j), " "
            END DO
            WRITE(61,"(A)")
        END DO
        CLOSE(61)
    END SUBROUTINE write_matrix


END MODULE declare_var