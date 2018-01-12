MODULE m_jacobi_solver
    CONTAINS
    SUBROUTINE Jacobi_Solver(N,k_max,d_min,uk,ukp1,fdx2,wall_time,k,d,show_state,mod_state)
        use declare_var, ONLY: MK
        use omp_lib
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: N, k_max
        REAL(MK), DIMENSION(N,N), INTENT(INOUT) :: uk, ukp1
        REAL(MK), DIMENSION(N,N), INTENT(IN) :: fdx2
        REAL(MK), INTENT(IN) :: d_min
        INTEGER, INTENT(OUT), OPTIONAL :: k
        REAL(MK), INTENT(OUT), OPTIONAL :: d, wall_time
        LOGICAL, OPTIONAL :: show_state
        INTEGER, OPTIONAL :: mod_state
        INTEGER :: i, j


        ! Setting optional variabels (Problems with setting default values: "Segmentation fault (core dumped)")
        IF (.NOT.PRESENT(show_state)) show_state = .true.
        IF (.NOT.PRESENT(mod_state)) mod_state = 20

        WRITE(*,*)
        WRITE(*,*) "Starting Jacobi iterations. (k_max=",k_max," N=",N," d_min=",d_min,")"
        wall_time = omp_get_wtime()
        DO k = 1,k_max
            d = 0d0
            DO i=2,N-1
                DO j=2,N-1
                    ukp1(i,j) = (uk(i,j-1)+uk(i,j+1)+uk(i-1,j)+uk(i+1,j)+fdx2(i,j))*25d-2
                    d = d + (ukp1(i,j)-uk(i,j))**2
                END DO
            END DO
            d = d/N**2
            ! Build convergence cretia
            IF (d < d_min.and.(k > 10)) THEN
                WRITE(*,*) "The solver converged after: ", k, "Iterations (k_max: ", k_max, ")"
                WRITE(*,"(A,ES8.2E2,A,ES8.2E2,A)") " d: ", d, " (d_min:",d_min,")"
                exit
            end if

            IF (MOD(k,mod_state)==0.and.show_state) THEN
                WRITE(*,"(A,I6,A,ES8.2E2,A)") " Solution is not converged yet. (k= ",k,", d= ",d,")"
            end if
            uk = ukp1
        end do
        wall_time = omp_get_wtime()-wall_time

        IF (k > k_max) THEN
            WRITE(*,*)
            WRITE(*,*) "!! WARNING: The solver did NOT converge within k_max:", k_max, " !!"
            WRITE(*,"(A,ES8.2E2,A,ES8.2E2,A)") " d: ", d, " (d_min:",d_min,")"
            WRITE(*,*)
        end if
        WRITE(*,*) "Wall time=", wall_time, " secounds"

    END SUBROUTINE Jacobi_Solver

    SUBROUTINE Jacobi_Solver_pal1(N,k_max,d_min,uk,ukp1,fdx2,wall_time,k,d,show_state,mod_state)
        use declare_var, ONLY: MK
        use omp_lib
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: N, k_max
        REAL(MK), DIMENSION(N,N), INTENT(INOUT) :: uk, ukp1
        REAL(MK), DIMENSION(N,N), INTENT(IN) :: fdx2
        REAL(MK), INTENT(IN) :: d_min
        INTEGER, INTENT(OUT), OPTIONAL :: k
        REAL(MK), INTENT(OUT), OPTIONAL :: d, wall_time
        LOGICAL, OPTIONAL :: show_state
        INTEGER, OPTIONAL :: mod_state
        INTEGER :: i, j

        ! Setting optional variabels (Problems with setting default values: "Segmentation fault (core dumped)")
        IF (.NOT.PRESENT(show_state)) show_state = .true.
        IF (.NOT.PRESENT(mod_state)) mod_state = 20

        WRITE(*,*)
        WRITE(*,*) "** Starting Jacobi parallel v1. (and timeing) **"
        WRITE(*,*) " N=",N, "k_max=",k_max, "N_th=",omp_get_num_threads()," d_min=",d_min
        wall_time = omp_get_wtime()
        DO k = 1,k_max
            d = 0d0
            !$omp parallel do default(none) shared(uk,ukp1,N,fdx2) private(i,j) reduction(+: d)
            DO i=2,N-1
                DO j=2,N-1
                    ukp1(i,j) = (uk(i,j-1)+uk(i,j+1)+uk(i-1,j)+uk(i+1,j)+fdx2(i,j))*25d-2
                    d = d + (ukp1(i,j)-uk(i,j))**2
                END DO
            END DO
            !$omp end parallel do

            ! Build convergence cretia
            IF (d < d_min.and.(k > 10)) exit


            IF (MOD(k,mod_state)==0.and.show_state) THEN
                WRITE(*,"(A,I6,A,ES8.2E2,A)") " Solution is not converged yet. (k= ",k,", d= ",d,")"
            end if
            uk = ukp1
        end do
        wall_time = omp_get_wtime()-wall_time
        IF (k > k_max) THEN
            WRITE(*,*)
            WRITE(*,*) "!! WARNING: The solver did NOT converge within k_max:", k_max, " !!"
            WRITE(*,"(A,ES8.2E2,A,ES8.2E2,A)") " d: ", d, " (d_min:",d_min,")"
            WRITE(*,*)
        ELSE
            WRITE(*,*) "The solver converged after: ", k, "Iterations (k_max: ", k_max, ")"
            WRITE(*,"(A,ES8.2E2,A,ES8.2E2,A)") " d: ", d, " (d_min:",d_min,")"
        end if
        WRITE(*,*) "Wall time=", wall_time, " secounds"

    END SUBROUTINE Jacobi_Solver_pal1

    SUBROUTINE Jacobi_Solver_pal2(N,k_max,d_min,uk,ukp1,fdx2,wall_time,k,d,show_state,mod_state)
        use declare_var, ONLY: MK
        use omp_lib
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: N, k_max
        REAL(MK), DIMENSION(N,N), INTENT(INOUT) :: uk, ukp1
        REAL(MK), DIMENSION(N,N), INTENT(IN) :: fdx2
        REAL(MK), INTENT(IN) :: d_min
        INTEGER, INTENT(OUT), OPTIONAL :: k
        REAL(MK), INTENT(OUT), OPTIONAL :: d, wall_time
        LOGICAL, OPTIONAL :: show_state
        INTEGER, OPTIONAL :: mod_state
        INTEGER :: i, j

        ! Setting optional variabels (Problems with setting default values: "Segmentation fault (core dumped)")
        IF (.NOT.PRESENT(show_state)) show_state = .true.
        IF (.NOT.PRESENT(mod_state)) mod_state = 20

        WRITE(*,*)
        WRITE(*,*) "** Starting Jacobi parallel v2. (and timeing) **"
        WRITE(*,*) " N=",N, " k_max=",k_max, " N_th=",omp_get_num_threads()," d_min=",d_min
        wall_time = omp_get_wtime()
        !$omp parallel default(none) shared(k,uk,ukp1,fdx2,N,k_max,d) private(i,j)
        DO k = 1,k_max
            d = 0d0
            !$omp do reduction(+: d)
            DO i=2,N-1
                DO j=2,N-1
                    ukp1(i,j) = (uk(i,j-1)+uk(i,j+1)+uk(i-1,j)+uk(i+1,j)+fdx2(i,j))*25d-2
                    d = d + (ukp1(i,j)-uk(i,j))**2
                END DO
            END DO
            !$omp end do

            ! Build convergence cretia
            !IF (d < d_min.and.(k > 10)) exit
            !$omp workshare
            uk = ukp1
            !$omp end workshare

        end do
        !$omp end parallel
        wall_time = omp_get_wtime()-wall_time

        IF (k > k_max) THEN
            WRITE(*,*)
            WRITE(*,*) "!! WARNING: The solver did NOT converge within k_max:", k_max, " !!"
            WRITE(*,"(A,ES8.2E2,A,ES8.2E2,A)") " d: ", d, " (d_min:",d_min,")"
            WRITE(*,*)
        ELSE
            WRITE(*,*) "The solver converged after: ", k, "Iterations (k_max: ", k_max, ")"
            WRITE(*,"(A,ES8.2E2,A,ES8.2E2,A)") " d: ", d, " (d_min:",d_min,")"
        end if
        WRITE(*,*) "Wall time=", wall_time, " secounds"

    END SUBROUTINE Jacobi_Solver_pal2
end module m_jacobi_solver