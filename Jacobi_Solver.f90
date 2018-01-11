MODULE m_jacobi_solver
    CONTAINS
    SUBROUTINE Jacobi_Solver(N,k_max,d_min,uk,ukp1,f,dx2,k,d,show_state,mod_state)
        use declare_var, ONLY: MK
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: N, k_max

        REAL(MK), DIMENSION(N,N), INTENT(INOUT) :: uk, ukp1
        REAL(MK), DIMENSION(N,N), INTENT(IN) :: f
        REAL(MK), INTENT(IN) :: d_min,dx2
        INTEGER, INTENT(OUT), OPTIONAL :: k
        REAL(MK), INTENT(OUT), OPTIONAL :: d
        LOGICAL, OPTIONAL :: show_state
        INTEGER, OPTIONAL :: mod_state
        REAL(MK) :: rN2 ! Convergence variabel
        INTEGER :: i, j


        ! Setting optional variabels (Problems with setting default values: "Segmentation fault (core dumped)")
        IF (.NOT.PRESENT(show_state)) show_state = .true.
        IF (.NOT.PRESENT(mod_state)) mod_state = 20

        WRITE(*,*)
        WRITE(*,*) "Starting Jacobi iterations. (k_max=",k_max," N=",N," d_min=",d_min,")"

        rN2 = 1d0/N**2
        DO k = 1,k_max
            d = 0d0
            DO i=2,N-1
                DO j=2,N-1
                    ukp1(i,j) = 25d-2*(uk(i,j-1)+uk(i,j+1)+uk(i-1,j)+uk(i+1,j)+dx2*f(i,j))
                    d = d + (ukp1(i,j)-uk(i,j))**2
                END DO
            END DO
            !d = d*rN2
            ! Build convergence cretia
            IF (d < d_min.and.(k > 10)) THEN
                WRITE(*,*) "The solver converged after: ", k, "Iterations (k_max: ", k_max, ")"
                WRITE(*,"(A,ES8.2E2,A,ES8.2E2,A)") " With d: ", d, " (d_min:",d_min,")"
                exit
            end if

            IF (MOD(k,mod_state)==0.and.show_state) THEN
                WRITE(*,"(A,I4,A,ES8.2E2,A)") " Solution is not converged yet. (k= ",k,", d= ",d,")"
            end if
            uk = ukp1
        end do

        IF (k > k_max) THEN
            WRITE(*,*)
            WRITE(*,*) "!! WARNING: The solver did NOT converge within k_max:", k_max, " !!"
            WRITE(*,"(A,ES8.2E2,A,ES8.2E2,A)") " d: ", d, " (d_min:",d_min,")"
            WRITE(*,*)
        end if

    END SUBROUTINE Jacobi_Solver
end module m_jacobi_solver