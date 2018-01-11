program main
    ! Declearing variabels
    USE m_jacobi_solver
    USE m_gs_solver
    USE declare_var

    ! Initlize variabels
    CALL initilize()

    ! Set boundary condition
    CALL set_bond_harmonic

    ! Set source field
    CALL set_f_harmonic

    ! Solve via Jacobi solver
    IF (solver_type == 2) THEN
        CALL GS_Solver(N,k_max,d_min,uk,ukp1,fdx2,wall_time,k,d,.true.,20)
    ELSE
        CALL Jacobi_Solver(N,k_max,d_min,uk,ukp1,fdx2,wall_time,k,d,.true.,20)
    end if

    ! Write out soulution to text file
    CALL write_matrix(ukp1,filename)
end program main