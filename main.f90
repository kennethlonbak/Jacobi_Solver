program main
    ! Declearing variabels
    USE m_jacobi_solver
    USE m_gs_solver
    USE declare_var

    ! Initlize variabels
    CALL initilize()

    ! Set boundary condition
    IF (problem == 2) THEN
        CALL set_bond_harmonic
    ELSE
        CALL set_bond_problem
    end if


    ! Set source field
    IF (problem == 2) THEN
        CALL set_f_harmonic
    ELSE
        CALL set_f_problem
    end if

    ! Solve via Jacobi solver
    IF (solver_type == 2) THEN
        CALL GS_Solver(N,k_max,d_min,uk,ukp1,fdx2,wall_time,k,d,show_state,mod_state)
    ELSE IF (solver_type == 3) THEN
        CALL Jacobi_Solver_pal1(N,k_max,d_min,uk,ukp1,fdx2,wall_time,k,d,show_state,mod_state)
    ELSE IF (solver_type == 3) THEN
        CALL Jacobi_Solver_pal2(N,k_max,d_min,uk,ukp1,fdx2,wall_time,k,d,show_state,mod_state)
    ELSE
        CALL Jacobi_Solver(N,k_max,d_min,uk,ukp1,fdx2,wall_time,k,d,show_state,mod_state)
    end if

    ! Write out soulution to text file
    CALL write_matrix(ukp1,filename)
end program main