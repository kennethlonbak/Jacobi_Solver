program main
    ! Declearing variabels
    USE m_jacobi_solver
    USE declare_var

    ! Initlize variabels
    CALL initilize()

    ! Set boundary condition
    CALL set_bond_harmonic

    ! Set source field
    CALL set_f_harmonic

    ! Solve via Jacobi solver
    CALL Jacobi_Solver(N,k_max,d_min,uk,ukp1,f,dx2,k,d,.true.,20)

    ! Write out soulution to text file
    CALL write_matrix(ukp1,filename)
end program main