#!/usr/bin/env bash
JACOBI_SOLVER N=400 d_min=1e-20 k_max=10000 filesub=pal1 solver_type=3 N_th=1
JACOBI_SOLVER N=400 d_min=1e-20 k_max=10000 filesub=pal1 solver_type=3 N_th=2
JACOBI_SOLVER N=400 d_min=1e-20 k_max=10000 filesub=pal1 solver_type=3 N_th=4
JACOBI_SOLVER N=400 d_min=1e-20 k_max=10000 filesub=pal1 solver_type=3 N_th=8
JACOBI_SOLVER N=400 d_min=1e-20 k_max=10000 filesub=pal1 solver_type=3 N_th=16
JACOBI_SOLVER N=400 d_min=1e-20 k_max=10000 filesub=pal1 solver_type=3 N_th=32
