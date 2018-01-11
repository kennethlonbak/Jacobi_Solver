#!/usr/bin/env bash
JACOBI_SOLVER N=$1 d_min=1e-20 k_max=$2 filesub=pal1$3 show_state=.false. solver_type=3 N_th=1
JACOBI_SOLVER N=$1 d_min=1e-20 k_max=$2 filesub=pal1$3 show_state=.false. solver_type=3 N_th=2
JACOBI_SOLVER N=$1 d_min=1e-20 k_max=$2 filesub=pal1$3 show_state=.false. solver_type=3 N_th=4
JACOBI_SOLVER N=$1 d_min=1e-20 k_max=$2 filesub=pal1$3 show_state=.false. solver_type=3 N_th=6
JACOBI_SOLVER N=$1 d_min=1e-20 k_max=$2 filesub=pal1$3 show_state=.false. solver_type=3 N_th=12
JACOBI_SOLVER N=$1 d_min=1e-20 k_max=$2 filesub=pal1$3 show_state=.false. solver_type=3 N_th=16
JACOBI_SOLVER N=$1 d_min=1e-20 k_max=$2 filesub=pal1$3 show_state=.false. solver_type=3 N_th=24
