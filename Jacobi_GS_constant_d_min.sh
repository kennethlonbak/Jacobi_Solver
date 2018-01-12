#!/usr/bin/env bash
n=${1:-5}
N_th=(20 40 80 160 320)
d_m=(1e-14 1e-10 1e-8 1e-5 1e-4)
for ((i=1;i <=$n;i++))
do
    JACOBI_SOLVER problem=1 N=${N_th[$i-1]} d_min=${d_m[$i-1]} k_max=$2 filesub=jac_d show_state=.false. write_mat=.false. solver_type=1
    JACOBI_SOLVER problem=1 N=${N_th[$i-1]} d_min=${d_m[$i-1]} k_max=$2 filesub=gs_d show_state=.false. write_mat=.false. solver_type=2
done

