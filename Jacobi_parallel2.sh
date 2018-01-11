#!/usr/bin/env bash
n=${4:-7}
N_th=(1 2 4 6 12 16 24)
for ((i=1;i <=$n;i++))
do
    JACOBI_SOLVER N=$1 d_min=1e-20 k_max=$2 filesub=pal1$3 show_state=.false. solver_type=4 N_th=${N_th[$i-1]}
done
