#!/usr/bin/env bash
n=${4:-7}
N_th=(1 2 4 6 12 16 24)
for ((i=1;i <=$n;i++))
do
    OMP_WAIT_POLICY=ACTIVE JACOBI_SOLVER N=$1 d_min=1e-20 k_max=$2 filesub=$5pal2$3 show_state=.false. write_mat=.false. solver_type=4 N_th=${N_th[$i-1]}
done
#OMP_WAIT_POLICY=ACTIVE JACOBI_SOLVER N=2000 d_min=1e-20 k_max=2000 filesub=pal2_workeshare show_state=.false. write_mat=.false. solver_type=4 N_th=24