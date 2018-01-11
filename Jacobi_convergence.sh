#!/usr/bin/env bash
JACOBI_SOLVER N=20 d_min=1e-20 k_max=10000 filesub=jaccon
JACOBI_SOLVER N=40 d_min=1e-20 k_max=10000 filesub=jaccon
JACOBI_SOLVER N=80 d_min=1e-20 k_max=10000 filesub=jaccon
JACOBI_SOLVER N=160 d_min=1e-20 k_max=10000 filesub=jaccon
JACOBI_SOLVER N=320 d_min=1e-20 k_max=50000 filesub=jaccon mod_state=100