#!/bin/bash
#SBATCH -J my_program
#SBATCH -p test
#SBATCH	-t 00:10:00
#SBATCH -N 1
aprun -n 10 ./heat_mpi

