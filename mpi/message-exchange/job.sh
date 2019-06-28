#!/bin/bash
#SBATCH -J my_program
#SBATCH -p test
#SBATCH	-t 00:01:00
#SBATCH -n 2
aprun -n 2 ./exchange

