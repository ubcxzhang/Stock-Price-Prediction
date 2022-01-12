#!/bin/bash
#SBATCH -J BV1
#SBATCH --account=rrg-ubcxzh
#SBATCH --cpus-per-task=1
#SBATCH --mem=40G
#SBATCH -t 0-04:59:00

module load StdEnv/2020
module load gcc/9.3.0 r/4.1.0

Rscript --vanilla ./code/ensemble_ELN_daily.R > ./rout/ensemble_ELN_daily.Rout 2>&1


 
