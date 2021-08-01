#!/bin/bash
#SBATCH -J BV1
#SBATCH --account=def-ubcxzh
#SBATCH --cpus-per-task=1
#SBATCH --mem=40G
#SBATCH -t 0-00:10:00

module load nixpkgs/16.09 
module load gcc/7.3.0 
module spider r/4.0.2
module load r/4.0.2

Rscript --vanilla ./code/figure.R > ./rout/figure.Rout 2>&1


 
