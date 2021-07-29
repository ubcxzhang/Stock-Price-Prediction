#!/bin/bash
#SBATCH -J BV1
#SBATCH --account=def-ubcxzh
#SBATCH --cpus-per-task=1
#SBATCH --mem=40G
#SBATCH -t 0-3:00:00

module load nixpkgs/16.09
module load gcc/7.3.0
module spider r/3.6.1
module load r/3.6.1

Rscript --vanilla ./code/feature_encoding.R > ./rout/feature_encoding.Rout 2>&1


 
