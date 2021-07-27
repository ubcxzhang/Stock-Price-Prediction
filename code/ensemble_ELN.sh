#!/bin/bash
#SBATCH -J BV1
#SBATCH --account=def-ubcxzh
#SBATCH --cpus-per-task=1
#SBATCH --mem=30G
#SBATCH -t 0-05:59:00

module load nixpkgs/16.09 gcc/7.3.0 r/3.6.1

Rscript --vanilla ~/projects/def-ubcxzh/y2huang/midprice_predict/thesis/ensemble_ELN.R > ~/projects/def-ubcxzh/y2huang/midprice_predict/thesis/result/ensemble_ELN.Rout 2>&1


 
