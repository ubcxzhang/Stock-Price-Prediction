#!/bin/bash
#SBATCH -J BV1
#SBATCH --account=def-ubcxzh
#SBATCH --cpus-per-task=1
#SBATCH --mem=40G
#SBATCH -t 0-20:00:00

module load nixpkgs/16.09 gcc/7.3.0 r/3.6.1

Rscript --vanilla ~/projects/def-ubcxzh/y2huang/midprice_predict/final_version_2/data_cleaning.R > ~/projects/def-ubcxzh/y2huang/midprice_predict/final_version_2/result/data_cleaning.Rout 2>&1


 
