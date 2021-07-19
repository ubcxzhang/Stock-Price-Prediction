#!/bin/bash
#SBATCH -J BV1
#SBATCH --account=rrg-ubcxzh
#SBATCH --cpus-per-task=1
#SBATCH --mem=50G
#SBATCH -t 0-2:59:00


module load nixpkgs/16.09 gcc/7.3.0 r/3.6.1


Rscript --vanilla ~/projects/def-ubcxzh/y2huang/midprice_predict/final_version_2/sample_ELN_full.R $1 > ~/projects/def-ubcxzh/y2huang/midprice_predict/final_version_2/result_ELN_nocut/sample_ELN_full.$1.Rout 2>&1


 
