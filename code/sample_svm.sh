#!/bin/bash
#SBATCH -J BV1
#SBATCH --account=def-ubcxzh
#SBATCH --cpus-per-task=1
#SBATCH --mem=40G
#SBATCH -t 0-02:58:00


module load nixpkgs/16.09 gcc/7.3.0 r/3.6.1


Rscript --vanilla ~/projects/def-ubcxzh/y2huang/midprice_predict/thesis/sample.R $1 > ~/projects/def-ubcxzh/y2huang/midprice_predict/thesis/result/sample_svm.$1.Rout 2>&1


 
