#!/bin/bash
#SBATCH -J BV1
#SBATCH --account=def-ubcxzh
#SBATCH --cpus-per-task=1
#SBATCH --mem=50G
#SBATCH -t 0-03:00:00


module load nixpkgs/16.09 
module load gcc/7.3.0 
module spider r/4.0.2
module load r/4.0.2


Rscript --vanilla ./code/sample.R $1 > ./rout/sample_svm.$1.Rout 2>&1


 
