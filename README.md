# Novel Modelling of High-frequency Stock Trading Data
---

## About this project
Though machine learning-based methods are widely applied in stock mid-price prediction tasks, the feature engineering strategies specialized in high-frequency data are not fully explored. We propose three novel modelling strategies aiming to make better use of high-frequency data and solve the existing data redundancy issues. 

## Directory Layout
- We assume all the files are in the default directory at **Graham** at 
~~~
    . /projects/def-ubcxzh/y2huang/midprice_predict/final_version_2  
~~~

<details><summary>code</summary>

    ├── code  
    │        ├── 01_derived_slopes.R		# process phenotype files and derive FEV1 decline slope estimates
    │ 	 ├── 02_gwas.R 				# applies gwas analysis
    │ 	 ├── 03_merge_gwas.R 			# merges gwas results from 02_gwas.R
    │ 	 ├── 04_manhattan.R 			# draws a manhattan plot    
    │ 	 ├── 05_gwas_topN.R 			# get top N smallest p-value SNPs    
    │ 	 ├── 06_geno_topN.R 			# get geno for topN SNPs    
    │ 	 ├── 07_geno_info.R			# get geno info(MAF, REF, ALT, LOC, ...) for topN SNPs    
    │ 	 ├── 08_genoround.R			# round(hardcall) genotype dosage 			
    │ 	 ├── 09_geno_LD.R			# LD screening LD for top 10,000 SNPs 			
    │ 	 ├── 10_multisnp_dat.R			# prepare data for multisnp model(100 repeats) 			
    │ 	 ├── 11_multisnp.R			# Fit the 5 fold multisnp model for each of 100 repeats
    │ 	 ├── 12_multisnp_summary.R		# visualize #calls of multisnp model 			
    │ 	 ├── 13_cv_prediction.R			# calculate prediction mse for multisnp model 			
    │	 ├── 14_prediction_result.R		# merge results from 13_cv_prediction.R and visualize results	
    │	 ├── 15_ensemble_model.R		# build ensemble models	
    │	 ├── 16_eclipse.R			# calculate ECLIPSE scores and exacerbation rate
    │	 ├── 17_eclipse_derive_slopes.R		# derived outcomes for ECLIPSE
    │	 ├── 18_pair_visualization.R		# visualize correlation results between scores and outcomes
    │	 ├── 19_best_model.R			# visualization for manuscript
    │	 ├── 99_utils.R				# utility functions
    │        ├
    │        ├── 01_derived_slopes.sh		# sh files
    │ 	 ├── 02_gwas.sh					
    │ 	 ├── 03_merge_gwas.sh 			
    │ 	 ├── 04_manhattan.sh 			
    │ 	 ├── 05_gwas_topN.sh 		
    │ 	 ├── 06_geno_topN.sh 			
    │ 	 ├── 07_geno_info.sh			
    │ 	 ├── 08_genoround.sh			
    │ 	 ├── 09_geno_LD.sh			
    │ 	 ├── 10_multisnp_dat.sh		
    │ 	 ├── 11_multisnp.sh			
    │ 	 ├── 12_multisnp_summary.sh		
    │ 	 ├── 13_cv_prediction.sh			
    │	 ├── 14_prediction_result.sh		
    │	 ├── 15_ensemble_model.sh	
    │	 ├── 16_eclipse.sh			
    │	 ├── 17_eclipse_derive_slopes.sh		
    │	 ├── 18_pair_visualization.sh	
    │	 └── 19_best_model.sh	
</details>


# 1. data cleaning

read in the dataset from `.txt file`;

select the Dow Jones 30 component stocks of our interest and save each stock as a single R file;

select the same set of variables for each stock data;

basic stock price cleaning as stated in the paper in section "Data Manipulation";

generate FPCA variables for each stock;

after data cleaning, save each stock price dataset as `[stock_name]_final.rda` file.


# 2. feature construction

read in read in `[stock_name]_final.rda` file;

create all variables listed in "Multi-resolution Features Construction" in our paper except FPCAs;

save new R file `[stock_name]_to_sample.rda` file.


# 3. experiments with SVM model
*note that this job will be submitted 100 times with random seed from 1 to 100*

read in R file `[stock_name]_to_sample.rda`;

label the response variable (stock mid-price movement);

read in random seed i, subsample sample of 10,000 obs with 8,000 training set and 2,000 testing set;

data winsorization and standardization;

conduct experiments: baseline model without ensemble/baseline model without FPCA/baseline model without "within-window" features;

calculate Recall, Precision and F1 score for each experiment above;

save file `[stock_name]_i_model_svm.rda`.


# 3a. experiments with ELN model
*note that this job will be submitted 100 times with random seed from 1 to 100*

read in R file `[stock_name]_to_sample.rda`;

label the response variable (stock mid-price movement);

read in random seed i, subsample sample of 10,000 obs with 8,000 training set and 2,000 testing set;

data winsorization and standardization;

conduct experiments: baseline ELN model without ensemble;

calculate Recall, Precision and F1 score with the application of manually defined function "get Accuracy" from "wiltest.r";

save file `[stock_name]_i_model_full.rda`.


# 4. ensemble results with SVM model

using loop i equals 1 to 100 and read in data `[stock_name]_i_model_svm.rda`;

skip experiments that don't have converged results;

use the voting scheme to make final predictions;

calculate Recall, Precision and F1 score for each ensemble experiment (e.g. baseline model/baseline model without FPCAs/baseline model without "within-window" vars);

store all accuracy as R file `[stock_name]_svm_ensemble_model.rda`.

 
# 4a. ensemble results with ELN model

using loop i equals 1 to 100 and read in data `[stock_name]_i_model_full.rda`;

skip experiments that don't have converged results;

use the voting scheme to make final predictions;

calculate Recall, Precision and F1 score for the ensemble experiment (e.g. baseline model with ELN);

store all accuracy as R file `[stock_name]_full_ensemble_model.rda`.


customized R functions are defined in `wiltest.R` file, `assessment.R` and `appendix.R` produce visualizations and test results




