# stock

Stock project
The following description is based on the files in folder "code"

# 1. data cleaning
read in the dataset from .txt file
select the Dow Jones 30 component stocks of our interest and save each stock as a single R file;
select the same set of variables for each stock data;
basic stock price cleaning as stated in the paper in section "Data Manipulation";
generate FPCA variables for each stock;
after data cleaning, save each stock price dataset as "[stock_name]_final.rda" file.

# 2. feature construction
read in read in "[stock_name]_final.rda" file;
create all variables listed in "Multi-resolution Features Construction" in our paper except FPCAs;
save new R file "[stock_name]_to_sample.rda" file.

# 3. experiments with SVM model
*note that this job will be submitted 100 times with random seed from 1 to 100*
read in R file "[stock_name]_to_sample.rda";
label the response variable (stock mid-price movement);
read in random seed i, subsample sample of 10,000 obs with 8,000 training set and 2,000 testing set;
data winsorization and standardization;
conduct experiments: baseline model without ensemble/baseline model without FPCA/baseline model without "within-window" features;
calculate Recall, Precision and F1 score for each experiment above;
save file "[stock_name]_i_model_svm.rda".

# 3a. experiments with ELN model
*note that this job will be submitted 100 times with random seed from 1 to 100*
read in R file "[stock_name]_to_sample.rda";
label the response variable (stock mid-price movement);
read in random seed i, subsample sample of 10,000 obs with 8,000 training set and 2,000 testing set;
data winsorization and standardization;
conduct experiments: baseline ELN model without ensemble;
calculate Recall, Precision and F1 score with the application of manually defined function "get Accuracy" from "wiltest.r";
save file "[stock_name]_i_model_full.rda".

# 4. ensemble results with SVM model
using loop i equals 1 to 100 and read in data "[stock_name]_i_model_svm.rda";
skip experiments that don't have converged results;
use the voting scheme to make final predictions;
calculate Recall, Precision and F1 score for each ensemble experiment (e.g. baseline model/baseline model without FPCAs/baseline model without "within-window" vars);
store all accuracy as R file "[stock_name]_svm_ensemble_model.rda".
 
# 4a. ensemble results with ELN model
using loop i equals 1 to 100 and read in data "[stock_name]_i_model_full.rda";
skip experiments that don't have converged results;
use the voting scheme to make final predictions;
calculate Recall, Precision and F1 score for the ensemble experiment (e.g. baseline model with ELN);
store all accuracy as R file "[stock_name]_full_ensemble_model.rda".


# customized R functions are defined in 'wiltest.R' file, 'assessment.R' and 'appendix.R' produces visualization and test results




