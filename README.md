# Novel Modelling of High-frequency Stock Trading Data
---

## About this Project
Though machine learning-based methods are widely applied in stock mid-price prediction tasks, the feature engineering strategies specialized in high-frequency data are not fully explored. We propose three novel modelling strategies aiming to make better use of high-frequency data and solve the existing data redundancy issues. 

---

## Directory Layout
- We assume all the codes and intermedia results are in the default directory at **Graham** at 
~~~
    . /projects/def-ubcxzh/y2huang/midprice_predict/final_version_2  
~~~
all the raw datasets from New York Stock Exchange are stored at
~~~
    . /projects/def-ubcxzh/SharedData/NYSE16/GroupingResult/NBBO  
~~~

<details><summary>code</summary>

    ├── code  
    │    ├── data_cleaning.R		    # clean the raw data 
    │ 	 ├── feature_encoding.R 		# feature construction
    │ 	 ├── sample.R 			        # single experiments with SVM model
    │ 	 ├── sample_ELN_full.R			# single experiments with ELN model    
    │ 	 ├── ensemble_svm.R			# ensemble 100 results with SVM model    
    │ 	 ├── ensemble_ELN.R			# ensemble 100 results with ELN model    
    │ 	 ├── asssessment.R			# Wilcoxon Sign Rank Test and Visualizations    
    │ 	 ├── appendix.R		        # Visualizations
    │ 	 ├── wiltest.R		        # Tool box with customized R functions
    │    ├── data_cleaning.sh		# sh files
    │ 	 ├── feature_encoding.sh					
    │ 	 ├── sample_svm.sh 			
    │ 	 ├── sample_ELN_full.sh 			
    │ 	 ├── ensemble_svm.sh		
    │ 	 └── ensemble_ELN.sh 				
</details>

<details><summary>data</summary>
    
    ├── data
    │        ├── EQY_US_ALL_NBBO_AAPL.txt
    │        ├── EQY_US_ALL_NBBO_MSFT.txt		
    │        ├── EQY_US_ALL_NBBO_MMM.txt		
    │        ├── EQY_US_ALL_NBBO_AXP.txt 	
    │        ├── EQY_US_ALL_NBBO_BA.txt
    │        ├── EQY_US_ALL_NBBO_CAT.txt		
    │        ├── EQY_US_ALL_NBBO_CVX.txt		
    │        ├── EQY_US_ALL_NBBO_CSCO.txt 	
    │        ├── EQY_US_ALL_NBBO_KO.txt
    │        ├── EQY_US_ALL_NBBO_DOW.txt		
    │        ├── EQY_US_ALL_NBBO_XOM.txt		
    │        ├── EQY_US_ALL_NBBO_WBA.txt 	
    │        ├── EQY_US_ALL_NBBO_GS.txt
    │        ├── EQY_US_ALL_NBBO_HD.txt		
    │        ├── EQY_US_ALL_NBBO_INTC.txt		
    │        ├── EQY_US_ALL_NBBO_IBM.txt 	
    │        ├── EQY_US_ALL_NBBO_JNJ.txt
    │        ├── EQY_US_ALL_NBBO_JPM.txt		
    │        ├── EQY_US_ALL_NBBO_MCD.txt		
    │        ├── EQY_US_ALL_NBBO_MRK.txt 	
    │        ├── EQY_US_ALL_NBBO_NKE.txt
    │        ├── EQY_US_ALL_NBBO_PFE.txt		
    │        ├── EQY_US_ALL_NBBO_PG.txt		
    │        ├── EQY_US_ALL_NBBO_TRV.txt 	
    │        ├── EQY_US_ALL_NBBO_UNH.txt
    │        ├── EQY_US_ALL_NBBO_UTX.txt		
    │        ├── EQY_US_ALL_NBBO_VZ.txt		
    │        ├── EQY_US_ALL_NBBO_V.txt 	
    │        ├── EQY_US_ALL_NBBO_WMT.txt 
    │	     └── EQY_US_ALL_NBBO_DIS.txt 
</details>

<details><summary>results & intermedia results</summary>

    ├──  intermedia result
    │    ├── [stock_name]_final.rda		    # after cleaning the raw data for each component stock 
    │ 	 ├── [stock_name]_to_sample.rda		# feature construction for each component stock
    │ 	 ├── [stock_name]_i_model_svm.rda 			# single experiments with SVM model for each component stock (i=1,...,100)
    │ 	 ├── [stock_name]_i_model_full.rda			# single experiments with ELN model for each component stock (i=1,...,100)    
    ├──  result 
    │ 	 ├── [stock_name]_svm_ensemble_model.rda			# ensemble 100 results with SVM model for each component stock        	
    │ 	 └── [stock_name]_full_ensemble_model.rda 		    # ensemble 100 results with ELN model for each component stock
</details>

---

## Package Dependencies
- Before running the .sh files, some packages should be installed.
- `wiltest.R` should be loaded to introduce the self-defined functions
~~~
install.packages(c('dbplyr','data.table','glmnet','fdapace','ggplot2','RColorBrewer','bit64', 'reshape2','graphics'))
~~~

---

## Running files (estimated times per 1 job)

- To run the files, submit .sh files with an order of 1. data cleaning.R to 4a. ensemble results with ELN model.R
- For example, xx.sh runs xx.R and saves the results at result/xx.
- Before submitting the .sh files, run the following codes in the terminal.
- Note that the .R files should be writable (can set this up by chmod +x xx.R)
~~~
cd /projects/def-ubcxzh/y2huang/midprice_predict/final_version_2 
module load nixpkgs/16.09	
module load gcc/7.3.0	
module spider r/3.6.1	
module load gcccore/.5.4.0	
module load r/3.6.1		
chmod +x xx.R # replace xx with the proper filename
~~~


<details><summary>1. data cleaning (20 hrs)</summary>

- read in the dataset from `.txt file`;

- select the Dow Jones 30 component stocks of our interest and save each stock as a single R file;

- select the same set of variables for each stock data;

- basic stock price cleaning as stated in the paper in section "Data Manipulation";

- generate FPCA variables for each stock;

- after data cleaning, save each stock price dataset as `[stock_name]_final.rda` file.

~~~
    sbatch data_cleaning.sh
~~~
 </details>


<details><summary>2. feature construction (3hrs)</summary>

- read in read in `[stock_name]_final.rda` file;

- create all variables listed in "Multi-resolution Features Construction" in our paper except FPCAs;

- save new R file `[stock_name]_to_sample.rda` file.
    
~~~
    sbatch feature_encoding.sh
~~~
</details>

<details><summary> 3. experiments with SVM model (6 hrs, submit 100 jobs)</summary>
*note that this job will be submitted 100 times with random seed from 1 to 100*

- read in R file `[stock_name]_to_sample.rda`;

- label the response variable (stock mid-price movement);

- read in random seed i, subsample sample of 10,000 obs with 8,000 training set and 2,000 testing set;

- data winsorization and standardization;

- conduct experiments: baseline model without ensemble/baseline model without FPCA/baseline model without "within-window" features;

- calculate Recall, Precision and F1 score for each experiment above;

- save file `[stock_name]_i_model_svm.rda`.

 ~~~
    sbatch sample_svm.sh
 ~~~
</details>


<details><summary> 3a. experiments with ELN model (3 hrs, submit 100 jobs)</summary>
*note that this job will be submitted 100 times with random seed from 1 to 100*

- read in R file `[stock_name]_to_sample.rda`;

- label the response variable (stock mid-price movement);

- read in random seed i, subsample sample of 10,000 obs with 8,000 training set and 2,000 testing set;

- data winsorization and standardization;

- conduct experiments: baseline ELN model without ensemble;

- calculate Recall, Precision and F1 score with the application of manually defined function "get Accuracy" from "wiltest.r";

- save file `[stock_name]_i_model_full.rda`.

 ~~~
    sbatch sample_ELN_full.sh
 ~~~  
</details>



<details><summary> 4. ensemble results with SVM model (6 hrs)</summary>

- using loop i equals 1 to 100 and read in data `[stock_name]_i_model_svm.rda`;

- skip experiments that don't have converged results;

- use the voting scheme to make final predictions;

- calculate Recall, Precision and F1 score for each ensemble experiment (e.g. baseline model/baseline model without FPCAs/baseline model without "within-window" vars);

- store all accuracy as R file `[stock_name]_svm_ensemble_model.rda`.
    
~~~
    sbatch ensemble_svm.sh
~~~ 
</details>


<details><summary> 4a. ensemble results with ELN model (3 hrs)</summary>

- using loop i equals 1 to 100 and read in data `[stock_name]_i_model_full.rda`;

- skip experiments that don't have converged results;

- use the voting scheme to make final predictions;

- calculate Recall, Precision and F1 score for the ensemble experiment (e.g. baseline model with ELN);

- store all accuracy as R file `[stock_name]_full_ensemble_model.rda`.
        
~~~
    sbatch ensemble_ELN.sh
~~~ 
</details>


customized R functions are defined in `wiltest.R` file, `assessment.R` and `appendix.R` produce visualizations and test results, which can be run on local server




