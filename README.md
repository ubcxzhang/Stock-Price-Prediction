# Novel Modelling of High-frequency Stock Trading Data
---

## About this Project
Though machine learning-based methods are widely applied in stock mid-price prediction tasks, the feature engineering strategies specialized in high-frequency data are not fully explored. We propose three novel modelling strategies aiming to make better use of high-frequency data and solve the existing data redundancy issues. 

---
## Directory Layout
![image](https://github.com/ubcxzhang/Stock-Price-Prediction/blob/master/illustration.png)

We assume the user set the default directory at **Graham** at Compute Canada
~~~
    /project/6003851/[your_deirctory]  
~~~
all the R codes are in the subdirectory directory at **code** 
~~~
    /project/6003851/[your_deirctory]/code  
~~~
all additional data to produce graphs except for the NYSE stock dataset are in the subdirectory directory at **rda** 
~~~
    /project/6003851/[your_deirctory]/rda  
~~~
all the .sh files that run the R files are in the subdirectory directory at **sh** 
~~~
    /project/6003851/[your_deirctory]/sh  
~~~
all the log files are in the subdirectory directory at **rout** 
~~~
    /project/6003851/[your_deirctory]/rout  
~~~
all the final prediction results/intermedia results are in the subdirectory directory at **result** 
~~~
    /project/6003851/y2huang/midprice_predict/final_version_2/result  
~~~
all the graphs in the paper are in the subdirectory directory at **figure** 
~~~
    /project/6003851/y2huang/midprice_predict/final_version_2/figure  
~~~
all the **raw datasets** from New York Stock Exchange are stored at, which are accessible to all group members
~~~
    /projects/def-ubcxzh/SharedData/NYSE16/GroupingResult/NBBO  
~~~

<details><summary>code</summary>

    ├── code  
    │    ├── data_cleaning.R		    # clean the raw data 
    │ 	 ├── feature_encoding.R 		# feature construction
    │ 	 ├── sample.R 			        # single experiments with SVM model
    │ 	 ├── sample_ELN_full.R			# single experiments with ELN model    
    │ 	 ├── ensemble_svm.R			# ensemble 100 results with SVM model    
    │ 	 ├── ensemble_ELN.R			# ensemble 100 results with ELN model    
    │ 	 ├── figure.R			# Wilcoxon Sign Rank Test and Visualizations    
    │ 	 ├── appendix_table.R		        # Visualizations
    │ 	 └── wiltest.R		        # Tool box with customized R functions					
</details>
<details><summary>rda</summary>

    ├── rda    
    │ 	 ├── date.rda		        # a file that records the trading dates
    │ 	 └── dj30.csv		        # a dataset to reproduce Dow Jones 30 index graph in the paper 					
</details>
<details><summary>sh</summary>

    ├── sh  
    │    ├── data_cleaning.sh		# sh files
    │ 	 ├── feature_encoding.sh					
    │ 	 ├── sample_svm.sh 			
    │ 	 ├── sample_ELN_full.sh 			
    │ 	 ├── ensemble_svm.sh
    │ 	 ├── ensemble_ELN.sh
    │ 	 ├── figure.sh
    │ 	 └── appendix_table.sh				
</details>
<details><summary>rout</summary>

    ├──  log files after submitting jobs
    │    ├── data_cleaning.Rout		    # log file for data_cleaning.sh
    │ 	 ├── feature_encoding.Rout		# log file for feature_encoding.sh
    │ 	 ├── sample_svm.i.Rout 			# log file for sample_svm.sh for each seed i (i=1,...,100)
    │ 	 ├── sample_ELN_full.i.Rout	    # log file for sample_ELN_full.sh for each seed i (i=1,...,100)     
    │ 	 ├── ensemble_svm.Rout			# log file for ensemble_svm.sh 
    │ 	 ├── ensemble_ELN.Rout 		    # log file for ensemble_ELN.sh
    │ 	 ├── figure.Rout                # log file for figure.sh
    │ 	 └── appendix_table.Rout        # log file for appendix_table.sh, generated Latex tables will be stored here
</details>
<details><summary>results (final & intermedia results)</summary>

    ├──  intermedia result
    │    ├── [stock_name]_final.rda		    # after cleaning the raw data for each component stock 
    │ 	 ├── [stock_name]_to_sample.rda		# feature construction for each component stock
    │ 	 ├── [stock_name]_i_model_svm.rda 			# single experiments with SVM model for each component stock (i=1,...,100)
    │ 	 ├── [stock_name]_i_model_full.rda			# single experiments with ELN model for each component stock (i=1,...,100)    
    ├──  final result 
    │ 	 ├── [stock_name]_svm_ensemble_model.rda			# ensemble 100 results with SVM model for each component stock        	
    │ 	 └── [stock_name]_full_ensemble_model.rda 		    # ensemble 100 results with ELN model for each component stock
</details>

<details><summary>figure</summary>

    ├── figure    
    │ 	 ├── combined_plot.pdf
    │ 	 ├── ensemble_ELN_SVM_plot.pdf
    │ 	 ├── barplot.pdf
    │ 	 └── dj30.pdf					
</details>


<details><summary>raw data</summary>
    
    ├── raw data
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





---
## Before you start
1. decide the path of [your_directory] to replicate our results;
2. create the subdirectories **code**, **rda**, **sh**, **rout**, **result**, **figure** at [your_directory]；
3. allocate all relevant files into each subdirectory. The **rout**, **result** and **figure** folders will be empty at the beginning while the **code**, **sh** and **rda** folders should look like the figure below:

![image2](https://github.com/ubcxzhang/Stock-Price-Prediction/blob/master/illustration2.png)

5. in the main directory, use the following commands to load R/4.0.2 language in Compute Canada:
~~~
module load nixpkgs/16.09	
module load gcc/7.3.0	
module spider r/4.0.2	
module load r/4.0.2	
~~~
4. before we run the .sh files, we use in the following commands in R (version 4.0.2) to install some R packages needed for the task
~~~
install.packages(c('dbplyr','data.table','glmnet','fdapace','ggplot2','RColorBrewer','bit64', 'reshape2','graphics', 'e1071', 'caret', 'stringr', 'MTPS', 'Matrix', 'tidyr', 'xtable'))
~~~

---


## Running files (estimated time per job)

- To run the files, submit .sh files with an order of 1. data cleaning.R to 6. appendix_table.R
- Always submit your job under [your_directory] instead of any of the subdirectory
- The first and second jobs (e.g. "1. data cleaning", "2. feature construction") must be submitted in order; jobs "3. experiments with SVM model" and "3a. experiments with ELN model" can be submitted simultaneously; jobs "4. ensemble results with SVM model" and "4a. ensemble results with ELN model" can also be submitted at the same time; "5. figure.R" and "6. appendix_table.R" can be submitted simultaneously.
- For example, ./sh/xx.sh runs ./code/xx.R, saves the results at ./result/xx, produce graphs at ./figure/, generates table result in Latex format at ./rout/appendix_table.Rout and the log files at ./rout/xx


<details><summary>1. data cleaning (8 hrs)</summary>

- read in the raw dataset from `/projects/def-ubcxzh/SharedData/NYSE16/GroupingResult/NBBO/`, load './rda/date.rda' and './code/wiltest.r';

    - select the Dow Jones 30 component stocks of our interest and save each stock as a single R file;

    - select the same set of variables for each stock data;

    - basic stock price cleaning as stated in the paper in section "Data Manipulation";

    - generate FPCA variables for each stock;

- after data cleaning, save each stock price dataset as `./result/[stock_name]_final.rda` file.

 </details>
 
 ~~~
    sbatch ./sh/data_cleaning.sh
~~~


<details><summary>2. feature construction (3hrs)</summary>

- read in read in `./result/[stock_name]_final.rda` file;

    - create all variables listed in "Multi-resolution Features Construction" in our paper except FPCAs;

- save new R file `./result/[stock_name]_to_sample.rda` file.

</details>

~~~
    sbatch ./sh/feature_encoding.sh
~~~


<details><summary> 3. experiments with SVM model (3 hrs, submit 100 jobs)</summary>
**note that this job will be submitted 100 times with random seed i from 1 to 100**

- read in R file `./result/[stock_name]_to_sample.rda`;

    - label the response variable (stock mid-price movement);

    - read in random seed i, subsample sample of 10,000 obs with 8,000 training set and 2,000 testing set;

    - data winsorization and standardization;

    - conduct experiments: baseline model without ensemble/baseline model without FPCA/baseline model without "within-window" features;

    - calculate Recall, Precision and F1 score for each experiment above;

- save file `./result/[stock_name]_i_model_svm.rda`.
    
</details>


<details><summary> 3a. experiments with ELN model (3 hrs, submit 100 jobs)</summary>
**note that this job will be submitted 100 times with random seed from 1 to 100**

- read in R file `./result/[stock_name]_to_sample.rda`;

    - label the response variable (stock mid-price movement);

    - read in random seed i, subsample sample of 10,000 obs with 8,000 training set and 2,000 testing set;

    - data winsorization and standardization;

    - conduct experiments: baseline ELN model without ensemble;

    - calculate Recall, Precision and F1 score with the application of manually defined function "get Accuracy" from "wiltest.r";

- save file `./result/[stock_name]_i_model_full.rda`.

</details>

~~~
    for ii in {1..100}; do sbatch ./sh/sample_svm.sh $ii; done
    for ii in {1..100}; do sbatch ./sh/sample_ELN_full.sh $ii; done
 ~~~  


<details><summary> 4. ensemble results with SVM model (6 hrs, submit 30 jobs)</summary>
    **SVM model ensemble is too slow, divide it into 30 separate jobs representing 30 targeted stocks**

- using loop i equals 1 to 100 and read in data `./result/[stock_name]_i_model_svm.rda`;

    - skip experiments that don't have converged results;

    - use the voting scheme to make final predictions;

    - calculate Recall, Precision and F1 score for each ensemble experiment (e.g. baseline model/baseline model without FPCAs/baseline model without "within-window" vars);

- store all accuracy as R file `./result/[stock_name]_svm_ensemble_model.rda`.
    
</details>


<details><summary> 4a. ensemble results with ELN model (6 hrs)</summary>

- using loop i equals 1 to 100 and read in data `./result/[stock_name]_i_model_full.rda`;

    - skip experiments that don't have converged results;

    - use the voting scheme to make final predictions;

    - calculate Recall, Precision and F1 score for the ensemble experiment (e.g. baseline model with ELN);

- store all accuracy as R file `./result/[stock_name]_full_ensemble_model.rda`.
        
</details>

~~~
    for ii in {1..30}; do sbatch ./sh/ensemble_svm.sh $ii; done
    sbatch ./sh/ensemble_ELN.sh
~~~ 

customized R functions are defined in `wiltest.R` file; `figure.R` and `appendix_table.R` produce visualizations and table results, which can be run on local server

<details><summary> 5. generating figures in the paper (10 mins)</summary>
- read in data `./rda/dow_jones30_daily.csv`;

    - illstrates the daily price change of Dow Jones 30 index;

- store figure 1 `./figure/dj30.pdf`.

- using loop i equals 1 to 30 and read in data `./result/[char_name]_svm_ensemble_model.rda`;

    - produces boxplots using ggplot;

    - shows comparisons between baseline model v.s. ensemble model, baseline model v.s. no FPCA model, and baseline model v.s. no within-window model;

- store figure 2 `./figure/combined_plot.pdf`.

- using loop j equals 1 to 30 and read in data `./result/[char_name]_j_model_full.rda`;

    - produces boxplots using ggplot;

    - shows comparisons between ensemble ELN model v.s. ELN model, ELN model v.s. SVM model, and ensemble ELN model v.s. SVM model;

- store figure 3 `./figure/ensemble_ELN_SVM_plot.pdf`.
    
- using loop k equals 1 to 30 and read in data `./result/[char_name]_k_model_full.rda`;

    - produces barplots using ggplot;

    - shows histogram of selected variables by ELN model in all three mid-price direction;

- store figure 4 `./figure/barplot.pdf`.
    
        
</details>

~~~
    sbatch ./sh/figure.sh
~~~ 
<details><summary> 6. produces tables in the paper (30 mins)</summary>
**All the table results output are in latex format, they are printed in the log file at './rout/appendix_table.Rout'**

- produces table1 showing the median values of Recall, Precision and F1 score of the baseline model over all 100 experiments;

- produces table2 showing the median values of Recall, Precision and F1 score of the ensemble with SVM, nofpca, and no within-win models over all 100 experiments respectively;

- produces tables showing the summary statistics of the features of the full sample, read in data `./rda/mkt_cap.csv`;

- save file `./result/intermedia_table.rda`
        
</details>

~~~
    sbatch ./sh/appendix_table.sh
~~~ 



