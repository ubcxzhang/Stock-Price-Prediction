#getwd()
rm(list=ls())
# server <- TRUE 
# path0 <- ifelse(server, "/project/6003851/y2huang/midprice_predict/final_version_2", "/Users/ying/Desktop/UVic/Year1_summer2020/mid_price_prediction/code/local_test")
# setwd(path0)
source('./code/wiltest.r')
# setwd(file.path(path0,'result'))
library(Matrix)
library(tidyr)
library(stringr)
library(ggplot2)
library(xtable)


options(digits = 6)
char_name <- c('AAPL','MSFT','MMM','AXP','BA','CAT','CVX','CSCO','KO','DOW','XOM',
               'WBA','GS','HD','INTC','IBM','JNJ','JPM','MCD','MRK','NKE','PFE','PG',
               'TRV','UNH','UTX','VZ','V','WMT','DIS')

table.baseline <- matrix(NA,ncol=3,nrow=30)
table.ensem <- matrix(NA,ncol=3,nrow=30)
table.nofpca <- matrix(NA,ncol=3,nrow=30)
table.nowin <- matrix(NA,ncol=3,nrow=30)


for(ii in 1:length(char_name)){
  warnings('off')
  # check if the stock file exists
  char <- char_name[ii]
  print(char)
  test <- try(load(paste0('./result/',char,'_svm_ensemble_model.rda')),silent=T)
  if(class(test)%in%'try-error') next

  
  # benchmark of the 100 SVM model
  benchmark <- overall(Accuracy_sum,Accuracy_sum)$x
  table.baseline[ii,] <-(round(c(median(benchmark[,1],na.rm = T),median(benchmark[,2],na.rm = T),median(benchmark[,3],na.rm = T)),digits = 4))
  
  
  # benchmark of the 100 SVM-ensemble model
  benchmark_ensem <- overall(Accuracy_sum_ensem,Accuracy_sum_ensem)$x
  table.ensem[ii,] <-(round(c(median(benchmark_ensem[,1],na.rm = T),median(benchmark_ensem[,2],na.rm = T),median(benchmark_ensem[,3],na.rm = T)),digits = 4))
  
  
  # benchmark of the 100 SVM-ensemble model
  benchmark_nofpca <- overall(Accuracy_sum_nofpca,Accuracy_sum_nofpca)$x
  table.nofpca[ii,] <-(round(c(median(benchmark_nofpca[,1],na.rm = T),median(benchmark_nofpca[,2],na.rm = T),median(benchmark_nofpca[,3],na.rm = T)),digits = 4))
  
  # benchmark of the 100 SVM-ensemble model
  benchmark_nowin <- overall(Accuracy_sum_nowin,Accuracy_sum_nowin)$x
  table.nowin[ii,] <-(round(c(median(benchmark_nowin[,1],na.rm = T),median(benchmark_nowin[,2],na.rm = T),median(benchmark_nowin[,3],na.rm = T)),digits = 4))
  
}

# The output aims for basic latex format, for prettier table, you need to adjust the code in latex
# this is for the table in the body, showing the result of baseline model
rownames(table.baseline) <- char_name
colnames(table.baseline) <- c('P','R','F1')
xtable(table.baseline,digits=3)

# this is for the table in the appendix
table.result <- cbind(table.ensem,table.nofpca,table.nowin)
rownames(table.result) <- char_name
colnames(table.result) <- rep(c('P','R','F1'),3)
xtable(table.result,digits=3)


###################feature table in the appendix#######################
table.feature <- array(NA,c(5,22,30))


for(k in 1:length(char_name)){
    char <- char_name[k]
    print(k)
load(paste0('./result/',char,'_to_sample.rda'))
for(tt in 1:length(features)){
    if(sum(is.infinite(features[[tt]]))>0) features[[tt]][which(is.infinite(features[[tt]]))] <- NA
    if(sum(is.na(features[[tt]]))>0) features[[tt]][which(is.na(features[[tt]]))] <- NA
    if(sum(is.nan(features[[tt]]))>0) features[[tt]][which(is.nan(features[[tt]]))] <- NA
    if(sum(is.null(features[[tt]]))>0) features[[tt]][which(is.null(features[[tt]]))] <- NA
    
    table.feature[1,,k] <- sapply(features,function(x) mean(x, na.rm=T))
    table.feature[2,,k] <- sapply(features,function(x) median(x, na.rm=T))
    table.feature[3,,k] <- sapply(features,function(x) sd(x, na.rm=T))                                
    table.feature[4,,k] <- sapply(features,function(x) min(x, na.rm=T))
    table.feature[5,,k] <- sapply(features,function(x) max(x, na.rm=T))                                
  }
       
}

# colnames(table.feature) <- names_mfeatures
colnames(table.feature) <- c( 'Best Bid Price (USD)', 'Best Ask Price (USD)', 'Best Bid Size (Share)', 'Best Ask Size (Share)',
'Mid-price (USD)', 'Bid-ask Spread Return (%)', 'Best Bid Price Difference Return (%)', 'Best Ask Price Difference Return (%)', 'Bid-ask Spread Crossing Return (%)', 'Mean Bid Price (USD)', 'Mean Ask Price (USD)', 'Mean Mid-price (USD)', 'Ask Price Depth (USD)', 'Bid Price Depth (USD)', 'Within-Window Standard Deviation', 'Window Slope', 'Arrival Rate', 'Bid Price Derivative', 'Ask Price Derivative', 'Bid Volume Derivative', 'Ask Volume Derivative', 'Mid-price Derivative')
rownames(table.feature) <- c('Mean','Median','Std Dev','Min','Max')
dimnames(table.feature)[[3]] <- char_name
                                  
# save(table.feature, file='./result/intermedia_table.rda')
       
for(i in 1:length(char_name)){
char <- char_name[i]
print(xtable(t(table.feature[,,i]),caption=paste0('Summary features of stock ', char)),caption.placement = "top")
}
