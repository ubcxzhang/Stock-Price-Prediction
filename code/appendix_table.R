rm(list=ls())

source('./code/wiltest.r')
library(dbplyr)
library(data.table)
library(glmnet)
library(fdapace)
library(bit64)
library(reshape2)
library(Matrix)
library(tidyr)
library(stringr)
library(ggplot2)
library(xtable)


options(digits = 6)
char_name <- c('AAPL','MSFT','MMM','AXP','BA','CAT','CVX','CSCO','KO','DOW','XOM',
               'WBA','GS','HD','INTC','IBM','JNJ','JPM','MCD','MRK','NKE','PFE','PG',
               'TRV','UNH','UTX','VZ','V','WMT','DIS')

##################################################################################
# Table of Running Time
##################################################################################
table.time.svm <- matrix(NA,ncol=30,nrow=100)
table.time.eln <- matrix(NA,ncol=30,nrow=100)

# single ELN model fitting time
for (ii in 1:length(char_name)){
  char <- char_name[ii]
    print(ii)
for(i in 1:100){
test <- try(load(paste0('./result/', char,'_',i,'_eln_daily.rda')),silent=T)
  if(class(test)%in%'try-error') next
table.time.eln[i,ii] <- time_ELN_fit
}  
}
# change minute to seconds
table.time.eln[which(table.time.eln<10)] <- table.time.eln[which(table.time.eln<10)]*60
table.time.eln <- colMeans(table.time.eln, na.rm=T)

# single SVM model fitting time
for (ii in 1:length(char_name)){
  char <- char_name[ii]
    print(ii)
for(i in 1:100){
test <- try(load(paste0('./result/', char,'_',i,'_model_svm_daily.rda')),silent=T)
  if(class(test)%in%'try-error') next
table.time.svm[i,ii] <- time_SVM_fit
}  
}

# change minute to seconds
table.time.svm[which(table.time.svm<10)] <- table.time.svm[which(table.time.svm<10)]*60
table.time.svm <- colMeans(table.time.svm, na.rm=T)


# SVM/ELN model ensemble prediction time
table.time.ensem <- matrix(NA,ncol=2,nrow=30)
for (ii in 1:length(char_name)){
  char <- char_name[ii]
  print(char)
  # only use F1 score to draw the pictures
  load(paste0('./result/', char,'_svm_ensemble_model_daily.rda'))
  table.time.ensem[ii,1] <- time_SVM
    
  load(paste0('./result/',char,'_eln_ensemble_model_daily.rda'))
  table.time.ensem[ii,2] <- time_ELN
}

# converting SVM hour to sec and ELN min to sec
table.time.ensem[,1] <- 3600*table.time.ensem[,1]
table.time.ensem[,2] <- 60*table.time.ensem[,2]

time <- cbind(table.time.svm, table.time.eln,table.time.ensem)
colnames(time) <- c("SVM", "ELN", "Ensemble SVM", "Ensemble ELN")
rownames(time) <- char_name
xtable(time, digits=3)

##################################################################################
# Table of SVM model performance (baseline model & all-inclusive)
##################################################################################
table.baseline <- matrix(NA,ncol=3,nrow=30)
table.ensem <- matrix(NA,ncol=3,nrow=30)
table.nofpca <- matrix(NA,ncol=3,nrow=30)
table.nowin <- matrix(NA,ncol=3,nrow=30)


for(ii in 1:length(char_name)){
  warnings('off')
  # check if the stock file exists
  char <- char_name[ii]
  print(char)
  test <- try(load(paste0('./result/',char,'_svm_ensemble_model_daily.rda')),silent=T)
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
# this is for the table in the body, showing the result of baseline SVM model
rownames(table.baseline) <- char_name
colnames(table.baseline) <- c('P','R','F1')
xtable(table.baseline,digits=3)

# this is for the table in the appendix
table.result <- cbind(table.baseline, table.ensem,table.nofpca,table.nowin)
rownames(table.result) <- char_name
colnames(table.result) <- rep(c('P','R','F1'), 4)
xtable(table.result,digits=3)

##################################################################################
# Table of ELN model performance (baseline model & all-inclusive)
##################################################################################
table.baseline <- matrix(NA,ncol=3,nrow=30)
table.ensem <- matrix(NA,ncol=3,nrow=30)
table.nofpca <- matrix(NA,ncol=3,nrow=30)
table.nowin <- matrix(NA,ncol=3,nrow=30)


for(ii in 1:length(char_name)){
  warnings('off')
  # check if the stock file exists
  char <- char_name[ii]
  print(char)
    test <- try(load(paste0('./result/',char,'_eln_ensemble_model_daily.rda')),silent=T)
    load(paste0('./result/',char,'_eln_summary_daily.rda'))
    Accuracy_sum_nofpca <-  Accuracy_sum_nofpca[flag_nofpca]
    Accuracy_sum_nowin <-  Accuracy_sum_nowin[flag_nowin]
  if(class(test)%in%'try-error') next

  
  # benchmark of the 100 ELN model
  benchmark <- overall(Accuracy_sum_ELN,Accuracy_sum_ELN)$x
  table.baseline[ii,] <-(round(c(median(benchmark[,1],na.rm = T),median(benchmark[,2],na.rm = T),median(benchmark[,3],na.rm = T)),digits = 4))
  
  
  # benchmark of the 100 ELN-ensemble model
  benchmark_ensem <- overall(Accuracy_sum_ensem_ELN,Accuracy_sum_ensem_ELN)$x
  table.ensem[ii,] <-(round(c(median(benchmark_ensem[,1],na.rm = T),median(benchmark_ensem[,2],na.rm = T),median(benchmark_ensem[,3],na.rm = T)),digits = 4))
  
  
  # benchmark of the 100 ELN-ensemble model without FPCA
  benchmark_nofpca <- overall(Accuracy_sum_nofpca,Accuracy_sum_nofpca)$x
  table.nofpca[ii,] <-(round(c(median(benchmark_nofpca[,1],na.rm = T),median(benchmark_nofpca[,2],na.rm = T),median(benchmark_nofpca[,3],na.rm = T)),digits = 4))
  
  # benchmark of the 100 ELN-ensemble model without high-frequency vars
  benchmark_nowin <- overall(Accuracy_sum_nowin,Accuracy_sum_nowin)$x
  table.nowin[ii,] <-(round(c(median(benchmark_nowin[,1],na.rm = T),median(benchmark_nowin[,2],na.rm = T),median(benchmark_nowin[,3],na.rm = T)),digits = 4))
  
}


# this is for the table in the appendix
table.result <- cbind(table.baseline, table.ensem,table.nofpca,table.nowin)
rownames(table.result) <- char_name
colnames(table.result) <- rep(c('P','R','F1'),4)
xtable(table.result,digits=3)


##################################################################################
# Table of Econmetric
##################################################################################


# the variable of daily volume
TRADE_volume <- list()

for(kk in 1:length(char_name)){

  char <- char_name[kk]
  print(char)
  print(Sys.time())
    daily_volume <- try(fread(file=paste0('/project/6003851/SharedData/NYSE16/GroupingResult/TRADE/EQY_US_ALL_TRADE_',char,'.txt'),sep='|',header=F),silent=T)
    daily_volume <- as.data.table(daily_volume)
    daily_volume$V10 <- as.character(daily_volume$V10)
    daily_volume$V10 <- as.factor(daily_volume$V10)
    
    TRADE_volume[[kk]] <- daily_volume[,sum(V5),by=V10]$V1
}    

# the variable of bid-ask spread (basis points), mid-price and market depth(share)
Bidask_spread <- list()
Mid_price <- list()
Mkt_depth <- list()
    
    
    for(k in 1:length(char_name)){
    char <- char_name[k]
    print(k)
  test <- try(load(paste0('./result/', char,'_final.rda')),silent = T)
  if(class(test)%in%"try-error") next
  
  Bidask_spread[[k]] <- (stock$Best_Offer_Price-stock$Best_Bid_Price)/stock$midprice
  Mkt_depth[[k]] <- (stock$Best_Bid_Size+stock$Best_Offer_Size)*100
  Mid_price[[k]] <- stock$midprice
        
        }

Mkt_cap <- read.csv('./rda/mkt_cap.csv')
Mkt_cap$mkt_cap[which(Mkt_cap$unit=='T')] <- Mkt_cap$mkt_cap[which(Mkt_cap$unit=='T')]*1000

table.feature <- matrix(NA,ncol=5,nrow=5)
rownames(table.feature) <- c('Mean','Median','Std Dev','Min','Max')
colnames(table.feature) <- c('MktCap ($billion)','Volume (million)','Spread (bps)', 'Midprice ($ per share)', 'Depth (shares)')

feature <- list(Mkt_cap$mkt_cap, unlist(TRADE_volume),unlist(Bidask_spread),unlist(Mid_price),
               unlist(Mkt_depth))
feature[[3]] <- feature[[3]][-which(feature[[3]]<0)]

table.feature[1,] <- sapply(feature,function(x) mean(x,na.rm=T))
table.feature[2,] <- sapply(feature,function(x) median(x,na.rm=T))
table.feature[3,] <- sapply(feature,function(x) sd(x,na.rm=T))
table.feature[4,] <- sapply(feature,function(x) min(x,na.rm=T))
table.feature[5,] <- sapply(feature,function(x) max(x,na.rm=T))

# adjusting the unit
table.feature[,2] <- table.feature[,2]/10^6
table.feature[,3] <- table.feature[,3]*10^4

xtable(table.feature,digits=3)
                            
                            
                            
##################################################################################
# Table of valid number of ELN model 
##################################################################################
saved <- failed <- list()
for (ii in 1:length(char_name)){
char <- char_name[ii]
print(char)
flag_sum_NA <-  flag_ensem_NA <-  vector()
# only use F1 score to draw the pictures
load(paste0('./result/', char,'_eln_ensemble_model_daily.rda'))
for(j in 1:length(flag)){
    if(is.na(Accuracy_sum_ELN[[j]][[7]])) {Accuracy_sum_ELN[[j]][[7]] <-  0
    flag_nofpca_NA <- c(flag_sum_NA,j)}
    if(is.na(Accuracy_sum_ELN[[j]][[8]])) {Accuracy_sum_ELN[[j]][[8]] <-  0
    flag_nofpca_NA <- c(flag_sum_NA,j)}
    if(is.na(Accuracy_sum_ELN[[j]][[9]])) {Accuracy_sum_ELN[[j]][[9]] <-  0
    flag_nofpca_NA <- c(flag_sum_NA,j)}
    if(is.na(Accuracy_sum_ensem_ELN[[j]][[7]])) {Accuracy_sum_ensem_ELN[[j]][[7]] <-  0
    flag_nofpca_NA <- c(flag_ensem_NA,j)}
    if(is.na(Accuracy_sum_ensem_ELN[[j]][[8]])) {Accuracy_sum_ensem_ELN[[j]][[8]] <-  0
    flag_nofpca_NA <- c(flag_ensem_NA,j)}
    if(is.na(Accuracy_sum_ensem_ELN[[j]][[9]])) {Accuracy_sum_ensem_ELN[[j]][[9]] <-  0
    flag_nofpca_NA <- c(flag_ensem_NA,j)}
    
  load(paste0('./result/',char,'_eln_summary_daily.rda'))
  # predictions failed
  failed[[ii]] <- list(unique(flag_sum_NA), unique(flag_ensem_NA), unique(flag_nofpca_NA), unique(flag_nowin_NA))
  failed[[ii]] <- sapply(failed[[ii]], length)
  # model succeeded to be saved
  saved[[ii]] <-list(unique(flag), c(1:100), unique(flag_nofpca), unique(flag_nowin))
  saved[[ii]] <- sapply(saved[[ii]], length)
}
}
# Note that failed counts the value of NaN in prediction while saved counts the value of successfully saved model
failed <- matrix(unlist(failed), ncol=30)
saved <- matrix(unlist(saved), ncol=30)

colnames(failed)=colnames(saved)=char_name
rownames(failed)=c('full','ensem','nofpca','nowin')
rownames(saved)=c('full','nofpca','nowin')
# total.working <- saved-failed[c(1,3,4),]
total.working <- saved-failed
total.working <- t(total.working)                            
# xtable(failed, caption='The number of samples that give bad predictions')
# xtable(saved, caption='The number of models that are saved')
xtable(total.working, caption='The number of samples that are valid')                          
                            
                            
##################################################################################
# Table of valid number of SVM model (we don't need a table since there is no missing model
##################################################################################                            
# for (ii in 1:length(char_name)){
# + char <- char_name[ii]
# + print(char)
# + load(paste0('./result/', char,'_svm_ensemble_model_daily.rda'))
# + print(length(flag))
# + }                            
                            
##################################################################################
# Table of number of FPC in each stocks
##################################################################################                                   
 numFPC <- vector()
  for(jj in 1:length(char_name) ){
  # setwd(file.path(path0,'result'))
  char <- char_name[jj] 
  print(char)
  test <- try(load(paste0('./result/', char,'_to_sample.rda')))
  if(class(test)%in%"try-error") next
  numFPC <- c(numFPC, length(grep("daily_fpca",colnames(stock))) )  
 }
                            
# > numFPC
#  [1] 1 2 2 1 1 1 2 2 2 2 1 2 2 2 2 1 2 1 2 2 2 2 1 2 1 2 1 1 2 2                            
                            
                            
##################################################################################
# Table of fdr adjusted p-values
##################################################################################                             
# SVM model
##################################################################################
result <- matrix(NA,ncol=30,nrow=100)
result_ensem <- matrix(NA,ncol=30,nrow=100)
result_nofpca <- matrix(NA,ncol=30,nrow=100)
result_nowin <- matrix(NA,ncol=30,nrow=100)

for (ii in 1:length(char_name)){
  char <- char_name[ii]
  print(char)
  # only use F1 score to draw the pictures
  load(paste0('./result/', char,'_svm_ensemble_model_daily.rda'))
  result[flag,ii] <- overall(Accuracy_sum,Accuracy_sum_ensem)$x[,3]
  result_ensem[flag,ii] <-overall(Accuracy_sum,Accuracy_sum_ensem)$y[,3]
  result_nofpca[flag,ii] <- overall(Accuracy_sum_nofpca,Accuracy_sum_nowin)$x[,3]
  result_nowin[flag,ii] <- overall(Accuracy_sum_nofpca,Accuracy_sum_nowin)$y[,3]
}



Result <- data.frame(value=c(matrix(result,ncol=1,nrow=3000)-matrix(result_nowin,ncol=1,nrow=3000), matrix(result_ensem,ncol=1,nrow=3000)-matrix(result,ncol=1,nrow=3000),matrix(result,ncol=1,nrow=3000)-matrix(result_nofpca,ncol=1,nrow=3000)),
                     stock=rep(rep(char_name,each=100),3),ensemble=as.character(rep(c('non-ensemble', 'ensemble','non-ensemble'),c(3000,3000,3000))),fpca=rep(c('nofpca','nofpca','fpca'),c(3000,3000,3000)),win=rep(c('window','non-window'),c(3000,6000)),
                     diff=as.factor(rep(c("F1 improvement: Strategy I",'F1 improvement: Strategy II','F1 improvement: Strategy III'),each=3000)))



Result <- data.frame(value=c(matrix(result,ncol=1,nrow=3000)-matrix(result_nowin,ncol=1,nrow=3000), matrix(result_ensem,ncol=1,nrow=3000)-matrix(result,ncol=1,nrow=3000),matrix(result,ncol=1,nrow=3000)-matrix(result_nofpca,ncol=1,nrow=3000)),
                     stock=rep(rep(char_name,each=100),3),
                     diff=as.factor(rep(c("F1 improvement: Strategy I",'F1 improvement: Strategy II','F1 improvement: Strategy III'),each=3000)))


Result$stock <- as.factor(Result$stock)

# one-sided test, alternative equals 'greater'
test1 <- list()
for(i in 1:length(char_name)){
  char <- char_name[i]
  # print(char)
  test1[[i]] <- wilcox.test(result_ensem[,i],result[,i],paired = T,mu=0, alternative='greater')$p.value
}


# baseline model versus no-fpca, wilcoxon sign rank test, two-sided
test2 <- list()
for(i in 1:length(char_name)){
  char <- char_name[i]
  # print(char)
  test2[[i]] <- wilcox.test(result[,i],result_nofpca[,i],paired = T,mu=0, alternative='greater')$p.value
}


# # baseline model versus no-win, wilcoxon sign rank test, two-sided
test3 <- list()
for(i in 1:length(char_name)){
  char <- char_name[i]
  # print(char)
  test3[[i]] <- wilcox.test(result[,i],result_nowin[,i],paired = T,mu=0, alternative='greater')$p.value
}

# test_result <- matrix(cbind(unlist(test1), unlist(test2), unlist(test3)), nrow=30)
test_result <- matrix(cbind(p.adjust(unlist(test3), method='fdr'), p.adjust(unlist(test1), method='fdr'), p.adjust(unlist(test2), method='fdr')), nrow=30)
                      
colnames(test_result) <- c('F1 improvement: Strategy I', 'F1 improvement: Strategy II', 'F1 improvement: Strategy III')
rownames(test_result) <- char_name
xtable(test_result, display=c('e','e','e','e'))
                            
# ENet model
##################################################################################                            
result <- matrix(NA,ncol=30,nrow=100)
result_ensem <- matrix(NA,ncol=30,nrow=100)
result_nofpca <- matrix(NA,ncol=30,nrow=100)
result_nowin <- matrix(NA,ncol=30,nrow=100)

for (ii in 1:length(char_name)){
  char <- char_name[ii]
  print(char)
    flag_sum_NA <-  flag_ensem_NA <-  vector()
  # only use F1 score to draw the pictures
  load(paste0('./result/', char,'_eln_ensemble_model_daily.rda'))
  for(j in 1:length(flag)){
    if(is.na(Accuracy_sum_ELN[[j]][[7]])) {Accuracy_sum_ELN[[j]][[7]] <-  0
    flag_nofpca_NA <- c(flag_sum_NA,j)}
    if(is.na(Accuracy_sum_ELN[[j]][[8]])) {Accuracy_sum_ELN[[j]][[8]] <-  0
    flag_nofpca_NA <- c(flag_sum_NA,j)}
    if(is.na(Accuracy_sum_ELN[[j]][[9]])) {Accuracy_sum_ELN[[j]][[9]] <-  0
    flag_nofpca_NA <- c(flag_sum_NA,j)}
    if(is.na(Accuracy_sum_ensem_ELN[[j]][[7]])) {Accuracy_sum_ensem_ELN[[j]][[7]] <-  0
    flag_nofpca_NA <- c(flag_ensem_NA,j)}
    if(is.na(Accuracy_sum_ensem_ELN[[j]][[8]])) {Accuracy_sum_ensem_ELN[[j]][[8]] <-  0
    flag_nofpca_NA <- c(flag_ensem_NA,j)}
    if(is.na(Accuracy_sum_ensem_ELN[[j]][[9]])) {Accuracy_sum_ensem_ELN[[j]][[9]] <-  0
    flag_nofpca_NA <- c(flag_ensem_NA,j)}
      
  }
  # In the comparison between ensemble and baseline model, valid sample sizes are saved model without failures  
  if(length(union(flag_sum_NA, flag_ensem_NA))!=0) flag = flag[-union(flag_sum_NA, flag_ensem_NA)]
  result[flag,ii] <- overall(Accuracy_sum_ELN,Accuracy_sum_ensem_ELN)$x[,3]
  result_ensem[flag,ii] <-overall(Accuracy_sum_ELN,Accuracy_sum_ensem_ELN)$y[,3]
  
  load(paste0('./result/',char,'_eln_summary_daily.rda'))
    
  # In the comparison between baseline model and nofpca, valid sample sizes are saved model without fpca failures intersect{flag, (flag_nofpca-flag_nofpca_NA)}    
  if(length(intersect(flag_nowin_NA, flag_nowin_NA))!=0) flag_nowin = flag_nowin[-flag_nowin_NA]
  if(length(intersect(flag_nofpca_NA, flag_nofpca_NA))!=0) flag_nofpca = flag_nofpca[-flag_nofpca_NA]
    
  flag_nowin <- intersect(flag, flag_nowin)
  flag_nofpca <- intersect(flag, flag_nofpca)
    
  Accuracy_sum_nofpca <- Accuracy_sum_nofpca[flag_nofpca]
  Accuracy_sum_nowin <- Accuracy_sum_nowin[flag_nowin]
  result_nofpca[flag_nofpca,ii] <- overall(Accuracy_sum_nofpca,Accuracy_sum_nofpca)$x[,3]
  result_nowin[flag_nowin,ii] <- overall(Accuracy_sum_nowin,Accuracy_sum_nowin)$y[,3]
}


Result <- data.frame(value=c(matrix(result_ensem,ncol=1,nrow=3000)-matrix(result,ncol=1,nrow=3000),matrix(result,ncol=1,nrow=3000)-matrix(result_nofpca,ncol=1,nrow=3000),matrix(result,ncol=1,nrow=3000)-matrix(result_nowin,ncol=1,nrow=3000)),
                     stock=rep(rep(char_name,each=100),3),
                     diff=as.factor(rep(c("F1 improvement: Strategy II",'F1 improvement: Strategy III','F1 improvement: Strategy I'),each=3000)))
levels(Result$diff) <- c("F1 improvement: Strategy I",'F1 improvement: Strategy II','F1 improvement: Strategy III')


Result$stock <- as.factor(Result$stock)

test1 <- list()
for(i in 1:length(char_name)){
  char <- char_name[i]
    test1[[i]] <- wilcox.test(result_ensem[,i],result[,i],paired = T,mu=0,na.omit=T, alternative='greater')$p.value
}

# baseline model versus no-fpca, wilcoxon sign rank test, two-sided
test2 <- list()
for(i in 1:length(char_name)){
  char <- char_name[i]
    test2[[i]] <- wilcox.test(result[,i],result_nofpca[,i],paired = T,mu=0,na.omit=T, alternative='greater')$p.value
}


# # baseline model versus no-win, wilcoxon sign rank test, two-sided
test3 <- list()
for(i in 1:length(char_name)){
  char <- char_name[i]
    test3[[i]] <- wilcox.test(result[,i],result_nowin[,i],paired = T,mu=0,na.omit=T, alternative='greater')$p.value
}


test_result <- matrix(cbind(p.adjust(unlist(test3), method='fdr'), p.adjust(unlist(test1), method='fdr'), p.adjust(unlist(test2), method='fdr')), nrow=30)
colnames(test_result) <- c('F1 improvement: Strategy I', 'F1 improvement: Strategy II', 'F1 improvement: Strategy III')
rownames(test_result) <- char_name
xtable(test_result, display=c('e','e','e','e'))                           
                            
                            
                            
                            
                            
                            
                            
                            
                            

