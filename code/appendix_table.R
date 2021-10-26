#getwd()
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

table.time.svm <- matrix(NA,ncol=30,nrow=100)
table.time.eln <- matrix(NA,ncol=30,nrow=100)

# single ELN model fitting time
for (ii in 1:length(char_name)){
  char <- char_name[ii]
    print(ii)
for(i in 1:100){
test <- try(load(paste0('./result/', char,'_',i,'_model_full.rda')),silent=T)
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
test <- try(load(paste0('./result/', char,'_',i,'_model_svm.rda')),silent=T)
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
  load(paste0('./result/', char,'_svm_ensemble_model.rda'))
  table.time.ensem[ii,1] <- time_SVM
    
  load(paste0('./result/',char,'_full_ensemble_model.rda'))
  table.time.ensem[ii,2] <- time_ELN
}

time <- cbind(table.time.svm, table.time.eln,table.time.ensem)
colnames(time) <- c("SVM (sec)", "ELN (sec)", "Ensemble SVM (hr)", "Ensemble ELN (mins)")
rownames(time) <- char_name
xtable(time, digits=3)


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
# this is for the table in the body, showing the result of baseline SVM model
rownames(table.baseline) <- char_name
colnames(table.baseline) <- c('P','R','F1')
xtable(table.baseline,digits=3)

# this is for the table in the appendix
table.result <- cbind(table.ensem,table.nofpca,table.nowin)
rownames(table.result) <- char_name
colnames(table.result) <- rep(c('P','R','F1'),3)
xtable(table.result,digits=3)


###################summary feature table in the data.source section#######################


# the variable of daily volume
TRADE_volume <- list()

for(kk in 1:length(char_name)){


#   if(kk>1) rm(list=ls()[-which(ls()%in%c('char_name','kk'))])
  char <- char_name[kk]
  print(char)
  print(Sys.time())
    daily_volume <- try(fread(file=paste0('/project/6003851/SharedData/NYSE16/GroupingResult/TRADE/EQY_US_ALL_TRADE_',char,'.txt'),sep='|',header=F),silent=T)
    daily_volume <- as.data.table(daily_volume)
#     V5 is the trade volume of each transaction, V10 is the date
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

