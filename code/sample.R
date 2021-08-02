# this is for the svm modeling
# the setting of kernel function and parameters will follow the paper 2015
rm(list=ls())
args <- commandArgs(trailingOnly = TRUE)

#setwd("/project/6003851/y2huang/midprice_predict")
# setwd("/Users/ying/Desktop/UVic/Year1_summer2020/mid_price_prediction/code/local_test")
# server <- TRUE 
# path0 <- ifelse(server, "/project/6003851/y2huang/midprice_predict/final_version_2", "/Users/ying/Desktop/UVic/Year1_summer2020/mid_price_prediction/code/local_test")
# setwd(path0)
source('./code/wiltest.r')
options(digits = 6)
library(dbplyr)
library(data.table)
library(glmnet)
library(fdapace)
library(caret)
library(e1071)
library(stringr)
# library(MTPS)

# Don Jones 30 component stocks
char_name <- c('AAPL','MSFT','MMM','AXP','BA','CAT','CVX','CSCO','KO','DOW','XOM',
               'WBA','GS','HD','INTC','IBM','JNJ','JPM','MCD','MRK','NKE','PFE','PG',
               'TRV','UNH','UTX','VZ','V','WMT','DIS')
k <- 5

# 2:length(char_name)
# AAPL requires more computing resources than other, so it needs to be submitted with 50G alone
for(jj in 1:length(char_name) ){
  # setwd(file.path(path0,'result'))
  char <- char_name[jj] 
  print(char)
  test <- try(load(paste0('./result/', char,'_to_sample.rda')))
  if(class(test)%in%"try-error") next
  
  nsize <- 10000
  nfold <- 5 # this is to guarantee the distn of testing and training are the same
  # in our setting the size of testing set is one fifth of the training set
  # on compute canada we will submit 100 jobs, in here we present as a loop
  
  # also need to include the FPCA variables in the mfeatures
  mfeatures <- matrix(unlist(features,use.names = T),nrow=nrow(stock)) 
  
  # in the last procedure, forget to label the window only vars
  names_mfeatures[6:16] <- str_c('win_',names_mfeatures[6:16])
  colnames(mfeatures) <- names_mfeatures
 
  stock <- data.frame(stock)
  F_test <- data.frame(mfeatures,stock[,grep("fpca",colnames(stock))])
  for(j in 1:ncol(F_test)){
    if(sum(is.na(F_test[,j]))!=0){F_test[which(is.na(F_test[,j])),j] <- 0}
  }
  
  # plot the corralation matrix before standarization
  # library(corrplot)
  # F_test_plot <- F_test[,which(colnames(F_test)%in%c(paste0("sec_midprice_t_",1:5),paste0("min_midprice_t_",1:5),paste0("hour_midprice_t_",1:3), paste0("daily_curve_fpca",1:16),
  #                                                    "windowslope",paste0("bid_ask_spread_t_",1:5),"klevel_diff_best_bid_return","klevel_diff_best_ask_return","mean_best_bid_size","mean_best_ask_size",
  #                                                    "b_bid_b_ask_sc_return"))]
  
  # corrplot(cor(F_test_plot),tl.cex=0.7,method='color',order='FPC')
  
  # because the ratio of the categories with affect the general accuracy
  
  # so we follow the ieee's method and give a proper rearrangement 
  # ATTENTION!, here I use t=3, the future 3 terms, which can be changed, but the followed should also be changed
  # we don't use the ieee's method to re-allocate the labels, we just compare the one with the last one
  
  # get the record for every k record(original data/features)
  # stock1 <- stock[which(1:nrow(stock)%%k==0),]
  # stock1$label <- c(0,diff(stock1$midprice))
  # stock1$label[stock1$label>0] <- 'U'
  # stock1$label[stock1$label==0] <- 'S'
  # stock1$label[stock1$label<0] <- 'D'
  # stock1$label <- as.factor(stock1$label)
  # 
  # ratio <- as.numeric(table(stock1$label))
  # 
  # if(ratio[1]<=ratio[2]){
  #   F_test <- F_test[which(1:nrow(stock)%%k==0),]
  #   stock <- stock1 
  # }
  
 
    a <- 10^(-5)
    stock$compare <- rowMeans(data.table(matrix(cbind(unlist(data.table::shift(stock$midprice,-(1:k),type='lag'))),nrow=length(stock$midprice),ncol=k)))
    stock$compare <-  stock$compare/stock$midprice
    stock$label[stock$compare>1+a] <- 'U'
    stock$label[stock$compare<1-a] <- 'D'
    stock$label[stock$compare>(1-a)&stock$compare<(1+a)] <- 'S'
    
    stock <- stock[1:(nrow(stock)-k),]
    F_test <- F_test[1:(nrow(stock)-k),]
    
    stock <- stock[which(1:nrow(stock)%%k==0),]
    stock$label <- as.factor(stock$label)
    F_test <- F_test[which(1:nrow(F_test)%%k==0),]
  
  
  
  i <- as.numeric(args[1]) #input number from outside
  set.seed(i)
  # ratio <- as.numeric(table(stock$label))
  # if(1.2*ratio[1]<ratio[2]){
    sample_indexS <- sample(which(stock$label=='S'),3400,replace = F)
    sample_indexU <- sample(which(stock$label=='U'),3300,replace = F)
    sample_indexD <- sample(which(stock$label=='D'),3300,replace = F)
    
    sample_index <- c(sample_indexS,sample_indexU,sample_indexD)
  # }else sample_index <- sample(1:nrow(stock),nsize,replace=F)
  
  F_test_temp <- F_test[sample_index,]
  F_test_temp$label <- stock$label[sample_index]
  
  index <- createFolds(F_test_temp$label,nfold,list=F)
  
  # generate test sample and train sample, but now we need to use 
  # train sample to standadize both test and train samples
  F_test_temp1 <- F_test_temp
  test_sample1 <- F_test_temp1[index==1,]
  train_sample1 <- F_test_temp1[index!=1,]
  
  # first remove what we regard as outlier and then do the scale
  # replace the obs which are not within Q1-1.5IQR,Q3+1.5IQR,IQR=Q3-Q1
  # then scale the whole size 10000 sample with the attris from train_sample
  # we want the mean and sd after winsorization, but we actually don't need the winsorization to our data
  
  for(j in 1:(ncol(train_sample1)-1)){
    train_Q1quantl <- round(quantile(train_sample1[,j],0.25,na.rm=T),10)
    train_Q3quantl <- round(quantile(train_sample1[,j],0.75,na.rm=T),10)
    if(train_Q1quantl==train_Q3quantl){
      F_test_temp[,j] <- (F_test_temp[,j]-attr(scale(train_sample1[,j]),"scaled:center"))/attr(scale(train_sample1[,j]),"scaled:scale")
    }
    else{
      IQR <- train_Q3quantl-train_Q1quantl
      upper <- train_Q3quantl+1.5*IQR
      lower <- train_Q1quantl-1.5*IQR
      train_sample1[which(train_sample1[,j]>upper),j] <- upper
      train_sample1[which(train_sample1[,j]<lower),j] <- lower
      F_test_temp[,j] <- (F_test_temp[,j]-attr(scale(train_sample1[,j]),"scaled:center"))/attr(scale(train_sample1[,j]),"scaled:scale")
    }
  }
  
  for(tt in 1:(ncol(F_test_temp)-1)){
    if(sum(is.infinite(F_test_temp[,tt]))>0) F_test_temp[which(is.infinite(F_test_temp[,tt])),tt] <- median(F_test_temp[,tt], na.rm=T)
    if(sum(is.na(F_test_temp[,tt]))>0) F_test_temp[which(is.na(F_test_temp[,tt])),tt] <- median(F_test_temp[,tt], na.rm=T)
    if(sum(is.nan(F_test_temp[,tt]))>0) F_test_temp[which(is.nan(F_test_temp[,tt])),tt] <- median(F_test_temp[,tt], na.rm=T)
    if(sum(is.null(F_test_temp[,tt]))>0) F_test_temp[which(is.null(F_test_temp[,tt])),tt] <- median(F_test_temp[,tt], na.rm=T)
  }
#     if(sum(is.na(F_test_temp[,ncol(F_test_temp)]))>0) F_test_temp <- F_test_temp[-which(is.na(F_test_temp[,ncol(F_test_temp)])),]
  
  test_sample <- F_test_temp[index==1,]
  train_sample <- F_test_temp[index!=1,]
  
  # setwd(file.path(path0,'result'))
  
  
  # full
  svmfit <- svm(label~.,data=train_sample,kernel="polynomial",degree=2,coef0=1,cost=0.25,scale=F,na.action = na.omit)
  fit.pre <- predict(svmfit,as.matrix(test_sample[,-which(colnames(train_sample)%in%"label")]))
  
  if(sum(fit.pre=='U')==0) P_U <- 0 else P_U  <- length(intersect(which(fit.pre=='U'),which(test_sample$label=='U')))/sum(fit.pre=='U')
  if(sum(fit.pre=='S')==0) P_S <- 0 else P_S  <- length(intersect(which(fit.pre=='S'),which(test_sample$label=='S')))/sum(fit.pre=='S')
  if(sum(fit.pre=='D')==0) P_D <- 0 else P_D  <- length(intersect(which(fit.pre=='D'),which(test_sample$label=='D')))/sum(fit.pre=='D')
  R_U  <- length(intersect(which(fit.pre=='U'),which(test_sample$label=='U')))/sum(test_sample$label=='U')
  R_S  <- length(intersect(which(fit.pre=='S'),which(test_sample$label=='S')))/sum(test_sample$label=='S')
  R_D  <- length(intersect(which(fit.pre=='D'),which(test_sample$label=='D')))/sum(test_sample$label=='D')
  F1_U  <- 2*P_U *R_U /(P_U +R_U )
  F1_S  <- 2*P_S *R_S /(P_S +R_S )
  F1_D  <- 2*P_D *R_D /(P_D +R_D )
  Accuracy <- list(P_U,P_S,P_D,R_U,R_S,R_D,F1_U,F1_S,F1_D)
  
  # nofpca
  test_sample_nofpca <- F_test_temp[index==1,-grep("fpca",colnames(F_test_temp))]
  train_sample_nofpca <- F_test_temp[index!=1,-grep("fpca",colnames(F_test_temp))]
  
  svmfit_nofpca <- svm(label~.,data=train_sample_nofpca,kernel="polynomial",degree=2,coef0=1,cost=0.25,scale=F,na.action = na.omit)
  fit.pre <- predict(svmfit_nofpca,as.matrix(test_sample_nofpca[,-which(colnames(train_sample_nofpca)%in%"label")]))
  
  if(sum(fit.pre=='U')==0) P_U <- 0 else P_U  <- length(intersect(which(fit.pre=='U'),which(test_sample$label=='U')))/sum(fit.pre=='U')
  if(sum(fit.pre=='S')==0) P_S <- 0 else P_S  <- length(intersect(which(fit.pre=='S'),which(test_sample$label=='S')))/sum(fit.pre=='S')
  if(sum(fit.pre=='D')==0) P_D <- 0 else P_D  <- length(intersect(which(fit.pre=='D'),which(test_sample$label=='D')))/sum(fit.pre=='D')
  R_U  <- length(intersect(which(fit.pre=='U'),which(test_sample$label=='U')))/sum(test_sample$label=='U')
  R_S  <- length(intersect(which(fit.pre=='S'),which(test_sample$label=='S')))/sum(test_sample$label=='S')
  R_D  <- length(intersect(which(fit.pre=='D'),which(test_sample$label=='D')))/sum(test_sample$label=='D')
  F1_U  <- 2*P_U *R_U /(P_U +R_U )
  F1_S  <- 2*P_S *R_S /(P_S +R_S )
  F1_D  <- 2*P_D *R_D /(P_D +R_D )
  Accuracy_nofpca <- list(P_U,P_S,P_D,R_U,R_S,R_D,F1_U,F1_S,F1_D)
  
  # nowindow variables
  test_sample_nowin <- F_test_temp[index==1,-grep("win",colnames(F_test_temp))]
  train_sample_nowin <- F_test_temp[index!=1,-grep("win",colnames(F_test_temp))]
  
  svmfit_nowin <- svm(label~.,data=train_sample_nowin,kernel="polynomial",degree=2,coef0=1,cost=0.25,scale=F,na.action = na.omit)
  fit.pre <- predict(svmfit_nowin,as.matrix(test_sample_nowin[,-which(colnames(train_sample_nowin)%in%"label")]))
  
  if(sum(fit.pre=='U')==0) P_U <- 0 else P_U  <- length(intersect(which(fit.pre=='U'),which(test_sample$label=='U')))/sum(fit.pre=='U')
  if(sum(fit.pre=='S')==0) P_S <- 0 else P_S  <- length(intersect(which(fit.pre=='S'),which(test_sample$label=='S')))/sum(fit.pre=='S')
  if(sum(fit.pre=='D')==0) P_D <- 0 else P_D  <- length(intersect(which(fit.pre=='D'),which(test_sample$label=='D')))/sum(fit.pre=='D')
  R_U  <- length(intersect(which(fit.pre=='U'),which(test_sample$label=='U')))/sum(test_sample$label=='U')
  R_S  <- length(intersect(which(fit.pre=='S'),which(test_sample$label=='S')))/sum(test_sample$label=='S')
  R_D  <- length(intersect(which(fit.pre=='D'),which(test_sample$label=='D')))/sum(test_sample$label=='D')
  F1_U  <- 2*P_U *R_U /(P_U +R_U )
  F1_S  <- 2*P_S *R_S /(P_S +R_S )
  F1_D  <- 2*P_D *R_D /(P_D +R_D )
  Accuracy_nowin <- list(P_U,P_S,P_D,R_U,R_S,R_D,F1_U,F1_S,F1_D)
  
  save(test_sample,train_sample,svmfit,Accuracy,svmfit_nofpca,Accuracy_nofpca,svmfit_nowin,Accuracy_nowin,file=paste0('./result/', char,'_',i,'_model_svm.rda'))
}




