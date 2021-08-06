# This is for ensemble with SVM model

rm(list=ls())

options(digits = 4)
library(dbplyr)
library(data.table)
library(glmnet)
library(e1071)
library(caret)
# Don Jones 30 component stocks
char_name <- c('AAPL','MSFT','MMM','AXP','BA','CAT','CVX','CSCO','KO','DOW','XOM',
               'WBA','GS','HD','INTC','IBM','JNJ','JPM','MCD','MRK','NKE','PFE','PG',
               'TRV','UNH','UTX','VZ','V','WMT','DIS')

# SVM model ensemble takes too long altogether, divide it into 30 separate jobs in Compute Canada
# read in the number it passes
args <- commandArgs(trailingOnly = TRUE)

time <- Sys.time()
jj <- as.numeric(args[1]) #input number from outside

  char <- char_name[jj] 
  print(char)
  print(Sys.time())
  
  Accuracy_sum <- Accuracy_sum_nofpca <-  Accuracy_sum_nowin  <- list()
    model_sum <- model_sum_nofpca <- model_sum_nowin <-test_sum <-Accuracy_sum_ensem <-list()

  test <- vector()
  flag <- vector()
  for(i in 1:100){
    test <- try(load(paste0('./result/',char,'_',i,'_model_svm.rda')),silent=T)
    if(class(test)%in%'try-error') next
    else flag <- c(flag,i) 
    
    Accuracy_sum[[i]] <- Accuracy
    Accuracy_sum_nofpca[[i]] <- Accuracy_nofpca
    Accuracy_sum_nowin[[i]] <- Accuracy_nowin
    model_sum[[i]] <- svmfit
    model_sum_nofpca[[i]] <- svmfit_nofpca
    model_sum_nowin[[i]] <- svmfit_nowin
    test_sum[[i]] <- test_sample
  }
  
  Accuracy_sum <- Accuracy_sum[flag]
  Accuracy_sum_nofpca <- Accuracy_sum_nofpca[flag]
  Accuracy_sum_nowin <- Accuracy_sum_nowin[flag] 
  model_sum <- model_sum[flag]
  model_sum_nofpca <- model_sum_nofpca[flag]
  model_sum_nowin <- model_sum_nowin[flag]
  test_sum <- test_sum[flag]
  
  # Attention! the value of nrow will change if the spliting change in sample
  fit.pre.sum <- fit.pre.sum.prob <- matrix(0,ncol=length(flag),nrow=2000)
  
################################################
# Ready to ensemble baseline SVM models of stock char_name[jj]
################################################
  
  for(k in 1:length(flag)){
    
     print(k)
    if(nrow(test_sum[[k]])<2000){
      size <- nrow(test_sum[[k]])
      test_sum[[k]] <- rbind(test_sum[[k]],test_sum[[k]][1:(2000-size),])
    }
    if(nrow(test_sum[[k]])>2000){
      size <- nrow(test_sum[[k]])
      test_sum[[k]] <- test_sum[[k]][1:2000,]
    }
    
    for(j in 1:length(flag)){
      
      
      fit.pre.sum[,j] <- as.character(predict(model_sum[[j]],as.matrix(test_sum[[k]][,-which(colnames(train_sample)%in%"label")])) )
 
    }
    
    # for each dataset we have all j models and use the voting scheme
    # if tie exist (usually very rare case), discard the record
    list.fit.pre <-apply(as.matrix(fit.pre.sum),1,list)
    list.fit.pre <-lapply(list.fit.pre,table)
    num.fit.pre <- unlist(lapply(list.fit.pre,length))
    fit.pre <- vector()
    for(l in 1:length(num.fit.pre)){
      if(num.fit.pre[l]==1) fit.pre[l] <- names(list.fit.pre[[l]])
      if(num.fit.pre[l]==2){
        if(identical(as.numeric(list.fit.pre[[l]])[1],as.numeric(list.fit.pre[[l]])[2])){
         
          fit.pre[l] <- NA } 
        # else fit.pre[l] <- NA
         else fit.pre[l] <- names(list.fit.pre[[l]])[order(as.numeric(list.fit.pre[[l]]))==2]
      }
      if(num.fit.pre[l]==3){
        if(identical(order(as.numeric(list.fit.pre[[l]]))[2],order(as.numeric(list.fit.pre[[l]]))[3])){
         
          fit.pre[l] <- NA}
        # else fit.pre[l] <- NA
         else fit.pre[l] <- names(list.fit.pre[[l]])[order(as.numeric(list.fit.pre[[l]]))[3]]
      }
    }
    
    # the number of NA in the prediction will affect the accuracy of Recall
    # so we need to record the number of NAs in the prediction and minus it in the denominator
    flag_NA <- length(which(is.na(fit.pre)))
    
    if(sum(fit.pre=='U',na.rm=T)==0) P_U <- 0 else P_U  <- length(intersect(which(fit.pre=='U'),which(test_sample$label=='U')))/sum(fit.pre=='U',na.rm=T)
    if(sum(fit.pre=='S',na.rm=T)==0) P_S <- 0 else P_S  <- length(intersect(which(fit.pre=='S'),which(test_sample$label=='S')))/sum(fit.pre=='S',na.rm=T)
    if(sum(fit.pre=='D',na.rm=T)==0) P_D <- 0 else P_D  <- length(intersect(which(fit.pre=='D'),which(test_sample$label=='D')))/sum(fit.pre=='D',na.rm=T)
    
    R_U  <- length(intersect(which(fit.pre=='U'),which(test_sum[[k]]$label=='U')))/(sum(test_sum[[k]]$label=='U',na.rm=T)-flag_NA)
    R_S  <- length(intersect(which(fit.pre=='S'),which(test_sum[[k]]$label=='S')))/(sum(test_sum[[k]]$label=='S',na.rm=T)-flag_NA)
    R_D  <- length(intersect(which(fit.pre=='D'),which(test_sum[[k]]$label=='D')))/(sum(test_sum[[k]]$label=='D',na.rm=T)-flag_NA)
    F1_U  <- 2*P_U *R_U /(P_U +R_U )
    F1_S  <- 2*P_S *R_S /(P_S +R_S )
    F1_D  <- 2*P_D *R_D /(P_D +R_D )
    
    Accuracy_sum_ensem[[k]] <- list(P_U,P_S,P_D,R_U,R_S,R_D,F1_U,F1_S,F1_D)
  }
  
  time_SVM <- print(Sys.time()-time)
  
  save(Accuracy_sum,model_sum,test_sum,Accuracy_sum_ensem,
       Accuracy_sum_nofpca,Accuracy_sum_nowin,time_SVM,flag, file=paste0('./result/', char,'_svm_ensemble_model.rda'))
  
  

