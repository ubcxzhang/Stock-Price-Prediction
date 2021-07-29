# This is for ensemble
# version3
# module load nixpkgs/16.09 gcc/7.3.0 r/3.6.1
rm(list=ls())
#setwd("/project/6003851/y2huang/midprice_predict")
# setwd("/Users/ying/Desktop/UVic/Year1_summer2020/mid_price_prediction/code/local_test")
# server <- TRUE 
# path0 <- ifelse(server, "/project/6003851/y2huang/midprice_predict/final_version_2", "/Users/ying/Desktop/UVic/Year1_summer2020/mid_price_prediction/code/local_test")
# setwd(path0)
# load('cutoff.rda')
# for the trail of no cutoff
# setwd(file.path(path0,'result_ELN_nocut'))
# otherwise
# setwd(file.path(path0,'result'))
#setwd(file.path(path0,'AAPL'))
options(digits = 4)
library(dbplyr)
library(data.table)
library(glmnet)
#library(e1071)
#library(randomForest)
#library(fdapace)
#library(bit64)
library(caret)

# Don Jones 30 component stocks
char_name <- c('AAPL','MSFT','MMM','AXP','BA','CAT','CVX','CSCO','KO','DOW','XOM',
               'WBA','GS','HD','INTC','IBM','JNJ','JPM','MCD','MRK','NKE','PFE','PG',
               'TRV','UNH','UTX','VZ','V','WMT','DIS')

 
 
 
for(jj in 1:length(char_name)){
  time <- Sys.time()
  char <- char_name[jj] 
  # cutoff_stock <- as.numeric(cutoff[jj])
  print(char)
  print(Sys.time())
Accuracy_sum2 <- model_sum1<-test_sum1 <-Accuracy_sum_ensem_ELN <-list()
test <- try(load(paste0('./result/', char,'_1_model_full.rda')),silent=T)
if(class(test)%in%'try-error') next
test <- vector()
flag <- vector()
for(i in 1:100){
  test <- try(load(paste0('./result/', char,'_',i,'_model_full.rda')),silent=T)
  if(class(test)%in%'try-error') next
  else flag <- c(flag,i) 
  
  try(load(paste0('./result/', char,'_',i,'_model_full.rda')),silent=T)
  Accuracy_sum2[[i]] <- Accuracy
  model_sum1[[i]] <- fit.glm
  test_sum1[[i]] <- test_sample
}
Accuracy_sum_ELN <- Accuracy_sum2[flag]
model_sum <- model_sum1[flag]
test_sum <- test_sum1[flag]
# a problem occurs, data is unbalanced. It's very possible that no prediction of 'S'
# k is for data and j is for model
# Attention! the value of nrow will change if the spliting change in sample
# ATTENTION! the number of test sample is 2001 but train sample is 7999 maybe the former R file has a minor mistake
fit.pre.sum <- fit.pre.sum.prob <- matrix(0,ncol=length(flag),nrow=2000)

# ATTENTION! don't know why sample size be 2000 or 2001 and 1999
# k represents the dataset and j represents the model
# for each dataset we have all j models and use the voting scheme
for(k in 1:length(flag)){
   # print(k)
  if(nrow(test_sum[[k]])<2000){
    size <- nrow(test_sum[[k]])
    test_sum[[k]] <- rbind(test_sum[[k]],test_sum[[k]][1:(2000-size),])
  }
  if(nrow(test_sum[[k]])>2000){
    size <- nrow(test_sum[[k]])
    test_sum[[k]] <- test_sum[[k]][1:2000,]
  }
  
  for(j in 1:length(flag)){
    
  # store the probabilities
    fit.pre.sum.prob[,j] <- apply(predict(model_sum[[j]],as.matrix(test_sum[[k]][,-which(colnames(train_sample)%in%"label")]),type='response',s=model_sum[[j]]$lambda),1,max)
    
  # according to the cutoff value, get the final classification
    # since this is a no cutoff involved ensemble, only use the original result
    # if(sum(is.nan(predict(model_sum[[j]],as.matrix(test_sum[[k]][,-which(colnames(train_sample)%in%"label")]),type='response',s=model_sum[[j]]$lambda)))>0){fit.pre.sum[,j] <- predict(model_sum[[j]],as.matrix(test_sum[[k]][,-which(colnames(train_sample)%in%"label")]),type='class',s=model_sum[[j]]$lambda)} 
    # else {fit.pre.sum[,j] <- apply(predict(model_sum[[j]],as.matrix(test_sum[[k]][,-which(colnames(train_sample)%in%"label")]),type='response',s=model_sum[[j]]$lambda),1,
    #                          function(x,value=cutoff_stock){if(x[2]>value) return('S')
    #                            else if(x[1]>x[3]) return('D')
    #                            else return('U')})}
    
    fit.pre.sum[,j] <- predict(model_sum[[j]],as.matrix(test_sum[[k]][,-which(colnames(train_sample)%in%"label")]),type='class',s=model_sum[[j]]$lambda)
    
  }
  
  list.fit.pre <-apply(as.matrix(fit.pre.sum),1,list)
  list.fit.pre <-lapply(list.fit.pre,table)
  num.fit.pre <- unlist(lapply(list.fit.pre,length))
  fit.pre <- vector()
  for(l in 1:length(num.fit.pre)){
    if(num.fit.pre[l]==1) fit.pre[l] <- names(list.fit.pre[[l]])
    if(num.fit.pre[l]==2){
      if(identical(as.numeric(list.fit.pre[[l]])[1],as.numeric(list.fit.pre[[l]])[2])){
        sum1 <- sum(fit.pre.sum.prob[l,which(fit.pre.sum[l,]==names(table(fit.pre.sum[l,]))[1]) ],na.rm = T)
        sum2 <- sum(fit.pre.sum.prob[l,which(fit.pre.sum[l,]==names(table(fit.pre.sum[l,]))[2]) ],na.rm = T)
        if(sum1>sum2) fit.pre[l] <- names(table(fit.pre.sum[l,]))[1]
        if(sum1<sum2) fit.pre[l] <- names(table(fit.pre.sum[l,]))[2] } 
      
      else fit.pre[l] <- names(list.fit.pre[[l]])[order(as.numeric(list.fit.pre[[l]]))==2]
    }
    if(num.fit.pre[l]==3){
      if(identical(order(as.numeric(list.fit.pre[[l]]))[2],order(as.numeric(list.fit.pre[[l]]))[3])){
        sum1 <- sum(fit.pre.sum.prob[l,which(fit.pre.sum[l,]==names(table(fit.pre.sum[l,]))[2]) ],na.rm = T)
        sum2 <- sum(fit.pre.sum.prob[l,which(fit.pre.sum[l,]==names(table(fit.pre.sum[l,]))[3]) ],na.rm = T)
        if(sum1>sum2) fit.pre[l] <- names(table(fit.pre.sum[l,]))[2]
        if(sum1<sum2) fit.pre[l] <- names(table(fit.pre.sum[l,]))[3] }
      
      else fit.pre[l] <- names(list.fit.pre[[l]])[order(as.numeric(list.fit.pre[[l]]))[3]]
    }
  }
  
  # the number of NA in the prediction will affect the accuracy of Recall
  # so need to record the number of NA in the prediction and minus it in the denominator
  flag_NA <- length(which(is.na(fit.pre)))
  
  if(sum(fit.pre=='U',na.rm=T)==0) P_U <- 0 else P_U  <- length(intersect(which(fit.pre=='U'),which(test_sum[[k]]$label=='U')))/sum(fit.pre=='U',na.rm=T)
  if(sum(fit.pre=='S',na.rm=T)==0) P_S <- 0 else P_S  <- length(intersect(which(fit.pre=='S'),which(test_sum[[k]]$label=='S')))/sum(fit.pre=='S',na.rm=T)
  if(sum(fit.pre=='D',na.rm=T)==0) P_D <- 0 else P_D  <- length(intersect(which(fit.pre=='D'),which(test_sum[[k]]$label=='D')))/sum(fit.pre=='D',na.rm=T)
  R_U  <- length(intersect(which(fit.pre=='U'),which(test_sum[[k]]$label=='U')))/(sum(test_sum[[k]]$label=='U',na.rm=T)-flag_NA)
  R_S  <- length(intersect(which(fit.pre=='S'),which(test_sum[[k]]$label=='S')))/(sum(test_sum[[k]]$label=='S',na.rm=T)-flag_NA)
  R_D  <- length(intersect(which(fit.pre=='D'),which(test_sum[[k]]$label=='D')))/(sum(test_sum[[k]]$label=='D',na.rm=T)-flag_NA)
  F1_U  <- 2*P_U *R_U /(P_U +R_U )
  F1_S  <- 2*P_S *R_S /(P_S +R_S )
  F1_D  <- 2*P_D *R_D /(P_D +R_D )
  
  Accuracy_sum_ensem_ELN[[k]] <- list(P_U,P_S,P_D,R_U,R_S,R_D,F1_U,F1_S,F1_D)
}


time_ELN <- print(Sys.time()-time)
save(Accuracy_sum_ELN,model_sum,test_sum,Accuracy_sum_ensem_ELN,time_ELN,flag, file=paste0('./result/',char,'_full_ensemble_model.rda'))


}
