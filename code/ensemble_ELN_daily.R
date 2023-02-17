# This is for ensemble baseline model with ELN

rm(list=ls())

options(digits = 4)
library(dbplyr)
library(data.table)
library(glmnet)
library(caret)

# Don Jones 30 component stocks
char_name <- c('AAPL','MSFT','MMM','AXP','BA','CAT','CVX','CSCO','KO','DOW','XOM',
               'WBA','GS','HD','INTC','IBM','JNJ','JPM','MCD','MRK','NKE','PFE','PG',
               'TRV','UNH','UTX','VZ','V','WMT','DIS')

 
 
for(jj in 1:length(char_name)){
  time <- Sys.time()
  char <- char_name[jj] 
  print(char)
  print(Sys.time())
Accuracy_sum2 <- model_sum1<-test_sum1 <-Accuracy_sum_ensem_ELN <-list()

test <- vector()
flag <- vector()
for(i in 1:100){
  test <- try(load(paste0('./result/', char,'_',i,'_eln_daily.rda')),silent=T)
  if(class(test)%in%'try-error') next
  else flag <- c(flag,i) 
  
  try(load(paste0('./result/', char,'_',i,'_eln_daily.rda')),silent=T)
  Accuracy_sum2[[i]] <- Accuracy
  model_sum1[[i]] <- fit.glm
  test_sum1[[i]] <- test_sample
}
Accuracy_sum_ELN <- Accuracy_sum2[flag]
model_sum <- model_sum1[flag]
test_sum <- test_sum1[flag]

  
fit.pre.sum <- fit.pre.sum.prob <- matrix(0,ncol=length(flag),nrow=2000)

  ################################################
  # Ensemble 100 ELN baseline models
  ################################################
# k represents the dataset and j represents the model
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
    
    
    fit.pre.sum[,j] <- predict(model_sum[[j]],as.matrix(test_sum[[k]][,-which(colnames(train_sample)%in%"label")]),type='class',s=model_sum[[j]]$lambda)
    
  }
  

  # for each dataset we use the voting scheme
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
save(Accuracy_sum_ELN,model_sum,test_sum,Accuracy_sum_ensem_ELN,time_ELN,flag, file=paste0('./result/',char,'_eln_ensemble_model_daily.rda'))


}
