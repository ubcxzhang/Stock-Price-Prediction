overall <- function(x,y){
  OP <- OR <- OF1 <- matrix(NA,ncol=1,nrow=length(x))
  OP1 <- OR1 <- OF11 <- matrix(NA,ncol=1,nrow=length(y))
  for(t in 1:length(x)){
    if(sum(sapply(x[[t]],is.nan))>0) x[[t]][which(sapply(x[[t]],is.nan))] <- 0
    OP[t] <- sum(as.numeric(x[[t]][3]),as.numeric(x[[t]][2]),as.numeric(x[[t]][1]))/3
    OR[t] <- sum(as.numeric(x[[t]][4]),as.numeric(x[[t]][5]),as.numeric(x[[t]][6]))/3
    OF1[t] <- 2*OP[t]*OR[t]/(OR[t]+OP[t])
     #OF1[t] <- sum(as.numeric(x[[t]][7]),as.numeric(x[[t]][8]),as.numeric(x[[t]][9]))/3
    
    if(sum(sapply(y[[t]],is.nan))>0) y[[t]][which(sapply(y[[t]],is.nan))] <- 0
    OP1[t] <- sum(as.numeric(y[[t]][3]),as.numeric(y[[t]][2]),as.numeric(y[[t]][1]))/3
    OR1[t] <- sum(as.numeric(y[[t]][4]),as.numeric(y[[t]][5]),as.numeric(y[[t]][6]))/3
    OF11[t] <- 2*OP1[t]*OR1[t]/(OR1[t]+OP1[t])
    }
  result_x <- data.frame(OP,OR,OF1)
  result_y <- data.frame(OP1,OR1,OF11)
  if(sum(which(colSums(apply(result_x,1,is.na))>0))!=0){result_y <- result_y[-which(colSums(apply(result_x,1,is.na))>0),] 
  result_x <- result_x[-which(colSums(apply(result_x,1,is.na))>0),]}
  if(sum(which(colSums(apply(result_y,1,is.na))>0))!=0){result_x <- result_x[-which(colSums(apply(result_y,1,is.na))>0),]
  result_y <- result_y[-which(colSums(apply(result_y,1,is.na))>0),]}
  
  return(list(x=result_x,y=result_y))
}

wiltest <- function(x,y,char){
  P_D <- P_S <- P_U <- R_D <- R_S <- R_U <- F1_D <- F1_S <- F1_U <- matrix(NA,ncol=2,nrow=length(x))
  for(t in 1:length(x)){
    P_D[t,1] <- as.numeric(x[[t]][3])
    P_S[t,1] <- as.numeric(x[[t]][2])
    P_U[t,1] <- as.numeric(x[[t]][1])
    R_D[t,1] <- as.numeric(x[[t]][6])
    R_S[t,1] <- as.numeric(x[[t]][5])
    R_U[t,1] <- as.numeric(x[[t]][4])
    F1_D[t,1] <- as.numeric(x[[t]][9])
    F1_S[t,1] <- as.numeric(x[[t]][8])
    F1_U[t,1] <-  as.numeric(x[[t]][7])
    if((P_D[t,1]==0)&&(R_D[t,1]==0)) F1_D[t,1] <- 0
    if((P_S[t,1]==0)&&(R_S[t,1]==0)) F1_S[t,1] <- 0
    if((P_U[t,1]==0)&&(R_U[t,1]==0)) F1_U[t,1] <- 0
    
    P_D[t,2] <- as.numeric(y[[t]][3])
    P_S[t,2] <- as.numeric(y[[t]][2])
    P_U[t,2] <- as.numeric(y[[t]][1])
    R_D[t,2] <- as.numeric(y[[t]][6])
    R_S[t,2] <- as.numeric(y[[t]][5])
    R_U[t,2] <- as.numeric(y[[t]][4])
    F1_D[t,2] <- as.numeric(y[[t]][9])
    F1_S[t,2] <- as.numeric(y[[t]][8])
    F1_U[t,2] <-  as.numeric(y[[t]][7])
    if((P_D[t,2]==0)&&(R_D[t,2]==0)) F1_D[t,2] <- 0
    if((P_S[t,2]==0)&&(R_S[t,2]==0)) F1_S[t,2] <- 0
    if((P_U[t,2]==0)&&(R_U[t,2]==0)) F1_U[t,2] <- 0
   
  }
  
  
  
   if(sum(which(colSums(apply(P_D,1,is.na))!=0))!=0){
                 P_D <- P_D[-which(colSums(apply(P_D,1,is.na))!=0),]} 
   if(sum(which(colSums(apply(P_S,1,is.na))!=0))!=0){
                 P_S <- P_S[-which(colSums(apply(P_S,1,is.na))!=0),]}
   if(sum(which(colSums(apply(P_U,1,is.na))!=0))!=0){
                  P_U <- P_U[-which(colSums(apply(P_U,1,is.na))!=0),]} 
   if(sum(which(colSums(apply(R_D,1,is.na))!=0))!=0){
     R_D <- R_D[-which(colSums(apply(R_D,1,is.na))!=0),]} 
   if(sum(which(colSums(apply(R_S,1,is.na))!=0))!=0){
     R_S <- R_S[-which(colSums(apply(R_S,1,is.na))!=0),]} 
   if(sum(which(colSums(apply(R_U,1,is.na))!=0))!=0){
                  R_U <- R_U[-which(colSums(apply(R_U,1,is.na))!=0),]}
   if(sum(which(colSums(apply(F1_D,1,is.na))!=0))!=0){
                   F1_D <- F1_D[-which(colSums(apply(F1_D,1,is.na))!=0),]} 
   if(sum(which(colSums(apply(F1_S,1,is.na))!=0))!=0){
                   F1_S <- F1_S[-which(colSums(apply(F1_S,1,is.na))!=0),]} 
   if(sum(which(colSums(apply(F1_U,1,is.na))!=0))!=0){
                    F1_U <- F1_U[-which(colSums(apply(F1_U,1,is.na))!=0),]} 
    
   
  
  #Precision
  # under paired situation, x-y by default, if p<0.05 means reject H0 
  # H0:x>=y, H1:x<y, p<0.05 reject H0
  result1 <-wilcox.test(P_U[,1], y = P_U[,2],
                    alternative = c("two.sided"),
                    exact=F, paired = TRUE,na.omit=T)$p.value
  
  result2 <- wilcox.test(P_S[,1], y = P_S[,2],
                     alternative = c("two.sided"),
                     exact=F, paired = TRUE,na.omit=T)$p.value
  
  result3 <-wilcox.test(P_D[,1], y = P_D[,2],
                    alternative = c("two.sided"),
                    exact=F, paired = TRUE,na.omit=T)$p.value
  
  result4 <-wilcox.test(overall(x,y)$x[,1], overall(x,y)$y[,1],
                         alternative = c("two.sided"),
                         exact=F, paired = TRUE,na.omit=T)$p.value 
  
  #recall
  
  result5 <-wilcox.test(R_U[,1], y = R_U[,2],
                    alternative = c("two.sided"),
                    exact=F, paired = TRUE,na.omit=T)$p.value
  
  result6 <-wilcox.test(R_S[,1], y = R_S[,2],
                    alternative = c("two.sided"),
                    exact=F, paired = TRUE,na.omit=T)$p.value
  
  result7 <-wilcox.test(R_D[,1], y = R_D[,2],
                    alternative = c("two.sided"),
                    exact=F, paired = TRUE,na.omit=T)$p.value
  
  result8 <-wilcox.test(overall(x,y)$x[,2], overall(x,y)$y[,2],
                         alternative = c("two.sided"),
                         exact=F, paired = TRUE,na.omit=T)$p.value 
  
  #F_1 score
  result9 <-wilcox.test(F1_U[,1], y = F1_U[,2],
                    alternative = c("two.sided"),
                    exact=F, paired = TRUE,na.omit=T)$p.value
  
  result10 <-wilcox.test(F1_S[,1], y = F1_S[,2],
                    alternative = c("two.sided"),
                    exact=F, paired = TRUE,na.omit=T)$p.value
  
  result11 <-wilcox.test(F1_D[,1], y = F1_D[,2],
                    alternative = c("two.sided"),
                    exact=F, paired = TRUE,na.omit=T)$p.value 
  
  
  result12 <-wilcox.test(overall(x,y)$x[,3], overall(x,y)$y[,3],
                        alternative = c("two.sided"),
                        exact=F, paired = TRUE,na.omit=T)$p.value 
  
  # print(paste0("The Wilcoxon signed rank test between ",substitute(x)," and ",substitute(y)," of ", char))
  #result <- matrix(0,nrow=4,ncol=3)
  result <- matrix(as.vector(sapply(paste0('result',1:12),function(x){get0((as.character(x)))})),nrow=4,ncol=3)
  # colnames(result) <- c('Percision','Recall','F1_Score')
  # rownames(result) <- c('Upwards','Stationary','Downwards','Gerneral')
  # print(result)
  return(result)
}

##############use the parameters to evaluate the significance of the vars#############
# combine the 100 models and summary the number of times of its occurances and their parameters
# this is the full model
parameter <- function(modelsm,testsm,num,char){
  
  Dfull_model_significance <- matrix(NA,ncol=2)
  Sfull_model_significance <- matrix(NA,ncol=2)
  Ufull_model_significance <- matrix(NA,ncol=2)
  for(i in 1:length(modelsm)){
    if(i==1){Dfull_model_significance <- summary(modelsm[[1]]$beta$D)[c("i","x")]}
    else Dfull_model_significance <- rbind(Dfull_model_significance,summary(modelsm[[i]]$beta$D)[c("i","x")])
    if(i==1){Sfull_model_significance <- summary(modelsm[[1]]$beta$S)[c("i","x")]}
    else Sfull_model_significance <- rbind(Sfull_model_significance,summary(modelsm[[i]]$beta$S)[c("i","x")])
    if(i==1){Ufull_model_significance <- summary(modelsm[[1]]$beta$U)[c("i","x")]}
    else Ufull_model_significance <- rbind(Ufull_model_significance,summary(modelsm[[i]]$beta$U)[c("i","x")])
  }
  
  
  # calculate the parameters
  # ATTENTION! the parameters need to change if the criterion change
  paragenerator <- data.frame(Dfull_model_significance,trend=rep('D', nrow(Dfull_model_significance)))
  paragenerator <- rbind(paragenerator,cbind(Sfull_model_significance,trend=rep('S',nrow(Sfull_model_significance))))
  paragenerator <- rbind(paragenerator,cbind(Ufull_model_significance,trend=rep('U',nrow(Ufull_model_significance))))
  paragenerator_agg <- aggregate(x~.,data=paragenerator,mean)
  
  result <- list()
  # count the parameters
  print(paste0("The parameters/coefficients of ",char," > ",num, " are:"))
  print('--------------Stationary---------------')
  print(colnames(testsm[[1]])[which(as.numeric(table(Sfull_model_significance$i))>num)])
  print(as.numeric(table(Sfull_model_significance$i))[which(as.numeric(table(Sfull_model_significance$i))>num)])
  print(paragenerator_agg[paragenerator_agg$trend=='S',][which(as.numeric(table(Sfull_model_significance$i))>num),3])
  result[['S']] <- list(var=colnames(testsm[[1]])[which(as.numeric(table(Sfull_model_significance$i))>num)],
                        freq=as.numeric(table(Sfull_model_significance$i))[which(as.numeric(table(Sfull_model_significance$i))>num)],
                        coef=paragenerator_agg[paragenerator_agg$trend=='S',][which(as.numeric(table(Sfull_model_significance$i))>num),3])
  
  
  print('---------------Downwards----------------')
  print(colnames(testsm[[1]])[which(as.numeric(table(Dfull_model_significance$i))>num)])
  print(as.numeric(table(Dfull_model_significance$i))[which(as.numeric(table(Dfull_model_significance$i))>num)])
  print(paragenerator_agg[paragenerator_agg$trend=='D',][which(as.numeric(table(Dfull_model_significance$i))>num),3])
  
  result[['D']] <- list(var=colnames(testsm[[1]])[which(as.numeric(table(Dfull_model_significance$i))>num)],
                        freq=as.numeric(table(Dfull_model_significance$i))[which(as.numeric(table(Dfull_model_significance$i))>num)],
                        coef=paragenerator_agg[paragenerator_agg$trend=='D',][which(as.numeric(table(Dfull_model_significance$i))>num),3])
  
  print('----------------Upwards-----------------')
  print(colnames(testsm[[1]])[which(as.numeric(table(Ufull_model_significance$i))>num)])
  print(as.numeric(table(Ufull_model_significance$i))[which(as.numeric(table(Ufull_model_significance$i))>num)])
  print(paragenerator_agg[paragenerator_agg$trend=='U',][which(as.numeric(table(Ufull_model_significance$i))>num),3])
  result[['U']] <- list(var=colnames(testsm[[1]])[which(as.numeric(table(Ufull_model_significance$i))>num)],
                        freq=as.numeric(table(Ufull_model_significance$i))[which(as.numeric(table(Ufull_model_significance$i))>num)],
                        coef=paragenerator_agg[paragenerator_agg$trend=='U',][which(as.numeric(table(Ufull_model_significance$i))>num),3])
  
  return(result)
  
}


# write a function to compute the accuracy
# after updating F_test_temp, re-distribute
getAccuracy <- function(char,x,i,cha,index,cutoff1){
  var_args <- list(char,x,i,cha,index,cutoff1)
  F_test_temp <- x
  for(tt in 1:ncol(F_test_temp)){
    if(sum(is.infinite(F_test_temp[,tt]))>0) F_test_temp[which(is.infinite(F_test_temp[,tt])),tt] <- 0
    if(sum(is.na(F_test_temp[,tt]))>0) F_test_temp[which(is.na(F_test_temp[,tt])),tt] <- 0
    if(sum(is.nan(F_test_temp[,tt]))>0) F_test_temp[which(is.nan(F_test_temp[,tt])),tt] <- 0
  }
  
  test_sample <- F_test_temp[index==1,]
  train_sample <- F_test_temp[index!=1,]
  
  index_t <- createFolds(train_sample$label,5,list=F)
  
  cvfit <- cv.glmnet2(data.matrix(train_sample[,-which(colnames(train_sample)%in%"label")]),train_sample$label,nfolds=5,family='multinomial',
                      type.measure = 'class',foldid=index_t,alpha=seq(5,10,by=1)/10,lambda=exp(seq(log(10^-8), log(5), length.out=100)),maxit=10000)
  # there is some NA in the relevant sd variable should re-calculate it
  
  fit.glm <- glmnet(data.matrix(train_sample[,-which(colnames(train_sample)%in%"label")]),train_sample$label,lambda=cvfit$lambda.1se,alpha=cvfit$alpha,family='multinomial')
  
  fit.pre1 <- predict(fit.glm,as.matrix(test_sample[,-which(colnames(train_sample)%in%"label")]),type="response",s=cvfit$lambda.1se)
  
  fit.pre1 <- matrix(fit.pre1,ncol=3,nrow=nrow(test_sample))
  
  if(length(which(is.na(fit.pre1)))>0) fit.pre1 <- fit.pre1[-which(is.na(fit.pre1)),]
  if(length(which(is.null(fit.pre1)))>0) fit.pre1 <- fit.pre1[-which(is.null(fit.pre1)),]
  
  if(!is.null(var_args[['cutoff1']])){
  test <- try(fit.pre <- apply(fit.pre1,1,function(x,value=cutoff1)
  {if(x[2]>value) return('S')
    else if(x[1]>x[3]) return('D')
    else return('U')}),silent = T)
  if(class(test)[1]%in%"try-error") {print('error') 
    fit.pre <- predict(fit.glm,as.matrix(test_sample[,-which(colnames(train_sample)%in%"label")]),type="class",s=cvfit$lambda.1se)}
  }
  else fit.pre <- predict(fit.glm,as.matrix(test_sample[,-which(colnames(train_sample)%in%"label")]),type="class",s=cvfit$lambda.1se)
  
  
  if(sum(fit.pre=='U',na.rm=T)==0) P_U <- 0 else P_U  <- length(intersect(which(fit.pre=='U'),which(test_sample$label=='U')))/sum(fit.pre=='U',na.rm=T)
  if(sum(fit.pre=='S',na.rm=T)==0) P_S <- 0 else P_S  <- length(intersect(which(fit.pre=='S'),which(test_sample$label=='S')))/sum(fit.pre=='S',na.rm=T)
  if(sum(fit.pre=='D',na.rm=T)==0) P_D <- 0 else P_D  <- length(intersect(which(fit.pre=='D'),which(test_sample$label=='D')))/sum(fit.pre=='D',na.rm=T)
  R_U  <- length(intersect(which(fit.pre=='U'),which(test_sample$label=='U')))/sum(test_sample$label=='U',na.rm=T)
  R_S  <- length(intersect(which(fit.pre=='S'),which(test_sample$label=='S')))/sum(test_sample$label=='S',na.rm=T)
  R_D  <- length(intersect(which(fit.pre=='D'),which(test_sample$label=='D')))/sum(test_sample$label=='D',na.rm=T)
  F1_U  <- 2*P_U *R_U /(P_U +R_U )
  F1_S  <- 2*P_S *R_S /(P_S +R_S )
  F1_D  <- 2*P_D *R_D /(P_D +R_D )
  Accuracy <- list(P_U,P_S,P_D,R_U,R_S,R_D,F1_U,F1_S,F1_D)
  save(test_sample,train_sample,fit.glm,Accuracy,cvfit,file=paste0(char,'_',i,'_model_',cha,'.rda'))
  return(Accuracy)
}

delete <- function(x,y){
  delete5 <- c(x[1],x[1:(length(x)-1)])
  if(sum((x/delete5>1.5)|(x/delete5<0.5))==0) return(y)
  else {
    test <- c(which((x/delete5>1.5)|(x/delete5<0.5))[1],which((x/delete5>1.5)|(x/delete5<0.5))[1]-1)
    y <- y[-test,]
  x <- x[-test]
  return(delete(x,y))
  }
}

plot.FPCA1 <- function (x, openNewDev = FALSE, addLegend = TRUE, ...) 
{
  fpcaObj <- x
  oldPar <- par(no.readonly = TRUE)
  if (any(oldPar[["pin"]] < 0)) {
    stop("Figure margin too large")
  }
  else {
    on.exit(par(oldPar))
  }
  if (!"FPCA" %in% class(fpcaObj)) {
    stop("Input class is incorrect; plot.FPCA() is only usable for FPCA objects.")
  }
  else {
    t = fpcaObj$inputData$Lt
    if (openNewDev) {
      dev.new(width = 6.2, height = 6.2, noRStudioGD = TRUE)
    }
    fves = fpcaObj$cumFVE
    mu = fpcaObj$mu
    obsGrid = fpcaObj$obsGrid
    workGrid = fpcaObj$workGrid
    par(mfrow = c(2, 2))
    CreateDesignPlot(t, addLegend = addLegend, noDiagonal = fpcaObj$optns$error)
    plot(workGrid, mu, type = "l", xlab = "time", ylab = "", 
         main = "Mean Function", panel.first = grid(), axes = TRUE)
    CreateScreePlot(fpcaObj)
    K = ncol(fpcaObj$phi)
    k = 1
    if (K > 3) {
      k = 3
    }
    else {
      k = K
    }
    if (addLegend) {
      newplt <- par()[["plt"]]
      newplt[2] <- newplt[1] + 0.85 * (newplt[2] - newplt[1])
      par(plt = newplt)
    }
    # matplot(workGrid, fpcaObj$phi[, 2:k], type = "n", main = paste(collapse = "", 
    #                                                                c("First ", as.character(k), " Eigenfunctions")), 
    #         xlab = "time", ylab = "")
    # 
    # par(new=T)
    # plot(c(workGrid[1],workGrid[length(workGrid)]), c(min(fpcaObj$phi[, 1]),max(fpcaObj$phi[, 1])),type='n',ylab='',xlab='',axes=F)
    # lines(workGrid, fpcaObj$phi[,1],type='l')
    # axis(side=4)
    # abline(h = 0, col = "gray9")
    # grid()
    # matlines(workGrid, fpcaObj$phi[, 1:k])
    # pars <- par()
    # if (addLegend) {
    #   legend("right", col = 1:k, lty = 1:k, legend = do.call(expression, 
    #                                                          sapply(1:k, function(u) return(bquote(phi[.(u)])))), 
    #          border = FALSE, xpd = TRUE, inset = -pars[["mai"]][4]/pars[["pin"]][1] * 
    #            1.8, seg.len = 1.2)
    # }
    #par(mai=c(0.5,0.5,0.5,0.5))
    matplot(workGrid, fpcaObj$phi[, 2:3], type = "n", main = paste(collapse = "", 
                                                                   c("First ", as.character(3), " Eigenfunctions")), 
            xlab = "time", ylab = "",las=1)
    matlines(workGrid, fpcaObj$phi[, 1:3],type=c('n','l','l'),col=c('red','dark blue','blue'),las=1)
    axis(side=2,col = "blue",col.axis="blue",las=1)
    pars <- par()
    
    par(new=T)
    plot(c(workGrid[1],workGrid[length(workGrid)]), c(min(fpcaObj$phi[, 1]),max(fpcaObj$phi[, 1])),type='n',ylab='',xlab='',axes=F)
    axis(side=4,col = "red",col.axis="red",col.ticks = "red",las=1)
    lines(workGrid, fpcaObj$phi[,1],type='l',col='red')
    
    if (addLegend) {
      legend("bottom",ncol=3, col = c('red','dark blue','blue'), lty = 1:k, legend = do.call(expression,sapply(1:k, function(u) return(bquote(phi[.(u)])))), 
             border = FALSE, xpd = T, seg.len = 1, inset = -pars[["mai"]][4]/pars[["pin"]][1] * 3.2,bty='n')}
    abline(h = 0, col = "gray9")
    grid()
    
  }
}

