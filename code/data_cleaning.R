### This file is for basis data cleaning ####
###-------------------------------------------

rm(list=ls())
# set path
setwd("/project/6003851/y2huang/midprice_predict/final_version_2")
# load packages and sel-defined functions
library(dbplyr)
library(data.table)
library(glmnet)
library(fdapace)
library(bit64)
library(reshape2)
library(ggplot2)
library(graphics)
source('wiltest.r')

# Don Jones 30 component stocks
char_name <- c('AAPL','MSFT','MMM','AXP','BA','CAT','CVX','CSCO','KO','DOW','XOM',
               'WBA','GS','HD','INTC','IBM','JNJ','JPM','MCD','MRK','NKE','PFE','PG',
               'TRV','UNH','UTX','VZ','V','WMT','DIS')

for(ii in 1:length(char_name)){
  setwd("/project/6003851/y2huang/midprice_predict/final_version_2")
  # decide which stock to read
  char <- char_name[ii]
  print(char)
  print(Sys.time())

  # show the digits for the variable 'Time'
  stock <- try(fread(file=paste0('/project/6003851/SharedData/NYSE16/GroupingResult/NBBO/EQY_US_ALL_NBBO_',char,'.txt'),sep='|',header=F),silent=T)
  # if(class(test)[1]%in%'try-error') next
  # stock <- fread(file=paste0('/project/6003851/SharedData/NYSE16/GroupingResult/NBBO/EQY_US_ALL_NBBO_',char,'.txt'),sep='|',header=F)
  
  colnames(stock) <-c('Time','Exchange','Symbol','Bid_Price','Bid_Size','Offer_Price','Offer_Size','Quote_Condition','Best_Bid_Exchange','Best_Bid_Price','Best_Bid_Size','Best_Offer_Price','Best_Offer_Size','Best_Offer_FINRA_Market_Maker_ID')
  str(stock)
  # remove the columns that are useless
  Symbol <- stock$Symbol[1]
  stock <- subset(stock,select=-c(Exchange,Best_Bid_Exchange,Best_Offer_FINRA_Market_Maker_ID,Quote_Condition,Symbol,Bid_Price,Bid_Size,Offer_Price,Offer_Size))
  
  # try to seperate time zoom into individual counting
  # notice that there is no specific date in the data, try to create date
  stock$hour <- stock$Time%/%10^13
  stock$min <- (stock$Time-stock$hour*10^13)%/%10^11
  stock$sec <- (stock$Time-stock$hour*10^13-stock$min*10^11)%/%10^9
  stock$nanosec <- stock$Time-stock$hour*10^13-stock$min*10^11-stock$sec*10^9
  stock$date <- NA
  
  # notice that MFST, GOOG, FB has 64 days, AAPL has 65 days, so we decide to make all the stocks to 64 days
  stock <- subset(stock,select=-Time)
  
  
  # delete first two rows for calculating the date
  # !!ATTENTION: should vary for different stock 
  # stock <- stock[-c(1,2),]
  # add the date variable
  load('date.rda')
  indicator <- stock$hour[2:nrow(stock)]/stock$hour[1:(nrow(stock)-1)]
  fre <- diff(c(0,which(indicator<1),nrow(stock)))
  if(length(fre)<64) next
  # decide to make all the stocks 64 days, from 2nd June to 31st August
  if(length(fre)==65){stock <- stock[-c(1:fre[1]),]
  Date <- Date[2:length(Date)]
  fre <- fre[2:length(fre)]
  stock$date <- rep(Date,fre)
  }else{Date <- Date[2:length(Date)]
  stock$date <- rep(Date,fre)}
  
  
  #1 Eliminate records beyond the exchange opening time, which is, from 9:30 am to 4 pm
  #Since this is not time type data, 9:00 is hard to satisfied, break it into two steps
  if(length(which(stock$hour<9))) stock <- stock[-which(stock$hour<9),]
  if(length(which((stock$hour==9)&&(stock$min<30)))>0) stock <- stock[-which((stock$hour==9)&&(stock$min<30)),]
  if(length(which(stock$hour>16))>0) stock <- stock[-which(stock$hour>16),]
  if(length(which((stock$hour==16)&&(stock$min>0)))>0) stock <- stock[-which((stock$hour==16)&&(stock$min>0)),]
  
  #2 Eliminate quotes with negative price or size or with bid price greater than ask price
  if(length(which(stock$Best_Bid_Size<0|stock$Best_Offer_Size<0))>0) stock <- stock[-which(stock$Best_Bid_Size<0|stock$Best_Offer_Size<0),]
  if(length(which(stock$Best_Bid_Price<=0|stock$Best_Offer_Price<=0))>0) stock <- stock[-which(stock$Best_Bid_Price<=0|stock$Best_Offer_Price<=0),]
  
  #3 Eliminate trades with prices more than (less than) 150% (50%) of the previous trade price
  stock <- delete(stock$Best_Bid_Price,stock)
  stock <- delete(stock$Best_Offer_Price,stock)
  
  #4 Exclude quotes when the quoted spread is greater than 25% of the quote midpoint 
  # or when the ask price is more than 150% of the bid price
  #create variable midprice for later analysis
  stock$midprice <- (stock$Best_Offer_Price+stock$Best_Bid_Price)/2
  if(length(which((stock$Best_Offer_Price-stock$Best_Bid_Price)>(stock$midprice*0.25)))>0) stock <- stock[-which((stock$Best_Offer_Price-stock$Best_Bid_Price)>(stock$midprice*0.25)),]
  if(length(which(stock$Best_Offer_Price>1.5*stock$Best_Bid_Price))>0) stock <- stock[-which(stock$Best_Offer_Price>1.5*stock$Best_Bid_Price),]
  
  
  
  ### we want to compare the difference between the FPCAs under two definitions ###
  ###--------------------------------------------
  
  # longterm variable 1: daily FPCA under method1
  longterm <- aggregate(midprice~date+hour,data=stock,mean)
  longterm <- longterm[order(longterm$date),]
  
  # use longterm minus its group average
  # longterm$midprice <- longterm$midprice-rep(aggregate(midprice~date+hour,data=longterm,mean)[,2],as.numeric(table(longterm$date)))
  longterm$month <-substr(longterm$date,5,6) 
  
  Flies <- MakeFPCAInputs(longterm$date, longterm$hour, longterm$midprice)
  fpcaObjFlies <- FPCA(Flies$Ly, Flies$Lt)
  
  # to ensure FPCA can explain over 99%
  num <- min(which(cumsum(fpcaObjFlies$lambda/sum(fpcaObjFlies$lambda))>0.99))
  
  # create FPCA that account for 99% of the variances
  for(t in 1:num){
    fpca.test <- vector()
    
    for(i in 1:length(fre)){
      if(i==1){fpca.test[i]<- NA}
      else if(length(Flies$Lt[[i-1]])<8){
        ord<- Flies$Lt[[i-1]]
        fpca.test[i] <- Flies$Ly[[i-1]]%*%fpcaObjFlies$phi[,t][unlist(lapply(as.numeric(ord),function(x){which(grepl(x,c(9:16)))}))]
        
      }
      else {fpca.test[i] <- Flies$Ly[[i-1]]%*%fpcaObjFlies$phi[,t]
      }
    }
    
    # stock is a data.table, so it's different to assign it a variable
    stock[,paste0("daily_m1_fpca",t)] <- rep(fpca.test,as.numeric(table(stock$date)))
}
  
  
  ##########################################################
  # longterm variable 2-----------------------------------
  # longterm1 <- melt(longterm,id.vars=c('date','hour','midprice'),value.name='midprice',factorsAsStrings = F)
  # construct a new longterm data.frame for FPCA for a week lag
  # discard the first 5 bday, every of 59*40 date has a length of 40 obs
  # ATTENTION! if we change file 'Date', we have to rewrite the number here
  longterm2 <- split(longterm$midprice,longterm$date)
  
  # use an NA to substitute the missing value and use 'na.rm' in the next step
  for(t in 1:length(longterm2)){
    if(length(longterm2[[t]])!=8){
      num <- as.numeric(longterm$hour[which(longterm$date==names(longterm2)[t])])
      temp <- rep(NA,8)
      temp[c(9:16)%in%num] <- longterm2[[t]]
      longterm2[[t]] <- temp
    }
  }
  # price is a variable that store all the hourly price and use NA as the missing values
  price <- as.vector(unlist(longterm2))
  longterm2 <- data.frame(hour=rep(1:40,59))
  longterm2$date <- rep(Date[6:64],each=40)
  for(i in 1:59){
    xname <- unique(longterm2$date)[i]
    xorder <- which(Date==xname)-1
    longterm2$midprice[which(longterm2$date==xname)] <- price[((xorder-5)*8+1):(xorder*8)]
  }
  
  
  # use longterm minus its mean function
  # detrend it using the mean of every 40hrs, using mean is much better than median
  # in the meeting2 Prof.Zhang said we only need to detrend the daily trend and repeat it for a week instead of a weekly trend
  # ATTENTION! Here we use longterm's midprice, so everything changed in the last variable is related to the next
  
  longterm2$midprice <- longterm2$midprice-rep(aggregate(midprice~hour,data=longterm,mean)[,2],295)
  
  # find put that it might need 5FPCs to explain the variance
  Flies2 <- MakeFPCAInputs(longterm2$date, longterm2$hour, longterm2$midprice,na.rm=T)
  fpcaObjFlies2 <- FPCA(Flies2$Ly, Flies2$Lt)
  
  # to ensure FPCA can explain over 99%
  num <- min(which(cumsum(fpcaObjFlies2$lambda/sum(fpcaObjFlies2$lambda))>0.99))
  
  # create FPCA that account for 99% of the variances
  # ATTENTION! we no longer use the "i-1" as the "daily_m1_fpca"s is because that when we construct
  # the variable longterm2, it contains the weekly lag
  for(t in 1:num){
    fpca.test <- vector()
    
    for(i in 1:(length(fre)-5)){
      if(length(Flies2$Lt[[i]])<40){
        fpca.test[i] <- Flies2$Ly[[i]]%*%fpcaObjFlies2$phi[,t][Flies2$Lt[[i]]]  
      }
      else {fpca.test[i] <- Flies2$Ly[[i]]%*%fpcaObjFlies2$phi[,t]
      }
    }
    
    # stock is a data.table, so it's different to assign it a variable
    stock[,paste0("weekly_m1_fpca",t)] <- c(rep(NA,sum(as.numeric(table(stock$date))[1:5])),rep(fpca.test,as.numeric(table(stock$date))[6:64]))
  }
  
  
  ##########################################################
  # longterm variable 3-----------------------------------
  # using the method 2
  # longterm variable 3 (daily)
  longterm3 <- aggregate(midprice~date+hour,data=stock,mean)
  longterm3 <- longterm3[order(longterm3$date),]
  var3 <- vector()
  for(i in 1:((length(table(longterm3$date))*8-8))){
    var3[((i-1)*8+1):((i-1)*8+8)] <- window(price,start=i,end=(i+7),frequency=1)
  }
  longterm3d <- data.frame(midprice=var3,obs=rep(1:8,(length(table(longterm3$date))*8-8)))
  longterm3d$date <- rep(names(table(longterm3$date))[2:64],each=64)
  longterm3d$hr <- rep(rep(9:16,each=8),63)
  longterm3d$id <- rep(1:(63*8),each=8)
  

  Flies3 <- MakeFPCAInputs(longterm3d$id, longterm3d$obs, longterm3d$midprice,na.rm=T)
  fpcaObjFlies3 <- FPCA(Flies3$Ly, Flies3$Lt, list(plot = F, methodMuCovEst = 'smooth', userBwCov = 2))
  
  # to ensure FPCA can explain over 99%
  num <- min(which(cumsum(fpcaObjFlies3$lambda/sum(fpcaObjFlies3$lambda))>0.99))
  
  # create FPCA that account for 99% of the variances
  for(t in 1:num){
    fpca.test <- vector()
    
    for(i in 1:length(Flies3$Ly)){
      if(i==1){fpca.test[i]<- NA}
      else{ 
        if(length(Flies3$Lt[[i]])<8){
        fpca.test[i] <- Flies3$Ly[[i]]%*%fpcaObjFlies3$phi[,t][Flies3$Lt[[i]]]
      }
      else {fpca.test[i] <- Flies3$Ly[[i]]%*%fpcaObjFlies3$phi[,t]}
        }
    }
    # stock is a data.table, so it's different to assign it a variable
    stock[,paste0("daily_m2_fpca",t)] <- c(rep(NA,sum(as.numeric(table(stock$date))[1])),rep(fpca.test,as.vector(table(stock$hour,stock$date))[9:(64*8)]))
  }
  
  
  
  ##########################################################
  # longterm4d variable 4-------------------------------
  longterm4d <- aggregate(midprice~date+hour,data=stock,mean)
  longterm4d <- longterm4d[order(longterm4d$date),]
  
  var4 <- vector()
  for(i in 1:((length(table(longterm4d$date))*8-40))){
    var4[((i-1)*40+1):((i-1)*40+40)] <- window(price,start=i,end=(i+39),frequency=1)
  }
  
  longterm4d <- data.frame(midprice=var4,obs=rep(1:40,(length(table(longterm4d$date))*8-40)))
  longterm4d$date <- rep(names(table(longterm4d$date))[6:64],each=320)
  longterm4d$hr <- rep(rep(9:16,each=40),59)
  longterm4d$id <- rep(1:472,each=40)
  
  # detrending
  # detrend the repeated daily curve (mean function) of variable 3 
  daily_trend3 <-aggregate(midprice~obs,data=longterm3d,mean)[,2]
  longterm4d$midprice <- longterm4d$midprice-rep( daily_trend3,(nrow(longterm4d)/8))
  
  Flies4 <- MakeFPCAInputs(longterm4d$id, longterm4d$obs, longterm4d$midprice,na.rm=T)
  fpcaObjFlies4 <- FPCA(Flies4$Ly, Flies4$Lt, list(plot = F, methodMuCovEst = 'smooth', userBwCov = 2))
  
  # to ensure FPCA can explain over 99%
  num <- min(which(cumsum(fpcaObjFlies4$lambda/sum(fpcaObjFlies4$lambda))>0.99))
  
  # create FPCA that account for 99% of the variances
  # ATTENTION! we no longer use the "i-1" as the "daily_m1_fpca"s is because that when we construct
  # the variable longterm2, it contains the weekly lag
  for(t in 1:num){
    fpca.test <- vector()
    
    for(i in 1:length(Flies4$Ly)){
      if(length(Flies4$Lt[[i]])<40){
        fpca.test[i] <- Flies4$Ly[[i]]%*%fpcaObjFlies4$phi[,t][Flies4$Lt[[i]]]  
      }
      else {fpca.test[i] <- Flies4$Ly[[i]]%*%fpcaObjFlies4$phi[,t]
      }
    }
    
    # stock is a data.table, so it's different to assign it a variable
    stock[,paste0("weekly_m2_fpca",t)] <- c(rep(NA,sum(as.numeric(table(stock$date))[1:5])),rep(fpca.test,as.vector(table(stock$hour,stock$date))[(5*8+1):(64*8)]))
  }
  
  
  setwd("/project/6003851/y2huang/midprice_predict/final_version_2/result")
  save(stock,file=paste0(char,'_final.rda'))
  rm(stock)
}
