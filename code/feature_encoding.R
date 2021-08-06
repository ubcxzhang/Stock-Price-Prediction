### This file is for feature encoding ####
###-------------------------------------------

rm(list=ls())

# Don Jones 30 component stocks
char_name <- c('AAPL','MSFT','MMM','AXP','BA','CAT','CVX','CSCO','KO','DOW','XOM',
               'WBA','GS','HD','INTC','IBM','JNJ','JPM','MCD','MRK','NKE','PFE','PG',
               'TRV','UNH','UTX','VZ','V','WMT','DIS')

for(kk in 1:length(char_name)){

  # setwd("/project/6003851/y2huang/midprice_predict/final_version_2/result")
  library(dbplyr)
  library(data.table)
  library(glmnet)
  #library(e1071)
  library(fdapace)
  library(bit64)

  if(kk>1) rm(list=ls()[-which(ls()%in%c('char_name','kk'))])
  char <- char_name[kk]
  print(char)
  print(Sys.time())
  test <- try(load(paste0('./result/', char,'_final.rda')),silent = T)
  if(class(test)%in%"try-error") next
  
  stock$spread <- stock$Best_Offer_Price-stock$Best_Bid_Price
  stock$Best_Bid_Size <- stock$Best_Bid_Size*100
  stock$Best_Offer_Size <- stock$Best_Offer_Size*100
  
  #################################################################
  # Features Encoding
  #################################################################
  
  # features collections(short term, derived directly from raw (aggregated) data)
  # basic set
  features <- list()
  
  # the original bid/ask prices/ volume
  k <- 5 # we set the level of the variables, we have fixed the interval is 10 events
  
  # variable 1--Prices and volume (1 level)
  features[[1]] <- matrix(cbind(unlist(data.table::shift(stock$Best_Bid_Price,k,type='lag'))),nrow=length(stock$Best_Bid_Price),ncol=1)
  
  features[[2]] <- matrix(cbind(unlist(data.table::shift(stock$Best_Offer_Price,k,type='lag'))),nrow=length(stock$Best_Offer_Price),ncol=1)
  
  features[[3]] <- matrix(cbind(unlist(data.table::shift(stock$Best_Bid_Size,k,type='lag'))),nrow=length(stock$Best_Bid_Size),ncol=1)
  
  features[[4]] <- matrix(cbind(unlist(data.table::shift(stock$Best_Offer_Size,k,type='lag'))),nrow=length(stock$Best_Offer_Size),ncol=1)
  
  features[[5]] <- (unlist(features[[1]])+unlist(features[[2]]))/2
  
  # variable 2--bid-ask spread return
  features[[6]] <-  matrix(cbind(unlist(data.table::shift(stock$spread,k,type='lag'))),nrow=length(stock$spread),ncol=1)/unlist(features[[5]])
  
  # variable 3--variables made up (only for window data)
  features[[7]] <- (unlist(features[[1]])-matrix(data.table::shift(stock$Best_Bid_Price,(2*k-1),type='lag'),nrow=length(stock$Best_Bid_Price),ncol=1))/matrix(data.table::shift(stock$Best_Bid_Price,(2*k-1),type='lag'),nrow=length(stock$Best_Bid_Price),ncol=1)
  
  features[[8]] <- (unlist(features[[2]])-matrix(data.table::shift(stock$Best_Offer_Price,(2*k-1),type='lag'),nrow=length(stock$Best_Offer_Price),ncol=1))/matrix(data.table::shift(stock$Best_Offer_Price,(2*k-1),type='lag'),nrow=length(stock$Best_Offer_Price),ncol=1)
  
  features[[9]] <- (unlist(features[[1]])-matrix(data.table::shift(stock$Best_Offer_Price,(2*k-1),type='lag'),nrow=length(stock$Best_Offer_Price),ncol=1))/matrix(data.table::shift(stock$Best_Offer_Price,(2*k-1),type='lag'),nrow=length(stock$Best_Offer_Price),ncol=1)
           
  features[[10]] <- rowMeans(matrix(cbind(unlist(data.table::shift(as.data.table(stock$Best_Bid_Price),k:(2*k-1),type='lag'))),nrow=length(stock$Best_Bid_Price),ncol=k),na.rm=T)
  
  
  features[[11]] <- rowMeans(matrix(cbind(unlist(data.table::shift(as.data.table(stock$Best_Offer_Price),k:(2*k-1),type='lag'))),nrow=length(stock$Best_Offer_Price),ncol=k),na.rm=T)
  
  features[[12]] <- rowMeans(matrix(cbind(unlist(data.table::shift(as.data.table(stock$midprice),k:(2*k-1),type='lag'))),nrow=length(stock$midprice),ncol=k),na.rm=T)
  
  features[[13]] <- rowSums(matrix(cbind(unlist(data.table::shift(as.data.table(stock$Best_Bid_Size),k:(2*k-1),type='lag'))),nrow=length(stock$Best_Bid_Size),ncol=k),na.rm=T)
  
  
  features[[14]] <- rowSums(matrix(cbind(unlist(data.table::shift(as.data.table(stock$Best_Offer_Size),k:(2*k-1),type='lag'))),nrow=length(stock$Best_Offer_Size),ncol=k),na.rm=T)
          
  features[[15]] <- matrix(cbind(unlist(data.table::shift(as.data.table(features[[5]]),0:9,type='lag'))),nrow=length(stock$Best_Bid_Size),ncol=k)        
  features[[15]] <- apply(features[[15]], 1, sd)
  
  # variable 4--window slope (only for window data) 
  # the problem is how to turn over a gap if a date change, discard the obs that doesn't happend in the same day
  slope1 <-data.frame(date=stock$date,min=stock$min,sec=stock$sec,nano=stock$nanosec)
  min_diff <- as.numeric(diff(slope1$min,lag=(k-1)))
  min_diff[min_diff<0] <- min_diff[min_diff<0]+60
  min_diff[min_diff>15] <- NA
    
  sec_diff <- as.numeric(diff(slope1$sec,lag=(k-1)))
  min_diff[sec_diff<0] <- min_diff[sec_diff<0]-1
  sec_diff[sec_diff<0] <-  sec_diff[sec_diff<0]+60
  
  nano_diff <- as.numeric(diff(slope1$nano,lag=(k-1)))
  sec_diff[nano_diff<0] <- sec_diff[nano_diff<0]-1
  nano_diff[nano_diff<0] <- nano_diff[nano_diff<0]+1000000000
  
  min_diff[min_diff<0] <- NA
  sec_diff[sec_diff<0] <- NA
 
  # in case that there is negative number, we need to discuss in cases
  time_diff <- (min_diff*60*1000000000+sec_diff*1000000000+nano_diff)/1000000000
  
  
  time_diff <- time_diff[1:(length(time_diff)-1)]
  time_diff <- as.numeric(time_diff)
  stock$window_slope <- NA
  stock$window_slope[(k+1):nrow(stock)] <- 1/time_diff
  # since the variable represents the k events that happen before the last window
  # so we need to move the time difference 
  features[[16]] <-matrix(unlist(data.table::shift(as.data.table(stock$window_slope,10,type='lag'))),nrow=length(stock$window_slope),ncol=1)  
  
  
  # variable 5--liquidity in the most recent 1 second
  fre_sec <- stock[,length(midprice),by=c("date","hour","min","sec")][,5]
  fre_sec <- as.vector(data.matrix(fre_sec))
  # fre_sec is the liquidity and also the frequency of each second, but remember that we need to use 
  # the most recent second which is the last second
  features[[17]] <- rep(unlist(data.table::shift(as.data.table(fre_sec),1,type='lag')),fre_sec)
  
  # variable 6-- price and volume derivatives
  dev_bid_min <- stock[,range(Best_Bid_Price)[1],by=c("date","hour","min","sec")][,5]
  dev_bid_max <- stock[,range(Best_Bid_Price)[2],by=c("date","hour","min","sec")][,5]
  features[[18]] <- rep(unlist(data.table::shift(as.data.table(dev_bid_max-dev_bid_min),1,type='lag')),fre_sec)
  
  dev_ask_min <- stock[,range(Best_Offer_Price)[1],by=c("date","hour","min","sec")][,5]
  dev_ask_max <- stock[,range(Best_Offer_Price)[2],by=c("date","hour","min","sec")][,5]
  features[[19]] <- rep(unlist(data.table::shift(as.data.table(dev_ask_max-dev_ask_min),1,type='lag')),fre_sec)
  
  dev_bid_v_min <- stock[,range(Best_Bid_Size)[1],by=c("date","hour","min","sec")][,5]
  dev_bid_v_max <- stock[,range(Best_Bid_Size)[2],by=c("date","hour","min","sec")][,5]
  features[[20]] <- rep(unlist(data.table::shift(as.data.table(dev_bid_v_max-dev_bid_v_min),1,type='lag')),fre_sec)
  
  dev_ask_v_min <- stock[,range(Best_Offer_Size)[1],by=c("date","hour","min","sec")][,5]
  dev_ask_v_max <- stock[,range(Best_Offer_Size)[2],by=c("date","hour","min","sec")][,5]
  features[[21]] <- rep(unlist(data.table::shift(as.data.table(dev_ask_v_max-dev_ask_v_min),1,type='lag')),fre_sec)
  
  dev_mid_min <- stock[,range(midprice)[1],by=c("date","hour","min","sec")][,5]
  dev_mid_max <- stock[,range(midprice)[2],by=c("date","hour","min","sec")][,5]
  features[[22]] <- rep(unlist(data.table::shift(as.data.table(dev_mid_max-dev_mid_min),1,type='lag')),fre_sec)
 
  
  # remove the first k samples that contains NA in fpca because they have the shift
  flag <- 1
  for(i in grep('fpca',colnames(stock))[1]:grep('fpca',colnames(stock))[length(grep('fpca',colnames(stock)))]){
    if(max(which(is.na(stock[[i]])))>flag) flag <- max(which(is.na(stock[[i]])))
  }
  stock <- stock[(flag+1):nrow(stock),]
  
  # try to remove the rows that contain NA, but not necessarily all of them
  flag1 <- vector()
  for(j in 1:ncol(stock)){
    if(length(is.na(stock[[j]]))!=0) flag1 <- c(flag1,which(is.na(stock[[j]])))
  }
  if(length(flag1)>1) stock <- stock[-flag1,]
  
  for(ii in 1:length(features)){
    features[[ii]] <- features[[ii]][(flag+1):length(features[[ii]])]
    features[[ii]] <- features[[ii]][-flag1]
    # features[[ii]] <- features[[ii]][which(1:length(features[[ii]])%%(k)==0)]
  }
  
  
  # the best_bis_best_ask_spread_crossing we will have it as the return percentage, which means 
  # best_bid_best_ask_spread_crossing= (bid_{t-1}-ask_{t-k})/ask_{t-k}
  # we transform that in the first place
  # and we also need to turn bid_ask_spread into a percentage using (ask_{t}-bid_{t})/midprice_{t}


  
  # loop to extract the records of the features
  names_mfeatures <- vector()
  names_mfeatures <- c('bid_price','offer_price','bid_size','offer_size','mid_price',
                       'bid_ask_spread_return',
                       'bid_diff_return','ask_diff_return','bid_ask_sc_return','mean_bid','mean_ask','mean_mid','ask_depth','bid_depth','sd',
                       'window_slope',
                       'arrival_rate',
                       'dev_bid_p','dev_ask_p','dev_bid_v','dev_ask_v','dev_mid_p')
  
 # when you have done the test, should disable the following relocation 
  # setwd("/project/6003851/y2huang/midprice_predict/final_version_2/result_test")
  save(stock,features,names_mfeatures,file=paste0('./result/', char,'_to_sample.rda'))
}

