# This is for generating graphs that show the comparisons between different experimental designs
#getwd()
rm(list=ls())

source('./code/wiltest.r')
library(Matrix)
library(stringr)
library(ggplot2)

options(digits = 6)


warnings('off')


# Don Jones 30 component stocks
char_name <- c('AAPL','MSFT','MMM','AXP','BA','CAT','CVX','CSCO','KO','DOW','XOM',
               'WBA','GS','HD','INTC','IBM','JNJ','JPM','MCD','MRK','NKE','PFE','PG',
               'TRV','UNH','UTX','VZ','V','WMT','DIS')

###################illstration of DJ 30 index daily trend#######################
daily <- read.csv("./rda/dow_jones30_daily.csv", stringsAsFactors=F)
daily$Adj.Close <- as.numeric(gsub(",","", daily$Adj.Close))
daily$Date <- as.Date(daily$Date, format="%d-%b-%y")

pdf(file='./figure/dj30.pdf',width=10,height=7)
dj30 <- ggplot(daily,aes(Date,Adj.Close)) + 
  geom_line(aes(color="red")) +
  # ggtitle("Closing Stock Price: Dow Jones 30 Index") + 
  theme(plot.title = element_text(face='bold', colour='black', hjust=0.5, size=24))+
  theme(axis.title.x = element_text())+
  theme(axis.title.y = element_text())+
  ylab("Closing Stock Price (Currency in USD)")+
  xlab("Date")+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "light gray",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "light gray"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "light gray"), 
        axis.text.y= element_text(angle=0, hjust=1, face = "bold",size=14),
        axis.text.x = element_text(face = "bold", angle=45,size=16),
        axis.ticks = element_blank(),
        axis.title=element_text(size=18))+
        theme(legend.position="none")
dj30
dev.off()

# strip.text.y = element_text(size = 30, colour = "black", angle = -90, face = "bold", hjust=0.5),



###################result: picture 1#######################

# baseline model——includes all the high frequency and 'within-window' vars (Strategy I and Strategy III)
# full-ensemble model——ensemble baseline models
# nofpca model——exclude FPCA vars from baseline model
# nowin model ——exclude 'within-window' vars from baseline model

# what to compare
# baseline SVM model v.s. full ensemble model
# baseline SVM model v.s. nofpca model
# baseline SVM model v.s. ts model

# setwd(file.path(path0,'result'))
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


pos1 <- which(levels(Result$stock)%in%char_name[which(p.adjust(unlist(test1), method='fdr')>0.05)])
pos2 <- which(levels(Result$stock)%in%char_name[which(p.adjust(unlist(test2), method='fdr')>0.05)])
pos3 <- which(levels(Result$stock)%in%char_name[which(p.adjust(unlist(test3), method='fdr')>0.05)])



# y='Difference in F1 score (original - without within-window vars)',
# #9ECAE1"
color1 <- c(rep("#9ECAE1",90))
color2 <- c(rep("#7EC0EE",90))
# alternative='greater'
color1[c(75, 78,81,22, 23, 24, 26, 27, 28, 29, 30, 31, 32, 46, 33, 34, 35, 36, 38, 39, 40, 41, 42, 45, 47, 48, 49, 51, 52, 53)] <- "#ADADAD"
color2[c(75, 78,81,22, 23, 24, 26, 27, 28, 29, 30, 31, 32, 46, 33, 34, 35, 36, 38, 39, 40, 41, 42, 45, 47, 48, 49, 51, 52, 53)] <- "#A8A8A8"
# try to combine the three plots together

# Result$type <- as.factor(str_c(Result$diff,Result$stock))
Result$type <- str_c(Result$diff,Result$stock)

# remove outliers
# "F1 diff: Ensemble", "F1 diff: FPCA", "F1 diff: Within-window vars"
r1 <- union(which(Result[Result$diff=="F1 improvement: Strategy II",]$value>quantile(Result[Result$diff=="F1 improvement: Strategy II",]$value, 0.975)), which(Result[Result$diff=="F1 improvement: Strategy II",]$value<quantile(Result[Result$diff=="F1 improvement: Strategy II",]$value, 0.025)))

r2 <- union(which(Result[Result$diff=="F1 improvement: Strategy III",]$value>quantile(Result[Result$diff=="F1 improvement: Strategy III",]$value, 0.975)), which(Result[Result$diff=="F1 improvement: Strategy III",]$value<quantile(Result[Result$diff=="F1 improvement: Strategy III",]$value, 0.025)))

r3 <- union(which(Result[Result$diff=="F1 improvement: Strategy I",]$value>quantile(Result[Result$diff=="F1 improvement: Strategy I",]$value, 0.975)), which(Result[Result$diff=="F1 improvement: Strategy I",]$value<quantile(Result[Result$diff=="F1 improvement: Strategy I",]$value, 0.025)))

Result <- Result[-c(r3, 3000+r1, 6000+r2),]


# pdf(file='./figure/Figure5.pdf',width=8,height=4.8)
pdf(file='./figure/combined_plot_daily.pdf',width=8,height=4.8)

generalplot <- ggplot(Result,aes(x=stock,y=value,fill=type,col=type))+ geom_boxplot(outlier.shape=6,outlier.size=0, notch=F, width = 0.5)+
  coord_flip()+
  facet_grid(cols=vars(diff),scale = "free", space="free_y", switch = "y" )+
  scale_fill_manual(values = color1 )+
  scale_color_manual(values =color2 )+
xlab('Stock')+ylab(NULL)+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "light gray",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "light gray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "light gray"),
        axis.text.x=element_text(angle=30, hjust=1, size=12, face="bold"),
        strip.text.x = element_text(size = 12, face="bold"),
        axis.text.y=element_text(face="bold", size=11),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.1), "cm"),
       axis.title=element_text(size=14))+
#   scale_y_continuous(limits = c(-0.15,0.2))+
  guides(fill=FALSE, col=FALSE)+
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed")

generalplot
dev.off()


# ###################result: picture 2#######################
# significance test to color the boxplots
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
  
  # load(paste0('./result/',char,'_eln_summary_daily.rda'))
  load(paste0('./result/',char,'_eln_ensemble_model_daily.rda'))
    
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


# test_result <- matrix(cbind(unlist(test1), unlist(test2), unlist(test3)), nrow=30)
test_result <- matrix(cbind(p.adjust(unlist(test3), method='fdr'), p.adjust(unlist(test1), method='fdr'), p.adjust(unlist(test2), method='fdr')), nrow=30)
colnames(test_result) <- c('diff: Ensem', 'diff:FPCA', 'diff:win')
rownames(test_result) <- char_name
xtable(test_result, display=c('e','e','e','e'))


pos1 <- which(levels(Result$stock)%in%char_name[which(p.adjust(unlist(test1), method='fdr')>0.05)])
pos2 <- which(levels(Result$stock)%in%char_name[which(p.adjust(unlist(test2), method='fdr')>0.05)])
pos3 <- which(levels(Result$stock)%in%char_name[which(p.adjust(unlist(test3), method='fdr')>0.05)])


# y='Difference in F1 score (original - without within-window vars)',
# #9ECAE1"
color1 <- c(rep("#9ECAE1",90))
color2 <- c(rep("#7EC0EE",90))
# two-sided
# color1[c(1,6,8,19, 22:24, 26:39, 41:42, 44:53, 82)] <- "#ADADAD"
# color2[c(1,6,8, 19, 22:24, 26:39, 41:42, 44:53, 82)] <- "#A8A8A8"
# alternative='greater'
color1[c(1,8,19, 22:24, 26:42, 44:53, 82)] <- "#ADADAD"
color2[c(1,8,19, 22:24, 26:42, 44:53, 82)] <- "#A8A8A8"
# try to combine the three plots together

# Result$type <- as.factor(str_c(Result$diff,Result$stock))
Result$type <- str_c(Result$diff,Result$stock)

# manually clear outliers
r1 <- NULL

r2 <- union(which(Result[Result$diff=="F1 improvement: Strategy III",]$value>quantile(Result[Result$diff=="F1 improvement: Strategy III",]$value, 0.975,na.rm=T)), which(Result[Result$diff=="F1 improvement: Strategy III",]$value<quantile(Result[Result$diff=="F1 improvement: Strategy III",]$value, 0.025,na.rm=T)))

r3 <- union(which(Result[Result$diff=="F1 improvement: Strategy I",]$value>quantile(Result[Result$diff=="F1 improvement: Strategy I",]$value, 0.975,na.rm=T)), which(Result[Result$diff=="F1 improvement: Strategy I",]$value<quantile(Result[Result$diff=="F1 improvement: Strategy I",]$value, 0.025,na.rm=T)))

# Result <- Result[-c(r3, 6000+r2),]
Result <- Result[-c(r1, r2+3000, 6000+r3),]


# pdf(file='./figure/Figure5.pdf',width=8,height=4.8)
pdf(file='./figure/combined_plot_eln_daily.pdf',width=8,height=4.8)

generalplot <- ggplot(Result,aes(x=stock,y=value,fill=type,col=type))+ geom_boxplot(outlier.shape=6,outlier.size=0, notch=F, width = 0.5)+
  coord_flip()+
  facet_grid(cols=vars(diff),scale = "free", space="free_y", switch = "y" )+
  scale_fill_manual(values = color1 )+
  scale_color_manual(values =color2 )+
xlab('Stock')+ylab(NULL)+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "light gray",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "light gray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "light gray"),
        axis.text.x=element_text(angle=30, hjust=1, size=12, face="bold"),
        strip.text.x = element_text(size = 12, face="bold"),
        axis.text.y=element_text(face="bold", size=11),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.1), "cm"),
       axis.title=element_text(size=14))+
#   scale_y_continuous(limits = c(-0.1,0.15))+
  guides(fill=FALSE, col=FALSE)+
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed")

generalplot
dev.off()


# ###################result: picture 3#######################
# boxplot of features chosen
Accuracy_sum <- model_sum <- test_sum <- list()
chosen <- list()

for(kk in 1:length(char_name)){
    
    char <- char_name[kk]
    flag <- vector()
    for(j in 1:100){
    
#     flag[[j]] <- try(load(paste0('./result/',char,'_',j,'_model_full.rda')),silent = T)
   test <- try(load(paste0('./result/',char,'_',j,'_eln_daily.rda')),silent = T)
        if(class(test)%in%'try-error') next
        else flag <- c(flag,j) 
    Accuracy_sum[[j]] <- Accuracy
    model_sum[[j]] <- fit.glm
    test_sum[[j]] <- test_sample
  }
    model_sum <- model_sum[flag] 
    test_sum <- test_sum[flag]
    chosen[[char]] <- parameter(model_sum,test_sum,floor(length(flag)*0.8),char)
}
# save(chosen, file='./result/para.rda')

  ResultS <- list()
  ResultU <- list()
  ResultD <- list()
for(i in 1:length(char_name)){
  ResultS[[i]] <- chosen[[i]]$S$var
  ResultU[[i]] <- chosen[[i]]$U$var
  ResultD[[i]] <- chosen[[i]]$D$var
}
  # combine FPCA variables
  ResultS <- unlist(ResultS)
  ResultU <- unlist(ResultU)
  ResultD <- unlist(ResultD)
  
#   ResultS[grep('weekly_m1_fpca',ResultS)] <- 'weekly_m1_fpca'
#   ResultS[grep('weekly_m2_fpca',ResultS)] <- 'weekly_m2_fpca'
#   ResultS[grep('daily_fpca1',ResultS)] <- 'daily_fpca'
#   ResultS[grep('daily_fpca2',ResultS)] <- 'daily_fpca'
ResultS[grep('daily_fpca',ResultS)] <- 'daily_fpca'
  
#   ResultU[grep('weekly_m1_fpca',ResultU)] <- 'weekly_m1_fpca'
#   ResultU[grep('weekly_m2_fpca',ResultU)] <- 'weekly_m2_fpca'
#   ResultU[grep('daily_fpca1',ResultU)] <- 'daily_fpca'
#   ResultU[grep('daily_fpca2',ResultU)] <- 'daily_fpca'
ResultU[grep('daily_fpca',ResultU)] <- 'daily_fpca'
  
#   ResultD[grep('weekly_m1_fpca',ResultD)] <- 'weekly_m1_fpca'
#   ResultD[grep('weekly_m2_fpca',ResultD)] <- 'weekly_m2_fpca'
#   ResultD[grep('daily_fpca1',ResultD)] <- 'daily_fpca'
#   ResultD[grep('daily_fpca2',ResultD)] <- 'daily_fpca'
ResultD[grep('daily_fpca',ResultD)] <- 'daily_fpca'
  
  
  
  numS <- length(names(table(ResultS)))
  numU <- length(names(table(ResultU)))
  numD <- length(names(table(ResultD)))
 
  Result_barplot <- data.frame(selected=c(names(table(unlist(ResultS))), names(table(unlist(ResultU))), names(table(unlist(ResultD)))),
                               Frequency=c(as.numeric(table(unlist(ResultS))), as.numeric(table(unlist(ResultU))), as.numeric(table(unlist(ResultD)))),
                               label=c(rep('Stationary',numS), rep('Upwards',numU), rep('Downwards',numD)))

  Result_barplot$label <- as.factor(Result_barplot$label)
  Result_barplot$selected <- as.factor(Result_barplot$selected)
  levels(Result_barplot$selected) <- c('Arrival Rate', 'Bid-ask Spread Return', 'Best Bid Price', 'Best Bid Volume',
                                       'Daily FPCA', 'Best Ask Price Derivative', 'Best Ask Volume Derivative', 'Best Bid Price Derivative', 
                                       'Best Bid Volume Derivative', 'Mid-price Derivative', 'Mid-price', 'Best Ask Price',
                                       'Best Ask Volume', 'Best Ask Price Depth', 'Best Ask Price Difference Return', 'Bid-ask Spread Crossing Return', 
                                       'Best Bid Price Depth', 'Best Bid Price Difference Return', 'Mean Best Ask Price','Mean Best Bid Price',
                                       'Mean Mid-price',  'Within-Window Standard Deviation', 'Trade Intensity')


Result_barplot$selected <- factor(Result_barplot$selected, levels=c('Best Bid Price Difference Return', 'Best Ask Price Difference Return', 'Bid-ask Spread Crossing Return', 'Mean Best Ask Price','Mean Best Bid Price','Mean Mid-price', 'Best Ask Price Depth', 'Best Bid Price Depth',  'Within-Window Standard Deviation', 'Trade Intensity',
                                                                    
                                                                    'Best Ask Price', 'Best Bid Price','Mid-price', 'Best Ask Volume', 'Best Bid Volume', 'Bid-ask Spread Return',  'Best Ask Price Derivative', 'Best Bid Price Derivative', 'Mid-price Derivative', 'Best Ask Volume Derivative', 'Best Bid Volume Derivative','Arrival Rate', 
                                                                    'Daily FPCA') )


  
  
  pdf(file='./figure/barplot.pdf',width=10,height=7)
# pdf(file='./figure/Figure6.pdf',width=8,height=4.8)
  ggplot(Result_barplot, mapping=aes(x = selected, y = Frequency))+
    geom_bar(stat = 'identity')+
    # scale_color_manual(values=c("#15c3c9","#f87b72"))+
    # scale_fill_manual(values=c("#61d4d6","#f5a7a1"))+
    # scale_fill_brewer(palette = "Oranges")+
    # scale_y_log10(breaks = NULL)+
    # labs(title=title1)+
    theme(plot.title = element_text(face='bold', colour='black', hjust=0.5, size=24))+
    # coord_flip()+
    facet_wrap(~label, nrow=3, scale = "free_y",strip.position='right')+
    theme(axis.title.x = element_blank())+
    theme(panel.background = element_rect(fill = "white",
                                          colour = "light gray",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "light gray"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "light gray"), 
          axis.text.y=element_text(angle=0, hjust=1, face = "bold",size=12),
          axis.text.x = element_text(face = "bold", angle=75,hjust=1,size=12),
          axis.ticks = element_blank(),
          strip.text.y = element_text(size = 13, colour = "black", angle = 270, face = "bold", hjust=0.5),
         axis.title=element_text(size=15))+ 
    theme(legend.position="none")
dev.off()
