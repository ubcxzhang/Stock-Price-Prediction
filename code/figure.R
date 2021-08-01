# This is for comparison mainly focus on the SVM, including SVM, ensemble SVM, SVM_nowin and SVM_nofpca
#getwd()
rm(list=ls())

source('./code/wiltest.r')
library(Matrix)
library(stringr)
library(ggplot2)

options(digits = 6)

# what model to compare
# full model——includes all the high frequency and low frequency vars
# full-ensemble model——incudes full model and its counterpart full and ensemble model
# nofpca model——includes all vars of full model except the lack of all 16 fpca vars
# nowin1 / nowin2 model ——reduce high-frequency relevant variables, Idk if we require this temporarily
# svm / nowin model—— a selected model and its FPCA counterpart, svm has all high-fre vars while nowin has the FPCA as well
# ts model—— a model using a time series model 

warnings('off')
# what to compare
# full model non ensemble v.s. full model ensemble
# full model v.s. nofpca model
# full model v.s. ts model
# svm v.s. nowin
# svm v.s. full model
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
        axis.text.y= element_text(angle=0, hjust=1, face = "bold",size=10),
        axis.text.x = element_text(face = "bold", angle=45,size=15),
        axis.ticks = element_blank(),
        strip.text.y = element_text(size = 30, colour = "black", angle = -90, face = "bold", hjust=0.5))+
        theme(legend.position="none")
dj30
dev.off()




###################SVM picture 1#######################

# setwd(file.path(path0,'result'))
result <- matrix(NA,ncol=30,nrow=100)
result_ensem <- matrix(NA,ncol=30,nrow=100)
result_nofpca <- matrix(NA,ncol=30,nrow=100)
result_nowin <- matrix(NA,ncol=30,nrow=100)

for (ii in 1:length(char_name)){
  char <- char_name[ii]
  print(char)
  # only use F1 score to draw the pictures
  load(paste0('./result/', char,'_svm_ensemble_model.rda'))
  result[,ii] <- overall(Accuracy_sum,Accuracy_sum_ensem)$x[,3]
  result_ensem[,ii] <-overall(Accuracy_sum,Accuracy_sum_ensem)$y[,3]
  result_nofpca[,ii] <- overall(Accuracy_sum_nofpca,Accuracy_sum_nowin)$x[,3]
  result_nowin[,ii] <- overall(Accuracy_sum_nofpca,Accuracy_sum_nowin)$y[,3]
}


Result <- data.frame(value=c(matrix(result_ensem,ncol=1,nrow=3000)-matrix(result,ncol=1,nrow=3000),matrix(result,ncol=1,nrow=3000)-matrix(result_nofpca,ncol=1,nrow=3000),matrix(result,ncol=1,nrow=3000)-matrix(result_nowin,ncol=1,nrow=3000)),
                     stock=rep(rep(char_name,each=100),3),ensemble=as.character(rep(c('ensemble','non-ensemble'),c(3000,6000))),fpca=rep(c('nofpca','fpca','nofpca'),c(3000,3000,3000)),win=rep(c('window','non-window'),c(6000,3000)),
                     diff=as.factor(rep(c('Ensemble SVM-Baseline','Baseline-no FPCA','Baseline-no within-window'),each=3000)))

Result$stock <- as.factor(Result$stock)

# baseline model versus non-ensemble, wilcoxon sign rank test, two-sided
test1 <- list()
for(i in 1:length(char_name)){
  char <- char_name[i]
  # print(char)
  test1[[i]] <- wilcox.test(result_ensem[,i],result[,i],paired = T,mu=0)$p.value
}

# baseline model versus no-fpca, wilcoxon sign rank test, two-sided
test2 <- list()
for(i in 1:length(char_name)){
  char <- char_name[i]
  # print(char)
  test2[[i]] <- wilcox.test(result[,i],result_nofpca[,i],paired = T,mu=0)$p.value
}

# # baseline model versus no-win, wilcoxon sign rank test, two-sided
test3 <- list()
for(i in 1:length(char_name)){
  char <- char_name[i]
  # print(char)
  test3[[i]] <- wilcox.test(result[,i],result_nowin[,i],paired = T,mu=0)$p.value
}

pos1 <- which(levels(Result$stock)%in%char_name[which(test1 %>% unlist()>0.05)])
pos2 <- which(levels(Result$stock)%in%char_name[which(test2 %>% unlist()>0.05)])
pos3 <- which(levels(Result$stock)%in%char_name[which(test3 %>% unlist()>0.05)])


# y='Difference in F1 score (original - without within-window vars)',
# #9ECAE1"
color1 <- c(rep("#9ECAE1",90))
color2 <- c(rep("#7EC0EE",90))
color1[c(pos2,pos3+30,pos1+60)] <- "#ADADAD"
color2[c(pos2,pos3+30,pos1+60)] <- "#A8A8A8"
# try to combine the three plots together

Result$type <- as.factor(str_c(Result$diff,Result$stock))

pdf(file='./figure/combined_plot.pdf',width=8,height=4.8)

generalplot <- ggplot(Result,aes(x=stock,y=value,fill=type,col=type))+ geom_boxplot(outlier.shape=6,outlier.size=0, notch=T, width = 0.5)+
  coord_flip()+
  facet_grid(cols=vars(diff),scale = "free_y", space="free_y", switch = "y" )+
  scale_fill_manual(values = color1 )+
  scale_color_manual(values =color2 )+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "light gray",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "light gray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "light gray"),
        axis.text.x=element_text(angle=0, hjust=1, size=10, face="bold"),
        strip.text.x = element_text(size = 12, face="bold"),
        axis.text.y=element_text(face="bold"),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.1), "cm"))+
  labs(x='Stock')+ylab(NULL)+
  scale_y_continuous(limits = c(-0.1,0.13))+
  guides(fill=FALSE, col=FALSE)+
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed")

generalplot
dev.off()



####################################################################################################
####################################################################################################
Accuracy_sum <- model_sum <- test_sum <- list()
missing <- list()
for(ii in 1:length(char_name)){
  warnings('off')
  # check if the stock file exists
  char <- char_name[ii]
  print(char)
  flag <- list()
  
  # regular full model
  for(j in 1:100){
    i <- j
    flag[[j]] <- try(load(paste0('./result/',char,'_',j,'_model_full.rda')),silent = T)
    try(load(paste0('./result/',char,'_',j,'_model_full.rda')),silent = T)
    Accuracy_sum[[i]] <- Accuracy
    model_sum[[i]] <- fit.glm
    test_sum[[i]] <- test_sample
  }
  
  missing[[ii]] <-  which(sapply(flag,class)%in%'try-error')
}

# comparison among ensemble ELN and original ELN
result_ELN <- matrix(NA,ncol=30,nrow=100)
result_ELN_ensem <- matrix(NA,ncol=30,nrow=100)


for (ii in 1:length(char_name)){
  char <- char_name[ii]
  print(char)
  # only use F1 score to draw the pictures
  load(paste0('./result/',char,'_full_ensemble_model.rda'))
  if(length(missing[[ii]])==0){
  result_ELN[,ii] <- overall(Accuracy_sum_ELN,Accuracy_sum_ensem_ELN)$x[,3]
  result_ELN_ensem[,ii] <-overall(Accuracy_sum_ELN,Accuracy_sum_ensem_ELN)$y[,3]}
  
  else{ result_ELN[-missing[[ii]],ii] <- overall(Accuracy_sum_ELN,Accuracy_sum_ensem_ELN)$x[,3]
  result_ELN_ensem[-missing[[ii]],ii] <-overall(Accuracy_sum_ELN,Accuracy_sum_ensem_ELN)$y[,3]}
}


# ###################picture 2#######################
# significance test to color the boxplots
test1.ELN <- list()
for(i in 1:length(char_name)){
  char <- char_name[i]
  # print(char)
 test1.ELN[[i]] <- wilcox.test(result_ELN_ensem[,i],result_ELN[,i],paired = T,mu=0)$p.value
}

# SVM vs ELN ensemble
test2.ELN <- list()
for(i in 1:length(char_name)){
  char <- char_name[i]
  # print(char)
  test2.ELN[[i]] <- wilcox.test(result_ELN_ensem[,i],result[,i],paired = T,mu=0)$p.value
}

# SVM vs ELN
test3.ELN <- list()
for(i in 1:length(char_name)){
  char <- char_name[i]
  # print(char)
  test3.ELN[[i]] <- wilcox.test(result_ELN[,i],result[,i],paired = T,mu=0)$p.value
}

Appendix_Result1 <- data.frame(value=matrix(result_ELN_ensem,ncol=1,nrow=3000)-matrix(result_ELN,ncol=1,nrow=3000),stock=rep(char_name,each=100))

# setwd("/project/6003851/y2huang/midprice_predict/final_version_2")
Appendix_Result2 <- data.frame(value=matrix(result_ELN_ensem,ncol=1,nrow=3000)-matrix(result,ncol=1,nrow=3000),stock=rep(char_name,each=100))

Appendix_Result3 <- data.frame(value=matrix(result_ELN,ncol=1,nrow=3000)-matrix(result,ncol=1,nrow=3000),stock=rep(char_name,each=100))

Appendix_Result22 <- rbind(Appendix_Result1, Appendix_Result2,Appendix_Result3)
Appendix_Result22$type <- as.factor(rep(c("Ensemble ELN-ELN","Ensemble ELN-SVM","ELN-SVM"),each=3000))
Appendix_Result22$diff <- as.factor(str_c(Appendix_Result22$stock,Appendix_Result22$type))
# levels(Appendix_Result22$diff) <- as.factor(c(str_c(levels(Appendix_Result22$stock),1),str_c(levels(Appendix_Result22$stock),2)))
Appendix_Result22$stock <- as.factor(Appendix_Result22$stock)

pos1 <- which(levels(Appendix_Result22$stock)%in%char_name[which(test1.ELN %>% unlist()>0.05)])
pos2 <- which(levels(Appendix_Result22$stock)%in%char_name[which(test2.ELN %>% unlist()>0.05)])
pos3 <- which(levels(Appendix_Result22$stock)%in%char_name[which(test3.ELN %>% unlist()>0.05)])
color1 <- c(rep("#9ECAE1",90))
color2 <- c(rep("#7EC0EE",90))
color1[c(3*(pos3-1)+1,3*(pos1-1)+2,3*pos2)] <- "#ADADAD"
color2[c(3*(pos3-1)+1,3*(pos1-1)+2,3*pos2)] <- "#A8A8A8"
pdf(file='./figure/ensemble_ELN_SVM_plot.pdf',width=8,height=4.8)
Appendix_plot2 <- ggplot(Appendix_Result22,aes(x=stock,y=value,fill=diff,col=diff))+ geom_boxplot(outlier.shape=16,outlier.size=0, notch=T, width = 0.5)+coord_flip()+
  facet_grid(cols=vars(type),scale = "free_x", space="free_y", switch = "y" )+
  scale_fill_manual(values = color1)+
  scale_color_manual(values = color2)+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "light gray",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "light gray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "light gray"),
        axis.text.x=element_text(angle=0, hjust=1, size=10, face="bold"),
        strip.text.x = element_text(size = 12, face="bold"),
        axis.text.y=element_text(face="bold"),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.1), "cm"))+
  labs(x='Stock')+ylab(NULL)+
#   scale_y_continuous(limits = c(-0.25,0.3))+
  guides(fill="none", col="none")
Appendix_plot2+geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed")
# you can also use 'outlier.shape=NA' to exclude outliers but it just make outliers disappear instead of
# zooming in
dev.off()


# ###################picture 3#######################
# boxplot of features chosen
Accuracy_sum <- model_sum <- test_sum <- list()
chosen <- list()

for(kk in 1:length(char_name)){
    
    char <- char_name[kk]
    for(j in 1:100){
    
#     flag[[j]] <- try(load(paste0('./result/',char,'_',j,'_model_full.rda')),silent = T)
   test <- try(load(paste0('./result/',char,'_',j,'_model_full.rda')),silent = T)
        if(class(test)%in%'try-error') next
    Accuracy_sum[[j]] <- Accuracy
    model_sum[[j]] <- fit.glm
    test_sum[[j]] <- test_sample
  }
    
    chosen[[char]] <- parameter(model_sum,test_sum,80,char)
    

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
  
  ResultS[grep('weekly_m1_fpca',ResultS)] <- 'weekly_m1_fpca'
  ResultS[grep('weekly_m2_fpca',ResultS)] <- 'weekly_m2_fpca'
  ResultS[grep('daily_m1_fpca',ResultS)] <- 'daily_m1_fpca'
  ResultS[grep('daily_m2_fpca',ResultS)] <- 'daily_m2_fpca'
  
  ResultU[grep('weekly_m1_fpca',ResultU)] <- 'weekly_m1_fpca'
  ResultU[grep('weekly_m2_fpca',ResultU)] <- 'weekly_m2_fpca'
  ResultU[grep('daily_m1_fpca',ResultU)] <- 'daily_m1_fpca'
  ResultU[grep('daily_m2_fpca',ResultU)] <- 'daily_m2_fpca'
  
  ResultD[grep('weekly_m1_fpca',ResultD)] <- 'weekly_m1_fpca'
  ResultD[grep('weekly_m2_fpca',ResultD)] <- 'weekly_m2_fpca'
  ResultD[grep('daily_m1_fpca',ResultD)] <- 'daily_m1_fpca'
  ResultD[grep('daily_m2_fpca',ResultD)] <- 'daily_m2_fpca'
  
  
  
  numS <- length(names(table(ResultS)))
  numU <- length(names(table(ResultU)))
  numD <- length(names(table(ResultD)))
 
  Result_barplot <- data.frame(selected=c(names(table(unlist(ResultS))), names(table(unlist(ResultU))), names(table(unlist(ResultD)))),
                               Frequency=c(as.numeric(table(unlist(ResultS))), as.numeric(table(unlist(ResultU))), as.numeric(table(unlist(ResultD)))),
                               label=c(rep('Stationary',numS), rep('Upwards',numU), rep('Downwards',numD)))

  Result_barplot$label <- as.factor(Result_barplot$label)
  Result_barplot$selected <- as.factor(Result_barplot$selected)
  levels(Result_barplot$selected) <- c('Arrival Rate', 'Best Bid Price', 'Best Bid Size', 'Daily FPCA (d1)',
                                       'Daily FPCA (d2)','Ask Price Derivative', 'Ask Volume Derivative', 'Bid Price Derivative', 
                                       'Bid Volume Derivative',
                                       'Mid-price Derivative', 'Mid-price','Best Ask Price', 'Best Ask Size', 'Weekly FPCA (d1)',
                                       'Weekly FPCA (d2)', 'Ask Price Depth', 'Best Ask Price Difference Return', 'Bid-ask Spread Crossing',
                                       'Bid-ask Spread Return', 'Bid Price Depth', 'Best Bid Price Difference Return', 'Mean Ask Price',
                                       'Mean Bid Price', 'Mean Mid-price', 'Within-Window Standard Deviation', 'Window Slope')
  
  
  pdf(file='./figure/barplot.pdf',width=10,height=7)
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
          axis.text.y=element_text(angle=0, hjust=1, face = "bold",size=10),
          axis.text.x = element_text(face = "bold", angle=30,hjust=1,size=10),
          axis.ticks = element_blank(),
          strip.text.y = element_text(size = 10, colour = "black", angle = 270, face = "bold", hjust=0.5))+ 
    theme(legend.position="none")
dev.off()












