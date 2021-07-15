# This is for comparison mainly focus on the ELN, including ELN, ensemble ELN, ELN_nowin and ELN_nofpca and SVM
#getwd()
rm(list=ls())
server <- TRUE 
path0 <- ifelse(server, "/project/6003851/y2huang/midprice_predict/final_version_2", "/Users/ying/Desktop/UVic/Year1_summer2020/mid_price_prediction/code/local_test")
setwd(path0)
source('wiltest.r')
setwd(file.path(path0,'result'))
library(Matrix)
library(stringr)

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


#(length(char_name)/2)
for(q in 1:3){
for(ii in 1:length(char_name)){
  warnings('off')
  # check if the stock file exists
  char <- char_name[ii]
  print(char)
  test <- try(load(paste0(char,'_svm_ensemble_model.rda')),silent=T)
  if(class(test)%in%'try-error') next

  
  # benchmark of the 100 SVM model
  benchmark <- overall(Accuracy_sum,Accuracy_sum)$x
  # print(round(c(median(benchmark[,1],na.rm = T),median(benchmark[,2],na.rm = T),median(benchmark[,3],na.rm = T)),digits = 4))
  
  
  # benchmark of the 100 SVM-ensemble model
  benchmark_ensem <- overall(Accuracy_sum_ensem,Accuracy_sum_ensem)$x
  # print(paste0('&',round(c(median(benchmark_ensem[,1],na.rm = T),median(benchmark_ensem[,2],na.rm = T),median(benchmark_ensem[,3],na.rm = T)),digits = 3)))
  
  
  # benchmark of the 100 SVM-ensemble model
  benchmark_nofpca <- overall(Accuracy_sum_nofpca,Accuracy_sum_nofpca)$x
  # print(paste0('&',round(c(median(benchmark_nofpca[,1],na.rm = T),median(benchmark_nofpca[,2],na.rm = T),median(benchmark_nofpca[,3],na.rm = T)),digits = 3)))
  
  # benchmark of the 100 SVM-ensemble model
  benchmark_nowin <- overall(Accuracy_sum_nowin,Accuracy_sum_nowin)$x
  # print(paste0('&',round(c(median(benchmark_nowin[,1],na.rm = T),median(benchmark_nowin[,2],na.rm = T),median(benchmark_nowin[,3],na.rm = T)),digits = 3)))
  
  
# set up lists to store information    
# Accuracy_sum <- nofpca_Accuracy_sum <- nowin_Accuracy_sum<- svm_Accuracy_sum <- ensemble_Accuracy_sum  <- list()
# model_sum <- model_sum_nofpca <- model_sum_nowin<- model_sum_svm  <- list()
# test_sum<- test_sum_nofpca <- test_sum_nowin <-test_sum_svm  <- list()
# there are files that are missing so even use try function, the the values seem to 
# repeat, to avoid this, we hope to skip those missing files for all the loop
# flag <- list()
# 
# # regular full model
# for(j in 1:100){
#   i <- j
#   flag[[j]] <- try(load(paste0(char,'_',j,'_model_svm.rda')),silent = T)
#   try(load(paste0(char,'_',j,'_model_svm.rda')),silent = T)
#   Accuracy_sum[[i]] <- Accuracy
#   model_sum[[i]] <- svmfit
#   test_sum[[i]] <- test_sample
# }
# 
# flag_missing <- which(sapply(flag,class)%in%'try-error')
# # Accuracy_sum<- Accuracy_sum[[-flag_missing]]
# 
# setwd(file.path(path0,'result'))

# # ts_model
# for(i in 1:100){
#   try(load(paste0(char,'_',i,'_ts.rda')))
#   ts_Accuracy_sum[[i]] <- Accuracy_ts
# }

# # svm
# for(j in 1:100){
#   i <- j
#   try(load(paste0(char,'_',j,'_model_svm.rda')))
#   svm_Accuracy_sum[[i]] <- Accuracy
#   model_sum_svm[[i]] <- fit.glm
#   test_sum_svm[[i]] <- test_sample
# }


# nofpca model
# for(j in 1:100){
#   i <- j
#   try(load(paste0(char,'_',j,'_model_nofpca.rda')))
#   nofpca_Accuracy_sum[[i]] <- Accuracy
#   model_sum_nofpca[[i]] <- fit.glm
#   test_sum_nofpca[[i]] <- test_sample
# }
# 
# 
# nowin
# for(j in 1:100){
#   i <- j
#   try(load(paste0(char,'_',j,'_model_nowin.rda')))
#   nowin_Accuracy_sum[[i]] <- Accuracy
#   model_sum_nowin[[i]] <- fit.glm
#   test_sum_nowin[[i]] <- test_sample
# }



# full model
# ensemble
# load(paste0(char,'_svm_ensemble_model.rda'))
# ensemble_Accuracy_sum <- Accuracy_sum_ensem_ELN

# test <- 1
# Accuracy_sum2 <- ts_Accuracy_sum1 <- nofpca_Accuracy_sum1<- nowin_Accuracy_sum1 <- ensemble_Accuracy_sum1<- svm_Accuracy_sum1 <- ts_Accuracy_sum1 <-  list()
# for(k in 1:length(Accuracy_sum)){
#   if(isTRUE(k%in%flag_missing)) next
#   else{Accuracy_sum2[[test]] <- Accuracy_sum[[k]]
#   # ts_Accuracy_sum1[[test]] <- ts_Accuracy_sum[[k]]
#    svm_Accuracy_sum1[[test]] <- svm_Accuracy_sum[[k]]
#    nofpca_Accuracy_sum1[[test]] <-nofpca_Accuracy_sum[[k]]
#    nowin_Accuracy_sum1[[test]] <- nowin_Accuracy_sum[[k]]
#    # ensemble_Accuracy_sum1[[test]] <- ensemble_Accuracy_sum[[k]]
#    test <- test+1}
# }

# print('-----------------------------------------------')
# # # full model non ensemble v.s. full model ensemble
if(q==1)  print(wiltest(Accuracy_sum,Accuracy_sum_ensem))
# print(wiltest(Accuracy_sum,Accuracy_sum_ensemble,char))
# print('-----------------------------------------------')}
# 
# # full model v.s. nofpca model
if(q==2) print(wiltest(Accuracy_sum_nofpca,Accuracy_sum))
 # print(wiltest(Accuracy_sum,Accuracy_sum_nofpca,char))
# print('-----------------------------------------------')}
# 
# # full model v.s. nowin model
# print(wiltest(nowin_Accuracy_sum1,Accuracy_sum2,char))
# print('-----------------------------------------------')
# 
# # full model v.s. ts model
# wiltest(ts_Accuracy_sum1,Accuracy_sum2,char)
# print('-----------------------------------------------')
# 
# ensemble model v.s. svm model
if(q==3) print(wiltest(Accuracy_sum_nowin,Accuracy_sum))
 # print(wiltest(Accuracy_sum,Accuracy_sum_nowin,char))
print('-----------------------------------------------')
# 
# # nowin model v.s. ensemble model
# wiltest(nowin_Accuracy_sum1,ensemble_Accuracy_sum1,char)
# 
# # print('################################################')
# parameter(model_sum,test_sum,70,char)

}
}

setwd(file.path(path0,'result'))
result <- matrix(NA,ncol=30,nrow=100)
result_ensem <- matrix(NA,ncol=30,nrow=100)
result_nofpca <- matrix(NA,ncol=30,nrow=100)
result_nowin <- matrix(NA,ncol=30,nrow=100)

for (ii in 1:length(char_name)){
  char <- char_name[ii]
  print(char)
  # only use F1 score to draw the pictures
  load(paste0(char,'_svm_ensemble_model.rda'))
  result[,ii] <- overall(Accuracy_sum,Accuracy_sum_ensem)$x[,3]
  result_ensem[,ii] <-overall(Accuracy_sum,Accuracy_sum_ensem)$y[,3]
  result_nofpca[,ii] <- overall(Accuracy_sum_nofpca,Accuracy_sum_nowin)$x[,3]
  result_nowin[,ii] <- overall(Accuracy_sum_nofpca,Accuracy_sum_nowin)$y[,3]
}



setwd("/project/6003851/y2huang/midprice_predict/final_version_2")
# picture 1: ensemble vs non-ensemble
# Result1 <- data.frame(value=c(matrix(result,ncol=1,nrow=3000),matrix(result_ensem,ncol=1,nrow=3000)),ensem=rep(c('original','ensemble'),each=3000),stock=rep(rep(char_name,each=100),2))
Result1 <- data.frame(value=matrix(result_ensem,ncol=1,nrow=3000)-matrix(result,ncol=1,nrow=3000),stock=rep(char_name,each=100))
pdf(file='ensemble_plot.pdf',width=6,height=4.8)
ensembleplot <- ggplot(Result1,aes(x=stock,y=value,fill="#9ECAE1",col="#9ECAE1"))+ geom_boxplot(outlier.shape=16,outlier.size=0, notch=T, width = 0.5)+coord_flip()+
   scale_fill_manual(values = c("#9ECAE1"))+
  scale_color_manual(values = c("#6BAED6"))+
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
  scale_y_continuous(limits = quantile(Result1$value, c(0.001, 0.99),na.rm = T))+
  guides(fill=FALSE, col=FALSE)
ensembleplot+geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed")
# you can also use 'outlier.shape=NA' to exclude outliers but it just make outliers disappear instead of
# zooming in
dev.off()
# y='Difference in F1 score (Ensemble - non-Ensemble)',
#######################picture2##########################
Result2 <- data.frame(value=matrix(result,ncol=1,nrow=3000)-matrix(result_nofpca,ncol=1,nrow=3000),stock=rep(char_name,each=100))
pdf(file='nofpca_plot.pdf',width=6,height=4.8)
nofpcaplot <- ggplot(Result2,aes(x=stock,y=value,fill="#9ECAE1",col="#9ECAE1"))+ geom_boxplot(outlier.shape=16,outlier.size=0, notch=T, width = 0.5)+coord_flip()+
  scale_fill_manual(values = c("#9ECAE1"))+
  scale_color_manual(values = c("#6BAED6"))+
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
    scale_y_continuous(limits = quantile(Result1$value, c(0.001, 0.99),na.rm = T))+
  guides(fill=FALSE, col=FALSE)
nofpcaplot+geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed")
# you can also use 'outlier.shape=NA' to exclude outliers but it just make outliers disappear instead of
# zooming in
dev.off()
# y='Difference in F1 score (with FPCAs - without FPCAs)',
#######################picture3##########################
Result3 <- data.frame(value=matrix(result,ncol=1,nrow=3000)-matrix(result_nowin,ncol=1,nrow=3000),stock=rep(char_name,each=100))
pdf(file='nowin_plot.pdf',width=6,height=4.8)
nowinplot <- ggplot(Result3,aes(x=stock,y=value,fill="#9ECAE1",col="#9ECAE1"))+ geom_boxplot(outlier.shape=16,outlier.size=0, notch=T, width = 0.5)+coord_flip()+
  scale_fill_manual(values = c("#9ECAE1"))+
  scale_color_manual(values = c("#6BAED6"))+
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
   scale_y_continuous(limits = quantile(Result1$value, c(0.001, 0.99),na.rm = T))+
  guides(fill=FALSE, col=FALSE)
nowinplot+geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed")
# you can also use 'outlier.shape=NA' to exclude outliers but it just make outliers disappear instead of
# zooming in
dev.off()
# y='Difference in F1 score (original - without within-window vars)',
# #9ECAE1"
color1 <- c(rep("#9ECAE1",90))
color2 <- c(rep("#7EC0EE",90))
color1[c(34,42,41,58,57,56)] <- "#ADADAD"
color2[c(34,42,41,58,57,56)] <- "#A8A8A8"
# try to combine the three plots together
Result <- data.frame(value=c(matrix(result_ensem,ncol=1,nrow=3000)-matrix(result,ncol=1,nrow=3000),matrix(result,ncol=1,nrow=3000)-matrix(result_nofpca,ncol=1,nrow=3000),matrix(result,ncol=1,nrow=3000)-matrix(result_nowin,ncol=1,nrow=3000)),
                     stock=rep(rep(char_name,each=100),3),ensemble=as.character(rep(c('ensemble','non-ensemble'),c(3000,6000))),fpca=rep(c('nofpca','fpca','nofpca'),c(3000,3000,3000)),win=rep(c('window','non-window'),c(6000,3000)),
                     diff=as.factor(rep(c('Ensemble','Fpca','High-frequency'),each=3000)))
Result$type <- as.factor(str_c(Result$diff,Result$stock))

pdf(file='combined_plot.pdf',width=8,height=4.8)

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

# lm <- lm(value~0+ensemble+fpca+win,data=Result)
# summary(lm)
# FNE <- overall(Accuracy_sum)
# FE <- overall(Accuracy_sum1)
# RNE <- overall(Accuracy_sum2)
# RE <- overall(Accuracy_sum3)
# result <- rbind(FNE,FE,RNE,RE)
# 
# # try model*ensemble, this term can't be factors
# result$model <- as.factor(rep(c(1,0),each=200))
# result$ensemble <- as.factor(rep(c(0,1,0,1),each=100))
# 
#  result$model <- rep(c(1,0),each=200)
#  result$ensemble <- rep(c(0,1,0,1),each=100)
# 
# #try standarized
# result$OP <- scale(result$OP)
# result$OR <- scale(result$OR)
# result$OF1 <- scale(result$OF1)
# 
# # result <- result[sample(1:400,50,replace = F),]
# # # try log
# # result$OP <- log(result$OP)
# # result$OR <- log(result$OR)
# # result$OF1 <- log(result$OF1)
# 
# fitOP <- rlm(OP~model+ensemble+model*ensemble,data=result)
# fitOR <- rlm(OR~model+ensemble+model*ensemble,data=result)
# fitOF1 <- rlm(OF1~model+ensemble+model*ensemble,data=result)
# library(MASS)
# fitOP <- rlm(OP~ensemble,data=result)
# fitOR <- rlm(OR~ensemble,data=result)
# fitOF1 <- rlm(OF1~ensemble,data=result)
# 
# fitOP <- rlm(OP~model,data=result)
# fitOR <- rlm(OR~model,data=result)
# fitOF1 <- rlm(OF1~model,data=result)
# summary(fitOP)
# summary(fitOR)
# summary(fitOF1)
# 
# #try quantile regression
# # install.packages('quantreg')
# # library(quantreg)
# # quantile.fit <- rq(OR~model+ensemble,tau=seq(0.10, 0.90, by = 0.05),data=result)
# # summary(quantile.fit)
# 
# 
# # the significance of variables ensemble and model are not great
# # mainly because of non-normal distn of the y
# par(mfrow=c(3,2))
# qqnorm(result$OP);qqline(result$OP,col=2)
# hist(result$OP,freq=F)
# qqnorm(result$OR);qqline(result$OR,col=2)
# hist(result$OR,freq=F)
# qqnorm(result$OF1);qqline(result$OF1,col=2)
# hist(result$OF1,freq=F)
# 
# 
# 
# for boxplot
# dk how to get rid of the warning
result <- melt(result,id.vars=c("ensemble"),variable.name="type",value.name="value")
# emphasize the difference
result_diff <- data.frame(value=result$value[result$ensemble==1]-result$value[result$ensemble==0],type=rep(c('OP','OR','OF1'),each=200))
#--------------------------------
pdf(file='ensemble_plot.pdf',width=6,height=4.8)
ensembleplot <- ggplot(result_diff,aes(x=type,y=value))+ geom_boxplot(outlier.shape=16,outlier.size=0, notch=T, width = 0.3)+coord_flip()+
  scale_fill_manual(values = c("#9ECAE1","#6BAED6", "#4292C6","#2171B5","#084594"))+
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
        plot.margin = unit(c(0.1, 0.1, 0.2, 0.1), "cm"))+
  labs(y='Value (ensemble-nonensemble)',x='Measure Type')+
  scale_y_continuous(limits = quantile(result_diff$value, c(0.05, 0.95)))
ensembleplot+geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed")
# you can also use 'outlier.shape=NA' to exclude outliers but it just make outliers disappear instead of
# zooming in
dev.off()
# #--------------------------------
# pdf(file='ensemble_plot.pdf',width=6,height=4.8)
# ensembleplot <- ggplot(result,aes(x=type,y=value))+ geom_boxplot(aes(fill=ensemble),outlier.shape=16,outlier.size=0, notch=T, width = 0.3)+
#   scale_fill_manual(values = c("#9ECAE1","#6BAED6", "#4292C6","#2171B5","#084594"))+
#   theme(panel.background = element_rect(fill = "white",
#                                         colour = "light gray",
#                                         size = 0.5, linetype = "solid"),
#         panel.grid.major = element_line(size = 0.5, linetype = 'solid',
#                                         colour = "light gray"), 
#         panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
#                                         colour = "light gray"), 
#         axis.text.x=element_text(angle=0, hjust=1, size=10, face="bold"),
#         strip.text.x = element_text(size = 12, face="bold"),
#         axis.text.y=element_text(face="bold"),
#         axis.ticks = element_blank(),
#         plot.margin = unit(c(0.1, 0.1, 0.2, 0.1), "cm"))+
#   labs(y='Value',x='Measure Type')
# ensembleplot
# dev.off()
# 
# 





