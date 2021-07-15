#getwd()
rm(list=ls())
server <- TRUE 
path0 <- ifelse(server, "/project/6003851/y2huang/midprice_predict/final_version_2", "/Users/ying/Desktop/UVic/Year1_summer2020/mid_price_prediction/code/local_test")
setwd(path0)
source('wiltest.r')
setwd(file.path(path0,'result'))
library(Matrix)
library(tidyr)
library(stringr)


options(digits = 6)
char_name <- c('AAPL','MSFT','MMM','AXP','BA','CAT','CVX','CSCO','KO','DOW','XOM',
               'WBA','GS','HD','INTC','IBM','JNJ','JPM','MCD','MRK','NKE','PFE','PG',
               'TRV','UNH','UTX','VZ','V','WMT','DIS')

setwd('/project/6003851/y2huang/midprice_predict/final_version_2/result_ELN_nocut')
# we have the result of ensemble model of ELN, but we need to find
# the counterpart of the SVM model because the ELN model sometimes can be missing

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
    flag[[j]] <- try(load(paste0(char,'_',j,'_model_full.rda')),silent = T)
    try(load(paste0(char,'_',j,'_model_full.rda')),silent = T)
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
  load(paste0(char,'_full_ensemble_model.rda'))
  if(length(missing[[ii]])==0){
  result_ELN[,ii] <- overall(Accuracy_sum_ELN,Accuracy_sum_ensem_ELN)$x[,3]
  result_ELN_ensem[,ii] <-overall(Accuracy_sum_ELN,Accuracy_sum_ensem_ELN)$y[,3]}
  
  else{ result_ELN[-missing[[ii]],ii] <- overall(Accuracy_sum_ELN,Accuracy_sum_ensem_ELN)$x[,3]
  result_ELN_ensem[-missing[[ii]],ii] <-overall(Accuracy_sum_ELN,Accuracy_sum_ensem_ELN)$y[,3]}
}


###################Appendix picture 1#######################


# significance test to color the boxplots
test1 <- list()
for(i in 1:length(char_name)){
  char <- char_name[i]
  # print(char)
 test1[[i]] <- wilcox.test(result_ELN_ensem[,i],result_ELN[,i],paired = T)$p.value
}

setwd("/project/6003851/y2huang/midprice_predict/final_version_2")
# picture 1: ensemble vs non-ensemble
# Result1 <- data.frame(value=c(matrix(result,ncol=1,nrow=3000),matrix(result_ensem,ncol=1,nrow=3000)),ensem=rep(c('original','ensemble'),each=3000),stock=rep(rep(char_name,each=100),2))
Appendix_Result1 <- data.frame(value=matrix(result_ELN_ensem,ncol=1,nrow=3000)-matrix(result_ELN,ncol=1,nrow=3000),stock=rep(char_name,each=100))
pos1 <- which(levels(Appendix_Result1$stock)%in%char_name[which(test1 %>% unlist()>0.05)])
color1 <- c(rep("#9ECAE1",30))
color2 <- c(rep("#7EC0EE",30))
color1[pos1] <- "#ADADAD"
color2[pos1] <- "#A8A8A8"
pdf(file='ensemble_ELN_plot.pdf',width=6,height=4.8)
Appendix_plot1 <- ggplot(Appendix_Result1,aes(x=stock,y=value,fill=stock,col=stock))+ geom_boxplot(outlier.shape=16,outlier.size=0, notch=T, width = 0.5)+coord_flip()+
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
  scale_y_continuous(limits = c(-0.04,0.08))+
  guides(fill=FALSE, col=FALSE)
Appendix_plot1+geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed")
# you can also use 'outlier.shape=NA' to exclude outliers but it just make outliers disappear instead of
# zooming in
dev.off()




###################picture 2#######################
# this picture is about the comparison between SVM and ELN and SVM and ensemble ELN
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

# SVM vs ELN ensemble
test2 <- list()
for(i in 1:length(char_name)){
  char <- char_name[i]
  # print(char)
  test2[[i]] <- wilcox.test(result_ELN_ensem[,i],result[,i],paired = T)$p.value
}

# SVM vs ELN
test3 <- list()
for(i in 1:length(char_name)){
  char <- char_name[i]
  # print(char)
  test3[[i]] <- wilcox.test(result_ELN[,i],result[,i],paired = T)$p.value
}

setwd("/project/6003851/y2huang/midprice_predict/final_version_2")
Appendix_Result2 <- data.frame(value=matrix(result_ELN_ensem,ncol=1,nrow=3000)-matrix(result,ncol=1,nrow=3000),stock=rep(char_name,each=100))
pos2 <- which(levels(Appendix_Result2$stock)%in%char_name[which(test2 %>% unlist()>0.05)])

Appendix_Result3 <- data.frame(value=matrix(result_ELN,ncol=1,nrow=3000)-matrix(result,ncol=1,nrow=3000),stock=rep(char_name,each=100))
pos3 <- which(levels(Appendix_Result3$stock)%in%char_name[which(test3 %>% unlist()>0.05)])

Appendix_Result22 <- rbind(Appendix_Result2,Appendix_Result3)
Appendix_Result22$type <- as.factor(rep(c("Ensemble ELN-SVM","ELN-SVM"),each=3000))
Appendix_Result22$diff <- as.factor(str_c(Appendix_Result22$stock,Appendix_Result22$type))
levels(Appendix_Result22$diff) <- as.factor(c(str_c(levels(Appendix_Result22$stock),1),str_c(levels(Appendix_Result22$stock),2)))
color1 <- c(rep("#9ECAE1",60))
color2 <- c(rep("#7EC0EE",60))
if(length(pos2)>0){
color1[c((pos2*2),(pos3*2-1))] <- "#ADADAD"
color2[c((pos2*2),(pos3*2-1))] <- "#A8A8A8" }
pdf(file='ensemble_ELN_SVM_plot.pdf',width=6,height=4.8)
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
  # scale_y_continuous(limits = c(-0.2,0.2))+
  guides(fill=FALSE, col=FALSE)
Appendix_plot2+geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed")
# you can also use 'outlier.shape=NA' to exclude outliers but it just make outliers disappear instead of
# zooming in
dev.off()





