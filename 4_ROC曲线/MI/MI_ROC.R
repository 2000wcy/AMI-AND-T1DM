library(dplyr)
#ROC
rm(list = ls()) 
load(file = "step2_MI.Rdata")
probe_id<- rownames(exp)
exp <- data.frame(cbind(probe_id,exp))
roc <- inner_join(exp,ids,by='probe_id')
HUB <- read.table(file = "HUB.txt")
colnames(HUB)[1] <- "Gene_Symbol"
ROC <- merge(HUB,roc,by="Gene_Symbol")
ROC <- t(ROC)
colnames(ROC) <- ROC[1,]
ROC <- ROC[-1:-2,]
arg1 <- (ROC[,1:4]) %>% 
  apply(1,as.numeric) %>% 
  apply(2,mean) %>% 
  as.data.frame() 
colnames(arg1) <- c('ARG1')

ROC1 <- ROC[,5:10] %>% 
  apply(1,as.numeric) %>% 
  t() %>% 
  as.data.frame()

colnames(ROC1) <- colnames(ROC[,5:10])
ROC <- cbind(arg1,ROC1)
ROC<- cbind(ROC,type=c(rep(0,13),rep(1,21),rep(0,24),rep(1,15),
                       rep(0,13),rep(1,13)))
write.csv(ROC,"ROC_MI1.csv")

