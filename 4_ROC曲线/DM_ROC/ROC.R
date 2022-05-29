rm(list= ls())
library(ggplot2)
library(pROC)
source("ROC_Plot.R")
source("Theme_Publication.R")
HUB <- read.csv("ROC_DM.csv",row.names = NULL)
rownames(HUB) <- HUB$X
HUB <- HUB[,-1]
x1 <- HUB$ARG1
x2 <- HUB$CXCL1
x3 <- HUB$LRG1
x4 <- HUB$MMP9
x5 <- HUB$OSCAR
x6 <- HUB$TNF
x7 <- HUB$QPCT
y <- HUB$type

R1 <- roc(y,x1); R2 <- roc(y,x2); R3 <- roc(y,x3);R4 <- roc(y,x4)
R5 <- roc(y,x5);R6 <- roc(y,x6);R7 <- roc(y,x7)

dat1 <- data.frame(Sensitivities = R1$sensitivities, 
                   FalsePositiveRate = (1-R1$specificities), 
                   Model = rep("ARG1",length(R1$sensitivities)))
dat2 <- data.frame(Sensitivities = R2$sensitivities, 
                   FalsePositiveRate = (1-R2$specificities), 
                   Model = rep("CXCL1",length(R2$sensitivities)))
dat3 <- data.frame(Sensitivities = R3$sensitivities, 
                   FalsePositiveRate = (1-R3$specificities), 
                   Model = rep("LRG1",length(R3$sensitivities)))
dat4 <- data.frame(Sensitivities = R4$sensitivities, 
                   FalsePositiveRate = (1-R4$specificities), 
                   Model = rep("MMP9",length(R4$sensitivities)))
dat5 <- data.frame(Sensitivities = R5$sensitivities, 
                   FalsePositiveRate = (1-R5$specificities), 
                   Model = rep("OSCAR",length(R5$sensitivities)))
dat6 <- data.frame(Sensitivities = R6$sensitivities, 
                   FalsePositiveRate = (1-R6$specificities), 
                   Model = rep("TNF",length(R6$sensitivities)))
dat7 <- data.frame(Sensitivities = R7$sensitivities, 
                   FalsePositiveRate = (1-R7$specificities), 
                   Model = rep("QPCT",length(R7$sensitivities)))


dat <- rbind(dat1,dat2,dat3,dat4,dat5,dat6,dat7)
colnames(dat)[2] <- ("1-Specificities")

ggplot(dat,aes(`1-Specificities`,Sensitivities,colour = Model)) + geom_roc_plot()+
  scale_colour_Publication() + theme_Publication()+
  theme(panel.border = element_rect(colour = 'black'),legend.title = element_blank(),
        legend.position = c(0.9,0.15),legend.direction = 'vertical')
