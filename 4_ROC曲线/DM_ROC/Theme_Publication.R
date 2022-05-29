theme_Publication <- function(base_size=14, base_family="sans") {
      library(grid)
      library(ggthemes)
      (theme_foundation(base_size=base_size, base_family=base_family)
      + theme(plot.title = element_text(face = "bold",
                                        size = rel(1.2), hjust = 0.5),
              text = element_text(),
              panel.background = element_rect(colour = NA),
              plot.background = element_rect(colour = NA),
              panel.border = element_rect(colour = NA),
              axis.title = element_text(face = "bold",size = rel(1)),
              axis.title.y = element_text(angle=90,vjust =2),
              axis.title.x = element_text(vjust = -0.2),
              axis.text = element_text(), 
              axis.line.x = element_line(colour="black"),
              axis.line.y = element_line(colour="black"),
              axis.ticks = element_line(),
              panel.grid.major = element_line(colour="#f0f0f0"),
              panel.grid.minor = element_blank(),
              legend.key = element_rect(colour = NA),
              legend.position = "bottom",
              legend.direction = "horizontal",
              legend.key.size= unit(0.4, "cm"),
              legend.margin = unit(0.2, "cm"),
              legend.title = element_text(face="italic"),
              plot.margin=unit(c(10,5,5,5),"mm"),
              strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
              strip.text = element_text(face="bold")
      ))
      
}

scale_fill_Publication <- function(...){
      library(scales)
      discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
      
}

scale_colour_Publication <- function(...){
      library(scales)
      discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
      
}


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

