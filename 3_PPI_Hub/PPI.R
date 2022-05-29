#http://bioinformatics.psb.ugent.be/webtools/Venn/ 
#https://string-db.org/cgi/input.pl?sessionId=rKtCnBvlHDFo&input_page_active_form=multiple_identifiers
rm(list = ls())
library(ggplot2)
#PPI
coDEG <- read.table(file = "../2_富集分析/Common genes.txt",
                    sep = "\t")
write.table(coDEG,file = "string_input.txt",
            row.names = F,
            col.names = F,
            quote = F,
            sep = "\t")
HUB <- read.table(file = "HUB_degree.txt",sep = "\t",header = T)
library(forcats)
a = read.table("HUB_degree.txt",
               header = T,
               sep = "\t",quote = "")

p <-   ggplot(a, aes(x = reorder(Name,-Score), y = Score)) +
  geom_bar(stat = "identity",fill="cornflowerblue",width=0.5)

p <- p + theme(legend.position = "none") +labs(title = "Score of top 10 hub gene",
                                               x = NULL)+
  scale_y_continuous(name='Score', 
                     breaks=seq(0,14,2),
                     limits=c(0,14))+
  geom_text(aes(label=Score),vjust=1.5,colour="black")+
  theme_bw()+theme(panel.grid.major.x  =element_blank(),
                   panel.grid.major.y  =element_line(colour='lightgrey'),
                   panel.grid.minor=element_blank(),
                   panel.border = element_blank(),
                   axis.ticks=element_blank(),
                   plot.title = element_text(hjust = 0.5,lineheight=0.8, face="bold",size = 14)
                   
  )
p
ggsave(p,filename = "top 10 hub gene.pdf",width =6,height = 3)

