#02
#1.group_list(实验分组)和ids(芯片注释)，每次都需要改
rm(list = ls())  
load("../DM/step1_DM.Rdata")
library(stringr)
gr = pd$`disease state:ch1`
k1 = str_detect(gr,"type 1 diabetes");table(k1)
k2 = str_detect(gr,"control");table(k2)
exp = exp[,rownames(pd)[k1|k2]]

pd = pd[k1|k2,]

group = ifelse(str_detect(pd$`disease state:ch1`,"type 1 diabetes"),"type 1 diabetes","control")
table(group)
group = factor(group,levels = c("type 1 diabetes","control"))

#2.探针注释----
temp = read.table("../DM/GPL570.txt",
                  header = T,skip = 16,
                  sep = "\t",quote = "")
k2 = str_detect(temp$Gene.Symbol,'');table(k2)
ids = temp[k2,c(1,11)]
colnames(ids) = c("probe_id","Gene_Symbol")
head(ids)
save(exp,group,ids,file = "step2_DM.Rdata")
