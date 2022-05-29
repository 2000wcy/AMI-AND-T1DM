#02
#1.group_list(实验分组)和ids(芯片注释)，每次都需要改
rm(list = ls())  
load("../MI/step1_MI.Rdata")
library(stringr)
gr = pd$`disease_status:ch1`
k1 = str_detect(gr,"Myocardial Infarction");table(k1)
k2 = str_detect(gr,"Control");table(k2)
exp = exp[,rownames(pd)[k1|k2]]

pd = pd[k1|k2,]

group = ifelse(str_detect(pd$`disease_status:ch1`,"Myocardial Infarction"),"Myocardial Infarction","Control");table(group)
group = factor(group,levels = c("Myocardial Infarction","Control"))

#2.探针注释----
temp = read.table("../MI/GPL570.txt",
                  header = T,skip = 8,
                  sep = "\t",quote = "")
k2 = str_detect(temp$Gene.Symbol,'');table(k2)
ids = temp[k2,c(1,11)]
colnames(ids) = c("probe_id","Gene_Symbol")

head(ids)
# 方法2 读取gpl页面的soft文件，按列取子集
# 方法3 官网下载
# 方法4 自主注释 
save(exp,group,ids,file = "step2_MI.Rdata")

