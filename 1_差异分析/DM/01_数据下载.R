#01
#数据下载
rm(list = ls())
options(stringsAsFactors = F)
library(GEOquery)
gse = "GSE55098"
eSet <- getGEO(gse, 
               destdir = '.',
               getGPL = F)
#(1)提取表达矩阵exp
exp <- exprs(eSet[[1]])
exp[1:4,1:4]
boxplot(exp)
#exp = log2(exp+1)
#(2)提取临床信息
pd <- pData(eSet[[1]])
#(3)调整pd的行名顺序与exp列名完全一致
p = identical(rownames(pd),colnames(exp));p
if(!p) exp = exp[,match(rownames(pd),colnames(exp))]
#(4)提取芯片平台编号
gpl <- eSet[[1]]@annotation
save(gse,pd,exp,gpl,file = "step1_DM.Rdata")