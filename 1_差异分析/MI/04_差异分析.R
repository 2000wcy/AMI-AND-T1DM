#04
rm(list = ls()) 
load(file = "step2_MI.Rdata")
#差异分析，用limma包来做
#需要表达矩阵和group_list，不需要改
exp =exp
group_list = group
library(limma)
design=model.matrix(~group_list)
fit=lmFit(exp,design)
fit=eBayes(fit)
deg=topTable(fit,coef=2,number = Inf)

#为deg数据框添加几列
#1.加probe_id列，把行名变成一列
library(dplyr)
deg <- mutate(deg,probe_id=rownames(deg))
head(deg)
#2.加symbol列，火山图要用
deg <- inner_join(deg,ids,by="probe_id")
head(deg)
#按照symbol列去重复
deg <- deg[!duplicated(deg$Gene_Symbol),]
#3.加change列,标记上下调基因
logFC_t=1
P.Value_t = 0.05
k1 = (deg$P.Value < P.Value_t)&(deg$logFC < -logFC_t)
k2 = (deg$P.Value < P.Value_t)&(deg$logFC > logFC_t)
change = ifelse(k1,
                "down",
                ifelse(k2,
                       "up",
                       "stable"))
table(change)
deg <- mutate(deg,change)
ex_deg <- deg[k1|k2,]
save(group_list,deg,logFC_t,P.Value_t,ex_deg,file = "step4_MI.Rdata")

