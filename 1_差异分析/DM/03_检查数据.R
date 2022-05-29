#03
rm(list = ls())  
load(file = "step1_DM.Rdata")
load(file = "step2_DM.Rdata")
exp=exp
group_list = group
#输入数据：exp和group_list
#Principal Component Analysis
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials

dat=as.data.frame(t(exp))
library(FactoMineR)#画主成分分析图需要加载这两个包
library(factoextra) 
# pca的统一操作走起
dat.pca <- PCA(dat, graph = FALSE)
pca_plot <- fviz_pca_ind(dat.pca,
                         geom.ind = "point", # show points only (nbut not "text")
                         col.ind = group_list, # color by groups
                         #palette = c("#00AFBB", "#E7B800"),
                         addEllipses = TRUE, # Concentration ellipses
                         legend.title = "Groups"
)
pca_plot
save(pca_plot,file = "pca_DM.Rdata")
ggsave(pca_plot,file = "pca_DM.pdf",width = 8,height = 6)

#热图 
cg=names(tail(sort(apply(exp,1,sd)),1000))
n=exp[cg,]

annotation_col=data.frame(group=group_list)
rownames(annotation_col)=colnames(n) 
library(pheatmap)
pheatmap(n,
         show_colnames =F,
         show_rownames = F,
         annotation_col=annotation_col,
         scale = "row")

dev.off()

