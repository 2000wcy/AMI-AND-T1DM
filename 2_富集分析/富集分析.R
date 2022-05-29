library(patchwork)
library(clusterProfiler)
library(ggplot2)
library(org.Hs.eg.db)
rm(list=ls())
DM <- read.table("../1_差异分析/DM/ex_all_DM.txt",
                 header = T,
                 sep = "\t")
MI <- read.table("../1_差异分析/MI/ex_all_MI.txt",
                 header = T,
                 sep = "\t")
geneNames <- intersect(DM$Gene_Symbol,MI$Gene_Symbol)
write.table(geneNames,file = "Common genes.txt",
            row.names = F,col.names = F,quote = F)
write.table(DM$Gene_Symbol,file = "DM.txt",
            row.names = F,col.names = F,quote = F)
write.table(MI$Gene_Symbol,file = "MI.txt",
            row.names = F,col.names = F,quote = F)
gene <-  mapIds(org.Hs.eg.db, geneNames, 'ENTREZID', 'SYMBOL') #转化id形式   



#差异基因GO富集分析
ALL <- enrichGO(gene          = gene,
                    OrgDb         = 'org.Hs.eg.db',
                    ont           = "ALL",
                    pAdjustMethod = "BH",
                    pvalueCutoff  = 0.05,
                    qvalueCutoff  = 0.05)
dotplot(ALL,showCategory=30)
ALL <- data.frame(ALL)
write.csv(ALL,'enrichGO.csv',row.names = F,
          quote = F)

go_CC <- enrichGO(gene          = gene,
                   #universe     = row.names(dge.celltype),
                   OrgDb         = 'org.Hs.eg.db',
                   ont           = "CC",
                   pAdjustMethod = "BH",
                   pvalueCutoff  = 0.05,
                   qvalueCutoff  = 0.05)
go_MF <- enrichGO(gene          = gene,
                   OrgDb         = 'org.Hs.eg.db',
                   ont           = "MF",
                   pAdjustMethod = "BH",
                   pvalueCutoff  = 0.05,
                   qvalueCutoff  = 0.05)
go_BP <- enrichGO(gene          = gene,
                   OrgDb         = 'org.Hs.eg.db',
                   ont           = "BP",
                   pAdjustMethod = "BH",
                   pvalueCutoff  = 0.05,
                   qvalueCutoff  = 0.05)
go_CC@result$Description <- substring(go_CC@result$Description,1,70)
go_MF@result$Description <- substring(go_MF@result$Description,1,70)
go_BP@result$Description <- substring(go_BP@result$Description,1,70)

#GO富集气泡图
p_BP <- dotplot(go_BP,showCategory = 10) + ggtitle("Biological process")
p_CC <- dotplot(go_CC,showCategory = 10) + ggtitle("Cellular component")
p_MF <- dotplot(go_MF,showCategory = 10) + ggtitle("Molecular function")
plotc <- p_BP/p_CC/p_MF
ggsave('enrichGO.pdf', plotc, width = 14,height = 11,device = "pdf")

#KEGG
#http://kobas.cbi.pku.edu.cn/kobas3/genelist/
kegg = read.table("pathway_enrichment.txt",header=TRUE,
                  sep="\t",check.names = FALSE)
kegg$`Enrich ratio` <- kegg$`Input number`/kegg$`Background number`
kegg=kegg[kegg$p.adjust<0.05,]
kegg=arrange(kegg,by_group = kegg$`Input number`)
kegg = kegg[27:36,]##绘制top10 term
g = ggplot(kegg,aes(`Enrich ratio`,Term))+geom_point(aes(size=`Input number`,color=p.adjust))
g = g+scale_color_gradient(low="blue",high ="red")
g = g+labs(color=expression(p.adjust),size="Genes",x="Enrich ratio",y="Pathway",title="Pathway enrichment")
g
ggsave("KEGG_enrichment_KOBAS.pdf", width=6, height=6)


