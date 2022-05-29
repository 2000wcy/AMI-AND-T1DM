#05
rm(list = ls()) 
load(file = "step1_DM.Rdata")
load(file = "step2_DM.Rdata")
load(file = "step4_DM.Rdata")
exp = exp
group_list = group

#1.火山图----
library(dplyr)
library(ggplot2)
dat  = deg

#if(F){
# for_label <- dat%>% 
#  filter(symbol %in% c("TRPM3","SFRP1")) 
#}
if(F){
  for_label <- dat %>% head(10)
}
if(T) {
  x1 = dat[order(dat$logFC,decreasing = T),] %>% 
    filter(change == "up") %>% 
    head(2)
  x2 = dat[order(dat$logFC,decreasing = F),] %>% 
    filter(change == "down") %>% 
    head(2)
  for_label = rbind(x1,x2)
}
#%>%
#  head(10)
p <- ggplot(data = dat, 
            aes(x = logFC, 
                y = -log10(P.Value))) +
  geom_point(alpha=0.4, size=3.5, 
             aes(color=change)) +
  ylab("-log10(Pvalue)")+
  scale_color_manual(values=c("blue", "grey","red"))+
  geom_vline(xintercept=c(-logFC_t,logFC_t),lty=4,col="black",lwd=0.8) +
  geom_hline(yintercept = -log10(P.Value_t),lty=4,col="black",lwd=0.8) +
  theme_bw()
p
volcano_plot <- p +
  geom_point(size = 3, shape = 1, data = for_label) +
  ggrepel::geom_label_repel(
    aes(label =Gene_Symbol ),
    data = for_label,
    color="black"
  )
volcano_plot

#2.差异基因热图----
load(file = 'step2_DM.Rdata')
x=deg$logFC 
names(x)=deg$probe_id 
#cg=c(names(head(sort(x),30)),names(tail(sort(x),30)))
cg = deg$probe_id[deg$change !="stable"]
exp=exp[,order(group_list)]
group_list = group_list[order(group_list)]
n=exp[cg,]
dim(n)
#作热图
library(pheatmap)
annotation_col=data.frame(group=group_list)
rownames(annotation_col)=colnames(n) 
library(ggplotify)
heatmap_plot <- as.ggplot(pheatmap(n,show_colnames =F,
                                   show_rownames = F,
                                   scale = "row",
                                   cluster_cols = F, 
                                   annotation_col=annotation_col)) 
heatmap_plot
library(patchwork)
DM <- (heatmap_plot + p)+ plot_annotation(tag_levels = "A")
#要根据合并的图形将绘图区拉到合适的比例，否则会报错
save(DM,file = "T1D.Rdata")
ggsave(filename = "DM.pdf",width = 15,height = 6)
