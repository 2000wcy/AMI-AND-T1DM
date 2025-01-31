# first prepare BioManager on CRAN
options("repos" = c(CRAN="http://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
if(!require("BiocManager")) install.packages("BiocManager",update = F,ask = F)
options(BioC_mirror="http://mirrors.tuna.tsinghua.edu.cn/bioconductor/")
getOption("BioC_mirror")
getOption("repos")

cran_packages <- c('tidyr',
                   'tibble',
                   'dplyr',
                   'stringr',
                   'ggplot2',
                   'ggpubr',
                   'factoextra',
                   'FactoMineR',
                   'devtools') 
Biocductor_packages <- c('GEOquery',
                         'hgu133plus2.db',
                         "KEGG.db",
                         "limma",
                         "impute",
                         "GSEABase",
                         "GSVA",
                         "clusterProfiler",
                         "org.Hs.eg.db",
                         "preprocessCore",
                         "hugene10sttranscriptcluster.db",
                         "enrichplot",
                         "ggplotify",
                         "multiMiR")

for (pkg in cran_packages){
  if (! require(pkg,character.only=T) ) {
    install.packages(pkg,ask = F,update = F)
    require(pkg,character.only=T) 
  }
}


for (pkg in Biocductor_packages){
  if (! require(pkg,character.only=T) ) {
    BiocManager::install(pkg,ask = F,update = F)
    require(pkg,character.only=T) 
  }
}

#前面的所有提示和报错都先不要管。主要看这里
for (pkg in c(Biocductor_packages,cran_packages)){
  require(pkg,character.only=T) 
}
#没有error就是成功

#哪个报错，就回去安装哪个。如果你没有安装xx包，却提示你xx包不存在，这也正常，是因为复杂的依赖关系，缺啥补啥???
if(!require(AnnoProbe))devtools::install_local("./AnnoProbe-master.zip",upgrade = F)
if(!require(tinyarray))devtools::install_local("./tinyarray-master.zip")
library(AnnoProbe)
library(tinyarray)