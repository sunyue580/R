######################################1、富集分析
##GO富集分析
#ORA
library(clusterProfiler)
data(geneList, package="DOSE")
gene <- names(geneList)[abs(geneList) > 2]
ego <- enrichGO(gene          = gene,
                universe      = names(geneList),
                OrgDb         = org.Hs.eg.db,
                ont           = "CC",
                pAdjustMethod = "BH",
                pvalueCutoff  = 0.01,
                qvalueCutoff  = 0.05,
                readable      = TRUE)
head(ego)
#GSEA
ego3 <- gseGO(geneList     = geneList,
              OrgDb        = org.Hs.eg.db,
              ont          = "CC",
              nPerm        = 1000,
              minGSSize    = 100,
              maxGSSize    = 500,
              pvalueCutoff = 0.05,
              verbose      = FALSE)

##KEGG富集分析
#ORA
data(geneList, package="DOSE")
gene <- names(geneList)[abs(geneList) > 2]
kk <- enrichKEGG(gene         = gene,
                 organism     = 'hsa',
                 pvalueCutoff = 0.05)
head(kk)
#GSEA
kk2 <- gseKEGG(geneList     = geneList,
               organism     = 'hsa',
               nPerm        = 1000,
               minGSSize    = 120,
               pvalueCutoff = 0.05,
               verbose      = FALSE)
head(kk2)


##DO富集分析
#ORA
library(DOSE)
data(geneList)
gene <- names(geneList)[abs(geneList) > 1.5]
head(gene)
x <- enrichDO(gene          = gene,
              ont           = "DO",
              pvalueCutoff  = 0.05,
              pAdjustMethod = "BH",
              universe      = names(geneList),
              minGSSize     = 5,
              maxGSSize     = 500,
              qvalueCutoff  = 0.05,
              readable      = FALSE)
head(x)
x <- setReadable(x, 'org.Hs.eg.db')
head(x)
#GSEA
data(geneList)
y <- gseDO(geneList,
           nPerm         = 100,
           minGSSize     = 120,
           pvalueCutoff  = 0.2,
           pAdjustMethod = "BH",
           verbose       = FALSE)
head(y, 3)

##NCG富集分析——癌症基因
#ORA
gene2 <- names(geneList)[abs(geneList) < 3]
ncg <- enrichNCG(gene2)
head(ncg)
#GSEA
ncg <- gseNCG(geneList,
              nPerm         = 100,
              minGSSize     = 120,
              pvalueCutoff  = 0.2,
              pAdjustMethod = "BH",
              verbose       = FALSE)
ncg <- setReadable(ncg, 'org.Hs.eg.db')
head(ncg, 3)

##基因-疾病相关性富集分析
#ORA
dgn <- enrichDGN(gene)
head(dgn)
#GSEA
dgn <- gseDGN(geneList,
              nPerm         = 100,
              minGSSize     = 120,
              pvalueCutoff  = 0.2,
              pAdjustMethod = "BH",
              verbose       = FALSE)
dgn <- setReadable(dgn, 'org.Hs.eg.db')
head(dgn, 3)

##snp-基因-疾病相关性富集分析
#ORA
snp <- c("rs1401296", "rs9315050", "rs5498", "rs1524668", "rs147377392",
         "rs841", "rs909253", "rs7193343", "rs3918232", "rs3760396",
         "rs2231137", "rs10947803", "rs17222919", "rs386602276", "rs11053646",
         "rs1805192", "rs139564723", "rs2230806", "rs20417", "rs966221")
dgnv <- enrichDGNv(snp)
head(dgnv)

##reactome富集分析
#ORA
library(ReactomePA)
gene <- c("11171", "8243", "112464", "2194",
          "9318", "79026", "1654", "65003",
          "6240", "3476", "6238", "3836",
          "4176", "1017", "249")
yy = enrichPathway(gene, pvalueCutoff=0.05)
head(summary(yy))
#GSEA
y <- gsePathway(geneList, nPerm=10000,pvalueCutoff=0.2, pAdjustMethod="BH", verbose=FALSE)
res <- as.data.frame(y)
head(res)

##MeSH富集分析
#ORA
library(meshes)
data(geneList, package="DOSE")
de <- names(geneList)[1:100]
x <- enrichMeSH(de, MeSHDb = "MeSH.Hsa.eg.db", database='gendoo', category = 'C')
head(x)
#GSEA
y <- gseMeSH(geneList, MeSHDb = "MeSH.Hsa.eg.db", database = 'gene2pubmed', category = "G")
head(y)

##MSigdDB数据库富集分析
library(msigdbr)
library(clusterProfiler)
m_t2g <- msigdbr(species = "Homo sapiens",category = "C6") %>% 
  dplyr::select(gs_name,entrez_gene)
#ORA
x <- enricher(gene,TERM2GENE = m_t2g)
#GSEA
y <- GSEA(geneList,TERM2GENE = m_t2g)


######################################2、富集分析数据库
##KEGG数据库
library(KEGGREST)
listDatabases()#显示KEGGREST所包含的数据内容, 可以在进一步查询中使用这些数据。
org <- keggList("organism")
head(org)

gs<-keggGet('hsa04151')
names(gs[[1]]) 
kegggenes <- gs[[1]]$GENE[seq(1,length(gs[[1]]$GENE),2)]
png <- keggGet("hsa04151", "image") 
t <- tempfile()
library(png)
writePNG(png,t)
if (interactive()) browseURL(t)

##reactome数据库 
library(reactome.db)
ls("package:reactome.db")
keytypes(reactome.db)
#看此物件中的資料之欄位名稱
columns(reactome.db)
#直接读取特定key种类的值
keys(reactome.db, keys ="PATHNAME")
#最后使用keys來query此annotation database
AnnotationDbi::select(reactome.db, keys = c("6794"), columns = c("PATHID","PATHNAME"), keytypes="ENTREZID") ## 查看单个基因所在通路

a <- as.list(reactomePATHID2EXTID)$ "R-HSA-198203"
b <- as.list(reactomePATHID2EXTID)$ "R-HSA-199418"
c <- as.list(reactomePATHID2EXTID)$ "R-HSA-2219528"
reagenes <-union(c(a,b), c) ## 取并集
intersect(kegggenes, reagenes)
setdiff(kegggenes, reagenes) ## 取kegg数据库中特有元素
setdiff(reagenes, kegggenes) ## 取ReactomeDB数据库中特有元素

##DO数据库
library(DO.db)
ls("package:DO.db")
#可以查看每个DO与它所对应的上级条目DO，每个DO都会有不止一个的上级DO。
xx <- as.list(DOANCESTOR)
#可以查看每个DO与它所对应的父条目DO，每个DO都有且只有一个父DO。
xx <- as.list(DOPARENTS)
#可以查看每个DO与它所对应的下级DO的关系列表，大多数DO都不止一个子条目DO，所有的下级DO都会列出。
xx <- as.list(DOOFFSPRING)
#以查看每个DO与它所对应的子条目DO的关系列表，大多数DO都不止一个子条目DO。
xx <- as.list(DOCHILDREN)
