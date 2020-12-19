setwd('C:\\Users\\Administrator\\Desktop\\data')

samples <- read.table("samples.txt",header = T,row.names = NULL)
normal <- samples[substr(samples$row.names,1,1) == "N",]
tumor <- samples[substr(samples$row.names,1,1) == "T",]

normal$ID <- gsub("N","",normal$row.names)
tumor$ID <- gsub("T","",tumor$row.names)

all <- dplyr::inner_join(normal,tumor,by = "ID")

all_normal <- all[,c(1,2,4)];all_normal$group <- "N";all_normal <- all_normal[,-1];colnames(all_normal) <- c("ID","individual","group")
all_tumor <- all[,c(6,7,9)];all_tumor$group <- "T";all_tumor <- all_tumor[,-1];colnames(all_tumor) <- c("ID","individual","group")

all2 <- rbind(all_normal,all_tumor)

ma <- read.table('GSE66229_series_matrix.txt',header = T,row.names = 1)

rt <-read.table("GPL570-55999.txt",sep="\t",header=T,row.names = 1,skip = 16,fill = T,quote = "")
rt2<-rt[,c(1,11)]
dim(rt2)
rt3<-rt2[rt2$ENTREZ_GENE_ID!="",]
dim(rt3)
x<-grep("/",rt3$ENTREZ_GENE_ID)
rt4<-rt3[-x,]
dim(rt4) # 41834     2

ma2<-ma[rownames(ma) %in% rownames(rt4),]
dim(ma2) #32855    44

rt5<-rt4[rownames(ma2),]
dim(rt5)  #32855     2

#验证上面两行是否正确
length(intersect(rownames(rt5),rownames(ma2)))  #32855

ma3<-cbind(rt5,ma2)
ma4<-ma3[,-1]
rownames(ma4)<-NULL

rt<-ma4
rt$ENTREZ_GENE_ID<-as.character(rt$ENTREZ_GENE_ID)
#perl文件将探针转换成genebank ID、gene symbol变成geneMatrix.txt或geneMatrix2.txt
#分组：perl文件进行分组：geneMatrix.txt+sample1+sample2 转换成sampleExp.txt

#setwd("C:\\Users\\think\\Desktop\\20168\\dif")   
#rt<-read.table("sampleExp.txt",sep="\t",header=T)  # 1、读取数据
rt<-as.matrix(rt)   # 变成矩阵
rownames(rt)=rt[,1]     # 第一列命名为行名
exp<-rt[,2:ncol(rt)]   # 去掉第一列，剩下的全部为表达量数据
dimnames<-list(rownames(exp),colnames(exp))    # 命名行和列
exp<-matrix(as.numeric(as.matrix(exp)),nrow=nrow(exp),dimnames=dimnames)    # 全部设置为数值类型，避免有文本值

library("impute")
library(limma)
mat=impute.knn(exp)
rt=mat$data      # 以上三个，补全缺失数据
dim(rt)  #32855    44
rt=avereps(rt)# 相同基因取平均值
dim(rt)  #16780    44

pdf(file="rawbox.pdf")
boxplot(rt,col = "blue",xaxt = "n",outline = F)  # 导出原始表达量boxplot图
dev.off()     

rt=normalizeBetweenArrays(as.matrix(rt))   # 对数据矫正，归一化

pdf(file="normalbox.pdf")
boxplot(rt,col = "red",xaxt = "n",outline = F)    # 导出正态化表达量boxplot图
dev.off()

#rt=log2(rt)      # 4、数值取log2
# normal<-read.table("normal.txt",stringsAsFactors = F)
# AD<-read.table("AD.txt",stringsAsFactors = F)

rt2<-cbind(rt[,all_normal$ID],rt[,all_tumor$ID])
head(rt)
head(rt2)

# write.table(rt2,"GSE.txt",quote =FALSE,sep = "\t")  #第一行错开一列,自己补全


#####################################################差异分析######################################
library(limma)

exp <- rt2
logFoldChange= 0.585
adjustP=0.05

class <- c(rep("normal",98),rep("tumor",98))
class<- factor(class,levels=c("normal","tumor"),labels=c("normal","tumor"))
individual <- factor(all2$individual)
design <- model.matrix(~individual + class)
fit <- lmFit(exp,design)  #线性模型拟合
fit1 <- eBayes(fit)   #贝叶斯检验
allDiff<-topTable(fit1,coef = "classtumor",adjust='fdr',number=100000)
dim(allDiff)
head(allDiff)
allLimma=allDiff
allLimma=allLimma[order(allLimma$logFC),]
allLimma=rbind(Gene=colnames(allLimma),allLimma)
write.table(allLimma,"GSE66229_allDiff.txt",sep="\t",quote=F,col.names = F)

allDiff<- allDiff[with(allDiff, (abs(logFC)>=logFoldChange & adj.P.Val <= adjustP )), ]
dim(allDiff) #292   6
head(allDiff)

write.table(allDiff,"GSE66229_filteredDiff.txt",sep="\t",quote=F)   #保存所有基因的表达数据和统计量，这里导出的文件可用于画火山图


