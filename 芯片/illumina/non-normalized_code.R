rt <- read.table('GSE57691_non-normalized_data.txt'，header=T,sep="\t",row.name =1)
head(rt[,1:5])
#               Sample.1 Detection.Pval Sample.2 Detection.Pval.1 Sample.3
# ILMN_1802380    958.2        1.00000   3244.0          1.00000    592.4
# ILMN_1893287     93.9        0.95485    137.9          0.36388     81.5
# ILMN_1736104     92.8        0.64011    126.0          0.14608     81.0
# ILMN_1792389    146.9        0.95219    255.5          0.98539    106.2
# ILMN_1854015    102.2        0.55644    236.4          0.97875     92.4
# ILMN_1740305     96.1        0.92696    203.4          0.93891     92.0
detp<- rt[,seq(2,136,2)]
rt <- rt[,seq(1,136,2)]

present <- rownames(detp)[rowSums(detp<=0.05) > 0] # 统计每个探针在所有样本中分类为"P"的个数
rt <- rt[present,] # 留下至少在一个样本中表达的探针
dim(rt) #54675    44
rt <- tibble::rownames_to_column(rt,var="gene")

rt2 <- read.table("GPL10558-50081.txt",header = T,sep = "\t",fill = T,quote = "")
rt2 <- rt2[,c(1,6)]
rt3<-rt2[rt2$ILMN_Gene!="",]
dim(rt3)

rt4 <- dplyr::inner_join(rt3,rt,by=c("ID"="gene"))
rt4 <- rt4[,-1]

rt <- rt4

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

rt=log2(rt)      # 4、数值取log2

write.table(rt,"GSE57691.txt",sep = "\t",quote = F)

