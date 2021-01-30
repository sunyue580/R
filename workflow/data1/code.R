#加载包
library(org.Hs.eg.db)
library(clusterProfiler)
library(biomaRt)
library(dplyr)
library(stringr)
##提取三个基因
#读取文件
rt <- read.table("HTSeq - FPKM-UQ.merge.txt",header = T)
#把基因名.后面的版本号去掉用于后续转换基因名
rt$Tags <- sapply(strsplit(rt$Tags,"\\."),"[",1)

#以下四行准备基因名文件用于转化
ensembl = useEnsembl(biomart="ensembl", dataset="hsapiens_gene_ensembl")
genes <- getBM(attributes=c('ensembl_gene_id','hgnc_symbol'), mart = ensembl)
genes <- genes[!duplicated(genes$ensembl_gene_id),]
dim(genes) #67139     2

#转换基因名
rt2 <- dplyr::inner_join(genes,rt,by=c("ensembl_gene_id"="Tags"))
#去掉第一列的基因名
rt2 <- rt2[,-1]
#提取三个基因
rt3 <- rt2[rt2$hgnc_symbol %in% c("SRSF1","SRSF2","PTBP1"),] 
#行列转置
rt3 <- data.frame(t(rt3))
#第一行转换成列名
colnames(rt3) <- rt3[1,]
#去掉第一行
rt3 <- rt3[-1,]
#行名变成列
rt3 <- tibble::rownames_to_column(rt3,var = "ID")

#以下两行用于后续tumor和normal基因差异比较
gene_compare <- rt3
#提取肿瘤和正常编号赋值给group变量
gene_compare$group <- str_sub(gene_compare$ID,start=14,end=15)
gene_compare <- dplyr::mutate(gene_compare,group=ifelse(group=="11","Normal","Tumor"))  

#提取tcga编号前12位
rt3$ID <- str_sub(rt3$ID,start=1,end=12)
rt3 <- rt3[!duplicated(rt3$ID),]  

##临床信息
#读取临床文件
clinical <- read.table("Clinical BCR XML.merge.txt",header = T,sep = "\t")
#提取所需列
clinical <- clinical[,c(1:3,6,10,20)]
#列排序
clinical <- clinical[,c(1,3,4,2,5,6)]
#修改列命
colnames(clinical) <- c("samples","OS","status","gender","stage","age")
clinical <- clinical[clinical$stage != "",]

#处理变量
clinical <- dplyr::mutate(clinical,status=ifelse(status=="Alive",0,1),
                          gender = ifelse(gender=="FEMALE",1,2),
                          stage = ifelse(stage=="Stage I",1,
                                         ifelse(stage=="Stage II",1,2)),
                          age = ifelse(age <= 60,1,2))
#检查是否有重复样本
which(duplicated(clinical$samples))  #0
#样本名的-变成.
clinical$samples <- gsub("-",".",clinical$samples)

all <- dplyr::inner_join(clinical,rt3,by=c("samples"="ID"))
dim(all) #527   9
#列名变成行名
all <- tibble::column_to_rownames(all,var = "samples")
#三个基因表达量变成数值型
all$PTBP1 <- as.numeric(all$PTBP1)
all$SRSF1 <- as.numeric(all$SRSF1)
all$SRSF2 <- as.numeric(all$SRSF2)

##单因素cox回归
library(survival)
#77-97行输出全部单因素cox结果
#设置p值
pFilter=0.05 
outTab=data.frame()
sigGenes=c("OS","status")
for(i in colnames(all[,3:ncol(all)])){
  cox <- coxph(Surv(OS, status) ~ all[,i], data = all)
  coxSummary = summary(cox)
  coxP=coxSummary$coefficients[,"Pr(>|z|)"]
  if(coxP<=1){
    sigGenes=c(sigGenes,i)
    outTab=rbind(outTab,
                 cbind(id=i,
                       HR=coxSummary$conf.int[,"exp(coef)"],
                       HR.95L=coxSummary$conf.int[,"lower .95"],
                       HR.95H=coxSummary$conf.int[,"upper .95"],
                       pvalue=coxSummary$coefficients[,"Pr(>|z|)"])
    )
  }
}
write.table(outTab,file="uniCox_all.txt",sep="\t",row.names=F,quote=F)
uniSigExp=all[,sigGenes]
uniSigExp=cbind(id=row.names(uniSigExp),uniSigExp)
write.table(uniSigExp,file="uniSigExp_all.txt",sep="\t",row.names=F,quote=F)

#102-124行输出显著单因素cox结果
#设置p值
pFilter=0.05 
outTab=data.frame()
sigGenes=c("OS","status")
for(i in colnames(all[,3:ncol(all)])){
  cox <- coxph(Surv(OS, status) ~ all[,i], data = all)
  coxSummary = summary(cox)
  coxP=coxSummary$coefficients[,"Pr(>|z|)"]
  if(coxP<pFilter){
    sigGenes=c(sigGenes,i)
    outTab=rbind(outTab,
                 cbind(id=i,
                       HR=coxSummary$conf.int[,"exp(coef)"],
                       HR.95L=coxSummary$conf.int[,"lower .95"],
                       HR.95H=coxSummary$conf.int[,"upper .95"],
                       pvalue=coxSummary$coefficients[,"Pr(>|z|)"])
    )
  }
}
uniSigExp=all[,sigGenes]
uniSigExp=cbind(id=row.names(uniSigExp),uniSigExp)
write.table(uniSigExp,file="uniSigExp.txt",sep="\t",row.names=F,quote=F)


##多因素cox回归
#读取文件
rt=read.table("uniSigExp.txt",header=T,sep="\t",check.names=F,row.names=1)    #??ȡ?????ļ?

#多因素cox回归
multiCox=coxph(Surv(OS, status) ~ ., data = rt)
#逐步回归
multiCox=step(multiCox,direction = "both")  
multiCoxSum=summary(multiCox)

#134-143行输出显著多因素cox结果
outTab=data.frame()
outTab=cbind(
  coef=multiCoxSum$coefficients[,"coef"],    
  HR=multiCoxSum$conf.int[,"exp(coef)"],
  HR.95L=multiCoxSum$conf.int[,"lower .95"],
  HR.95H=multiCoxSum$conf.int[,"upper .95"],
  pvalue=multiCoxSum$coefficients[,"Pr(>|z|)"])    
outTab=cbind(id=row.names(outTab),outTab)
write.table(outTab,file="multiCox.txt",sep="\t",row.names=F,quote=F)

##生存分析
#读取文件
rt=read.table("uniSigExp_all.txt",sep="\t",header=T,check.names=F) 
rt$OS=rt$OS/365
#根据基因表达量分为高表达和低表达
rt <- dplyr::mutate(rt,
                    PTBP1_group = ifelse(PTBP1 > median(rt$PTBP1),"high","low"),
                    SRSF1_group = ifelse(SRSF1 > median(rt$SRSF1),"high","low"),
                    SRSF2_group = ifelse(SRSF2 > median(rt$SRSF2),"high","low"),
                    )
#PTBP1
diff=survdiff(Surv(OS,status) ~ PTBP1_group,data = rt)
pValue=1-pchisq(diff$chisq,df=1)
pValue=signif(pValue,4)     #保留四位小数
pValue=format(pValue, scientific = TRUE)    #科学计数法表示

fit <- survfit(Surv(OS,status) ~ PTBP1_group, data = rt)

#绘制生存曲线
library("survminer")
pdf(file="survival_PTBP1.pdf",onefile = FALSE,     #onefile = FALSE表示图片出现在pdf的一张图上面，否则会在图片前面出现一个空白页
    width = 5.5,             #图片的宽度
    height =5)             #图片的高度
ggsurvplot(fit, 
           data=rt,
           conf.int=TRUE,
           pval=paste0("p=",pValue),
           pval.size=4,
           risk.table=TRUE,
           legend.labs=c("High", "Low"),
           legend.title="Risk",
           xlab="Time(years)",
           break.time.by = 1,
           risk.table.title="",
           palette=c("red", "blue"),
           risk.table.height=.25)
dev.off()


#SRSF1
diff=survdiff(Surv(OS,status) ~ SRSF1_group,data = rt)
pValue=1-pchisq(diff$chisq,df=1)
pValue=signif(pValue,4)     #保留四位小数
pValue=format(pValue, scientific = TRUE)    #科学计数法表示

fit <- survfit(Surv(OS,status) ~ SRSF1_group, data = rt)

#绘制生存曲线
library("survminer")
pdf(file="survival_SRSF1.pdf",onefile = FALSE,     #onefile = FALSE表示图片出现在pdf的一张图上面，否则会在图片前面出现一个空白页
    width = 5.5,             #图片的宽度
    height =5)             #图片的高度
ggsurvplot(fit, 
           data=rt,
           conf.int=TRUE,
           pval=paste0("p=",pValue),
           pval.size=4,
           risk.table=TRUE,
           legend.labs=c("High", "Low"),
           legend.title="Risk",
           xlab="Time(years)",
           break.time.by = 1,
           risk.table.title="",
           palette=c("red", "blue"),
           risk.table.height=.25)
dev.off()

#SRSF2
diff=survdiff(Surv(OS,status) ~ SRSF2_group,data = rt)
pValue=1-pchisq(diff$chisq,df=1)
pValue=signif(pValue,4)     #保留四位小数
pValue=format(pValue, scientific = TRUE)    #科学计数法表示

fit <- survfit(Surv(OS,status) ~ SRSF2_group, data = rt)

#绘制生存曲线
library("survminer")
pdf(file="survival_SRSF2.pdf",onefile = FALSE,     #onefile = FALSE表示图片出现在pdf的一张图上面，否则会在图片前面出现一个空白页
    width = 5.5,             #图片的宽度
    height =5)             #图片的高度
ggsurvplot(fit, 
           data=rt,
           conf.int=TRUE,
           pval=paste0("p=",pValue),
           pval.size=4,
           risk.table=TRUE,
           legend.labs=c("High", "Low"),
           legend.title="Risk",
           xlab="Time(years)",
           break.time.by = 1,
           risk.table.title="",
           palette=c("red", "blue"),
           risk.table.height=.25)
dev.off()

##ROC
library(survivalROC)
#PTBP1
pdf(file="ROC_PTBP1.pdf",width=6,height=6)
par(oma=c(0.5,1,0,1),font.lab=1.5,font.axis=1.5)
roc=survivalROC(Stime=rt$OS, status=rt$status, marker = rt$PTBP1, 
                predict.time =1, method="KM")
plot(roc$FP, roc$TP, type="l", xlim=c(0,1), ylim=c(0,1),col='red', 
     xlab="False positive rate", ylab="True positive rate",
     main=paste("ROC curve (", "AUC = ",sprintf("%.3f",roc$AUC),")"),
     lwd = 2, cex.main=1.3, cex.lab=1.2, cex.axis=1.2, font=1.2)
abline(0,1)
dev.off()

#SRSF1
pdf(file="ROC_SRSF1.pdf",width=6,height=6)
par(oma=c(0.5,1,0,1),font.lab=1.5,font.axis=1.5)
roc=survivalROC(Stime=rt$OS, status=rt$status, marker = rt$SRSF1, 
                predict.time =1, method="KM")
plot(roc$FP, roc$TP, type="l", xlim=c(0,1), ylim=c(0,1),col='red', 
     xlab="False positive rate", ylab="True positive rate",
     main=paste("ROC curve (", "AUC = ",sprintf("%.3f",roc$AUC),")"),
     lwd = 2, cex.main=1.3, cex.lab=1.2, cex.axis=1.2, font=1.2)
abline(0,1)
dev.off()

#SRSF2
pdf(file="ROC_SRSF2.pdf",width=6,height=6)
par(oma=c(0.5,1,0,1),font.lab=1.5,font.axis=1.5)
roc=survivalROC(Stime=rt$OS, status=rt$status, marker = rt$SRSF2, 
                predict.time =1, method="KM")
plot(roc$FP, roc$TP, type="l", xlim=c(0,1), ylim=c(0,1),col='red', 
     xlab="False positive rate", ylab="True positive rate",
     main=paste("ROC curve (", "AUC = ",sprintf("%.3f",roc$AUC),")"),
     lwd = 2, cex.main=1.3, cex.lab=1.2, cex.axis=1.2, font=1.2)
abline(0,1)
dev.off()


#ROC cutoff分组生存分析
#ROC曲线cutoff值为(灵敏度+特异度-1)的最大值
#灵敏度=真阳性率；特异度=真阴性率=1-假阳性率
#ROC横坐标是假阳性率(1-特异度)；纵坐标是真阳性率(灵敏度)
roc1=survivalROC(Stime=rt$OS, status=rt$status, marker = rt$PTBP1, 
                predict.time =1, method="KM")
cutoff1= roc1$cut.values[which.max(roc1$TP-roc1$FP)] #计算最佳截断值

roc2=survivalROC(Stime=rt$OS, status=rt$status, marker = rt$SRSF1, 
                 predict.time =1, method="KM")
cutoff2= roc2$cut.values[which.max(roc2$TP-roc2$FP)] #计算最佳截断值

roc3=survivalROC(Stime=rt$OS, status=rt$status, marker = rt$SRSF2, 
                 predict.time =1, method="KM")
cutoff3= roc3$cut.values[which.max(roc3$TP-roc3$FP)] #计算最佳截断值

rt <- dplyr::mutate(rt,PTBP1_roccutoff=ifelse(PTBP1>cutoff1,"high","low"),
                    SRSF1_roccutoff=ifelse(SRSF1>cutoff2,"high","low"),
                    SRSF2_roccutoff=ifelse(SRSF2>cutoff3,"high","low"))

#PTBP1
diff=survdiff(Surv(OS,status) ~ PTBP1_roccutoff,data = rt)
pValue=1-pchisq(diff$chisq,df=1)
pValue=signif(pValue,4)     #保留四位小数
pValue=format(pValue, scientific = TRUE)    #科学计数法表示

fit <- survfit(Surv(OS,status) ~ PTBP1_roccutoff, data = rt)

#绘制生存曲线
library("survminer")
pdf(file="survival_PTBP1.pdf",onefile = FALSE,     #onefile = FALSE表示图片出现在pdf的一张图上面，否则会在图片前面出现一个空白页
    width = 5.5,             #图片的宽度
    height =5)             #图片的高度
ggsurvplot(fit, 
           data=rt,
           conf.int=TRUE,
           pval=paste0("p=",pValue),
           pval.size=4,
           risk.table=TRUE,
           legend.labs=c("High", "Low"),
           legend.title="Risk",
           xlab="Time(years)",
           break.time.by = 1,
           risk.table.title="",
           palette=c("red", "blue"),
           risk.table.height=.25)
dev.off()


#SRSF1
diff=survdiff(Surv(OS,status) ~ SRSF1__roccutoff,data = rt)
pValue=1-pchisq(diff$chisq,df=1)
pValue=signif(pValue,4)     #保留四位小数
pValue=format(pValue, scientific = TRUE)    #科学计数法表示

fit <- survfit(Surv(OS,status) ~ SRSF1__roccutoff, data = rt)

#绘制生存曲线
library("survminer")
pdf(file="survival_SRSF1.pdf",onefile = FALSE,     #onefile = FALSE表示图片出现在pdf的一张图上面，否则会在图片前面出现一个空白页
    width = 5.5,             #图片的宽度
    height =5)             #图片的高度
ggsurvplot(fit, 
           data=rt,
           conf.int=TRUE,
           pval=paste0("p=",pValue),
           pval.size=4,
           risk.table=TRUE,
           legend.labs=c("High", "Low"),
           legend.title="Risk",
           xlab="Time(years)",
           break.time.by = 1,
           risk.table.title="",
           palette=c("red", "blue"),
           risk.table.height=.25)
dev.off()

#SRSF2
diff=survdiff(Surv(OS,status) ~ SRSF2_roccutoff,data = rt)
pValue=1-pchisq(diff$chisq,df=1)
pValue=signif(pValue,4)     #保留四位小数
pValue=format(pValue, scientific = TRUE)    #科学计数法表示

fit <- survfit(Surv(OS,status) ~ SRSF2_roccutoff, data = rt)

#绘制生存曲线
library("survminer")
pdf(file="survival_SRSF2.pdf",onefile = FALSE,     #onefile = FALSE表示图片出现在pdf的一张图上面，否则会在图片前面出现一个空白页
    width = 5.5,             #图片的宽度
    height =5)             #图片的高度
ggsurvplot(fit, 
           data=rt,
           conf.int=TRUE,
           pval=paste0("p=",pValue),
           pval.size=4,
           risk.table=TRUE,
           legend.labs=c("High", "Low"),
           legend.title="Risk",
           xlab="Time(years)",
           break.time.by = 1,
           risk.table.title="",
           palette=c("red", "blue"),
           risk.table.height=.25)
dev.off()




##组间比较
library(beeswarm)
#fpkm取log2
gene_compare[,2:4] <- log2(do.call(cbind,lapply(gene_compare[,2:4],as.numeric)))

#PTBP1
#t检验
cliTest<-t.test(as.numeric(PTBP1) ~ group, data=gene_compare)
pValue=cliTest$p.value
pval=signif(pValue,2)
#pval=format(pval, scientific = TRUE)

pdf("PTBP1_compare.pdf")
par(oma=c(0.5,1,0,1))
boxplot(as.numeric(PTBP1) ~ group, data = gene_compare,  #names=xlabel,ylab = ylab,ylim=c(yMin,yMax),
        main=paste0("PTBP1"," (p=",pval,")"),#xlab=clinical,
        cex.main=1.4, cex.lab=1.4, cex.axis=1.3,outline = FALSE)
beeswarm(as.numeric(PTBP1) ~ group, data = gene_compare, lwd=0.1,col =c("blue","red"),
         pch = 16, add = TRUE, corral="wrap")
dev.off()

#SRSF1
#t检验
cliTest<-t.test(as.numeric(SRSF1) ~ group, data=gene_compare)
pValue=cliTest$p.value
pval=signif(pValue,2)
#pval=format(pval, scientific = TRUE)

pdf("SRSF1_compare.pdf")
par(oma=c(0.5,1,0,1))
boxplot(as.numeric(SRSF1) ~ group, data = gene_compare,  #names=xlabel,ylab = ylab,ylim=c(yMin,yMax),
          main=paste0("SRSF1"," (p=",pval,")"),#xlab=clinical,
          cex.main=1.4, cex.lab=1.4, cex.axis=1.3,outline = FALSE)
beeswarm(as.numeric(SRSF1) ~ group, data = gene_compare, lwd=0.1,col =c("blue","red"),
         pch = 16, add = TRUE, corral="wrap")
dev.off()

#SRSF2
cliTest<-t.test(as.numeric(SRSF2) ~ group, data=gene_compare)
pValue=cliTest$p.value
pval=signif(pValue,3)
#pval=format(pval, scientific = TRUE)

pdf("SRSF2_compare.pdf")
par(oma=c(0.5,1,0,1))
boxplot(as.numeric(SRSF2) ~ group, data = gene_compare,  #names=xlabel,ylab = ylab,ylim=c(yMin,yMax),
        main=paste0("SRSF2"," (p=",pval,")"),#xlab=clinical,
        cex.main=1.4, cex.lab=1.4, cex.axis=1.3,outline = FALSE)
beeswarm(as.numeric(SRSF2) ~ group, data = gene_compare, lwd=0.1,col =c("blue","red"),
         pch = 16, add = TRUE, corral="wrap")
dev.off()


