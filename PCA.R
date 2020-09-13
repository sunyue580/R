
DE <- read.csv("Differentially_Expressed_Genes.csv",sep = "\t",header = T,row.names = 1,check.names = F)
#DE <- read.csv("ALL_Differentially_Expressed_Genes.csv",sep = ",",header = T,row.names = 1,check.names = F)
colnames(DE) <- c("A1","A2","A3","B1","B2","B3")

####热图
library(pheatmap)
annotation_col = data.frame(
  group = factor(rep(c("A", "B"), each = 3))
)
rownames(annotation_col) = c("A1","A2","A3","B1","B2","B3")

pheatmap(DE,scale = "row",
         annotation_col = annotation_col,
         cluster_cols = F,
         show_rownames = F,
         border_color = NA,
         filename = "DE_heatmap.pdf")

DE_group <- as.data.frame(t(DE)) 
DE_group$group <- rep(c("A","B"),each = 3)
DE_group$sample <- rownames(DE_group)


####----------------------------------------7种PCA方法-------------------------------------------------------
#注：只有第4种结果不同

####第1种PCA——stats包的prcomp函数
pca = prcomp(t(DE), scale. = TRUE)
#每个主成分方差占比
percentVar <- pca$sdev^2 / sum( pca$sdev^2)
ss=summary(pca)
#pca$rotation  相关系数矩阵的特征向量(最后一种分解步骤PCA方法)
#pca$x   主成分得分(最后一种分解步骤PCA方法)

#碎石图
library(factoextra)
fviz_screeplot(pca, ncp=10, main="Scree plot (scale)",xlab="Principle components (PCs)")
#绘图：
library(ggplot2)
plot(pca$x[,1:2],col=rep(c(1,2),each=3))
library(ggrepel)
library(ggfortify)
p <- autoplot(pca,data = DE_group,colour = "group",size = 4) + 
  scale_colour_manual(values = c("A" = "#00dadf","B" = "#ff9288")) +
  geom_text_repel(aes(label = sample),nudge_x = 0.02,nudge_y = 0.02) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.position="right")
pdf("PCA.pdf")
p
dev.off()


####第2种PCA——PCAtools包的pca函数
library(PCAtools)
metadata <- data.frame(row.names = c("A1","A2","A3","B1","B2","B3"),
                       group = factor(rep(c("A", "B"), each = 3)))
pca <- pca(DE,metadata = metadata,scale = T)
## -----------------------------------------------------------
pca_loadings <- pca$loadings
pca_loadings[1:4, 1:4]
## -----------------------------------------------------------
pca_rotated <- pca$rotated
pca_rotated[1:4, 1:4]
## ---- message=FALSE-----------------------------------------
screeplot(pca)
## 用pca$rotated作图
biplot(pca, 
       x = 'PC1',                 # x 轴
       y = 'PC2',                 # y 轴
       colby = 'group',          # 颜色映射
       colkey = c("A" = "#00dadf","B" = "#ff9288"),
       #shape = 'stage',           # 形状映射
       legendPosition = 'right',  # 图例位置
)
## -----------------------------------------------------------
plotloadings(pca)
## -----------------------------------------------------------
#save(pca, pca_rotated, pca_loadings, file = 'pca.rdata')


####第3种PCA——stats包的princomp函数(只能用于观测数大于变量数)
pca <- princomp(as.data.frame(t(DE)),cor = F)
summary(pca)
screeplot(pca, type='lines')
biplot(pca)



####第4种PCA——gmodels包的fast.prcomp函数
library(gmodels)
library(ggpubr)
library(ggthemes)
pca.info <- fast.prcomp(DE,scale. = T)
pca.data <- data.frame(sample = rownames(pca.info$rotation),
                       Type = c(rep("Control",3),rep("Case",3)),
                       pca.info$rotation)
ggscatter(pca.data,x = "PC1",y = "PC2",
          color = "Type",
          #ellipse = T,
          size = 4,
          palette = c("#00dadf","#ff9288"),
          label = "sample",
          repel = T
)


####第5种PCA——FactoMineR包的PCA函数
library(factoextra)
library(FactoMineR)
DE_t <- as.data.frame(t(DE))
DE_t$group <- rep(c("A","B"),each = 3)

pca_fa <- PCA(as.data.frame(t(DE)),graph = T)
fviz_pca_ind(pca_fa,
             #geom.ind = "point", # show points only (nbut not "text")
             pointsize =3,pointshape = 21,fill.ind = as.factor(DE_t$group), # color by groups
             palette = c("#00dadf","#ff9288"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups",
             title="")+
  labs(x = "PC1 (77.8%)",y = "PC2 (7.9%)") +
  theme_classic() +
  theme(
    text=element_text(size=12,face="plain",color="black"),
    axis.title=element_text(size=11,face="plain",color="black"),
    axis.text = element_text(size=10,face="plain",color="black"),
    legend.title = element_text(size=11,face="plain",color="black"),
    legend.text = element_text(size=11,face="plain",color="black"),
    legend.background = element_blank(),
    legend.position=c(0.15,0.88)
  )



####第6种PCA——psych包的principal函数
library(psych)
pc<-principal(as.data.frame(t(DE)),nfactors=6,rotate = "none",score=TRUE)
pc$loadings  #SS loadings的平方根等于第1种PCA的pca$sdev
#head(pc$scores)



####第7种PCA——分解步骤
dat_scale=scale(as.data.frame(t(DE)),scale=T)
options(digits=4, scipen=4)
kable(head(dat_scale))

#求相关系数矩阵
dat_cor=cor(dat_scale)
options(digits=4, scipen=4)
kable(dat_cor)

#计算特征值和特征向量
dat_eigen=eigen(dat_cor)
dat_var=dat_eigen$values ## 相关系数矩阵的特征值
options(digits=4, scipen=4)
dat_var

pca_var=dat_var/sum(dat_var)
pca_var
pca_cvar=cumsum(dat_var)/sum(dat_var)
pca_cvar

#碎石图和累计贡献图
library(ggplot2)
p=ggplot(,aes(x=1:12,y=pca_var))
p1=ggplot(,aes(x=1:12,y=pca_cvar))
p+geom_point(pch=2,lwd=3,col=2)+geom_line(col=2,lwd=1.2)
p1+geom_point(pch=2,lwd=3,col=2)+geom_line(col=2,lwd=1.2)

#主成分载荷：主成分载荷表示各个主成分与原始变量的相关系数
pca_vect = dat_eigen$vector  ## 相关系数矩阵的特征向量
loadings=sweep(pca_vect,2,sqrt(pca_var),"*")
rownames(loadings)=colnames(USJudgeRatings)
options(digits=4, scipen=4)
kable(loadings[,1:2]) 

#主成分得分计算和图示：将中心化的变量dat_scale乘以特征向量矩阵即得到每个观测值的得分
pca_score=as.matrix(dat_scale)%*%pca_vect 
head(pca_score[,1:2])

p=ggplot(,aes(x=pca_score[,1],y=pca_score[,2]))+geom_point(color=USJudgeRatings[,1],pch=USJudgeRatings[,2])
p 

