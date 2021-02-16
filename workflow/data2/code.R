
rt <- read.table("GEO.txt")
head(rt)
#       V1
# 1  GSE6099
# 2  GSE6752
# 3  GSE6956
# 4  GSE8218
# 5 GSE32269
# 6  GSE2443
library(GEOmirror)
library(GEOquery)
phenoDat0 <- list()
for (i in rt$V1) {

eSet=geoChina(i)
eSet
eSet=eSet[[1]]

# ###########################表达矩阵
# probes_expr <- exprs(eSet);dim(probes_expr)
# head(probes_expr[,1:4])
# boxplot(probes_expr,las=2)

###########################表型信息
phenoDat <- pData(eSet)
#head(phenoDat[,1:4])
phenoDat <- cbind(dataset = i,phenoDat)
phenoDat0[[i]] <- phenoDat
###########################平台
#eSet@annotation

# grep("GPL",phenoDat)
# grep("scan",phenoDat)
# dim(phenoDat)[1]
}
#names(phenoDat0) <- rt$V1
save(phenoDat0,file="all.RData")

################################统计信息
load("all.RData",verbose = T)

info0 <- data.frame(GSE="",
                   platform="",
                   sample_num="",
                   institute="",
                   metastasis_num=""
                   )
for (i in 1:length(data)) {
  
col1 <- data[[i]][1,1]
col2 <- data[[i]][1,grep("GPL",data[[i]])]  
col3 <- dim(data[[i]])[1] 
col4 <- data[[i]][1,grep("institute",colnames(data[[i]]))] 
col5 <- length(grep("etasta",data[[i]][,grep("etasta",data[[i]])]))
col <- c(col1,col2,col3,col4,col5)
info0 <- rbind(info0,col)

}

info0 <- info0[-1,]
#write.csv(info0,"info.csv",row.names = F)

for (i in 1:length(data)) {
  x <- data[[i]]
  write.csv(x,paste("datasets/",names(data)[i],".csv",sep = "")) 
}


