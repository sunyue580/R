##离群值是小于Q1-(1.5)IQR或大于Q3+(1.5)IQR的值

#可视化离群值方法一
data("warpbreaks") #加载数据集
boxplot(warpbreaks)$out #画箱式图
#可视化离群值方法二
library(ggstatsplot)
ggbetweenstats(warpbreaks,wool, breaks, outlier.tagging = TRUE)


#删除离群值方法一
outliers <-boxplot(warpbreaks$breaks, plot=FALSE)$out
x<-warpbreaks 
x<- x[-which(x$breaks %in% outliers),]  ##提取出不包含离群值的数据集
#删除离群值方法二
Q <- quantile(warpbreaks$breaks, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(warpbreaks$breaks)  #计算25%与75%的值
up <- Q[2]+1.5*iqr # Upper Range 上限
low <- Q[1]-1.5*iqr # Lower Range   下限
eliminated <- subset(warpbreaks, warpbreaks$breaks > (Q[1] - 1.5*iqr) & warpbreaks$breaks < (Q[2]+1.5*iqr))
ggbetweenstats(eliminated, wool, breaks, outlier.tagging = TRUE)   #提取出不包含离群值的数据集
