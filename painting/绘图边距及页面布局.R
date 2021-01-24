####一、绘图边距设置
#外边距 margin设置 ，下左上右 ；mai(英寸边距)
par(oma=c(3,3,3,3)) 
#内边距 margin设置   下左上右       omi(英寸边距)
par(mar=c(6,5,4,3) + 0.1) # (坐标)标题超出后，可以适当设置 
#第一个元素为坐标轴位置到坐标轴标签的距离，以文本行高为单位。第二个元素为坐标轴位置到坐标刻度标签的距离。第三个元素为坐标轴位置到实际画的坐标轴的距离，通常是0。
par(mgp=c(4,2,0.5)) #根据情况调整
plot(1:10,bty="n") #不绘制边框，方便后面展示
#绘图区展示
text(3,7,"绘图区",col="red",cex=2)
text(4,9,"微信号：趁早一点点",col="red",cex=1)
box("plot",col="red",lty="dotted")
#内边距展示
mtext("内边距",side=3,line=2,cex=2,col="green")
box("figure",lty="solid",col="green")
#外边距展示 outer=TRUE
mtext("外边距",side=1,line=1,cex=2,col="blue",outer=TRUE,adj=0)
box("outer",col="blue")

####二、页面布局
##1)par 参数
par(mfrow=c(2,2)) #将画布分割为2*2格局
par(mfrow=c(3,1))# 将画布分割为3行，1列格局
plot(1:10);plot(1:10);boxplot(1:100)
##2)layout参数
#A：
layout(matrix(c(1,2,3,0,2,3,0,0,3),nr=3))  
layout.show(3) 
#B：按照矩阵编号进行分割，编号相同的为同一块
layout(matrix(c(1:3,3),2,2)) 
layout.show(3) #显示布局编号
#C：设置区块的宽度  高度比例
m<-matrix(c(1,1,2,1),2,2) #建立矩阵
layout(m,widths=c(2,1),heights=c(1,2)) #，宽度为2:1，高度为1:2
layout.show(2)
#D：0 不绘图，调整图形比例
m<-matrix(0:3,2,2)
layout(m,c(1,3),c(1,3)) #行为1:3,列为1:3
layout.show(3)



