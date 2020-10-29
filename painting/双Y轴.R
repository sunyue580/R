##自己生成一些测试数据
data<-sample(1:26)

x<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R", "S","T","U","V","W","X","Y","Z")

mydata <- data.frame(x=x,y=data)


library(ggplot2)
##最基本用ggplot2画barplot的方法
ggplot(mydata,aes(x,y))+geom_bar(stat="identity")+
  theme_classic()

p <- ggplot(mydata,aes(x,y))+geom_bar(stat="identity",aes(fill = x))+
  theme_classic()+
  scale_y_continuous(expand = c(0, 0),
                     sec.axis = sec_axis(~./2,name="a new y")
  )
p+geom_line(data=mydata,aes(x,y/2),group=1,color="red",size = 2)
