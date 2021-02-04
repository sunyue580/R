library(ggplot2)
#----------------------------------单数据系列极坐标柱形图-----------------------------------------
mydata <- data.frame(a=c("type1",
                         "type2",
                         "type3",
                         "type4"),
                     b=c(0.458624128,
                         0.49163067,
                         0.355987055,
                         2.02))
ggplot(mydata) +
  geom_bar(aes(x=a, y=b),width = 1,stat="identity",
           colour = "black",fill="#2d82b5") +
  coord_polar(theta = "x",start=0) +
  ylim(c(0,2.2))+
  labs(title = "baihua") +
  theme_light()+
  theme( panel.background = element_blank(),
         panel.grid.major = element_line(colour = "grey80",size=.25),
         axis.text.y = element_blank(),
         axis.line.y = element_line(size=0.25),
         axis.text.x=element_blank(),
         axis.title = element_blank(),
         plot.title = element_text(hjust = 0.5)
  ) 
ggsave("plot.pdf")
