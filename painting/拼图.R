1、ggpubr包——多个图共用同一个统一标度的legend
library(ggpubr)
ggarrange(p1, p2, p3,p4,ncol  = 2,nrow = 2,common.legend = TRUE,legend = "right")  
ggsave("go_kegg_scatter.pdf",width = 12,height = 10)
dev.off()

2、customLayout包：绘图布局

3、ggplotify::as.ggplot()

4、读取pdf图片并拼图
setwd('C:\\Users\\think\\Desktop\\Y\\FigureYa33DCA')
fnames<-Sys.glob("net_*.pdf")
p<-lapply(fnames,function(i){
  pn<-as.ggplot(image_read_pdf(i))
})
library(cowplot)
plot_grid(plotlist = p, ncol=2,
          labels = c("(A)","(B)"),
          label_size = 10, #A和B字体大小
          label_y = 0.75 #A和B的位置，默认值为1，太高
)
