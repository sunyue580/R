1、ggpubr包——多个图共用同一个统一标度的legend
library(ggpubr)
ggarrange(p1, p2, p3,p4,ncol  = 2,nrow = 2,common.legend = TRUE,legend = "right")  
ggsave("go_kegg_scatter.pdf",width = 12,height = 10)
dev.off()

2、customLayout包：绘图布局

3、ggplotify::as.ggplot()
