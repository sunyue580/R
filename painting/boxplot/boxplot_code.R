setwd('C:\\Users\\think\\Desktop\\data')

rt <- read.table("data.txt",header = T,row.names = NULL,sep = "\t")

library(ggplot2)

darkblue <- "#0772B9"
lightblue <- "#48C8EF"

p3 <- ggplot(rt, aes(drug, auc, fill=group)) + 
  geom_boxplot(aes(col = group),outlier.shape = NA) + 
  # geom_text(aes(drug, y=min(auc) * 1.1, 
  #               label=paste("p=",formatC(p,format = "e",digits = 1))),
  #           data=rt, 
  #           inherit.aes=F) + 
  geom_text(aes(drug, y=max(auc)), 
            label=rt$s,
            data=rt, 
            inherit.aes=F) + 
  scale_fill_manual(values = c(darkblue, lightblue)) + 
  scale_color_manual(values = c(darkblue, lightblue)) + 
  xlab(NULL) + ylab("Estimated AUC value") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5,vjust = 0.5,size = 10),
        legend.position = "bottom",
        legend.title = element_blank()) 
dat <- ggplot_build(p3)$data[[1]]

pdf("boxplot.pdf")
p3 + geom_segment(data=dat, aes(x=xmin, xend=xmax, y=middle, yend=middle), color="white", inherit.aes = F)
dev.off()

