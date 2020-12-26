setwd('C:\\Users\\think\\Desktop\\data')

rt <- read.table("data.txt",header = T,row.names = NULL,sep = "\t")
rownames(rt) <- rt$drug

library(ggplot2)

darkblue <- "#0772B9"
lightblue <- "#48C8EF"

pdf("dot-line.pdf")
ggplot(data = rt,aes(r,forcats::fct_reorder(drug,r,.desc = T))) +
  geom_segment(aes(xend=0,yend=drug),linetype = 2) +
  geom_point(aes(size=p),col = darkblue) +
  scale_size_continuous(range =c(2,8)) +
  scale_x_reverse(breaks = c(0, -0.3, -0.5),
                  expand = expansion(mult = c(0.01,.1))) + #左右留空
  theme_classic() +
  labs(x = "Correlation coefficient", y = "", size = bquote("-log"[10]~"("~italic(P)~"-value)")) + 
  theme(legend.position = "bottom", 
        axis.line.y = element_blank())
dev.off()
