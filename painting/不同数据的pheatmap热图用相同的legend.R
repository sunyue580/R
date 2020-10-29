set.seed(2017-11-12)
d = data.frame(matrix(rnorm(100), ncol=10))
colnames(d) = paste0('t', 1:10)
rownames(d) = paste0('g', 1:10)

d2 = abs(d) * 1.2

library(RColorBrewer)
cc = colorRampPalette(rev(brewer.pal(n = 7, 
                                     name = "RdYlBu")))
min(d) #-1.735683
max(d) #2.665766
min(d2) #0.02534463
max(d2) #3.198919

breaks = seq(min(unlist(c(d, d2))), 
             max(unlist(c(d, d2))), length.out=100)

library(pheatmap)

p1 = pheatmap(d, color=cc(100), breaks=breaks)
p2 = pheatmap(d2, color=cc(100), breaks=breaks)

cowplot::plot_grid(p1$gtable, p2$gtable, ncol=2)
