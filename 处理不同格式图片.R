1、EBImage包——支持jpeg, png, tiff格式
#读取图片并保存成pdf
pdf("ex1.pdf")
img = readImage("ex1.jpg")
#display(img)
display(img, method="raster")
dev.off()

2、qpdf包
#多个pdf文件合并
qpdf::pdf_combine(c('heatmap1.pdf','heatmap2.pdf'),output = 'combine_12.pdf')



