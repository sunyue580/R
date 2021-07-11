jmzeng1314/AnnoProbe
jmzeng1314/idmap3
hgu133a.db、hgu133plus2.db 

#基因注释R包代替平台文件
library(hgu133plus2.db)
ls("package:hgu133plus2.db")
ids=toTable(hgu133plus2SYMBOL)
head(ids)
