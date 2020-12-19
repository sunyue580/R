#显示英文报错信息
Sys.setenv(LANGUAGE = "en") 
options(stringsAsFactors = FALSE) #禁止chr转成factor

#中文乱码：
Sys.setlocale("LC_ALL","Chinese")

# 释放内存
gc() 
