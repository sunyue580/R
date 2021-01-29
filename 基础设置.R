#显示英文报错信息
Sys.setenv(LANGUAGE = "en") 
options(stringsAsFactors = FALSE) #禁止chr转成factor

#中文乱码：
Sys.setlocale("LC_ALL","Chinese")

# 释放内存
gc() 

#获取.Renviron路径
Sys.getenv("HOME")

#设置R语言默认浏览器
options(browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
