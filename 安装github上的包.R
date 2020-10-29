#github上下载yulab.utils-master.zip
devtools::install_local("C:\\Users\\think\\Desktop\\yulab.utils-master.zip")
library(yulab.utils)

#方法一：github上下载ggtree.zip,然后本地安装
install_zip("ggtree.zip")
#方法二：类似于devtools::install_github(),先从网上下载zip包，再调用install_zip()进行编译安装
install_zip_gh("yulab-smu/ggtree")
