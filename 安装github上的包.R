#github上下载yulab.utils-master.zip
devtools::install_local("C:\\Users\\think\\Desktop\\yulab.utils-master.zip")
library(yulab.utils)

#方法一：github上下载ggtree.zip,然后本地安装
install_zip("ggtree.zip")
#方法二：类似于devtools::install_github(),先从网上下载zip包，再调用install_zip()进行编译安装
install_zip_gh("yulab-smu/ggtree")


安装github包报错如下:
#Error: Failed to install 'unknown package' from GitHub:
#  Failed to connect to api.github.com port 443: Connection refused
解决：tools——terminal——new terminal
      窗口内输入git config --global http.proxy http://127.0.0.1:1080 
                或
                git config --global https.proxy http://127.0.0.1:1080
然后重新安装：devtools::install_github("zzwch/crosslinks")
