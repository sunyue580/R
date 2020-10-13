1、按默认路径安装rtools
2、在RStudio里面运行以下脚本：
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
3、重新启动RStudio，然后运行以下代码：
Sys.which("make")
## "C:\\rtools40\\usr\\bin\\make.exe"
4、尝试安装一个包
install.packages("jsonlite", type = "source")
