##1、平行运算
library(parallel)
detectCores()
df <- as.data.frame(matrix(1:8e+6, ncol = 8))
cl <- makeCluster(8)
parApply(cl,df, 2, median)
stopCluster(cl)

##2、选中要测试的代码，然后按住快捷键ctrl+Alt+shift+p测试代码运行时间
#等同于如profvis::profvis({plot(price ~ carat, data = diamonds)})

##3、用R语言去看电脑是否需要更换
#3.1 检测计算机的相对性能,和其他计算机相比的计算速度
library("benchmarkme")
res_io = benchmark_io(runs = 1, size = 5)  #测试大数据电脑的运行速度，则将size = 50，测试写50M的时候电脑的速度
upload_results(res_io)
plot(res_io)
#3.2 检测计算机的CPU性能
res = benchmark_std(runs = 1)
upload_results(res)
plot(res)
