data(Affairs,package = "AER")
df<-Affairs
df$ynaffairs<-ifelse(df$affairs>0,1,0)
table(df$ynaffairs)
df$ynaffairs<-factor(df$ynaffairs,
                     levels = c(0,1),
                     labels = c("No","Yes"))
table(df$ynaffairs)

fit.full<-glm(ynaffairs~gender+age+yearsmarried+
                children+religiousness+education+occupation+rating,
              data=df,family = binomial())
summary(fit.full)

##第一种
fit.reduced<-glm(ynaffairs~age+yearsmarried+
                   religiousness+rating,
                 data=df,family = binomial())

#anova()函数对它们进行比较，对于广义线性回归，可用卡方检验
anova(fit.full,fit.reduced,test = "Chisq")

#测试集
testdata<-data.frame(rating=c(1,2,3,4,5),
                     age=mean(df$age),
                     yearsmarried=mean(df$yearsmarried),
                     religiousness=mean(df$religiousness))

testdata$prob<-predict(fit.reduced,newdata = testdata,
                       type = "response")

library(ggplot2)
ggplot(testdata,aes(x=rating,y=prob))+
  geom_col(aes(fill=factor(rating)),show.legend = F)+
  geom_label(aes(label=round(prob,2)))+
  theme_bw()

##第二种
fit_step <- step(fit.full)
summary(fit_step)
#检测模型显著性——变量从前往后纳入，最后一个变量纳入模型时p值小于0.05，最终模型有显著性
anova(object = fit_step,test = "Chisq")
#模型系数
coef(fit_step)
#优势比OR值
exp(coef(fit_step))

#Logistic回归模型的拟合优度检验一般使用偏差卡方检验、皮尔逊卡方检验和HL统计量检验三种方法，其中前两种检验适合模型中只有离散的自变量，而后一种适合模型中包含连续的自变量。
#验证集
testdata <- df  #这里有测试集代替验证集
prob<-predict(object =fit_step,newdata=testdata,type = "response")
pred<-ifelse(prob>=0.5,"yes","no")
pred<-factor(pred,levels = c("no","yes"),order=TRUE)
table(testdata$ynaffairs,pred)

exp(coef(fit_step))

#ROC
library(pROC)
roc_curve <- roc(testdata$ynaffairs,prob)
#ROC画法一
plot(roc_curve, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)
#ROC画法二
names(roc_curve)
x <- 1-roc_curve$specificities
y <- roc_curve$sensitivities
library(ggplot2)
p <- ggplot(data = NULL, mapping = aes(x= x, y = y))
p + geom_line(colour = 'red') +geom_abline(intercept = 0, slope = 1)+ 
  annotate('text', x = 0.4, y = 0.5, label =paste('AUC=',round(roc_curve$auc,2))) +
  labs(x = '1-specificities',y = 'sensitivities', title = 'ROC Curve')

