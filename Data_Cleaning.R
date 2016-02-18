#脏数据的一般存在形式：1）缺失值2）异常值 3）数据的不一致性
#一、缺失值
#根据实际的业务需求不同，可以对缺失值采用不同的处理办法，如需要给会员推送短信，而某些会员恰好手机号不存在，可以考虑剔除；
 #如性别不知道，可以使用众数替代；如年龄未知，可以考虑用均值替换。当然还有其他处理缺失值的办法，如多重插补法

#模拟一批含缺失值的数据集
set.seed(1234)
Tel <- 13812341000:13812341999
Sex <- sample(c('F','M'), size = 1000, replace = T, prob = c(0.4,0.6))
Age <- round(runif(n = 1000, min = 18, max = 60))
Freq <- round(runif(n = 1000, min = 1, max = 368))
Amount <- rnorm(n = 1000, mean = 134, sd = 10)
ATV <- runif(n = 1000, min = 23, max = 138)
df <- data.frame(Tel = Tel, Sex = Sex, Age = Age, Freq = Freq, Amount = Amount, ATV = ATV)
#查看原始数据集的概要
summary(df)

#随机产生100个缺失值
#随机参数某行某列的下标
set.seed(1234)
i <- sample(1:6, size = 100, replace = T)
j <- sample(1:1000, size = 100)
#将下标组合成矩阵
index <- as.matrix(data.frame(j,i))
#将原始数据框转换为矩阵
df <- as.matrix(df)
#将随机参数的行列赋值为NA
df[index] <- NA
#重新将矩阵转换为数据框
df2 <- as.data.frame(df)
#变换变量类型
df2$Age <- as.integer(df2$Age)
df2$Freq <- as.integer(df2$Freq)
df2$Amount <- as.numeric(df2$Amount)
df2$ATV <- as.numeric(df2$ATV)
#再一次查看赋予缺失值后的数据框概要
summary(df2)


#使用VIM包中的aggr()函数绘制缺失值的分布情况
library(VIM)
aggr(df2, prop = FALSE, numbers = TRUE)

#缺失值处理：对Tel变量缺失的观测进行剔除；对Sex变量的缺失值用众数替换；
 #Age变量用平均值替换；Freq变量、Amount变量和ATV变量用多重插补法填充。
#剔除Tel变量的缺失观测
df3 <- df2[is.na(df2$Tel)==FALSE,]
#分别用众数和均值替换性别和年龄
#性别的众数
Sex_mode <- names(which.max(table(df3$Sex)))
#年龄的均值
Age_mean <- mean(df3$Age, na.rm = TRUE)
library(tidyr)
df3 <- replace_na(df3,replace = list(Sex = Sex_mode, Age = Age_mean))
summary(df3)

#通过mice包实现多重插补法，该包可以对数值型数据和因子型数据进行插补。
 #对于数值型数据，默认使用随机回归添补法(pmm)；对二元因子数据，默认使用Logistic回归添补法(logreg)；
 #对多元因子数据，默认使用分类回归添补法(polyreg)。
library(mice)
#对缺失值部分，进行5次的多重插补，这里默认使用随机回归添补法(pmm)
imp <- mice(data = df3, m = 5)
#查看一下插补的结果
imp$imp
#计算5重插补值的均值
Freq_imp <- apply(imp$imp$Freq,1,mean)
Amount_imp <- apply(imp$imp$Amount,1,mean)
ATV_imp <- apply(imp$imp$ATV,1,mean)
#并用该均值替换原来的缺失值
df3$Freq[is.na(df3$Freq)] <- Freq_imp
df3$Amount[is.na(df3$Amount)] <- Amount_imp
df3$ATV[is.na(df3$ATV)] <- ATV_imp
#再次查看填补完缺失值后的数据集和原始数据集概况
summary(df3)
summary(df2)


#二、异常值
#首先，需要识别出哪些值是异常值或离群点，其次如何处理这些异常值。
#1、识别异常值
#一般通过绘制盒形图来查看哪些点是离群点，而离群点的判断标准是四分位数与四分位距为基础。
 #即离群点超过上四分位数的1.5倍四分位距或低于下四分位数的1.5倍四分位距。

#随机产生一组数据
set.seed(1234)
value <- c(rnorm(100, mean = 10, sd = 3), runif(20, min = 0.01, max = 30), rf(30, df1 = 5, df2 = 20))
#绘制箱线图,并用红色的方块标注出异常值
library(ggplot2)
ggplot(data = NULL, mapping = aes(x = '', y = value)) + geom_boxplot(outlier.colour = 'red', outlier.shape = 15, width = 1.2)


#计算下四分位数、上四分位数和四分位距
QL <- quantile(value, probs = 0.25)
QU <- quantile(value, probs = 0.75)
QU_QL <- QU-QL
QL;QU;QU_QL

#2、找出异常点
which(value > QU + 1.5*QU_QL)
value[which(value > QU + 1.5*QU_QL)]

#用离异常点最近的点替换
test01 <- value
out_imp01 <- max(test01[which(test01 <= QU + 1.5*QU_QL)])
test01[which(test01 > QU + 1.5*QU_QL)] <- out_imp01

#用上四分位数的1.5倍四分位距或下四分位数的1.5倍四分位距替换
test02 <- value
out_imp02 <- QU + 1.5*QU_QL
test02[which(test02 > QU + 1.5*QU_QL)] <- out_imp02

#对比替换前后的数据概览
summary(value)
summary(test01)
summary(test02)





