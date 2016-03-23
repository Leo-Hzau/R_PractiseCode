library(C50)
data(churn)
train <- churnTrain
test <- churnTest

#剔除无意义的区域编码变量及因变量
train <- churnTrain[,c(-1,-3,-20)]
test <- churnTest[,c(-1,-3,-20)]

#combine the data set
my_data <- rbind(train, test)
str(my_data)
#Since PCA works on numeric variables, let’s see if we have any variable other than numeric
library(dummies)
#create a dummy data frame
new_my_data <- dummy.data.frame(my_data, names = c("international_plan","voice_mail_plan"))
prin_comp <- prcomp(new_my_data, scale. = T)
names(prin_comp)
#outputs the mean of variables
prin_comp$center
#outputs the standard deviation of variables
prin_comp$scale
prin_comp$rotation
dim(prin_comp$x)
biplot(prin_comp, scale = 0)
#compute standard deviation of each principal component
std_dev <- prin_comp$sdev
#compute variance
pr_var <- std_dev^2
#check variance of first 10 components
pr_var[1:10]
#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:19]
#scree plot
plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")
#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")
#结果显示12个PC即可代表样本信息
#参考：http://www.analyticsvidhya.com/blog/2016/03/practical-guide-principal-component-analysis-python/?utm_source=feedburner&utm_medium=email&utm_campaign=Feed%3A+AnalyticsVidhya+%28Analytics+Vidhya%29


#在R里手工统计过程如下：
#数据集
y <- USArrests
#相关矩阵
c<-cor(y)
#特征值
e<-eigen(c)
e
e$values #特征值
e$vectors #特征向量，也就是主成分的表达式
# 计算标准化的主成分得分
scale( as.matrix(y))%*%e$vector

#R中下面两个函数可以用做主成分分析
princomp(x, cor = FALSE, scores = TRUE, covmat = NULL,
         subset = rep(TRUE, nrow(as.matrix(x))), ...)
cor =TRUE #是使用相关矩阵求主成分，否则使用协方差矩阵。
prcomp(x, retx = TRUE, center = TRUE, scale. = FALSE,
       tol = NULL, ...)
scale =TRUE #即使用相关矩阵求主成分,否则使用协方差矩阵求主成分。
# prcomp() 的用法
p=prcomp(USArrests, scale=T)
p
summary(p)
#计算标准化的主成分得分
predict(p) 
#结果和手工统计的一样。


