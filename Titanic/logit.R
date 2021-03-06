training.data.raw<-read.csv('train.csv',header = T,na.strings = c(""))
sapply(training.data.raw, function(x) sum(is.na(x)))
sapply(training.data.raw, function(x) length(unique(x)))
library(Amelia)
missmap(training.data.raw, main = "Missing values vs observed")
#把Passengerld去掉，因为它只有索引和票。使用subset()函数，从原始数据集中只选择与之相关的列
train<-subset(training.data.raw,select = c(2,3,5,6,7,8,10,12))
#使用均值法填充缺失值
train$Age[is.na(train$Age)] <- mean(train$Age,na.rm=T)
train <- train[!is.na(train$Embarked),]
rownames(train) <- NULL

test.data.raw<-read.csv('test.csv',header = T, na.strings = c(""))
missmap(test.data.raw, main = "Missing values vs observed")
sapply(test.data.raw, function(x) sum(is.na(x)))
#把Passengerld去掉，因为它只有索引和票。使用subset()函数，从原始数据集中只选择与之相关的列
test<-subset(test.data.raw,select = c(2,4,5,6,7,9,11,12))
#使用均值法填充缺失值
test$Age[is.na(test$Age)] <- mean(test$Age,na.rm=T)
test <- test[!is.na(test$Fare),]
rownames(test) <- NULL


#模型拟合
model <- glm(Survived ~.,family=binomial(link='logit'),data = train)
summary(model)
#可以看到SibSp，Fare和Embarked并不是重要的变量。对于那些比较重要的变量，sex的p值最低，这说明sex与乘客的生存几率的关系是最强的。
 #这个负系数预测变量表明其它变量都一样，男乘客生存的机率更低。逻辑模型的因变量是对数机率：ln(odds) = ln(p/(1-p)) = ax1 + bx2 + … + z*xn。
  #male是一个优化变量，男性的生还机率下载2.75个对数机率，而年龄的增大则下降0.037个对数机率。

#接下来使用anova()函数来分析这个数据表的偏差：
anova(model, test="Chisq")
library(pscl)
pR2(model)

#预测
fitted.results <- predict(model,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))#精度为87%
#如果想要得到更精确的精度，需要执行一些交叉检验，例如k次交叉检验
#交叉验证
k = 5
train$id <- sample(1:k, nrow(train), replace = TRUE)
list <- 1:k
# 每次迭代的预测用数据框，测试用数据框
# the folds
prediction <- data.frame()
testsetCopy <- data.frame()
# 写一个进度条，用来了解CV的进度
progress.bar <- create_progress_bar("text")
progress.bar$init(k)
#k层的函数
for(i in 1:k){
  # 删除id为i的行，创建训练集
  # 选id为i的行，创建训练集
  trainingset <- subset(train, id %in% list[-i])
  testset <- subset(train, id %in% c(i))
  
  #运行一个逻辑回归模型
  mymodel <- glm(trainingset$Survived ~.,family=binomial(link='logit'),data = trainingset)
  
  #去掉回应列1, Survived
  temp <- predict(mymodel, newdata=testset[,-1],type='response')
  temp <- as.data.frame(ifelse(temp > 0.5,1,0))
  
  # 将迭代出的预测结果添加到预测数据框的末尾
  prediction <- rbind(prediction, temp)
  
  # 将迭代出的测试集结果添加到测试集数据框的末尾
  # 只保留Sepal Length一列
  testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,1]))
  
  progress.bar$step()
}
# 将预测和实际值放在一起
result <- cbind(prediction, testsetCopy[, 1])
names(result) <- c("Predicted", "Actual")
result$Difference <- mean(result$Actual != result$Predicted)
# 用误差的绝对平均值作为评估 
summary(result$Difference)


#最后一步，采用ROC曲线并计算AUC（曲线下的面积），它常用于预测二元分类器的模型表现。
#ROC曲线是一种曲线，它可以通过设定各种极值来让正例律（TPR)来抵消反正例律(FPR)，它就在ROC曲线之下。
 #通常来说，一个预测能力强的模型应当能让ROC接近1（1是理想的）而不是0.5。
library(ROCR)
pr <- prediction(fitted.results, test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


#数据探查
#生还与死亡对比
barplot(table(train$Survived),names.arg=c("死亡","生还"),
        main="生还 vs 死亡")
#不同舱位等级的影响
survive.rate.class <- table(train$Survived,train$Pclass)
barplot(survive.rate.class,names.arg=c("一等","二等","三等"),
        main="不同舱位生还 vs 死亡",
        legend.text=c("死亡","生还"),
        args.legend=list(x="topleft"))
#不同仓位生还率
round((survive.rate.class[2,]/colSums(survive.rate.class))*100,2)
#不同性别的生还率对比
survive.rate.sex <- table(train$Survived,train$Sex)
barplot(survive.rate.sex,names.arg=c("女","男"),
        main="不同性别生还 vs 死亡",
        legend.text=c("死亡","生还"),
        args.legend=list(x="topleft"))
round((survive.rate.sex[2,]/colSums(survive.rate.sex))*100,2)
#不同年龄的生还率
age.breaker <- c(0,18,50,100)
age.cut <- cut(train$Age,breaks=age.breaker,labels=c("小孩","成年人","老人"))
train$age.cut <- age.cut
survive.rate.age <- table(train$Survived,train$age.cut)
barplot(survive.rate.age,
        main="不同年龄生还 vs 死亡",
        legend.text=c("死亡","生还"),
        args.legend=list(x="topleft"))
round((survive.rate.age[2,]/colSums(survive.rate.age))*100,2)

#马赛克图，利用mosaicplot图
mosaicplot(train$Pclass ~ train$Survived,
           main="不同舱位等级生还 vs 死亡 ", shade=FALSE,
           color=TRUE,  xlab="舱位", ylab="生还")
mosaicplot(train$Sex ~ train$Survived,
           main="不同性别生还 vs 死亡 ", shade=FALSE,
           color=TRUE,  xlab="性别", ylab="生还")
#相关性分析
#前面大致可以得出性别，年龄，舱位对生还率有很大影响，那么其他的票价，上船的港口，在那个cabin等等变量对生还率有影响吗？这里用相关性分析来观察一下：
train.corrgram <- train
# 相关性分析要求全部为数字
train.corrgram$Survived <-  as.numeric(train.corrgram$Survived)
train.corrgram$Pclass <-  as.numeric(train.corrgram$Pclass)
train.corrgram$Embarked <-  as.numeric(train.corrgram$Embarked)
train.corrgram$Sex <- as.numeric(train.corrgram$Sex)
train.corrgram[which(is.na(train.corrgram$Embarked)),]$Embarked=3
cor(train.corrgram[,corrgram.vars])
## Error: object 'corrgram.vars' not found

#这里大致可以看出除了Pclass,Sex,Age以为，Fare还和生还率有关，而Embarked似乎也有一定关系（虽然理论上来讲登船地点应该与这个无关？）而Age由于有大量缺失值这里相关性为NA,下一步的目标似乎应该是在于如何处理这些缺失的数据上面。
#图形分析也许更简单快捷，直接使用corrgram来分析。
require(corrgram)
## Loading required package: corrgram
## Loading required package: seriation
# 字符变量这里不考虑
corrgram.vars = c("Survived", "Pclass",  "Sex", "Age",
                  "SibSp", "Parch", "Fare", "Embarked")
corrgram(train.corrgram[,corrgram.vars],  lower.panel=panel.ellipse, 
         upper.panel=panel.pie,text.panel=panel.txt,  main="泰坦尼克生还率相关性分析")
#http://www.aiweibang.com/yuedu/64709031.html