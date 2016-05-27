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